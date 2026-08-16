// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use getrandom::getrandom;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::num::NonZeroIsize;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError, Sender};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;
use webview2_com::AcceleratorKeyPressedEventHandler;
use webview2_com::Microsoft::Web::WebView2::Win32::{
    COREWEBVIEW2_KEY_EVENT_KIND, COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN,
    COREWEBVIEW2_KEY_EVENT_KIND_SYSTEM_KEY_DOWN,
};
use windows::Win32::Foundation::HWND;
use windows::Win32::UI::WindowsAndMessaging::{
    DispatchMessageW, IsWindow, MSG, PM_REMOVE, PeekMessageW, TranslateMessage,
};
use wry::dpi::{PhysicalPosition, PhysicalSize};
use wry::http::{Method, Request as HttpRequest, Response as HttpResponse};
use wry::raw_window_handle::{
    HandleError, HasWindowHandle, RawWindowHandle, Win32WindowHandle,
    WindowHandle,
};
use wry::{
    PageLoadEvent, PermissionResponse, Rect, WebView, WebViewBuilder,
    WebViewBuilderExtWindows, WebViewExtWindows,
};
use yunge_reader::epub::{EpubError, Publication};

use super::{BUILD_ID, Error};

const PROTOCOL_VERSION: u32 = 1;
const BOOK_PROTOCOL: &str = "yunge-reader-book";
const MAX_RESOURCE_REQUESTS: usize = 8;
const MAX_RESOURCE_URI_PATH_BYTES: usize = 196_605;
const RESOURCE_CSP: &str = concat!(
    "default-src 'none'; ",
    "img-src 'self' data:; ",
    "style-src 'self' 'unsafe-inline'; ",
    "font-src 'self' data:; ",
    "media-src 'self'; ",
    "script-src 'none'; ",
    "object-src 'none'; ",
    "frame-src 'none'; ",
    "connect-src 'none'; ",
    "base-uri 'none'; ",
    "form-action 'none'"
);
const CAPABILITIES: [&str; 14] = [
    "publication-close",
    "publication-info",
    "publication-open",
    "publication-resources",
    "view-bounds",
    "view-clear-selection",
    "view-create",
    "view-destroy",
    "view-events",
    "view-focus",
    "view-focus-parent",
    "view-info",
    "view-status",
    "view-visible",
];
const MAX_VIEW_EXTENT: u32 = 32_768;
const MESSAGE_PUMP_INTERVAL: Duration = Duration::from_millis(8);
const ESCAPE_VIRTUAL_KEY: u32 = 0x1b;
const CLEAR_SELECTION_SCRIPT: &str = r#"
const selection = window.getSelection();
if (selection) selection.removeAllRanges();
"#;

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Request {
    id: u64,
    op: String,
    #[serde(default)]
    params: Value,
}

#[derive(Debug, Serialize)]
struct ProtocolError {
    code: &'static str,
    message: String,
}

#[derive(Debug, Serialize)]
struct Response {
    id: Option<u64>,
    ok: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<ProtocolError>,
}

#[derive(Debug, Serialize)]
struct ViewEvent {
    kind: &'static str,
    event: &'static str,
    view: u64,
}

#[derive(Debug)]
struct ServiceError {
    code: &'static str,
    message: String,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct Bounds {
    x: i32,
    y: i32,
    width: u32,
    height: u32,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct CreateParams {
    view: u64,
    parent: u64,
    bounds: Bounds,
    #[serde(default = "default_visible")]
    visible: bool,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ViewParams {
    view: u64,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct PublicationOpenParams {
    path: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct PublicationParams {
    publication: u64,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct BoundsParams {
    view: u64,
    bounds: Bounds,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct VisibleParams {
    view: u64,
    visible: bool,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct EmptyParams {}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Control {
    Continue,
    Shutdown,
}

enum Incoming {
    Request(Request),
    Invalid(String),
}

struct ParentWindow(NonZeroIsize);

struct NativeView {
    webview: WebView,
    accelerator_token: i64,
    loaded: Arc<AtomicBool>,
    bounds: Bounds,
    visible: bool,
}

struct StoredPublication {
    token: String,
    publication: Publication,
}

#[derive(Default)]
struct PublicationStore {
    entries: HashMap<u64, StoredPublication>,
    tokens: HashMap<String, u64>,
}

type SharedPublications = Arc<Mutex<PublicationStore>>;

struct ResourcePermit(Arc<AtomicUsize>);

impl Drop for NativeView {
    fn drop(&mut self) {
        // SAFETY: The token was registered on this controller, and all
        // WebView operations run on the service's single UI thread.
        unsafe {
            let _ = self
                .webview
                .controller()
                .remove_AcceleratorKeyPressed(self.accelerator_token);
        }
    }
}

impl HasWindowHandle for ParentWindow {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        let handle = Win32WindowHandle::new(self.0);
        let raw = RawWindowHandle::Win32(handle);
        // SAFETY: `create_view' checks that the HWND names a live window.
        // Pinned Wry copies only the HWND on its Windows child path; this
        // private adapter is not exposed to other raw-handle consumers.
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

impl PublicationStore {
    fn insert(
        &mut self,
        id: u64,
        publication: Publication,
    ) -> Result<&StoredPublication, ServiceError> {
        let token = loop {
            let candidate = publication_token()?;
            if !self.tokens.contains_key(&candidate) {
                break candidate;
            }
        };
        self.tokens.insert(token.clone(), id);
        self.entries
            .insert(id, StoredPublication { token, publication });
        Ok(self
            .entries
            .get(&id)
            .expect("inserted publication is present"))
    }

    fn remove(&mut self, id: u64) -> Option<StoredPublication> {
        let publication = self.entries.remove(&id)?;
        self.tokens.remove(&publication.token);
        Some(publication)
    }

    fn by_token_mut(&mut self, token: &str) -> Option<&mut StoredPublication> {
        let id = self.tokens.get(token).copied()?;
        self.entries.get_mut(&id)
    }

    fn clear(&mut self) {
        self.entries.clear();
        self.tokens.clear();
    }
}

impl ResourcePermit {
    fn acquire(active: Arc<AtomicUsize>) -> Option<Self> {
        let mut current = active.load(Ordering::Acquire);
        loop {
            if current >= MAX_RESOURCE_REQUESTS {
                return None;
            }
            match active.compare_exchange_weak(
                current,
                current + 1,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => return Some(Self(active)),
                Err(updated) => current = updated,
            }
        }
    }
}

impl Drop for ResourcePermit {
    fn drop(&mut self) {
        self.0.fetch_sub(1, Ordering::AcqRel);
    }
}

struct Service {
    views: HashMap<u64, NativeView>,
    publications: SharedPublications,
    resource_requests: Arc<AtomicUsize>,
    next_publication: u64,
    version: Result<String, String>,
    event_sender: Sender<ViewEvent>,
}

impl ServiceError {
    fn new(code: &'static str, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
        }
    }
}

impl Response {
    fn success(id: u64, result: Value) -> Self {
        Self {
            id: Some(id),
            ok: true,
            result: Some(result),
            error: None,
        }
    }

    fn failure(
        id: Option<u64>,
        code: &'static str,
        message: impl Into<String>,
    ) -> Self {
        Self {
            id,
            ok: false,
            result: None,
            error: Some(ProtocolError {
                code,
                message: message.into(),
            }),
        }
    }
}

impl Bounds {
    fn validate(self) -> Result<Self, ServiceError> {
        if self.x < 0 || self.y < 0 {
            return Err(ServiceError::new(
                "invalid-view-bounds",
                "view position must be non-negative",
            ));
        }
        if self.width == 0 || self.height == 0 {
            return Err(ServiceError::new(
                "invalid-view-bounds",
                "view width and height must be positive",
            ));
        }
        if self.width > MAX_VIEW_EXTENT || self.height > MAX_VIEW_EXTENT {
            return Err(ServiceError::new(
                "invalid-view-bounds",
                format!(
                    "view width and height must not exceed {MAX_VIEW_EXTENT}"
                ),
            ));
        }
        Ok(self)
    }

    fn rect(self) -> Rect {
        Rect {
            position: PhysicalPosition::new(self.x, self.y).into(),
            size: PhysicalSize::new(self.width, self.height).into(),
        }
    }
}

impl Service {
    fn new(event_sender: Sender<ViewEvent>) -> Self {
        Self {
            views: HashMap::new(),
            publications: Arc::new(Mutex::new(PublicationStore::default())),
            resource_requests: Arc::new(AtomicUsize::new(0)),
            next_publication: 1,
            version: wry::webview_version().map_err(|error| error.to_string()),
            event_sender,
        }
    }

    fn parse<T: for<'de> Deserialize<'de>>(
        value: Value,
    ) -> Result<T, ServiceError> {
        let value = if value.is_null() { json!({}) } else { value };
        serde_json::from_value(value).map_err(|error| {
            ServiceError::new("invalid-params", error.to_string())
        })
    }

    fn info(&self, params: Value) -> Result<Value, ServiceError> {
        Self::parse::<EmptyParams>(params)?;
        Ok(info_result(&self.version))
    }

    fn open_publication(
        &mut self,
        params: Value,
    ) -> Result<Value, ServiceError> {
        let params: PublicationOpenParams = Self::parse(params)?;
        let path = std::path::Path::new(&params.path);
        if !path.is_absolute() {
            return Err(ServiceError::new(
                "invalid-publication-path",
                "publication path must be absolute",
            ));
        }
        let publication =
            Publication::open(path).map_err(ServiceError::from)?;
        let id = self.next_publication;
        self.next_publication = id.checked_add(1).ok_or_else(|| {
            ServiceError::new(
                "publication-id-exhausted",
                "no publication IDs remain",
            )
        })?;
        let mut publications = lock_publications(&self.publications)?;
        let publication = publications.insert(id, publication)?;
        let result = publication_result(id, publication);
        Ok(result)
    }

    fn publication_info(&self, params: Value) -> Result<Value, ServiceError> {
        let params: PublicationParams = Self::parse(params)?;
        let publications = lock_publications(&self.publications)?;
        let publication = publications
            .entries
            .get(&params.publication)
            .ok_or_else(|| unknown_publication(params.publication))?;
        Ok(publication_result(params.publication, publication))
    }

    fn close_publication(
        &mut self,
        params: Value,
    ) -> Result<Value, ServiceError> {
        let params: PublicationParams = Self::parse(params)?;
        let mut publications = lock_publications(&self.publications)?;
        if publications.remove(params.publication).is_none() {
            return Err(unknown_publication(params.publication));
        }
        Ok(json!({
            "publication": params.publication,
            "closed": true,
        }))
    }

    fn create_view(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: CreateParams = Self::parse(params)?;
        if self.views.contains_key(&params.view) {
            return Err(ServiceError::new(
                "duplicate-view",
                format!("view {} already exists", params.view),
            ));
        }
        if let Err(message) = &self.version {
            return Err(ServiceError::new(
                "webview-unavailable",
                dependency_message(message),
            ));
        }
        let parent_value = isize::try_from(params.parent).map_err(|_| {
            ServiceError::new(
                "invalid-parent-window",
                "parent window handle does not fit this process",
            )
        })?;
        let parent_value =
            NonZeroIsize::new(parent_value).ok_or_else(|| {
                ServiceError::new(
                    "invalid-parent-window",
                    "parent window handle must be nonzero",
                )
            })?;
        let parent_hwnd = HWND(parent_value.get() as _);
        if !unsafe { IsWindow(Some(parent_hwnd)) }.as_bool() {
            return Err(ServiceError::new(
                "invalid-parent-window",
                "parent window handle does not name a live window",
            ));
        }
        let bounds = params.bounds.validate()?;
        let parent = ParentWindow(parent_value);
        let html = spike_html(params.view);
        let loaded = Arc::new(AtomicBool::new(false));
        let load_state = Arc::clone(&loaded);
        let publications = Arc::clone(&self.publications);
        let resource_requests = Arc::clone(&self.resource_requests);
        let build = || {
            WebViewBuilder::new()
                .with_bounds(bounds.rect())
                .with_focused(false)
                .with_visible(params.visible)
                .with_asynchronous_custom_protocol(
                    BOOK_PROTOCOL.into(),
                    move |_webview_id, request, responder| {
                        let Some(permit) = ResourcePermit::acquire(Arc::clone(
                            &resource_requests,
                        )) else {
                            responder.respond(resource_error_response(
                                503,
                                "too many EPUB resource requests",
                            ));
                            return;
                        };
                        let publications = Arc::clone(&publications);
                        thread::spawn(move || {
                            let response =
                                resource_response(&publications, request);
                            responder.respond(response);
                            drop(permit);
                        });
                    },
                )
                .with_https_scheme(true)
                .with_html(html)
                .with_navigation_handler(|url| url == "about:blank")
                .with_on_page_load_handler(move |event, _url| {
                    if matches!(event, PageLoadEvent::Finished) {
                        load_state.store(true, Ordering::Release);
                    }
                })
                .with_permission_handler(|_| PermissionResponse::Deny)
                .build_as_child(&parent)
        };
        let view = catch_unwind(AssertUnwindSafe(build))
            .map_err(|_| {
                ServiceError::new(
                    "view-create-failed",
                    "WebView creation panicked for the supplied parent window",
                )
            })?
            .map_err(|error| {
                ServiceError::new("view-create-failed", error.to_string())
            })?;
        let accelerator_token = install_accelerator_handler(
            &view,
            params.view,
            self.event_sender.clone(),
        )?;
        self.views.insert(
            params.view,
            NativeView {
                webview: view,
                accelerator_token,
                loaded,
                bounds,
                visible: params.visible,
            },
        );
        Ok(json!({
            "view": params.view,
            "bounds": bounds,
            "visible": params.visible,
        }))
    }

    fn set_bounds(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: BoundsParams = Self::parse(params)?;
        let bounds = params.bounds.validate()?;
        let view = self.view_mut(params.view)?;
        view.webview.set_bounds(bounds.rect()).map_err(|error| {
            ServiceError::new("view-update-failed", error.to_string())
        })?;
        view.bounds = bounds;
        Ok(json!({ "view": params.view, "bounds": bounds }))
    }

    fn set_visible(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: VisibleParams = Self::parse(params)?;
        let view = self.view_mut(params.view)?;
        view.webview.set_visible(params.visible).map_err(|error| {
            ServiceError::new("view-update-failed", error.to_string())
        })?;
        view.visible = params.visible;
        Ok(json!({ "view": params.view, "visible": params.visible }))
    }

    fn focus(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: ViewParams = Self::parse(params)?;
        self.view(params.view)?.webview.focus().map_err(|error| {
            ServiceError::new("view-focus-failed", error.to_string())
        })?;
        Ok(json!({ "view": params.view, "focused": true }))
    }

    fn focus_parent(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: ViewParams = Self::parse(params)?;
        self.view(params.view)?
            .webview
            .focus_parent()
            .map_err(|error| {
                ServiceError::new("view-focus-failed", error.to_string())
            })?;
        Ok(json!({ "view": params.view, "focused": false }))
    }

    fn clear_selection(
        &mut self,
        params: Value,
    ) -> Result<Value, ServiceError> {
        let params: ViewParams = Self::parse(params)?;
        self.view(params.view)?
            .webview
            .evaluate_script(CLEAR_SELECTION_SCRIPT)
            .map_err(|error| {
                ServiceError::new("view-update-failed", error.to_string())
            })?;
        Ok(json!({ "view": params.view, "selection": false }))
    }

    fn destroy(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: ViewParams = Self::parse(params)?;
        if self.views.remove(&params.view).is_none() {
            return Err(unknown_view(params.view));
        }
        Ok(json!({ "view": params.view, "destroyed": true }))
    }

    fn status(&self, params: Value) -> Result<Value, ServiceError> {
        let params: ViewParams = Self::parse(params)?;
        let view = self.view(params.view)?;
        Ok(json!({
            "view": params.view,
            "loaded": view.loaded.load(Ordering::Acquire),
            "bounds": view.bounds,
            "visible": view.visible,
        }))
    }

    fn view(&self, id: u64) -> Result<&NativeView, ServiceError> {
        self.views.get(&id).ok_or_else(|| unknown_view(id))
    }

    fn view_mut(&mut self, id: u64) -> Result<&mut NativeView, ServiceError> {
        self.views.get_mut(&id).ok_or_else(|| unknown_view(id))
    }

    fn handle(&mut self, request: Request) -> (Response, Control) {
        if request.op == "shutdown" {
            let result =
                Self::parse::<EmptyParams>(request.params).and_then(|_| {
                    self.views.clear();
                    lock_publications(&self.publications)?.clear();
                    Ok(json!({ "stopped": true }))
                });
            let control = if result.is_ok() {
                Control::Shutdown
            } else {
                Control::Continue
            };
            return (response(request.id, result), control);
        }
        let result = match request.op.as_str() {
            "publication-open" => self.open_publication(request.params),
            "publication-info" => self.publication_info(request.params),
            "publication-close" => self.close_publication(request.params),
            "view-info" => self.info(request.params),
            "view-create" => self.create_view(request.params),
            "view-bounds" => self.set_bounds(request.params),
            "view-clear-selection" => self.clear_selection(request.params),
            "view-visible" => self.set_visible(request.params),
            "view-focus" => self.focus(request.params),
            "view-focus-parent" => self.focus_parent(request.params),
            "view-status" => self.status(request.params),
            "view-destroy" => self.destroy(request.params),
            _ => Err(ServiceError::new(
                "unsupported-operation",
                format!("unsupported operation: {}", request.op),
            )),
        };
        (response(request.id, result), Control::Continue)
    }
}

fn default_visible() -> bool {
    true
}

fn unknown_view(id: u64) -> ServiceError {
    ServiceError::new("unknown-view", format!("view {id} does not exist"))
}

fn publication_token() -> Result<String, ServiceError> {
    let mut bytes = [0_u8; 16];
    getrandom(&mut bytes).map_err(|error| {
        ServiceError::new(
            "publication-token-unavailable",
            format!("could not create a publication token: {error}"),
        )
    })?;
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut token = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        token.push(HEX[(byte >> 4) as usize] as char);
        token.push(HEX[(byte & 0x0f) as usize] as char);
    }
    Ok(token)
}

fn lock_publications(
    publications: &SharedPublications,
) -> Result<std::sync::MutexGuard<'_, PublicationStore>, ServiceError> {
    publications.lock().map_err(|_| {
        ServiceError::new(
            "publication-store-failed",
            "the publication store is unavailable",
        )
    })
}

fn resource_response(
    publications: &SharedPublications,
    request: HttpRequest<Vec<u8>>,
) -> HttpResponse<Vec<u8>> {
    if request.method() != Method::GET && request.method() != Method::HEAD {
        return resource_error_response(405, "method not allowed");
    }
    let (token, path) = match resource_request_target(request.uri()) {
        Ok(target) => target,
        Err(message) => return resource_error_response(400, message),
    };
    let mut publications = match publications.lock() {
        Ok(publications) => publications,
        Err(_) => {
            return resource_error_response(500, "publication store failed");
        }
    };
    let Some(publication) = publications.by_token_mut(&token) else {
        return resource_error_response(404, "publication not found");
    };
    let resource = match publication.publication.read_resource(&path) {
        Ok(resource) => resource,
        Err(error) => return epub_resource_error_response(error),
    };
    let content_length = resource.bytes().len();
    let media_type = resource.media_type().to_owned();
    let body = if request.method() == Method::HEAD {
        Vec::new()
    } else {
        resource.into_bytes()
    };
    build_resource_response(200, &media_type, content_length, body)
}

fn resource_request_target(
    uri: &wry::http::Uri,
) -> Result<(String, String), &'static str> {
    if uri.scheme_str() != Some(BOOK_PROTOCOL) {
        return Err("invalid EPUB resource scheme");
    }
    if uri.query().is_some() {
        return Err("EPUB resource queries are not supported");
    }
    let token = uri
        .authority()
        .map(|authority| authority.as_str())
        .filter(|authority| {
            authority.len() == 32
                && authority.bytes().all(|byte| {
                    byte.is_ascii_digit() || matches!(byte, b'a'..=b'f')
                })
        })
        .ok_or("invalid EPUB publication token")?;
    let encoded = uri
        .path()
        .strip_prefix('/')
        .filter(|path| !path.is_empty())
        .ok_or("EPUB resource path is empty")?;
    if encoded.len() > MAX_RESOURCE_URI_PATH_BYTES {
        return Err("EPUB resource path is too long");
    }
    validate_percent_encoding(encoded)?;
    let path = percent_encoding::percent_decode_str(encoded)
        .decode_utf8()
        .map_err(|_| "EPUB resource path is not UTF-8")?;
    if path.starts_with('/')
        || path.contains(['\\', '\0'])
        || path.split('/').any(|component| {
            component.is_empty() || matches!(component, "." | "..")
        })
    {
        return Err("EPUB resource path is not normalized");
    }
    Ok((token.to_owned(), path.into_owned()))
}

fn validate_percent_encoding(value: &str) -> Result<(), &'static str> {
    let bytes = value.as_bytes();
    let mut index = 0;
    while index < bytes.len() {
        if bytes[index] != b'%' {
            index += 1;
            continue;
        }
        if index + 2 >= bytes.len()
            || !bytes[index + 1].is_ascii_hexdigit()
            || !bytes[index + 2].is_ascii_hexdigit()
        {
            return Err("EPUB resource path has invalid percent encoding");
        }
        let first = bytes[index + 1].to_ascii_lowercase();
        let second = bytes[index + 2].to_ascii_lowercase();
        if matches!((first, second), (b'2', b'f') | (b'5', b'c') | (b'0', b'0'))
        {
            return Err("EPUB resource path encodes a separator");
        }
        index += 3;
    }
    Ok(())
}

fn epub_resource_error_response(error: EpubError) -> HttpResponse<Vec<u8>> {
    let status = match error.code() {
        "invalid-epub-path" => 400,
        "epub-resource-not-found" => 404,
        "epub-limit-exceeded" => 413,
        "unsupported-epub-resource" => 415,
        _ => 500,
    };
    resource_error_response(status, error.message())
}

fn resource_error_response(
    status: u16,
    message: impl AsRef<str>,
) -> HttpResponse<Vec<u8>> {
    let body = message.as_ref().as_bytes().to_vec();
    build_resource_response(
        status,
        "text/plain; charset=utf-8",
        body.len(),
        body,
    )
}

fn build_resource_response(
    status: u16,
    media_type: &str,
    content_length: usize,
    body: Vec<u8>,
) -> HttpResponse<Vec<u8>> {
    HttpResponse::builder()
        .status(status)
        .header("Content-Type", media_type)
        .header("Content-Length", content_length)
        .header("Cache-Control", "no-store")
        .header("Access-Control-Allow-Origin", "*")
        .header("X-Content-Type-Options", "nosniff")
        .header("Referrer-Policy", "no-referrer")
        .header("Content-Security-Policy", RESOURCE_CSP)
        .body(body)
        .expect("resource response headers are valid")
}

fn unknown_publication(id: u64) -> ServiceError {
    ServiceError::new(
        "unknown-publication",
        format!("publication {id} does not exist"),
    )
}

fn publication_result(id: u64, publication: &StoredPublication) -> Value {
    json!({
        "publication": id,
        "metadata": publication.publication.metadata(),
        "entry-count": publication.publication.entry_count(),
        "expanded-bytes": publication.publication.expanded_size(),
        "resource-root": format!(
            "{BOOK_PROTOCOL}://{}/",
            publication.token
        ),
    })
}

impl From<EpubError> for ServiceError {
    fn from(error: EpubError) -> Self {
        Self::new(error.code(), error.message())
    }
}

fn is_escape_key(kind: COREWEBVIEW2_KEY_EVENT_KIND, key: u32) -> bool {
    key == ESCAPE_VIRTUAL_KEY
        && (kind == COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN
            || kind == COREWEBVIEW2_KEY_EVENT_KIND_SYSTEM_KEY_DOWN)
}

fn install_accelerator_handler(
    webview: &WebView,
    view: u64,
    event_sender: Sender<ViewEvent>,
) -> Result<i64, ServiceError> {
    let handler = AcceleratorKeyPressedEventHandler::create(Box::new(
        move |_controller, args| {
            let Some(args) = args else {
                return Ok(());
            };
            let mut kind = COREWEBVIEW2_KEY_EVENT_KIND::default();
            let mut key = 0;
            // SAFETY: WebView2 owns the callback arguments for the duration
            // of this callback and initializes both out parameters.
            unsafe {
                args.KeyEventKind(&mut kind)?;
                args.VirtualKey(&mut key)?;
                if is_escape_key(kind, key) {
                    args.SetHandled(true)?;
                    let _ = event_sender.send(ViewEvent {
                        kind: "event",
                        event: "escape",
                        view,
                    });
                }
            }
            Ok(())
        },
    ));
    let mut token = 0;
    // SAFETY: The callback remains owned by the controller until its token
    // is removed when `NativeView' is dropped.
    unsafe {
        webview
            .controller()
            .add_AcceleratorKeyPressed(&handler, &mut token)
            .map_err(|error| {
                ServiceError::new("view-create-failed", error.to_string())
            })?;
    }
    Ok(token)
}

fn response(id: u64, result: Result<Value, ServiceError>) -> Response {
    match result {
        Ok(value) => Response::success(id, value),
        Err(error) => Response::failure(Some(id), error.code, error.message),
    }
}

fn dependency_message(detail: &str) -> String {
    format!(
        concat!(
            "Microsoft Edge WebView2 Runtime is unavailable; ",
            "install it and restart Yunge Reader ({})"
        ),
        detail
    )
}

fn info_result(version: &Result<String, String>) -> Value {
    match version {
        Ok(version) => json!({
            "platform": "windows",
            "engine": "webview2",
            "available": true,
            "version": version,
        }),
        Err(error) => json!({
            "platform": "windows",
            "engine": "webview2",
            "available": false,
            "message": dependency_message(error),
        }),
    }
}

fn ready_message(version: &Result<String, String>) -> Value {
    let mut ready = info_result(version);
    let object = ready.as_object_mut().expect("info is an object");
    object.insert("kind".into(), json!("webview-ready"));
    object.insert("protocol".into(), json!(PROTOCOL_VERSION));
    object.insert("build-id".into(), json!(BUILD_ID));
    object.insert("capabilities".into(), json!(CAPABILITIES));
    ready
}

fn spike_html(view: u64) -> String {
    format!(
        r#"<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta http-equiv="Content-Security-Policy"
      content="default-src 'none'; style-src 'unsafe-inline'">
<meta name="viewport" content="width=device-width, initial-scale=1">
<style>
html, body {{ margin: 0; min-height: 100%; background: #fafafa; }}
body {{ box-sizing: border-box; padding: 4rem; color: #202020;
       font: 20px/1.65 system-ui, sans-serif; }}
main {{ margin: auto; max-width: 42rem; }}
h1 {{ font-size: 2rem; line-height: 1.2; }}
code {{ font-family: ui-monospace, monospace; }}
</style>
</head>
<body>
<main>
<h1>Yunge Reader WebView spike</h1>
<p>This is native reflowable text in view <code>{view}</code>.</p>
<p>Resize or split the Emacs window, then select and copy this text.
Press Escape to clear the selection and return focus to Emacs.</p>
</main>
</body>
</html>"#
    )
}

fn write_message(
    mut output: impl Write,
    message: &impl Serialize,
) -> Result<(), Error> {
    serde_json::to_writer(&mut output, message)?;
    output.write_all(b"\n")?;
    output.flush()?;
    Ok(())
}

fn pump_messages() {
    let mut message = MSG::default();
    while unsafe { PeekMessageW(&mut message, None, 0, 0, PM_REMOVE) }.as_bool()
    {
        unsafe {
            let _ = TranslateMessage(&message);
            DispatchMessageW(&message);
        }
    }
}

fn write_events(
    receiver: &Receiver<ViewEvent>,
    output: &mut impl Write,
) -> Result<(), Error> {
    while let Ok(event) = receiver.try_recv() {
        write_message(&mut *output, &event)?;
    }
    Ok(())
}

pub(super) fn serve() -> Result<(), Error> {
    let (sender, receiver) = mpsc::channel();
    let (event_sender, event_receiver) = mpsc::channel();
    thread::spawn(move || {
        for line in io::stdin().lock().lines() {
            let incoming = match line {
                Ok(line) if line.trim().is_empty() => continue,
                Ok(line) => match serde_json::from_str(&line) {
                    Ok(request) => Incoming::Request(request),
                    Err(error) => Incoming::Invalid(error.to_string()),
                },
                Err(error) => Incoming::Invalid(error.to_string()),
            };
            if sender.send(incoming).is_err() {
                break;
            }
        }
    });

    let mut service = Service::new(event_sender);
    let stdout = io::stdout();
    let mut output = stdout.lock();
    write_message(&mut output, &ready_message(&service.version))?;
    loop {
        pump_messages();
        write_events(&event_receiver, &mut output)?;
        let incoming = match receiver.recv_timeout(MESSAGE_PUMP_INTERVAL) {
            Ok(incoming) => incoming,
            Err(RecvTimeoutError::Timeout) => continue,
            Err(RecvTimeoutError::Disconnected) => break,
        };
        let (response, control) = match incoming {
            Incoming::Request(request) => service.handle(request),
            Incoming::Invalid(message) => (
                Response::failure(None, "invalid-request", message),
                Control::Continue,
            ),
        };
        write_message(&mut output, &response)?;
        write_events(&event_receiver, &mut output)?;
        if control == Control::Shutdown {
            break;
        }
    }
    service.views.clear();
    if let Ok(mut publications) = service.publications.lock() {
        publications.clear();
    }
    pump_messages();
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::{self, File};
    use std::io::Write;
    use std::path::PathBuf;
    use std::sync::atomic::AtomicU64;
    use webview2_com::Microsoft::Web::WebView2::Win32 as WebView2;
    use zip::write::SimpleFileOptions;
    use zip::{CompressionMethod, ZipWriter};

    static TEMPORARY_ID: AtomicU64 = AtomicU64::new(1);

    struct TemporaryEpub(PathBuf);

    impl Drop for TemporaryEpub {
        fn drop(&mut self) {
            let _ = fs::remove_file(&self.0);
        }
    }

    fn test_epub() -> TemporaryEpub {
        let id = TEMPORARY_ID.fetch_add(1, Ordering::Relaxed);
        let path = std::env::temp_dir().join(format!(
            "yunge-reader-webview-{}-{id}.epub",
            std::process::id()
        ));
        let container = concat!(
            "<container xmlns=\"",
            "urn:oasis:names:tc:opendocument:xmlns:container",
            "\" version=\"1.0\"><rootfiles>",
            "<rootfile full-path=\"OPS/package.opf\" media-type=\"",
            "application/oebps-package+xml",
            "\"/></rootfiles></container>"
        );
        let package = concat!(
            "<package xmlns=\"http://www.idpf.org/2007/opf\" ",
            "version=\"3.0\"><metadata xmlns:dc=\"",
            "http://purl.org/dc/elements/1.1/",
            "\"><dc:title>Protocol Book</dc:title>",
            "</metadata><manifest>",
            "<item id=\"chapter\" href=\"chapter.xhtml\" media-type=\"",
            "application/xhtml+xml\"/>",
            "</manifest></package>"
        );
        let file = File::create(&path).unwrap();
        let mut archive = ZipWriter::new(file);
        for (name, contents, method) in [
            (
                "mimetype",
                "application/epub+zip",
                CompressionMethod::Stored,
            ),
            (
                "META-INF/container.xml",
                container,
                CompressionMethod::Deflated,
            ),
            ("OPS/package.opf", package, CompressionMethod::Deflated),
            (
                "OPS/chapter.xhtml",
                "<html><body>Chapter</body></html>",
                CompressionMethod::Deflated,
            ),
        ] {
            let options =
                SimpleFileOptions::default().compression_method(method);
            archive.start_file(name, options).unwrap();
            archive.write_all(contents.as_bytes()).unwrap();
        }
        archive.finish().unwrap();
        TemporaryEpub(path)
    }

    #[test]
    fn bounds_reject_empty_negative_and_extreme_rectangles() {
        for bounds in [
            Bounds {
                x: -1,
                y: 0,
                width: 1,
                height: 1,
            },
            Bounds {
                x: 0,
                y: 0,
                width: 0,
                height: 1,
            },
            Bounds {
                x: 0,
                y: 0,
                width: MAX_VIEW_EXTENT + 1,
                height: 1,
            },
        ] {
            assert!(bounds.validate().is_err());
        }
    }

    #[test]
    fn spike_page_is_self_contained_and_identifies_the_view() {
        let html = spike_html(42);
        assert!(html.contains("view <code>42</code>"));
        assert!(html.contains("default-src 'none'"));
        assert!(!html.contains("http://"));
        assert!(!html.contains("https://"));
    }

    #[test]
    fn omitted_empty_parameters_are_accepted() {
        Service::parse::<EmptyParams>(Value::Null).unwrap();
    }

    #[test]
    fn publication_operations_own_query_and_release_one_epub() {
        let epub = test_epub();
        let (sender, _receiver) = mpsc::channel();
        let mut service = Service::new(sender);
        let path = epub.0.to_string_lossy().into_owned();
        let (opened, control) = service.handle(Request {
            id: 1,
            op: "publication-open".into(),
            params: json!({ "path": path }),
        });

        assert_eq!(control, Control::Continue);
        assert!(opened.ok);
        let result = opened.result.unwrap();
        assert_eq!(result["publication"], 1);
        assert_eq!(result["metadata"]["package-path"], "OPS/package.opf");
        assert_eq!(result["metadata"]["title"], "Protocol Book");
        assert_eq!(result["entry-count"], 4);
        assert!(result["expanded-bytes"].as_u64().unwrap() > 0);
        let resource_root =
            result["resource-root"].as_str().unwrap().to_owned();
        assert!(resource_root.starts_with("yunge-reader-book://"));

        let resource = resource_response(
            &service.publications,
            HttpRequest::builder()
                .uri(format!("{resource_root}OPS/chapter.xhtml"))
                .body(Vec::new())
                .unwrap(),
        );
        assert_eq!(resource.status(), 200);
        assert_eq!(resource.headers()["Content-Type"], "application/xhtml+xml");
        assert_eq!(resource.headers()["Cache-Control"], "no-store");
        assert_eq!(resource.headers()["X-Content-Type-Options"], "nosniff");
        assert!(
            resource.headers()["Content-Security-Policy"]
                .to_str()
                .unwrap()
                .contains("script-src 'none'")
        );
        assert_eq!(resource.body(), b"<html><body>Chapter</body></html>");

        let head = resource_response(
            &service.publications,
            HttpRequest::builder()
                .method(Method::HEAD)
                .uri(format!("{resource_root}OPS/chapter.xhtml"))
                .body(Vec::new())
                .unwrap(),
        );
        assert_eq!(head.status(), 200);
        assert_eq!(head.headers()["Content-Length"], "33");
        assert!(head.body().is_empty());

        let (info, _) = service.handle(Request {
            id: 2,
            op: "publication-info".into(),
            params: json!({ "publication": 1 }),
        });
        let info = info.result.unwrap();
        assert_eq!(info["metadata"]["title"], "Protocol Book");
        assert_eq!(info["resource-root"], resource_root);

        let (closed, _) = service.handle(Request {
            id: 3,
            op: "publication-close".into(),
            params: json!({ "publication": 1 }),
        });
        assert_eq!(closed.result.unwrap()["closed"], true);

        let released = resource_response(
            &service.publications,
            HttpRequest::builder()
                .uri(format!("{resource_root}OPS/chapter.xhtml"))
                .body(Vec::new())
                .unwrap(),
        );
        assert_eq!(released.status(), 404);

        let (missing, _) = service.handle(Request {
            id: 4,
            op: "publication-info".into(),
            params: json!({ "publication": 1 }),
        });
        assert_eq!(missing.error.unwrap().code, "unknown-publication");
    }

    #[test]
    fn publication_open_requires_an_absolute_strict_path() {
        let (sender, _receiver) = mpsc::channel();
        let mut service = Service::new(sender);
        let (relative, _) = service.handle(Request {
            id: 1,
            op: "publication-open".into(),
            params: json!({ "path": "book.epub" }),
        });
        assert_eq!(relative.error.unwrap().code, "invalid-publication-path");

        let (unknown, _) = service.handle(Request {
            id: 2,
            op: "publication-open".into(),
            params: json!({ "path": "book.epub", "extra": true }),
        });
        assert_eq!(unknown.error.unwrap().code, "invalid-params");
    }

    #[test]
    fn resource_protocol_rejects_unsafe_targets_and_methods() {
        let token = "0123456789abcdef0123456789abcdef";
        let valid = format!("{BOOK_PROTOCOL}://{token}/OPS/chapter.xhtml");
        assert_eq!(
            resource_request_target(&valid.parse().unwrap()).unwrap(),
            (token.into(), "OPS/chapter.xhtml".into())
        );
        for uri in [
            format!("{BOOK_PROTOCOL}://short/OPS/chapter.xhtml"),
            format!("{BOOK_PROTOCOL}://{token}/"),
            format!("{BOOK_PROTOCOL}://{token}/OPS/%2e%2e/chapter.xhtml"),
            format!("{BOOK_PROTOCOL}://{token}/OPS/a%2fchapter.xhtml"),
            format!("{BOOK_PROTOCOL}://{token}/OPS/chapter.xhtml?x=1"),
        ] {
            let parsed = uri.parse().unwrap();
            assert!(
                resource_request_target(&parsed).is_err(),
                "accepted {uri}"
            );
        }

        let (sender, _receiver) = mpsc::channel();
        let service = Service::new(sender);
        let response = resource_response(
            &service.publications,
            HttpRequest::builder()
                .method(Method::POST)
                .uri(valid)
                .body(Vec::new())
                .unwrap(),
        );
        assert_eq!(response.status(), 405);
    }

    #[test]
    fn resource_request_permits_are_bounded_and_released() {
        let active = Arc::new(AtomicUsize::new(0));
        let permits: Vec<_> = (0..MAX_RESOURCE_REQUESTS)
            .map(|_| ResourcePermit::acquire(Arc::clone(&active)).unwrap())
            .collect();
        assert!(ResourcePermit::acquire(Arc::clone(&active)).is_none());
        drop(permits);
        assert!(ResourcePermit::acquire(active).is_some());
    }

    #[test]
    fn escape_is_the_only_routed_accelerator() {
        assert!(is_escape_key(
            COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN,
            ESCAPE_VIRTUAL_KEY,
        ));
        assert!(is_escape_key(
            COREWEBVIEW2_KEY_EVENT_KIND_SYSTEM_KEY_DOWN,
            ESCAPE_VIRTUAL_KEY,
        ));
        assert!(!is_escape_key(
            WebView2::COREWEBVIEW2_KEY_EVENT_KIND_KEY_UP,
            ESCAPE_VIRTUAL_KEY,
        ));
        assert!(!is_escape_key(
            COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN,
            u32::from(b'J'),
        ));
        assert!(CLEAR_SELECTION_SCRIPT.contains("removeAllRanges"));
    }
}
