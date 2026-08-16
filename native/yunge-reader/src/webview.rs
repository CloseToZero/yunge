// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use getrandom::getrandom;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::borrow::Cow;
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
    NewWindowResponse, PageLoadEvent, PermissionResponse, Rect, WebView,
    WebViewBuilder, WebViewBuilderExtWindows, WebViewExtWindows,
};
use yunge_reader::epub::{EpubError, Publication};

use super::{BUILD_ID, Error};

const PROTOCOL_VERSION: u32 = 1;
const APP_PROTOCOL: &str = "yunge-reader-app";
const APP_URL: &str = "yunge-reader-app://localhost/index.html";
const APP_BROWSER_URL: &str = "https://yunge-reader-app.localhost/index.html";
const APP_BROWSER_ORIGIN: &str = "https://yunge-reader-app.localhost";
const BOOK_PROTOCOL: &str = "yunge-reader-book";
const BOOK_BROWSER_ROOT: &str = "https://yunge-reader-book.localhost/";
const APP_CSP: &str = concat!(
    "default-src 'none'; ",
    "script-src 'self'; ",
    "style-src 'self' blob: 'unsafe-inline'; ",
    "img-src blob: data:; ",
    "font-src blob: data:; ",
    "media-src blob: data:; ",
    "connect-src https://yunge-reader-book.localhost; ",
    "frame-src blob:; ",
    "object-src 'none'; ",
    "worker-src 'none'; ",
    "base-uri 'none'; ",
    "form-action 'none'"
);
const MAX_RESOURCE_REQUESTS: usize = 8;
const MAX_RESOURCE_CATALOG_BYTES: usize = 16 * 1_024 * 1_024;
const MAX_RESOURCE_URI_PATH_BYTES: usize = 196_605;
const MAX_EPUB_LOCATOR_TEXT_BYTES: usize = 3_072;
const MAX_RENDERER_MESSAGE_BYTES: usize = 8 * 1_024;
const MAX_RENDERER_ERROR_BYTES: usize = 4 * 1_024;
const RESOURCE_CATALOG_PATH: &str = ".yunge/resources.json";
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
const CAPABILITIES: [&str; 16] = [
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
    "view-navigate",
    "view-open-publication",
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
    #[serde(skip_serializing_if = "Option::is_none")]
    message: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    location: Option<EpubLocator>,
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
struct ViewPublicationParams {
    view: u64,
    publication: u64,
    #[serde(default)]
    location: Option<EpubLocator>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct EpubLocator {
    cfi: String,
    href: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    fraction: Option<f64>,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
enum NavigationCommand {
    PreviousScreen,
    NextScreen,
    GoTo,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ViewNavigateParams {
    view: u64,
    command: NavigationCommand,
    #[serde(default)]
    location: Option<EpubLocator>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct RendererMessage {
    protocol: u32,
    event: RendererEvent,
    #[serde(default)]
    message: Option<String>,
    #[serde(default)]
    location: Option<EpubLocator>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
enum RendererEvent {
    Location,
    NavigationError,
    PublicationError,
    PublicationReady,
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
    publication: Option<u64>,
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

impl EpubLocator {
    fn validate(self) -> Result<Self, ServiceError> {
        for (name, value) in [("CFI", &self.cfi), ("href", &self.href)] {
            if value.is_empty()
                || value.len() > MAX_EPUB_LOCATOR_TEXT_BYTES
                || value.chars().any(char::is_control)
            {
                return Err(ServiceError::new(
                    "invalid-epub-location",
                    format!("EPUB locator {name} is invalid"),
                ));
            }
        }
        if !self.cfi.starts_with("epubcfi(") || !self.cfi.ends_with(')') {
            return Err(ServiceError::new(
                "invalid-epub-location",
                "EPUB locator CFI is invalid",
            ));
        }
        if self.href.starts_with('/')
            || self.href.contains(['\\', ':', '?', '#'])
            || self
                .href
                .split('/')
                .any(|part| part.is_empty() || matches!(part, "." | ".."))
        {
            return Err(ServiceError::new(
                "invalid-epub-location",
                "EPUB locator href is not a canonical relative path",
            ));
        }
        if self.fraction.is_some_and(|value| {
            !value.is_finite() || !(0.0..=1.0).contains(&value)
        }) {
            return Err(ServiceError::new(
                "invalid-epub-location",
                "EPUB locator fraction must be between zero and one",
            ));
        }
        Ok(self)
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
        if self
            .views
            .values()
            .any(|view| view.publication == Some(params.publication))
        {
            return Err(ServiceError::new(
                "publication-in-use",
                format!(
                    "publication {} is attached to a live view",
                    params.publication
                ),
            ));
        }
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
        let loaded = Arc::new(AtomicBool::new(false));
        let load_state = Arc::clone(&loaded);
        let publications = Arc::clone(&self.publications);
        let resource_requests = Arc::clone(&self.resource_requests);
        let renderer_events = self.event_sender.clone();
        let view_id = params.view;
        let build = || {
            WebViewBuilder::new()
                .with_bounds(bounds.rect())
                .with_focused(false)
                .with_visible(params.visible)
                .with_custom_protocol(
                    APP_PROTOCOL.into(),
                    move |_webview_id, request| app_response(request),
                )
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
                .with_url(APP_URL)
                .with_navigation_handler(app_navigation_allowed)
                .with_on_page_load_handler(move |event, _url| {
                    if matches!(event, PageLoadEvent::Finished) {
                        load_state.store(true, Ordering::Release);
                    }
                })
                .with_ipc_handler(move |request| {
                    if let Some(event) = renderer_event(view_id, &request) {
                        let _ = renderer_events.send(event);
                    }
                })
                .with_permission_handler(|_| PermissionResponse::Deny)
                .with_new_window_req_handler(|_, _| NewWindowResponse::Deny)
                .with_download_started_handler(|_, _| false)
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
                publication: None,
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

    fn open_view_publication(
        &mut self,
        params: Value,
    ) -> Result<Value, ServiceError> {
        let params: ViewPublicationParams = Self::parse(params)?;
        let location =
            params.location.map(EpubLocator::validate).transpose()?;
        let view = self.view(params.view)?;
        if !view.loaded.load(Ordering::Acquire) {
            return Err(ServiceError::new(
                "view-not-ready",
                format!("view {} has not finished loading", params.view),
            ));
        }
        let resource_root = {
            let publications = lock_publications(&self.publications)?;
            let publication = publications
                .entries
                .get(&params.publication)
                .ok_or_else(|| unknown_publication(params.publication))?;
            format!("{BOOK_BROWSER_ROOT}{}/", publication.token)
        };
        let script = publication_open_script(
            params.view,
            &resource_root,
            location.as_ref(),
        );
        self.view(params.view)?
            .webview
            .evaluate_script(&script)
            .map_err(|error| {
                ServiceError::new(
                    "publication-render-failed",
                    error.to_string(),
                )
            })?;
        self.view_mut(params.view)?.publication = Some(params.publication);
        Ok(json!({
            "view": params.view,
            "publication": params.publication,
            "opening": true,
        }))
    }

    fn navigate_view(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: ViewNavigateParams = Self::parse(params)?;
        let location =
            params.location.map(EpubLocator::validate).transpose()?;
        match (params.command, location.as_ref()) {
            (NavigationCommand::GoTo, None) => {
                return Err(ServiceError::new(
                    "invalid-params",
                    "go-to navigation requires an EPUB location",
                ));
            }
            (NavigationCommand::GoTo, Some(_)) => {}
            (_, Some(_)) => {
                return Err(ServiceError::new(
                    "invalid-params",
                    "screen navigation does not accept an EPUB location",
                ));
            }
            (_, None) => {}
        }
        let view = self.view(params.view)?;
        if view.publication.is_none() {
            return Err(ServiceError::new(
                "view-has-no-publication",
                format!("view {} has no attached publication", params.view),
            ));
        }
        let script = publication_navigation_script(
            params.view,
            params.command,
            location.as_ref(),
        );
        view.webview.evaluate_script(&script).map_err(|error| {
            ServiceError::new("view-update-failed", error.to_string())
        })?;
        Ok(json!({
            "view": params.view,
            "command": params.command,
            "navigating": true,
        }))
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
            "publication": view.publication,
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
            "view-navigate" => self.navigate_view(request.params),
            "view-open-publication" => {
                self.open_view_publication(request.params)
            }
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

fn app_asset(path: &str) -> Option<(&'static str, &'static [u8])> {
    match path {
        "index.html" => Some((
            "text/html; charset=utf-8",
            include_bytes!("../renderer/index.html"),
        )),
        "style.css" => Some((
            "text/css; charset=utf-8",
            include_bytes!("../renderer/style.css"),
        )),
        "yunge-reader.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/yunge-reader.js"),
        )),
        "foliate-js/epub.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/epub.js"),
        )),
        "foliate-js/epubcfi.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/epubcfi.js"),
        )),
        "foliate-js/fixed-layout.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/fixed-layout.js"),
        )),
        "foliate-js/overlayer.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/overlayer.js"),
        )),
        "foliate-js/paginator.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/paginator.js"),
        )),
        "foliate-js/progress.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/progress.js"),
        )),
        "foliate-js/text-walker.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/text-walker.js"),
        )),
        "foliate-js/view.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/view.js"),
        )),
        _ => None,
    }
}

fn app_response(
    request: HttpRequest<Vec<u8>>,
) -> HttpResponse<Cow<'static, [u8]>> {
    if request.method() != Method::GET && request.method() != Method::HEAD {
        return app_error_response(405, "method not allowed");
    }
    let uri = request.uri();
    if uri.scheme_str() != Some(APP_PROTOCOL)
        || uri.authority().map(|value| value.as_str()) != Some("localhost")
        || uri.query().is_some()
    {
        return app_error_response(400, "invalid renderer asset target");
    }
    let Some(path) = uri.path().strip_prefix('/') else {
        return app_error_response(400, "invalid renderer asset path");
    };
    let Some((media_type, asset)) = app_asset(path) else {
        return app_error_response(404, "renderer asset not found");
    };
    let body: Cow<'static, [u8]> = if request.method() == Method::HEAD {
        Cow::Borrowed(&[])
    } else {
        Cow::Borrowed(asset)
    };
    build_app_response(200, media_type, asset.len(), body)
}

fn app_error_response(
    status: u16,
    message: &'static str,
) -> HttpResponse<Cow<'static, [u8]>> {
    build_app_response(
        status,
        "text/plain; charset=utf-8",
        message.len(),
        Cow::Borrowed(message.as_bytes()),
    )
}

fn build_app_response(
    status: u16,
    media_type: &str,
    content_length: usize,
    body: Cow<'static, [u8]>,
) -> HttpResponse<Cow<'static, [u8]>> {
    HttpResponse::builder()
        .status(status)
        .header("Content-Type", media_type)
        .header("Content-Length", content_length)
        .header("Cache-Control", "no-store")
        .header("X-Content-Type-Options", "nosniff")
        .header("Referrer-Policy", "no-referrer")
        .header("Content-Security-Policy", APP_CSP)
        .body(body)
        .expect("renderer response headers are valid")
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
    if path == RESOURCE_CATALOG_PATH {
        let body = match serde_json::to_vec(&json!({
            "resources": publication.publication.resource_catalog(),
        })) {
            Ok(body) if body.len() <= MAX_RESOURCE_CATALOG_BYTES => body,
            Ok(_) => {
                return resource_error_response(
                    413,
                    "EPUB resource catalog is too large",
                );
            }
            Err(_) => {
                return resource_error_response(
                    500,
                    "could not encode EPUB resource catalog",
                );
            }
        };
        return resource_body_response(
            request.method(),
            "application/json; charset=utf-8",
            body,
        );
    }
    let resource = match publication.publication.read_resource(&path) {
        Ok(resource) => resource,
        Err(error) => return epub_resource_error_response(error),
    };
    let media_type = resource.media_type().to_owned();
    resource_body_response(request.method(), &media_type, resource.into_bytes())
}

fn resource_body_response(
    method: &Method,
    media_type: &str,
    bytes: Vec<u8>,
) -> HttpResponse<Vec<u8>> {
    let content_length = bytes.len();
    let body = if method == Method::HEAD {
        Vec::new()
    } else {
        bytes
    };
    build_resource_response(200, media_type, content_length, body)
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
    uri.authority()
        .map(|authority| authority.as_str())
        .filter(|authority| *authority == "localhost")
        .ok_or("invalid EPUB resource authority")?;
    let encoded = uri
        .path()
        .strip_prefix('/')
        .filter(|path| !path.is_empty())
        .ok_or("EPUB resource path is empty")?;
    if encoded.len() > MAX_RESOURCE_URI_PATH_BYTES {
        return Err("EPUB resource path is too long");
    }
    let (token, encoded) = encoded
        .split_once('/')
        .filter(|(token, path)| {
            token.len() == 32
                && token.bytes().all(|byte| {
                    byte.is_ascii_digit() || matches!(byte, b'a'..=b'f')
                })
                && !path.is_empty()
        })
        .ok_or("invalid EPUB publication token")?;
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
        .header("Access-Control-Allow-Origin", APP_BROWSER_ORIGIN)
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
            "{BOOK_PROTOCOL}://localhost/{}/",
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
                        message: None,
                        location: None,
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

fn app_navigation_allowed(url: String) -> bool {
    matches!(url.as_str(), APP_URL | APP_BROWSER_URL)
}

fn renderer_event(
    view: u64,
    request: &HttpRequest<String>,
) -> Option<ViewEvent> {
    if !app_navigation_allowed(request.uri().to_string())
        || request.body().len() > MAX_RENDERER_MESSAGE_BYTES
    {
        return None;
    }
    let message: RendererMessage = serde_json::from_str(request.body()).ok()?;
    if message.protocol != PROTOCOL_VERSION
        || message
            .message
            .as_ref()
            .is_some_and(|value| value.len() > MAX_RENDERER_ERROR_BYTES)
    {
        return None;
    }
    let (event, detail, location) = match message.event {
        RendererEvent::Location => {
            if message.message.is_some() {
                return None;
            }
            let location = message.location?.validate().ok()?;
            ("location", None, Some(location))
        }
        RendererEvent::NavigationError => {
            if message.location.is_some() {
                return None;
            }
            let detail = message.message.filter(|value| !value.is_empty())?;
            ("navigation-error", Some(detail), None)
        }
        RendererEvent::PublicationReady => {
            if message.message.is_some() {
                return None;
            }
            let location = message.location?.validate().ok()?;
            ("publication-ready", None, Some(location))
        }
        RendererEvent::PublicationError => {
            if message.location.is_some() {
                return None;
            }
            let detail = message.message.filter(|value| !value.is_empty())?;
            ("publication-error", Some(detail), None)
        }
    };
    Some(ViewEvent {
        kind: "event",
        event,
        view,
        message: detail,
        location,
    })
}

fn publication_open_script(
    view: u64,
    resource_root: &str,
    location: Option<&EpubLocator>,
) -> String {
    let payload = serde_json::to_string(&json!({
        "view": view,
        "resourceRoot": resource_root,
        "location": location,
    }))
    .expect("publication open payload is serializable");
    format!("void globalThis.yungeReader.open({payload});")
}

fn publication_navigation_script(
    view: u64,
    command: NavigationCommand,
    location: Option<&EpubLocator>,
) -> String {
    let payload = serde_json::to_string(&json!({
        "view": view,
        "command": command,
        "location": location,
    }))
    .expect("publication navigation payload is serializable");
    format!("void globalThis.yungeReader.navigate({payload});")
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
    fn renderer_assets_are_bounded_and_script_safe() {
        let response = app_response(
            HttpRequest::builder()
                .uri(APP_URL)
                .body(Vec::new())
                .unwrap(),
        );
        assert_eq!(response.status(), 200);
        assert_eq!(response.headers()["Cache-Control"], "no-store");
        assert!(
            response.headers()["Content-Security-Policy"]
                .to_str()
                .unwrap()
                .contains("script-src 'self'")
        );
        let csp = response.headers()["Content-Security-Policy"]
            .to_str()
            .unwrap();
        assert!(csp.contains("style-src 'self' blob: 'unsafe-inline'"));
        assert!(csp.contains("img-src blob: data:"));
        assert!(csp.contains("font-src blob: data:"));
        assert!(csp.contains("media-src blob: data:"));
        assert!(!csp.contains("script-src 'self' blob:"));
        assert!(app_asset("foliate-js/view.js").is_some());
        assert!(app_asset("foliate-js/vendor/zip.js").is_none());
        for path in ["foliate-js/paginator.js", "foliate-js/fixed-layout.js"] {
            let source =
                std::str::from_utf8(app_asset(path).unwrap().1).unwrap();
            assert!(!source.contains("allow-same-origin allow-scripts"));
        }
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

        let catalog = resource_response(
            &service.publications,
            HttpRequest::builder()
                .uri(format!("{resource_root}{RESOURCE_CATALOG_PATH}"))
                .body(Vec::new())
                .unwrap(),
        );
        assert_eq!(catalog.status(), 200);
        assert_eq!(
            catalog.headers()["Content-Type"],
            "application/json; charset=utf-8"
        );
        let catalog: Value = serde_json::from_slice(catalog.body()).unwrap();
        assert_eq!(catalog["resources"][0]["path"], "OPS/chapter.xhtml");
        assert_eq!(catalog["resources"][0]["size"], 33);

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
        assert_eq!(
            resource.headers()["Access-Control-Allow-Origin"],
            APP_BROWSER_ORIGIN
        );
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
        let valid =
            format!("{BOOK_PROTOCOL}://localhost/{token}/OPS/chapter.xhtml");
        assert_eq!(
            resource_request_target(&valid.parse().unwrap()).unwrap(),
            (token.into(), "OPS/chapter.xhtml".into())
        );
        for uri in [
            format!("{BOOK_PROTOCOL}://wrong/{token}/OPS/chapter.xhtml"),
            format!("{BOOK_PROTOCOL}://localhost/short/OPS/chapter.xhtml"),
            format!("{BOOK_PROTOCOL}://localhost/{token}/"),
            format!(
                "{BOOK_PROTOCOL}://localhost/{token}/OPS/%2e%2e/chapter.xhtml"
            ),
            format!(
                "{BOOK_PROTOCOL}://localhost/{token}/OPS/a%2fchapter.xhtml"
            ),
            format!(
                "{BOOK_PROTOCOL}://localhost/{token}/OPS/chapter.xhtml?x=1"
            ),
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
    fn renderer_ipc_accepts_only_owned_bounded_messages() {
        let ready = HttpRequest::builder()
            .uri(APP_BROWSER_URL)
            .body(
                concat!(
                    r#"{"protocol":1,"event":"publication-ready","#,
                    r#""location":{"cfi":"epubcfi(/6/4)","#,
                    r#""href":"OPS/chapter.xhtml","fraction":0.25}}"#,
                )
                .into(),
            )
            .unwrap();
        let event = renderer_event(7, &ready).unwrap();
        assert_eq!(event.event, "publication-ready");
        assert_eq!(event.view, 7);
        assert!(event.message.is_none());
        assert_eq!(
            event.location,
            Some(EpubLocator {
                cfi: "epubcfi(/6/4)".into(),
                href: "OPS/chapter.xhtml".into(),
                fraction: Some(0.25),
            })
        );

        let location = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                concat!(
                    r#"{"protocol":1,"event":"location","location":{"#,
                    r#""cfi":"epubcfi(/6/6)","#,
                    r#""href":"OPS/next.xhtml"}}"#,
                )
                .into(),
            )
            .unwrap();
        let event = renderer_event(7, &location).unwrap();
        assert_eq!(event.event, "location");
        assert_eq!(event.location.unwrap().fraction, None);

        let error = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                r#"{"protocol":1,"event":"publication-error",
                    "message":"bad EPUB"}"#
                    .into(),
            )
            .unwrap();
        let event = renderer_event(8, &error).unwrap();
        assert_eq!(event.event, "publication-error");
        assert_eq!(event.message.as_deref(), Some("bad EPUB"));
        assert!(event.location.is_none());

        for request in [
            HttpRequest::builder()
                .uri("https://example.invalid/")
                .body(ready.body().clone())
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(r#"{"protocol":2,"event":"publication-ready"}"#.into())
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(r#"{"protocol":1,"event":"other"}"#.into())
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(r#"{"protocol":1,"event":"publication-ready"}"#.into())
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(
                    concat!(
                        r#"{"protocol":1,"event":"location","location":{"#,
                        r#""cfi":"bad","href":"../chapter.xhtml"}}"#,
                    )
                    .into(),
                )
                .unwrap(),
        ] {
            assert!(renderer_event(9, &request).is_none());
        }
    }

    #[test]
    fn publication_script_serializes_renderer_inputs() {
        let location = EpubLocator {
            cfi: "epubcfi(/6/4!/4/2)".into(),
            href: "OPS/chapter.xhtml".into(),
            fraction: Some(0.25),
        };
        let script = publication_open_script(
            4,
            "https://yunge-reader-book.localhost/token/",
            Some(&location),
        );
        assert!(script.starts_with("void globalThis.yungeReader.open("));
        assert!(script.contains(r#""view":4"#));
        assert!(script.contains(r#""resourceRoot":"https://"#));
        assert!(script.contains(r#""cfi":"epubcfi(/6/4!/4/2)"#));
        assert!(!script.contains("eval"));

        let navigation = publication_navigation_script(
            4,
            NavigationCommand::GoTo,
            Some(&location),
        );
        assert!(
            navigation.starts_with("void globalThis.yungeReader.navigate(")
        );
        assert!(navigation.contains(r#""command":"go-to"#));
        assert!(!navigation.contains("eval"));
    }

    #[test]
    fn epub_locations_are_bounded_and_canonical() {
        let valid = EpubLocator {
            cfi: "epubcfi(/6/4!/4/2)".into(),
            href: "OPS/chapter.xhtml".into(),
            fraction: Some(1.0),
        };
        assert_eq!(valid.clone().validate().unwrap(), valid);

        for invalid in [
            EpubLocator {
                cfi: "bad".into(),
                href: "OPS/chapter.xhtml".into(),
                fraction: None,
            },
            EpubLocator {
                cfi: "epubcfi(/6/4)".into(),
                href: "../chapter.xhtml".into(),
                fraction: None,
            },
            EpubLocator {
                cfi: "epubcfi(/6/4)".into(),
                href: "https:chapter.xhtml".into(),
                fraction: None,
            },
            EpubLocator {
                cfi: "epubcfi(/6/4)".into(),
                href: "OPS/chapter.xhtml".into(),
                fraction: Some(1.1),
            },
        ] {
            assert_eq!(
                invalid.validate().unwrap_err().code,
                "invalid-epub-location"
            );
        }
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
