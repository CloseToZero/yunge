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
use windows::Win32::UI::Input::KeyboardAndMouse::{
    GetKeyState, VIRTUAL_KEY, VK_CONTROL, VK_MENU, VK_NEXT, VK_PRIOR, VK_SHIFT,
};
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
const MAX_EPUB_OUTLINE_DEPTH: u32 = 256;
const MAX_EPUB_OUTLINE_ITEMS: usize = 4_096;
const MAX_EPUB_OUTLINE_TITLE_BYTES: usize = 1_024;
const MAX_EPUB_OUTLINE_TEXT_BYTES: usize = 384 * 1_024;
const MAX_EPUB_SELECTION_CHARACTERS: u32 = 1_048_576;
const MAX_EPUB_SELECTION_CHARACTER_LIMIT: u32 = 65_536;
const MAX_EPUB_SELECTION_RESULT_BYTES: usize = 512 * 1_024;
const MIN_EPUB_FONT_SCALE: f64 = 0.5;
const MAX_EPUB_FONT_SCALE: f64 = 3.0;
const MIN_EPUB_LINE_HEIGHT: f64 = 1.0;
const MAX_EPUB_LINE_HEIGHT: f64 = 3.0;
const MIN_EPUB_CONTENT_WIDTH: u32 = 320;
const MAX_EPUB_CONTENT_WIDTH: u32 = 1_600;
const MAX_EPUB_SIDE_PADDING: f64 = 20.0;
const MAX_RENDERER_MESSAGE_BYTES: usize = 1_024 * 1_024;
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
const CAPABILITIES: [&str; 18] = [
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
    "view-selection-text",
    "view-status",
    "view-style",
    "view-visible",
];
const MAX_VIEW_EXTENT: u32 = 32_768;
const MESSAGE_PUMP_INTERVAL: Duration = Duration::from_millis(8);
const ESCAPE_VIRTUAL_KEY: u32 = 0x1b;

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
#[serde(untagged)]
enum Outgoing {
    Response(Response),
    Event(ViewEvent),
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
    #[serde(skip_serializing_if = "Option::is_none")]
    outline: Option<EpubOutline>,
    #[serde(skip_serializing_if = "Option::is_none")]
    selection: Option<Option<EpubSelection>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    key: Option<String>,
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
    #[serde(default)]
    style: EpubStyle,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ViewStyleParams {
    view: u64,
    style: EpubStyle,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct ViewSelectionTextParams {
    view: u64,
    selection: EpubSelection,
    offset: u32,
    character_limit: u32,
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct EpubStyle {
    font_scale: f64,
    line_height: f64,
    content_width: u32,
    side_padding: f64,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct EpubLocator {
    cfi: String,
    href: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    fraction: Option<f64>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct EpubNavigationTarget {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    cfi: Option<String>,
    href: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    fraction: Option<f64>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct EpubOutlineItem {
    title: String,
    depth: u32,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    href: Option<String>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct EpubOutline {
    items: Vec<EpubOutlineItem>,
    truncated: bool,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct EpubSelection {
    href: String,
    start: String,
    end: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct RendererSelectionTextEnvelope {
    ok: bool,
    #[serde(default)]
    result: Option<RendererSelectionTextResult>,
    #[serde(default)]
    error: Option<RendererSelectionTextError>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct RendererSelectionTextResult {
    text: String,
    total: u32,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    next_offset: Option<u32>,
    done: bool,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct RendererSelectionTextError {
    code: String,
    message: String,
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
    location: Option<EpubNavigationTarget>,
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
    #[serde(default)]
    outline: Option<EpubOutline>,
    #[serde(default, deserialize_with = "deserialize_present_option")]
    selection: Option<Option<EpubSelection>>,
    #[serde(default)]
    key: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
enum RendererEvent {
    Accelerator,
    Location,
    NavigationError,
    PublicationError,
    PublicationReady,
    Selection,
    StyleError,
}

fn deserialize_present_option<'de, D, T>(
    deserializer: D,
) -> Result<Option<Option<T>>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: Deserialize<'de>,
{
    Option::<T>::deserialize(deserializer).map(Some)
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
    outgoing_sender: Sender<Outgoing>,
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
        if !valid_epub_href(&self.href, false) {
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

impl Default for EpubStyle {
    fn default() -> Self {
        Self {
            font_scale: 1.0,
            line_height: 1.6,
            content_width: 720,
            side_padding: 7.0,
        }
    }
}

impl EpubStyle {
    fn validate(self) -> Result<Self, ServiceError> {
        if !self.font_scale.is_finite()
            || !(MIN_EPUB_FONT_SCALE..=MAX_EPUB_FONT_SCALE)
                .contains(&self.font_scale)
            || !self.line_height.is_finite()
            || !(MIN_EPUB_LINE_HEIGHT..=MAX_EPUB_LINE_HEIGHT)
                .contains(&self.line_height)
            || !(MIN_EPUB_CONTENT_WIDTH..=MAX_EPUB_CONTENT_WIDTH)
                .contains(&self.content_width)
            || !self.side_padding.is_finite()
            || !(0.0..=MAX_EPUB_SIDE_PADDING).contains(&self.side_padding)
        {
            return Err(ServiceError::new(
                "invalid-epub-style",
                "EPUB reading style is outside its supported bounds",
            ));
        }
        Ok(self)
    }
}

impl EpubNavigationTarget {
    fn validate(self) -> Result<Self, ServiceError> {
        if let Some(cfi) = self.cfi.as_ref() {
            EpubLocator {
                cfi: cfi.clone(),
                href: self.href.clone(),
                fraction: self.fraction,
            }
            .validate()?;
            return Ok(self);
        }
        if self.href.is_empty()
            || self.href.len() > MAX_EPUB_LOCATOR_TEXT_BYTES
            || self.href.chars().any(char::is_control)
            || !valid_epub_href(&self.href, true)
        {
            return Err(ServiceError::new(
                "invalid-epub-target",
                "EPUB navigation href is not a bounded internal target",
            ));
        }
        if self.fraction.is_some() {
            return Err(ServiceError::new(
                "invalid-epub-target",
                "EPUB href-only targets do not accept a fraction",
            ));
        }
        Ok(self)
    }
}

impl EpubOutline {
    fn validate(self) -> Result<Self, ServiceError> {
        if self.items.len() > MAX_EPUB_OUTLINE_ITEMS {
            return Err(ServiceError::new(
                "invalid-epub-outline",
                "EPUB outline contains too many items",
            ));
        }
        let mut text_bytes = 0_usize;
        for item in &self.items {
            text_bytes = text_bytes
                .checked_add(item.title.len())
                .and_then(|value| {
                    value.checked_add(item.href.as_ref().map_or(0, String::len))
                })
                .ok_or_else(|| {
                    ServiceError::new(
                        "invalid-epub-outline",
                        "EPUB outline text exceeds its aggregate limit",
                    )
                })?;
            if item.title.trim().is_empty()
                || item.title.len() > MAX_EPUB_OUTLINE_TITLE_BYTES
                || text_bytes > MAX_EPUB_OUTLINE_TEXT_BYTES
                || item.title.chars().any(char::is_control)
                || item.depth > MAX_EPUB_OUTLINE_DEPTH
                || item.href.as_ref().is_some_and(|href| {
                    href.is_empty()
                        || href.len() > MAX_EPUB_LOCATOR_TEXT_BYTES
                        || href.chars().any(char::is_control)
                        || !valid_epub_href(href, true)
                })
            {
                return Err(ServiceError::new(
                    "invalid-epub-outline",
                    "EPUB outline contains an invalid item",
                ));
            }
        }
        Ok(self)
    }
}

impl EpubSelection {
    fn validate(self) -> Result<Self, ServiceError> {
        let start = EpubLocator {
            cfi: self.start,
            href: self.href,
            fraction: None,
        }
        .validate()?;
        let end = EpubLocator {
            cfi: self.end,
            href: start.href.clone(),
            fraction: None,
        }
        .validate()?;
        if start.cfi == end.cfi {
            return Err(ServiceError::new(
                "invalid-epub-selection",
                "EPUB selection endpoints must differ",
            ));
        }
        Ok(Self {
            href: start.href,
            start: start.cfi,
            end: end.cfi,
        })
    }
}

impl ViewSelectionTextParams {
    fn validate(mut self) -> Result<Self, ServiceError> {
        self.selection = self.selection.validate()?;
        if !(1..=MAX_EPUB_SELECTION_CHARACTER_LIMIT)
            .contains(&self.character_limit)
        {
            return Err(ServiceError::new(
                "invalid-selection-limit",
                format!(
                    concat!(
                        "EPUB selection character limit must be between ",
                        "1 and {}"
                    ),
                    MAX_EPUB_SELECTION_CHARACTER_LIMIT
                ),
            ));
        }
        if self.offset > MAX_EPUB_SELECTION_CHARACTERS {
            return Err(ServiceError::new(
                "invalid-selection-offset",
                format!(
                    "EPUB selection offset must not exceed {}",
                    MAX_EPUB_SELECTION_CHARACTERS
                ),
            ));
        }
        Ok(self)
    }
}

fn valid_epub_href(value: &str, allow_fragment: bool) -> bool {
    let path = if let Some((path, fragment)) = value.split_once('#') {
        if !allow_fragment || fragment.contains('#') {
            return false;
        }
        path
    } else {
        value
    };
    !path.is_empty()
        && !path.starts_with('/')
        && !value.contains(['\\', '?'])
        && !path.contains(':')
        && !path
            .split('/')
            .any(|part| part.is_empty() || matches!(part, "." | ".."))
}

impl Service {
    fn new(outgoing_sender: Sender<Outgoing>) -> Self {
        Self {
            views: HashMap::new(),
            publications: Arc::new(Mutex::new(PublicationStore::default())),
            resource_requests: Arc::new(AtomicUsize::new(0)),
            next_publication: 1,
            version: wry::webview_version().map_err(|error| error.to_string()),
            outgoing_sender,
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
        let renderer_events = self.outgoing_sender.clone();
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
                        let _ = renderer_events.send(Outgoing::Event(event));
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
            self.outgoing_sender.clone(),
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
        let script = publication_clear_selection_script(params.view);
        self.view(params.view)?
            .webview
            .evaluate_script(&script)
            .map_err(|error| {
                ServiceError::new("view-update-failed", error.to_string())
            })?;
        Ok(json!({ "view": params.view, "selection": false }))
    }

    fn selection_text(
        &self,
        id: u64,
        params: Value,
    ) -> Result<(), ServiceError> {
        let params =
            Self::parse::<ViewSelectionTextParams>(params)?.validate()?;
        let view = self.view(params.view)?;
        if view.publication.is_none() {
            return Err(ServiceError::new(
                "view-has-no-publication",
                format!("view {} has no attached publication", params.view),
            ));
        }
        let offset = params.offset;
        let character_limit = params.character_limit;
        let script = publication_selection_text_script(&params);
        let sender = self.outgoing_sender.clone();
        view.webview
            .evaluate_script_with_callback(&script, move |value| {
                let response = renderer_selection_text_response(
                    id,
                    offset,
                    character_limit,
                    &value,
                );
                let _ = sender.send(Outgoing::Response(response));
            })
            .map_err(|error| {
                ServiceError::new("selection-text-failed", error.to_string())
            })?;
        Ok(())
    }

    fn open_view_publication(
        &mut self,
        params: Value,
    ) -> Result<Value, ServiceError> {
        let params: ViewPublicationParams = Self::parse(params)?;
        let location =
            params.location.map(EpubLocator::validate).transpose()?;
        let style = params.style.validate()?;
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
            &style,
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
        let location = params
            .location
            .map(EpubNavigationTarget::validate)
            .transpose()?;
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

    fn set_view_style(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: ViewStyleParams = Self::parse(params)?;
        let style = params.style.validate()?;
        let view = self.view(params.view)?;
        if view.publication.is_none() {
            return Err(ServiceError::new(
                "view-has-no-publication",
                format!("view {} has no attached publication", params.view),
            ));
        }
        let script = publication_style_script(params.view, &style);
        view.webview.evaluate_script(&script).map_err(|error| {
            ServiceError::new("view-update-failed", error.to_string())
        })?;
        Ok(json!({
            "view": params.view,
            "style": style,
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

    fn handle(&mut self, request: Request) -> (Option<Response>, Control) {
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
            return (Some(response(request.id, result)), control);
        }
        if request.op == "view-selection-text" {
            let result = self.selection_text(request.id, request.params);
            let response = result.err().map(|error| {
                Response::failure(Some(request.id), error.code, error.message)
            });
            return (response, Control::Continue);
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
            "view-style" => self.set_view_style(request.params),
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
        (Some(response(request.id, result)), Control::Continue)
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

fn routed_key(
    kind: COREWEBVIEW2_KEY_EVENT_KIND,
    key: u32,
    control: bool,
    alt: bool,
    shift: bool,
) -> Option<&'static str> {
    if kind != COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN
        && kind != COREWEBVIEW2_KEY_EVENT_KIND_SYSTEM_KEY_DOWN
    {
        return None;
    }
    if key == ESCAPE_VIRTUAL_KEY {
        return Some("<escape>");
    }
    if alt || shift {
        return None;
    }
    match (key, control) {
        (key, true) if key == u32::from(b'G') => Some("C-g"),
        (key, true) if key == u32::from(b'D') => Some("C-d"),
        (key, true) if key == u32::from(b'U') => Some("C-u"),
        (key, false) if key == u32::from(VK_NEXT.0) => Some("<next>"),
        (key, false) if key == u32::from(VK_PRIOR.0) => Some("<prior>"),
        _ => None,
    }
}

fn key_state(key: VIRTUAL_KEY) -> bool {
    // SAFETY: `GetKeyState' accepts every Win32 virtual-key value and has no
    // pointer or lifetime requirements.
    unsafe { GetKeyState(i32::from(key.0)) < 0 }
}

fn install_accelerator_handler(
    webview: &WebView,
    view: u64,
    outgoing_sender: Sender<Outgoing>,
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
                let routed = routed_key(
                    kind,
                    key,
                    key_state(VK_CONTROL),
                    key_state(VK_MENU),
                    key_state(VK_SHIFT),
                );
                if let Some(key) = routed {
                    args.SetHandled(true)?;
                    let _ = outgoing_sender.send(Outgoing::Event(ViewEvent {
                        kind: "event",
                        event: "accelerator",
                        view,
                        message: None,
                        location: None,
                        outline: None,
                        selection: None,
                        key: Some(key.to_owned()),
                    }));
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

fn invalid_renderer_selection_result(
    id: u64,
    detail: impl Into<String>,
) -> Response {
    Response::failure(Some(id), "invalid-renderer-result", detail)
}

fn renderer_selection_text_response(
    id: u64,
    offset: u32,
    character_limit: u32,
    value: &str,
) -> Response {
    if value.len() > MAX_EPUB_SELECTION_RESULT_BYTES {
        return invalid_renderer_selection_result(
            id,
            "EPUB selection text result exceeds its byte limit",
        );
    }
    let envelope: RendererSelectionTextEnvelope =
        match serde_json::from_str(value) {
            Ok(envelope) => envelope,
            Err(error) => {
                return invalid_renderer_selection_result(
                    id,
                    format!("invalid EPUB selection text result: {error}"),
                );
            }
        };
    if envelope.ok {
        let Some(result) = envelope.result else {
            return invalid_renderer_selection_result(
                id,
                "successful EPUB selection text result has no payload",
            );
        };
        if envelope.error.is_some() {
            return invalid_renderer_selection_result(
                id,
                "successful EPUB selection text result contains an error",
            );
        }
        let characters = match u32::try_from(result.text.chars().count()) {
            Ok(characters) => characters,
            Err(_) => {
                return invalid_renderer_selection_result(
                    id,
                    "EPUB selection text chunk is too large",
                );
            }
        };
        let next = match offset.checked_add(characters) {
            Some(next) => next,
            None => {
                return invalid_renderer_selection_result(
                    id,
                    "EPUB selection text cursor overflowed",
                );
            }
        };
        let valid = result.total <= MAX_EPUB_SELECTION_CHARACTERS
            && characters <= character_limit
            && offset <= result.total
            && if result.done {
                result.next_offset.is_none() && next == result.total
            } else {
                characters > 0
                    && next < result.total
                    && result.next_offset == Some(next)
            };
        if !valid {
            return invalid_renderer_selection_result(
                id,
                "EPUB selection text batch is inconsistent",
            );
        }
        return Response::success(
            id,
            serde_json::to_value(result)
                .expect("validated selection text result is serializable"),
        );
    }
    if envelope.result.is_some() {
        return invalid_renderer_selection_result(
            id,
            "failed EPUB selection text result contains a payload",
        );
    }
    let Some(error) = envelope.error else {
        return invalid_renderer_selection_result(
            id,
            "failed EPUB selection text result has no error",
        );
    };
    if error.message.is_empty()
        || error.message.len() > MAX_RENDERER_ERROR_BYTES
    {
        return invalid_renderer_selection_result(
            id,
            "EPUB selection text error message is invalid",
        );
    }
    let code = match error.code.as_str() {
        "invalid-selection-offset" => "invalid-selection-offset",
        "selection-no-longer-current" => "selection-no-longer-current",
        "selection-too-large" => "selection-too-large",
        "selection-unavailable" => "selection-unavailable",
        _ => {
            return invalid_renderer_selection_result(
                id,
                "EPUB selection text error code is invalid",
            );
        }
    };
    Response::failure(Some(id), code, error.message)
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
    let (event, detail, location, outline, selection, key) = match message.event
    {
        RendererEvent::Accelerator => {
            if message.message.is_some()
                || message.location.is_some()
                || message.outline.is_some()
                || message.selection.is_some()
            {
                return None;
            }
            let key = message.key.filter(|key| {
                matches!(key.as_str(), "J" | "K" | "+" | "-" | "=" | "y")
            })?;
            ("accelerator", None, None, None, None, Some(key))
        }
        RendererEvent::Location => {
            if message.message.is_some()
                || message.outline.is_some()
                || message.selection.is_some()
                || message.key.is_some()
            {
                return None;
            }
            let location = message.location?.validate().ok()?;
            ("location", None, Some(location), None, None, None)
        }
        RendererEvent::NavigationError => {
            if message.location.is_some()
                || message.outline.is_some()
                || message.selection.is_some()
                || message.key.is_some()
            {
                return None;
            }
            let detail = message.message.filter(|value| !value.is_empty())?;
            ("navigation-error", Some(detail), None, None, None, None)
        }
        RendererEvent::PublicationReady => {
            if message.message.is_some()
                || message.selection.is_some()
                || message.key.is_some()
            {
                return None;
            }
            let location = message.location?.validate().ok()?;
            let outline = message.outline?.validate().ok()?;
            (
                "publication-ready",
                None,
                Some(location),
                Some(outline),
                None,
                None,
            )
        }
        RendererEvent::PublicationError => {
            if message.location.is_some()
                || message.outline.is_some()
                || message.selection.is_some()
                || message.key.is_some()
            {
                return None;
            }
            let detail = message.message.filter(|value| !value.is_empty())?;
            ("publication-error", Some(detail), None, None, None, None)
        }
        RendererEvent::Selection => {
            if message.message.is_some()
                || message.location.is_some()
                || message.outline.is_some()
                || message.key.is_some()
            {
                return None;
            }
            let selection = message
                .selection?
                .map(EpubSelection::validate)
                .transpose()
                .ok()?;
            ("selection", None, None, None, Some(selection), None)
        }
        RendererEvent::StyleError => {
            if message.location.is_some()
                || message.outline.is_some()
                || message.selection.is_some()
                || message.key.is_some()
            {
                return None;
            }
            let detail = message.message.filter(|value| !value.is_empty())?;
            ("style-error", Some(detail), None, None, None, None)
        }
    };
    Some(ViewEvent {
        kind: "event",
        event,
        view,
        message: detail,
        location,
        outline,
        selection,
        key,
    })
}

fn publication_open_script(
    view: u64,
    resource_root: &str,
    location: Option<&EpubLocator>,
    style: &EpubStyle,
) -> String {
    let payload = serde_json::to_string(&json!({
        "view": view,
        "resourceRoot": resource_root,
        "location": location,
        "style": style,
    }))
    .expect("publication open payload is serializable");
    format!("void globalThis.yungeReader.open({payload});")
}

fn publication_navigation_script(
    view: u64,
    command: NavigationCommand,
    location: Option<&EpubNavigationTarget>,
) -> String {
    let payload = serde_json::to_string(&json!({
        "view": view,
        "command": command,
        "location": location,
    }))
    .expect("publication navigation payload is serializable");
    format!("void globalThis.yungeReader.navigate({payload});")
}

fn publication_style_script(view: u64, style: &EpubStyle) -> String {
    let payload = serde_json::to_string(&json!({
        "view": view,
        "style": style,
    }))
    .expect("publication style payload is serializable");
    format!("void globalThis.yungeReader.setStyle({payload});")
}

fn publication_clear_selection_script(view: u64) -> String {
    let payload = serde_json::to_string(&json!({ "view": view }))
        .expect("selection payload is serializable");
    format!("void globalThis.yungeReader.clearSelection({payload});")
}

fn publication_selection_text_script(
    params: &ViewSelectionTextParams,
) -> String {
    let payload = serde_json::to_string(params)
        .expect("selection text payload is serializable");
    format!("globalThis.yungeReader.selectionText({payload});")
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

fn write_outgoing(
    receiver: &Receiver<Outgoing>,
    output: &mut impl Write,
) -> Result<(), Error> {
    while let Ok(message) = receiver.try_recv() {
        write_message(&mut *output, &message)?;
    }
    Ok(())
}

pub(super) fn serve() -> Result<(), Error> {
    let (sender, receiver) = mpsc::channel();
    let (outgoing_sender, outgoing_receiver) = mpsc::channel();
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

    let mut service = Service::new(outgoing_sender);
    let stdout = io::stdout();
    let mut output = stdout.lock();
    write_message(&mut output, &ready_message(&service.version))?;
    loop {
        pump_messages();
        write_outgoing(&outgoing_receiver, &mut output)?;
        let incoming = match receiver.recv_timeout(MESSAGE_PUMP_INTERVAL) {
            Ok(incoming) => incoming,
            Err(RecvTimeoutError::Timeout) => continue,
            Err(RecvTimeoutError::Disconnected) => break,
        };
        let (response, control) = match incoming {
            Incoming::Request(request) => service.handle(request),
            Incoming::Invalid(message) => (
                Some(Response::failure(None, "invalid-request", message)),
                Control::Continue,
            ),
        };
        if let Some(response) = response {
            write_message(&mut output, &response)?;
        }
        write_outgoing(&outgoing_receiver, &mut output)?;
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

    fn handle_immediate(
        service: &mut Service,
        request: Request,
    ) -> (Response, Control) {
        let (response, control) = service.handle(request);
        (
            response.expect("operation returns an immediate response"),
            control,
        )
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
        let adapter =
            std::str::from_utf8(app_asset("yunge-reader.js").unwrap().1)
                .unwrap();
        assert!(adapter.contains("post('accelerator', { key })"));
        assert!(
            adapter
                .contains("['J', 'K', '+', '-', '=', 'y'].includes(event.key)")
        );
        assert!(!adapter.contains("['j', 'k'].includes(event.key)"));
        assert!(adapter.contains("applyReadingStyle(view, style)"));
        assert!(adapter.contains("if (view.isFixedLayout) return"));
        assert!(adapter.contains("collapseCFI(relocation.cfi)"));
        assert!(adapter.contains("session.commandNavigation"));
        assert!(adapter.contains("if (session.opening) return"));
        assert!(adapter.contains("view.renderer.addEventListener('relocate'"));
        assert!(adapter.contains("pendingStyle"));
        assert!(adapter.contains("requestAnimationFrame("));
        assert!(adapter.contains("post('style-error'"));
        assert!(adapter.contains("installSelectionTracking"));
        assert!(adapter.contains(
            "clearSelection, navigate, open, selectionText, setStyle"
        ));
        assert!(adapter.contains("selectedRange"));
        assert!(adapter.contains("Array.from(text)"));
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
        let (opened, control) = handle_immediate(
            &mut service,
            Request {
                id: 1,
                op: "publication-open".into(),
                params: json!({ "path": path }),
            },
        );

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

        let (info, _) = handle_immediate(
            &mut service,
            Request {
                id: 2,
                op: "publication-info".into(),
                params: json!({ "publication": 1 }),
            },
        );
        let info = info.result.unwrap();
        assert_eq!(info["metadata"]["title"], "Protocol Book");
        assert_eq!(info["resource-root"], resource_root);

        let (closed, _) = handle_immediate(
            &mut service,
            Request {
                id: 3,
                op: "publication-close".into(),
                params: json!({ "publication": 1 }),
            },
        );
        assert_eq!(closed.result.unwrap()["closed"], true);

        let released = resource_response(
            &service.publications,
            HttpRequest::builder()
                .uri(format!("{resource_root}OPS/chapter.xhtml"))
                .body(Vec::new())
                .unwrap(),
        );
        assert_eq!(released.status(), 404);

        let (missing, _) = handle_immediate(
            &mut service,
            Request {
                id: 4,
                op: "publication-info".into(),
                params: json!({ "publication": 1 }),
            },
        );
        assert_eq!(missing.error.unwrap().code, "unknown-publication");
    }

    #[test]
    fn publication_open_requires_an_absolute_strict_path() {
        let (sender, _receiver) = mpsc::channel();
        let mut service = Service::new(sender);
        let (relative, _) = handle_immediate(
            &mut service,
            Request {
                id: 1,
                op: "publication-open".into(),
                params: json!({ "path": "book.epub" }),
            },
        );
        assert_eq!(relative.error.unwrap().code, "invalid-publication-path");

        let (unknown, _) = handle_immediate(
            &mut service,
            Request {
                id: 2,
                op: "publication-open".into(),
                params: json!({ "path": "book.epub", "extra": true }),
            },
        );
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
                    r#""href":"OPS/chapter.xhtml","fraction":0.25},"#,
                    r#""outline":{"items":[{"title":"Chapter","#,
                    r#""depth":0,"href":"OPS/chapter.xhtml#start"}],"#,
                    r#""truncated":false}}"#,
                )
                .into(),
            )
            .unwrap();
        let event = renderer_event(7, &ready).unwrap();
        assert_eq!(event.event, "publication-ready");
        assert_eq!(event.view, 7);
        assert!(event.message.is_none());
        assert!(event.selection.is_none());
        assert!(event.key.is_none());
        assert_eq!(
            event.outline,
            Some(EpubOutline {
                items: vec![EpubOutlineItem {
                    title: "Chapter".into(),
                    depth: 0,
                    href: Some("OPS/chapter.xhtml#start".into()),
                }],
                truncated: false,
            })
        );
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
        assert!(event.outline.is_none());
        assert!(event.selection.is_none());

        let selection = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                concat!(
                    r#"{"protocol":1,"event":"selection","selection":{"#,
                    r#""href":"OPS/chapter.xhtml","#,
                    r#""start":"epubcfi(/6/4!/4/2/1:0)","#,
                    r#""end":"epubcfi(/6/4!/4/2/1:7)"}}"#,
                )
                .into(),
            )
            .unwrap();
        let event = renderer_event(7, &selection).unwrap();
        assert_eq!(event.event, "selection");
        assert_eq!(
            event.selection,
            Some(Some(EpubSelection {
                href: "OPS/chapter.xhtml".into(),
                start: "epubcfi(/6/4!/4/2/1:0)".into(),
                end: "epubcfi(/6/4!/4/2/1:7)".into(),
            }))
        );

        let selection_clear = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                r#"{"protocol":1,"event":"selection","selection":null}"#.into(),
            )
            .unwrap();
        let event = renderer_event(7, &selection_clear).unwrap();
        assert_eq!(event.selection, Some(None));

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
        assert!(event.outline.is_none());
        assert!(event.selection.is_none());
        assert!(event.key.is_none());

        let style_error = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                r#"{"protocol":1,"event":"style-error",
                    "message":"bad style"}"#
                    .into(),
            )
            .unwrap();
        let event = renderer_event(8, &style_error).unwrap();
        assert_eq!(event.event, "style-error");
        assert_eq!(event.message.as_deref(), Some("bad style"));
        assert!(event.location.is_none());
        assert!(event.outline.is_none());
        assert!(event.selection.is_none());
        assert!(event.key.is_none());

        for key in ["J", "K", "+", "-", "=", "y"] {
            let payload = json!({
                "protocol": 1,
                "event": "accelerator",
                "key": key,
            })
            .to_string();
            let accelerator = HttpRequest::builder()
                .uri(APP_URL)
                .body(payload.into())
                .unwrap();
            let event = renderer_event(8, &accelerator).unwrap();
            assert_eq!(event.event, "accelerator");
            assert_eq!(event.key.as_deref(), Some(key));
            assert!(event.location.is_none());
            assert!(event.outline.is_none());
            assert!(event.selection.is_none());
            assert!(event.message.is_none());
        }

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
                    r#"{"protocol":1,"event":"accelerator","key":"j"}"#.into(),
                )
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(
                    r#"{"protocol":1,"event":"accelerator","key":"C-d"}"#
                        .into(),
                )
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
            HttpRequest::builder()
                .uri(APP_URL)
                .body(r#"{"protocol":1,"event":"selection"}"#.into())
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(
                    concat!(
                        r#"{"protocol":1,"event":"selection","#,
                        r#""selection":{"href":"OPS/chapter.xhtml","#,
                        r#""start":"epubcfi(/6/4)","#,
                        r#""end":"epubcfi(/6/4)"}}"#,
                    )
                    .into(),
                )
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(
                    concat!(
                        r#"{"protocol":1,"event":"location","#,
                        r#""selection":null,"location":{"#,
                        r#""cfi":"epubcfi(/6/6)","#,
                        r#""href":"OPS/next.xhtml"}}"#,
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
            &EpubStyle::default(),
        );
        assert!(script.starts_with("void globalThis.yungeReader.open("));
        assert!(script.contains(r#""view":4"#));
        assert!(script.contains(r#""resourceRoot":"https://"#));
        assert!(script.contains(r#""cfi":"epubcfi(/6/4!/4/2)"#));
        assert!(script.contains(r#""font-scale":1.0"#));
        assert!(script.contains(r#""line-height":1.6"#));
        assert!(script.contains(r#""content-width":720"#));
        assert!(script.contains(r#""side-padding":7.0"#));
        assert!(!script.contains("eval"));

        let style_script = publication_style_script(4, &EpubStyle::default());
        assert!(
            style_script.starts_with("void globalThis.yungeReader.setStyle(")
        );
        assert!(style_script.contains(r#""view":4"#));
        assert!(style_script.contains(r#""font-scale":1.0"#));
        assert!(!style_script.contains("eval"));

        let target = EpubNavigationTarget {
            cfi: Some(location.cfi.clone()),
            href: location.href.clone(),
            fraction: location.fraction,
        };
        let navigation = publication_navigation_script(
            4,
            NavigationCommand::GoTo,
            Some(&target),
        );
        assert!(
            navigation.starts_with("void globalThis.yungeReader.navigate(")
        );
        assert!(navigation.contains(r#""command":"go-to"#));
        assert!(!navigation.contains("eval"));

        let clear = publication_clear_selection_script(4);
        assert!(
            clear.starts_with("void globalThis.yungeReader.clearSelection(")
        );
        assert!(clear.contains(r#""view":4"#));
        assert!(!clear.contains("eval"));

        let selection_text =
            publication_selection_text_script(&ViewSelectionTextParams {
                view: 4,
                selection: EpubSelection {
                    href: "OPS/chapter.xhtml".into(),
                    start: "epubcfi(/6/4!/4/2/1:0)".into(),
                    end: "epubcfi(/6/4!/4/2/1:7)".into(),
                },
                offset: 0,
                character_limit: 16_384,
            });
        assert!(
            selection_text.starts_with("globalThis.yungeReader.selectionText(")
        );
        assert!(selection_text.contains(r#""character-limit":16384"#));
        assert!(selection_text.contains(r#""offset":0"#));
        assert!(!selection_text.contains("eval"));
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
    fn epub_selections_are_same_spine_distinct_ranges() {
        let valid = EpubSelection {
            href: "OPS/chapter.xhtml".into(),
            start: "epubcfi(/6/4!/4/2/1:0)".into(),
            end: "epubcfi(/6/4!/4/2/1:7)".into(),
        };
        assert_eq!(valid.clone().validate().unwrap(), valid);

        for invalid in [
            EpubSelection {
                href: "OPS/chapter.xhtml#part".into(),
                start: "epubcfi(/6/4!/4/2/1:0)".into(),
                end: "epubcfi(/6/4!/4/2/1:7)".into(),
            },
            EpubSelection {
                href: "OPS/chapter.xhtml".into(),
                start: "bad".into(),
                end: "epubcfi(/6/4!/4/2/1:7)".into(),
            },
            EpubSelection {
                href: "OPS/chapter.xhtml".into(),
                start: "epubcfi(/6/4!/4/2/1:0)".into(),
                end: "epubcfi(/6/4!/4/2/1:0)".into(),
            },
        ] {
            assert!(invalid.validate().is_err());
        }
        assert!(
            EpubSelection {
                href: "OPS/chapter.xhtml".into(),
                start: "epubcfi(/6/4!/4/2/1:0)".into(),
                end: format!("epubcfi({})", "x".repeat(3_072)),
            }
            .validate()
            .is_err()
        );
    }

    #[test]
    fn epub_selection_text_requests_are_strictly_bounded() {
        let selection = EpubSelection {
            href: "OPS/chapter.xhtml".into(),
            start: "epubcfi(/6/4!/4/2/1:0)".into(),
            end: "epubcfi(/6/4!/4/2/1:7)".into(),
        };
        let valid = ViewSelectionTextParams {
            view: 4,
            selection: selection.clone(),
            offset: MAX_EPUB_SELECTION_CHARACTERS,
            character_limit: MAX_EPUB_SELECTION_CHARACTER_LIMIT,
        };
        assert_eq!(valid.validate().unwrap().view, 4);

        let invalid_limit = ViewSelectionTextParams {
            view: 4,
            selection: selection.clone(),
            offset: 0,
            character_limit: 0,
        };
        assert_eq!(
            invalid_limit.validate().unwrap_err().code,
            "invalid-selection-limit"
        );
        let invalid_offset = ViewSelectionTextParams {
            view: 4,
            selection,
            offset: MAX_EPUB_SELECTION_CHARACTERS + 1,
            character_limit: 1,
        };
        assert_eq!(
            invalid_offset.validate().unwrap_err().code,
            "invalid-selection-offset"
        );
        assert!(
            Service::parse::<ViewSelectionTextParams>(json!({
                "view": 4,
                "selection": {
                    "href": "OPS/chapter.xhtml",
                    "start": "epubcfi(/6/4!/4/2/1:0)",
                    "end": "epubcfi(/6/4!/4/2/1:7)",
                },
                "offset": 0,
                "character-limit": 16,
                "extra": true,
            }))
            .is_err()
        );
    }

    #[test]
    fn renderer_selection_text_batches_are_independently_validated() {
        let response = renderer_selection_text_response(
            7,
            0,
            2,
            concat!(
                r#"{"ok":true,"result":{"text":"A😀","total":4,"#,
                r#""next-offset":2,"done":false}}"#,
            ),
        );
        assert!(response.ok);
        let result = response.result.unwrap();
        assert_eq!(result["text"], "A😀");
        assert_eq!(result["next-offset"], 2);
        assert_eq!(result["total"], 4);

        let done = renderer_selection_text_response(
            8,
            2,
            8,
            r#"{"ok":true,"result":{"text":"bc","total":4,
                "done":true}}"#,
        );
        assert!(done.ok);
        assert!(done.result.unwrap().get("next-offset").is_none());

        for invalid in [
            r#"null"#,
            r#"{"ok":true,"result":{"text":"abc","total":4,
                "next-offset":2,"done":false}}"#,
            r#"{"ok":true,"result":{"text":"","total":4,
                "next-offset":0,"done":false}}"#,
            r#"{"ok":true,"result":{"text":"a","total":1,
                "next-offset":1,"done":true}}"#,
        ] {
            let response = renderer_selection_text_response(9, 0, 2, invalid);
            assert!(!response.ok);
            assert_eq!(response.error.unwrap().code, "invalid-renderer-result");
        }

        let stale = renderer_selection_text_response(
            10,
            0,
            2,
            concat!(
                r#"{"ok":false,"error":{"code":"#,
                r#""selection-no-longer-current","message":"stale"}}"#,
            ),
        );
        assert_eq!(stale.error.unwrap().code, "selection-no-longer-current");
    }

    #[test]
    fn outgoing_messages_preserve_the_public_ndjson_shapes() {
        let response = serde_json::to_value(Outgoing::Response(
            Response::success(3, json!({ "scheduled": true })),
        ))
        .unwrap();
        assert_eq!(response["id"], 3);
        assert_eq!(response["ok"], true);
        assert!(response.get("kind").is_none());

        let event = serde_json::to_value(Outgoing::Event(ViewEvent {
            kind: "event",
            event: "accelerator",
            view: 4,
            message: None,
            location: None,
            outline: None,
            selection: None,
            key: Some("<escape>".into()),
        }))
        .unwrap();
        assert_eq!(event["kind"], "event");
        assert_eq!(event["event"], "accelerator");
        assert_eq!(event["view"], 4);
        assert_eq!(event["key"], "<escape>");
        assert!(event.get("id").is_none());
    }

    #[test]
    fn epub_styles_are_bounded_semantic_values() {
        let default = EpubStyle::default();
        assert_eq!(default.validate().unwrap(), default);

        let parsed = Service::parse::<ViewPublicationParams>(json!({
            "view": 4,
            "publication": 7,
        }))
        .unwrap();
        assert_eq!(parsed.style, default);

        let parsed = Service::parse::<ViewStyleParams>(json!({
            "view": 4,
            "style": {
                "font-scale": 1.25,
                "line-height": 1.8,
                "content-width": 640,
                "side-padding": 10.0,
            },
        }))
        .unwrap();
        assert_eq!(parsed.view, 4);
        assert_eq!(parsed.style.font_scale, 1.25);

        for invalid in [
            EpubStyle {
                font_scale: 0.49,
                ..default
            },
            EpubStyle {
                line_height: 3.1,
                ..default
            },
            EpubStyle {
                content_width: 319,
                ..default
            },
            EpubStyle {
                side_padding: 20.1,
                ..default
            },
            EpubStyle {
                font_scale: f64::NAN,
                ..default
            },
        ] {
            assert_eq!(
                invalid.validate().unwrap_err().code,
                "invalid-epub-style"
            );
        }

        assert!(
            Service::parse::<ViewPublicationParams>(json!({
                "view": 4,
                "publication": 7,
                "style": {
                    "font-scale": 1.0,
                    "line-height": 1.6,
                    "content-width": 720,
                    "side-padding": 7.0,
                    "color": "red",
                },
            }))
            .is_err()
        );
    }

    #[test]
    fn epub_navigation_accepts_bounded_internal_href_targets() {
        let target = EpubNavigationTarget {
            cfi: None,
            href: "OPS/chapter.xhtml#section-2".into(),
            fraction: None,
        };
        assert_eq!(target.clone().validate().unwrap(), target);

        for href in [
            "../chapter.xhtml#section",
            "/OPS/chapter.xhtml",
            "https:chapter.xhtml",
            "OPS/chapter.xhtml?query",
            "OPS/chapter.xhtml#one#two",
        ] {
            let invalid = EpubNavigationTarget {
                cfi: None,
                href: href.into(),
                fraction: None,
            };
            assert_eq!(
                invalid.validate().unwrap_err().code,
                "invalid-epub-target"
            );
        }
    }

    #[test]
    fn epub_outlines_are_bounded_and_internal() {
        let outline = EpubOutline {
            items: vec![EpubOutlineItem {
                title: "Part One".into(),
                depth: 0,
                href: Some("OPS/chapter.xhtml#part-one".into()),
            }],
            truncated: false,
        };
        assert_eq!(outline.clone().validate().unwrap(), outline);

        let invalid = EpubOutline {
            items: vec![EpubOutlineItem {
                title: "External".into(),
                depth: 0,
                href: Some("https:example.invalid".into()),
            }],
            truncated: false,
        };
        assert_eq!(
            invalid.validate().unwrap_err().code,
            "invalid-epub-outline"
        );

        let oversized = EpubOutline {
            items: (0..385)
                .map(|_| EpubOutlineItem {
                    title: "x".repeat(MAX_EPUB_OUTLINE_TITLE_BYTES),
                    depth: 0,
                    href: None,
                })
                .collect(),
            truncated: false,
        };
        assert_eq!(
            oversized.validate().unwrap_err().code,
            "invalid-epub-outline"
        );
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
    fn native_reader_keys_are_normalized_without_character_keys() {
        assert_eq!(
            routed_key(
                COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN,
                ESCAPE_VIRTUAL_KEY,
                false,
                false,
                false,
            ),
            Some("<escape>")
        );
        assert_eq!(
            routed_key(
                COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN,
                u32::from(b'G'),
                true,
                false,
                false,
            ),
            Some("C-g")
        );
        assert_eq!(
            routed_key(
                COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN,
                u32::from(b'D'),
                true,
                false,
                false,
            ),
            Some("C-d")
        );
        assert_eq!(
            routed_key(
                COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN,
                u32::from(b'U'),
                true,
                false,
                false,
            ),
            Some("C-u")
        );
        assert_eq!(
            routed_key(
                COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN,
                u32::from(VK_NEXT.0),
                false,
                false,
                false,
            ),
            Some("<next>")
        );
        assert_eq!(
            routed_key(
                COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN,
                u32::from(VK_PRIOR.0),
                false,
                false,
                false,
            ),
            Some("<prior>")
        );
        assert_eq!(
            routed_key(
                WebView2::COREWEBVIEW2_KEY_EVENT_KIND_KEY_UP,
                ESCAPE_VIRTUAL_KEY,
                false,
                false,
                false,
            ),
            None
        );
        assert_eq!(
            routed_key(
                COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN,
                u32::from(b'J'),
                false,
                false,
                true,
            ),
            None
        );
        assert_eq!(
            routed_key(
                COREWEBVIEW2_KEY_EVENT_KIND_SYSTEM_KEY_DOWN,
                u32::from(b'D'),
                true,
                true,
                false,
            ),
            None
        );
    }
}
