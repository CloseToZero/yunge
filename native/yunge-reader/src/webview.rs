// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::num::NonZeroIsize;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{self, RecvTimeoutError};
use std::thread;
use std::time::Duration;
use windows::Win32::Foundation::HWND;
use windows::Win32::UI::WindowsAndMessaging::{
    DispatchMessageW, IsWindow, MSG, PM_REMOVE, PeekMessageW, TranslateMessage,
};
use wry::dpi::{PhysicalPosition, PhysicalSize};
use wry::raw_window_handle::{
    HandleError, HasWindowHandle, RawWindowHandle, Win32WindowHandle,
    WindowHandle,
};
use wry::{PageLoadEvent, PermissionResponse, Rect, WebView, WebViewBuilder};

use super::{BUILD_ID, Error};

const PROTOCOL_VERSION: u32 = 1;
const CAPABILITIES: [&str; 7] = [
    "view-bounds",
    "view-create",
    "view-destroy",
    "view-focus",
    "view-info",
    "view-status",
    "view-visible",
];
const MAX_VIEW_EXTENT: u32 = 32_768;
const MESSAGE_PUMP_INTERVAL: Duration = Duration::from_millis(8);

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
    loaded: Arc<AtomicBool>,
    bounds: Bounds,
    visible: bool,
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

struct Service {
    views: HashMap<u64, NativeView>,
    version: Result<String, String>,
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
    fn new() -> Self {
        Self {
            views: HashMap::new(),
            version: wry::webview_version().map_err(|error| error.to_string()),
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
        let build = || {
            WebViewBuilder::new()
                .with_bounds(bounds.rect())
                .with_focused(false)
                .with_visible(params.visible)
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
        self.views.insert(
            params.view,
            NativeView {
                webview: view,
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
            let result = Self::parse::<EmptyParams>(request.params).map(|_| {
                self.views.clear();
                json!({ "stopped": true })
            });
            let control = if result.is_ok() {
                Control::Shutdown
            } else {
                Control::Continue
            };
            return (response(request.id, result), control);
        }
        let result = match request.op.as_str() {
            "view-info" => self.info(request.params),
            "view-create" => self.create_view(request.params),
            "view-bounds" => self.set_bounds(request.params),
            "view-visible" => self.set_visible(request.params),
            "view-focus" => self.focus(request.params),
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
<p>Resize or split the Emacs window, then select and copy this text.</p>
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

pub(super) fn serve() -> Result<(), Error> {
    let (sender, receiver) = mpsc::channel();
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

    let mut service = Service::new();
    let stdout = io::stdout();
    let mut output = stdout.lock();
    write_message(&mut output, &ready_message(&service.version))?;
    loop {
        pump_messages();
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
        if control == Control::Shutdown {
            break;
        }
    }
    service.views.clear();
    pump_messages();
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
