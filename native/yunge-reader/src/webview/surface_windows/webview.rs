// SPDX-FileCopyrightText: 2020-2024 Tauri Programme within The Commons Conservancy
// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use http::{Request as HttpRequest, Response as HttpResponse};
use std::borrow::Cow;
use std::fmt::Write as _;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use webview2_com::Microsoft::Web::WebView2::Win32::*;
use webview2_com::{
    AddScriptToExecuteOnDocumentCreatedCompletedHandler,
    CreateCoreWebView2ControllerCompletedHandler,
    CreateCoreWebView2EnvironmentCompletedHandler,
    ExecuteScriptCompletedHandler, NavigationStartingEventHandler,
    NewWindowRequestedEventHandler, PermissionRequestedEventHandler,
    WebMessageReceivedEventHandler, WebResourceRequestedEventHandler,
    take_pwstr,
};
use windows::Win32::Foundation::{
    E_POINTER, E_UNEXPECTED, HINSTANCE, HWND, LPARAM, LRESULT, RECT, WPARAM,
};
use windows::Win32::Graphics::Gdi::HBRUSH;
use windows::Win32::System::Com::{COINIT_APARTMENTTHREADED, CoInitializeEx};
use windows::Win32::System::LibraryLoader::GetModuleHandleW;
use windows::Win32::UI::Input::KeyboardAndMouse::SetFocus;
use windows::Win32::UI::Shell::SHCreateMemStream;
use windows::Win32::UI::WindowsAndMessaging::{
    CS_HREDRAW, CS_VREDRAW, CreateWindowExW, DefWindowProcW, DestroyWindow,
    GW_CHILD, GetWindow, HCURSOR, HICON, HWND_TOP, RegisterClassExW, SW_HIDE,
    SW_SHOW, SWP_ASYNCWINDOWPOS, SWP_NOACTIVATE, SWP_NOMOVE, SWP_NOOWNERZORDER,
    SWP_NOSIZE, SWP_NOZORDER, SetParent, SetWindowPos, ShowWindow,
    WINDOW_EX_STYLE, WM_SETFOCUS, WNDCLASSEXW, WS_CHILD, WS_CLIPCHILDREN,
    WS_VISIBLE,
};
use windows::core::{HSTRING, Interface, PCWSTR, PWSTR, w};

use super::super::protocol::ServiceError;
use super::super::renderer::{app_navigation_allowed, shell_ready};
use super::super::resources::{
    APP_BROWSER_ORIGIN, APP_BROWSER_URL, APP_PROTOCOL, BOOK_BROWSER_ORIGIN,
    BOOK_PROTOCOL, ResourceService, app_response,
};
use super::Bounds;

const IPC_SCRIPT: &str = concat!(
    "Object.defineProperty(window, 'ipc', { value: Object.freeze({ ",
    "postMessage: s => window.chrome.webview.postMessage(s) }) });"
);

type ScriptCallback = Box<dyn FnOnce(String) + Send>;

struct PendingScript {
    source: String,
    callback: Option<ScriptCallback>,
}

type PendingScripts = Arc<Mutex<Option<Vec<PendingScript>>>>;

#[derive(Default)]
struct HandlerTokens {
    navigation_starting: i64,
    web_message: i64,
    new_window: i64,
    permission: i64,
    resource: i64,
}

pub(super) struct NativeWebView {
    parent: Mutex<HWND>,
    hwnd: HWND,
    controller: ICoreWebView2Controller,
    webview: ICoreWebView2,
    _environment: ICoreWebView2Environment,
    tokens: HandlerTokens,
    loaded: Arc<AtomicBool>,
    pending: PendingScripts,
}

impl NativeWebView {
    pub(super) fn create(
        parent: HWND,
        bounds: Bounds,
        visible: bool,
        resources: ResourceService,
        on_ipc: impl Fn(HttpRequest<String>) + Send + Sync + 'static,
    ) -> Result<Self, ServiceError> {
        unsafe {
            let _ = CoInitializeEx(None, COINIT_APARTMENTTHREADED);
        }
        let hwnd = create_container(parent, bounds, visible)?;
        let result = (|| {
            let environment = create_environment()?;
            let controller = create_controller(hwnd, &environment)?;
            let webview =
                unsafe { controller.CoreWebView2().map_err(create_error)? };
            configure(&webview)?;
            add_document_script(&webview, IPC_SCRIPT)?;

            let loaded = Arc::new(AtomicBool::new(false));
            let pending = Arc::new(Mutex::new(Some(Vec::new())));
            let tokens = install_handlers(
                &webview,
                &environment,
                resources,
                Arc::new(on_ipc),
                Arc::clone(&loaded),
                Arc::clone(&pending),
            )?;
            unsafe {
                controller
                    .SetBounds(RECT {
                        left: 0,
                        top: 0,
                        right: bounds.width as i32,
                        bottom: bounds.height as i32,
                    })
                    .map_err(create_error)?;
                controller.SetIsVisible(visible).map_err(create_error)?;
                webview
                    .Navigate(&HSTRING::from(APP_BROWSER_URL))
                    .map_err(create_error)?;
            }
            Ok(Self {
                parent: Mutex::new(parent),
                hwnd,
                controller,
                webview,
                _environment: environment,
                tokens,
                loaded,
                pending,
            })
        })();
        if result.is_err() {
            unsafe {
                let _ = DestroyWindow(hwnd);
            }
        }
        result
    }

    pub(super) fn controller(&self) -> &ICoreWebView2Controller {
        &self.controller
    }

    pub(super) fn set_bounds(&self, bounds: Bounds) -> Result<(), String> {
        unsafe {
            self.controller
                .SetBounds(RECT {
                    left: 0,
                    top: 0,
                    right: bounds.width as i32,
                    bottom: bounds.height as i32,
                })
                .map_err(|error| error.to_string())?;
            SetWindowPos(
                self.hwnd,
                None,
                bounds.x,
                bounds.y,
                bounds.width as i32,
                bounds.height as i32,
                SWP_ASYNCWINDOWPOS | SWP_NOACTIVATE | SWP_NOZORDER,
            )
            .map_err(|error| error.to_string())?;
        }
        Ok(())
    }

    pub(super) fn set_parent(&self, parent: HWND) -> Result<(), String> {
        unsafe {
            SetParent(self.hwnd, Some(parent))
                .map_err(|error| error.to_string())?;
        }
        *self.parent.lock().map_err(|_| "parent lock failed")? = parent;
        Ok(())
    }

    pub(super) fn set_visible(&self, visible: bool) -> Result<(), String> {
        unsafe {
            let _ =
                ShowWindow(self.hwnd, if visible { SW_SHOW } else { SW_HIDE });
            self.controller
                .SetIsVisible(visible)
                .map_err(|error| error.to_string())?;
        }
        Ok(())
    }

    pub(super) fn focus(&self) -> Result<(), String> {
        unsafe {
            self.controller
                .MoveFocus(COREWEBVIEW2_MOVE_FOCUS_REASON_PROGRAMMATIC)
                .map_err(|error| error.to_string())
        }
    }

    pub(super) fn focus_parent(&self) -> Result<(), String> {
        let parent = *self.parent.lock().map_err(|_| "parent lock failed")?;
        unsafe {
            SetFocus(Some(parent)).map_err(|error| error.to_string())?;
        }
        Ok(())
    }

    pub(super) fn loaded(&self) -> bool {
        self.loaded.load(Ordering::Acquire)
    }

    pub(super) fn evaluate_script(&self, source: &str) -> Result<(), String> {
        self.evaluate(source, None)
    }

    pub(super) fn evaluate_script_with_callback(
        &self,
        source: &str,
        callback: impl FnOnce(String) + Send + 'static,
    ) -> Result<(), String> {
        self.evaluate(source, Some(Box::new(callback)))
    }

    fn evaluate(
        &self,
        source: &str,
        callback: Option<ScriptCallback>,
    ) -> Result<(), String> {
        let mut pending = self
            .pending
            .lock()
            .map_err(|_| "script queue lock failed".to_owned())?;
        if let Some(queue) = pending.as_mut() {
            queue.push(PendingScript {
                source: source.to_owned(),
                callback,
            });
            return Ok(());
        }
        drop(pending);
        execute_script(&self.webview, source, callback)
    }
}

impl Drop for NativeWebView {
    fn drop(&mut self) {
        unsafe {
            let _ = self
                .webview
                .remove_NavigationStarting(self.tokens.navigation_starting);
            let _ = self
                .webview
                .remove_WebMessageReceived(self.tokens.web_message);
            let _ = self
                .webview
                .remove_NewWindowRequested(self.tokens.new_window);
            let _ = self
                .webview
                .remove_PermissionRequested(self.tokens.permission);
            let _ = self
                .webview
                .remove_WebResourceRequested(self.tokens.resource);
            let _ = self.controller.Close();
            let _ = DestroyWindow(self.hwnd);
        }
    }
}

fn create_environment() -> Result<ICoreWebView2Environment, ServiceError> {
    let (sender, receiver) = mpsc::channel();
    unsafe {
        CreateCoreWebView2EnvironmentWithOptions(
            PCWSTR::null(),
            PCWSTR::null(),
            None,
            &CreateCoreWebView2EnvironmentCompletedHandler::create(Box::new(
                move |status, environment| {
                    let result: webview2_com::Result<ICoreWebView2Environment> =
                        (|| {
                            status?;
                            environment.ok_or_else(|| {
                                windows::core::Error::from(E_POINTER).into()
                            })
                        })();
                    sender
                        .send(result)
                        .map_err(|_| windows::core::Error::from(E_UNEXPECTED))
                },
            )),
        )
        .map_err(create_error)?;
    }
    webview2_com::wait_with_pump(receiver)
        .map_err(create_error)?
        .map_err(create_error)
}

fn create_controller(
    hwnd: HWND,
    environment: &ICoreWebView2Environment,
) -> Result<ICoreWebView2Controller, ServiceError> {
    let (sender, receiver) = mpsc::channel();
    let handler = CreateCoreWebView2ControllerCompletedHandler::create(
        Box::new(move |status, controller| {
            let result: webview2_com::Result<ICoreWebView2Controller> =
                (|| {
                    status?;
                    controller.ok_or_else(|| {
                        windows::core::Error::from(E_POINTER).into()
                    })
                })();
            sender
                .send(result)
                .map_err(|_| windows::core::Error::from(E_UNEXPECTED))
        }),
    );
    unsafe {
        environment
            .CreateCoreWebView2Controller(hwnd, &handler)
            .map_err(create_error)?;
    }
    webview2_com::wait_with_pump(receiver)
        .map_err(create_error)?
        .map_err(create_error)
}

fn configure(webview: &ICoreWebView2) -> Result<(), ServiceError> {
    unsafe {
        let settings = webview.Settings().map_err(create_error)?;
        settings
            .SetIsStatusBarEnabled(false)
            .map_err(create_error)?;
        settings
            .SetAreDefaultContextMenusEnabled(false)
            .map_err(create_error)?;
        settings
            .SetIsZoomControlEnabled(false)
            .map_err(create_error)?;
        settings
            .SetAreDevToolsEnabled(false)
            .map_err(create_error)?;
        settings.SetIsScriptEnabled(true).map_err(create_error)?;
        if let Ok(settings) = settings.cast::<ICoreWebView2Settings3>() {
            settings
                .SetAreBrowserAcceleratorKeysEnabled(false)
                .map_err(create_error)?;
        }
    }
    Ok(())
}

fn add_document_script(
    webview: &ICoreWebView2,
    source: &str,
) -> Result<(), ServiceError> {
    let webview = webview.clone();
    let source = source.to_owned();
    AddScriptToExecuteOnDocumentCreatedCompletedHandler::wait_for_async_operation(
        Box::new(move |handler| unsafe {
            webview
                .AddScriptToExecuteOnDocumentCreated(
                    &HSTRING::from(source),
                    &handler,
                )
                .map_err(Into::into)
        }),
        Box::new(|error, _| error),
    )
    .map_err(create_error)
}

fn install_handlers(
    webview: &ICoreWebView2,
    environment: &ICoreWebView2Environment,
    resources: ResourceService,
    on_ipc: Arc<dyn Fn(HttpRequest<String>) + Send + Sync>,
    loaded: Arc<AtomicBool>,
    pending: PendingScripts,
) -> Result<HandlerTokens, ServiceError> {
    let mut tokens = HandlerTokens::default();
    unsafe {
        webview
            .add_NavigationStarting(
                &NavigationStartingEventHandler::create(Box::new(
                    move |_webview, args| {
                        let Some(args) = args else {
                            return Ok(());
                        };
                        let mut uri = PWSTR::null();
                        args.Uri(&mut uri)?;
                        args.SetCancel(!app_navigation_allowed(take_pwstr(uri)))
                    },
                )),
                &mut tokens.navigation_starting,
            )
            .map_err(create_error)?;

        let message_pending = Arc::clone(&pending);
        let message_loaded = Arc::clone(&loaded);
        webview
            .add_WebMessageReceived(
                &WebMessageReceivedEventHandler::create(Box::new(
                    move |webview, args| {
                        let Some(webview) = webview else {
                            return Ok(());
                        };
                        let Some(args) = args else {
                            return Ok(());
                        };
                        let mut source = PWSTR::null();
                        let mut body = PWSTR::null();
                        args.Source(&mut source)?;
                        args.TryGetWebMessageAsString(&mut body)?;
                        if let Ok(request) = HttpRequest::builder()
                            .uri(take_pwstr(source))
                            .body(take_pwstr(body))
                        {
                            if shell_ready(&request) {
                                message_loaded.store(true, Ordering::Release);
                                let scripts = message_pending
                                    .lock()
                                    .ok()
                                    .and_then(|mut pending| pending.take())
                                    .unwrap_or_default();
                                for script in scripts {
                                    let _ = execute_script(
                                        &webview,
                                        &script.source,
                                        script.callback,
                                    );
                                }
                            } else {
                                on_ipc(request);
                            }
                        }
                        Ok(())
                    },
                )),
                &mut tokens.web_message,
            )
            .map_err(create_error)?;

        webview
            .add_NewWindowRequested(
                &NewWindowRequestedEventHandler::create(Box::new(
                    move |_webview, args| {
                        if let Some(args) = args {
                            args.SetHandled(true)?;
                        }
                        Ok(())
                    },
                )),
                &mut tokens.new_window,
            )
            .map_err(create_error)?;

        webview
            .add_PermissionRequested(
                &PermissionRequestedEventHandler::create(Box::new(
                    move |_webview, args| {
                        if let Some(args) = args {
                            args.SetState(COREWEBVIEW2_PERMISSION_STATE_DENY)?;
                        }
                        Ok(())
                    },
                )),
                &mut tokens.permission,
            )
            .map_err(create_error)?;

        add_resource_filter(webview, APP_BROWSER_ORIGIN)?;
        add_resource_filter(webview, BOOK_BROWSER_ORIGIN)?;
        let environment = environment.clone();
        webview
            .add_WebResourceRequested(
                &WebResourceRequestedEventHandler::create(Box::new(
                    move |_webview, args| {
                        let Some(args) = args else {
                            return Ok(());
                        };
                        let request = args.Request()?;
                        let mut uri = PWSTR::null();
                        let mut method = PWSTR::null();
                        request.Uri(&mut uri)?;
                        request.Method(&mut method)?;
                        let browser_uri = take_pwstr(uri);
                        let Some(uri) = protocol_uri(&browser_uri) else {
                            return Ok(());
                        };
                        let request = match HttpRequest::builder()
                            .uri(uri.as_str())
                            .method(take_pwstr(method).as_str())
                            .body(Vec::new())
                        {
                            Ok(request) => request,
                            Err(_) => return Ok(()),
                        };
                        let response = if uri.starts_with(APP_PROTOCOL) {
                            app_response(request)
                        } else {
                            resources.response(request)
                        };
                        let response =
                            web_resource_response(&environment, &response)?;
                        args.SetResponse(&response)
                    },
                )),
                &mut tokens.resource,
            )
            .map_err(create_error)?;
    }
    Ok(tokens)
}

unsafe fn add_resource_filter(
    webview: &ICoreWebView2,
    origin: &str,
) -> Result<(), ServiceError> {
    unsafe {
        webview
            .AddWebResourceRequestedFilter(
                &HSTRING::from(format!("{origin}/*")),
                COREWEBVIEW2_WEB_RESOURCE_CONTEXT_ALL,
            )
            .map_err(create_error)
    }
}

fn protocol_uri(uri: &str) -> Option<String> {
    if let Some(path) = uri.strip_prefix(APP_BROWSER_ORIGIN) {
        Some(format!("{APP_PROTOCOL}://localhost{path}"))
    } else {
        uri.strip_prefix(BOOK_BROWSER_ORIGIN)
            .map(|path| format!("{BOOK_PROTOCOL}://localhost{path}"))
    }
}

unsafe fn web_resource_response(
    environment: &ICoreWebView2Environment,
    response: &HttpResponse<Cow<'static, [u8]>>,
) -> windows::core::Result<ICoreWebView2WebResourceResponse> {
    let mut headers = String::new();
    for (name, value) in response.headers() {
        if let Ok(value) = value.to_str() {
            let _ = writeln!(headers, "{name}: {value}");
        }
    }
    unsafe {
        let stream = (!response.body().is_empty())
            .then(|| SHCreateMemStream(Some(response.body())))
            .flatten();
        environment.CreateWebResourceResponse(
            stream.as_ref(),
            i32::from(response.status().as_u16()),
            &HSTRING::from(
                response.status().canonical_reason().unwrap_or("OK"),
            ),
            &HSTRING::from(headers),
        )
    }
}

fn execute_script(
    webview: &ICoreWebView2,
    source: &str,
    callback: Option<ScriptCallback>,
) -> Result<(), String> {
    let callback = Mutex::new(callback);
    unsafe {
        webview
            .ExecuteScript(
                &HSTRING::from(source),
                &ExecuteScriptCompletedHandler::create(Box::new(
                    move |_status, result| {
                        if let Some(callback) = callback
                            .lock()
                            .ok()
                            .and_then(|mut callback| callback.take())
                        {
                            callback(result);
                        }
                        Ok(())
                    },
                )),
            )
            .map_err(|error| error.to_string())
    }
}

fn create_container(
    parent: HWND,
    bounds: Bounds,
    visible: bool,
) -> Result<HWND, ServiceError> {
    unsafe extern "system" fn window_proc(
        hwnd: HWND,
        message: u32,
        wparam: WPARAM,
        lparam: LPARAM,
    ) -> LRESULT {
        unsafe {
            if message == WM_SETFOCUS
                && let Ok(child) = GetWindow(hwnd, GW_CHILD)
            {
                let _ = SetFocus(Some(child));
            }
            DefWindowProcW(hwnd, message, wparam, lparam)
        }
    }

    let class_name = w!("YUNGE_READER_WEBVIEW");
    let instance =
        unsafe { GetModuleHandleW(PCWSTR::null()) }.map_err(create_error)?;
    let class = WNDCLASSEXW {
        cbSize: std::mem::size_of::<WNDCLASSEXW>() as u32,
        style: CS_HREDRAW | CS_VREDRAW,
        lpfnWndProc: Some(window_proc),
        cbClsExtra: 0,
        cbWndExtra: 0,
        hInstance: HINSTANCE(instance.0),
        hIcon: HICON::default(),
        hCursor: HCURSOR::default(),
        hbrBackground: HBRUSH::default(),
        lpszMenuName: PCWSTR::null(),
        lpszClassName: class_name,
        hIconSm: HICON::default(),
    };
    unsafe {
        let _ = RegisterClassExW(&class);
        let style = if visible {
            WS_CHILD | WS_CLIPCHILDREN | WS_VISIBLE
        } else {
            WS_CHILD | WS_CLIPCHILDREN
        };
        let hwnd = CreateWindowExW(
            WINDOW_EX_STYLE::default(),
            class_name,
            PCWSTR::null(),
            style,
            bounds.x,
            bounds.y,
            bounds.width as i32,
            bounds.height as i32,
            Some(parent),
            None,
            Some(instance.into()),
            None,
        )
        .map_err(create_error)?;
        SetWindowPos(
            hwnd,
            Some(HWND_TOP),
            0,
            0,
            0,
            0,
            SWP_ASYNCWINDOWPOS
                | SWP_NOACTIVATE
                | SWP_NOMOVE
                | SWP_NOOWNERZORDER
                | SWP_NOSIZE,
        )
        .map_err(create_error)?;
        Ok(hwnd)
    }
}

fn create_error(error: impl ToString) -> ServiceError {
    ServiceError::new("view-create-failed", error.to_string())
}

pub(super) fn webview_version() -> Result<String, String> {
    let mut version = PWSTR::null();
    unsafe {
        GetAvailableCoreWebView2BrowserVersionString(
            PCWSTR::null(),
            &mut version,
        )
        .map_err(|error| error.to_string())?;
    }
    Ok(take_pwstr(version))
}
