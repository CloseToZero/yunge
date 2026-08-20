// SPDX-FileCopyrightText: 2020-2024 Tauri Programme within The Commons Conservancy
// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use block2::{DynBlock, RcBlock};
use http::Request as HttpRequest;
use objc2::rc::Retained;
use objc2::runtime::{AnyObject, NSObject, ProtocolObject};
use objc2::{
    AnyThread, DeclaredClass, MainThreadMarker, MainThreadOnly, define_class,
    msg_send,
};
use objc2_app_kit::{NSAutoresizingMaskOptions, NSView};
use objc2_foundation::{
    NSError, NSJSONSerialization, NSJSONWritingOptions, NSObjectProtocol,
    NSString, NSURL, NSURLRequest, NSUTF8StringEncoding,
};
use objc2_web_kit::{
    WKNavigationAction, WKNavigationActionPolicy, WKNavigationDelegate,
    WKScriptMessage, WKScriptMessageHandler, WKUserContentController,
    WKUserScript, WKUserScriptInjectionTime, WKWebView, WKWebViewConfiguration,
};
use std::cell::RefCell;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use super::super::protocol::ServiceError;
use super::super::renderer::{RendererOrigin, shell_ready_for};
use super::SurfaceEvent;

const IPC_HANDLER: &str = "ipc";
const IPC_SCRIPT: &str = r#"Object.defineProperty(window, 'ipc', {
  value: Object.freeze({postMessage: function(s) {
    window.webkit.messageHandlers.ipc.postMessage(s);
  }})
});"#;

type IpcHandler = Arc<dyn Fn(HttpRequest<String>) + Send + Sync>;
type ScriptCallback = Box<dyn FnOnce(String) + Send>;

struct PendingScript {
    source: String,
    callback: Option<ScriptCallback>,
}

type PendingScripts = Rc<RefCell<Option<Vec<PendingScript>>>>;

struct MessageHandlerIvars {
    controller: Retained<WKUserContentController>,
    callback: IpcHandler,
    loaded: Arc<AtomicBool>,
    pending: PendingScripts,
    renderer: RendererOrigin,
}

define_class!(
    #[unsafe(super(NSObject))]
    #[thread_kind = MainThreadOnly]
    #[ivars = MessageHandlerIvars]
    struct MessageHandler;

    unsafe impl NSObjectProtocol for MessageHandler {}

    unsafe impl WKScriptMessageHandler for MessageHandler {
        #[unsafe(method(userContentController:didReceiveScriptMessage:))]
        fn did_receive(
            &self,
            _controller: &WKUserContentController,
            message: &WKScriptMessage,
        ) {
            let body = unsafe { message.body() };
            let Ok(body) = body.downcast::<NSString>() else {
                return;
            };
            let frame = unsafe { message.frameInfo() };
            let request = unsafe { frame.request() };
            let Some(url) = request.URL() else {
                return;
            };
            let Some(url) = url.absoluteString() else {
                return;
            };
            let Ok(request) = HttpRequest::builder()
                .uri(url.to_string())
                .body(body.to_string())
            else {
                return;
            };
            if shell_ready_for(&self.ivars().renderer, &request) {
                let Some(webview) = (unsafe { message.webView() }) else {
                    return;
                };
                self.ivars().loaded.store(true, Ordering::Release);
                let pending = self.ivars().pending.borrow_mut().take();
                for script in pending.unwrap_or_default() {
                    evaluate_now(&webview, &script.source, script.callback);
                }
                return;
            }
            (self.ivars().callback)(request);
        }
    }
);

impl MessageHandler {
    fn new(
        controller: Retained<WKUserContentController>,
        callback: IpcHandler,
        loaded: Arc<AtomicBool>,
        pending: PendingScripts,
        renderer: RendererOrigin,
        mtm: MainThreadMarker,
    ) -> Retained<Self> {
        let object = mtm.alloc::<Self>().set_ivars(MessageHandlerIvars {
            controller,
            callback,
            loaded,
            pending,
            renderer,
        });
        let object: Retained<Self> = unsafe { msg_send![super(object), init] };
        let protocol = ProtocolObject::from_ref(&*object);
        unsafe {
            object.ivars().controller.addScriptMessageHandler_name(
                protocol,
                &NSString::from_str(IPC_HANDLER),
            );
        }
        object
    }
}

struct NavigationDelegateIvars {
    renderer: RendererOrigin,
}

define_class!(
    #[unsafe(super(NSObject))]
    #[thread_kind = MainThreadOnly]
    #[ivars = NavigationDelegateIvars]
    struct NavigationDelegate;

    unsafe impl NSObjectProtocol for NavigationDelegate {}

    unsafe impl WKNavigationDelegate for NavigationDelegate {
        #[unsafe(method(webView:decidePolicyForNavigationAction:decisionHandler:))]
        fn navigation_policy(
            &self,
            _webview: &WKWebView,
            action: &WKNavigationAction,
            decision: &DynBlock<dyn Fn(WKNavigationActionPolicy)>,
        ) {
            let request = unsafe { action.request() };
            let allowed = request
                .URL()
                .and_then(|url| url.absoluteString())
                .is_some_and(|url| {
                    self.ivars().renderer.navigation_allowed(&url.to_string())
                });
            decision.call((if allowed {
                WKNavigationActionPolicy::Allow
            } else {
                WKNavigationActionPolicy::Cancel
            },));
        }
    }
);

impl NavigationDelegate {
    fn new(renderer: RendererOrigin, mtm: MainThreadMarker) -> Retained<Self> {
        let object = mtm
            .alloc::<Self>()
            .set_ivars(NavigationDelegateIvars { renderer });
        unsafe { msg_send![super(object), init] }
    }
}

pub(super) struct NativeWebView {
    webview: Retained<WKWebView>,
    manager: Retained<WKUserContentController>,
    _message_handler: Retained<MessageHandler>,
    _navigation_delegate: Retained<NavigationDelegate>,
    loaded: Arc<AtomicBool>,
    pending: PendingScripts,
}

impl NativeWebView {
    pub(super) fn create(
        host: &NSView,
        _view: u64,
        renderer: RendererOrigin,
        on_ipc: impl Fn(HttpRequest<String>) + Send + Sync + 'static,
        _on_event: impl Fn(SurfaceEvent) + Send + Sync + 'static,
    ) -> Result<Self, ServiceError> {
        let mtm = MainThreadMarker::new().ok_or_else(|| {
            ServiceError::new(
                "view-create-failed",
                "WKWebView must be created on the main thread",
            )
        })?;
        let renderer_url =
            NSURL::URLWithString(&NSString::from_str(renderer.url()))
                .ok_or_else(|| {
                    ServiceError::new(
                        "invalid-renderer-url",
                        "WKWebView rejected the EPUB renderer URL",
                    )
                })?;
        let created = catch_unwind(AssertUnwindSafe(|| unsafe {
            let configuration = WKWebViewConfiguration::new(mtm);
            let manager = configuration.userContentController();

            let user_script =
                WKUserScript::initWithSource_injectionTime_forMainFrameOnly(
                    WKUserScript::alloc(mtm),
                    &NSString::from_str(IPC_SCRIPT),
                    WKUserScriptInjectionTime::AtDocumentStart,
                    true,
                );
            manager.addUserScript(&user_script);
            let loaded = Arc::new(AtomicBool::new(false));
            let pending = Rc::new(RefCell::new(Some(Vec::new())));
            let message_handler = MessageHandler::new(
                manager.clone(),
                Arc::new(on_ipc),
                Arc::clone(&loaded),
                Rc::clone(&pending),
                renderer.clone(),
                mtm,
            );
            let navigation_delegate =
                NavigationDelegate::new(renderer.clone(), mtm);
            let webview = WKWebView::initWithFrame_configuration(
                WKWebView::alloc(mtm),
                host.bounds(),
                &configuration,
            );
            webview.setAutoresizingMask(
                NSAutoresizingMaskOptions::ViewWidthSizable
                    | NSAutoresizingMaskOptions::ViewHeightSizable,
            );
            webview.setNavigationDelegate(Some(ProtocolObject::from_ref(
                &*navigation_delegate,
            )));
            host.addSubview(&webview);

            let request = NSURLRequest::requestWithURL(&renderer_url);
            let _ = webview.loadRequest(&request);
            Self {
                webview,
                manager,
                _message_handler: message_handler,
                _navigation_delegate: navigation_delegate,
                loaded,
                pending,
            }
        }));
        created.map_err(|_| {
            ServiceError::new(
                "view-create-failed",
                "WKWebView creation failed for the EPUB surface",
            )
        })
    }

    pub(super) fn set_frame(&self, frame: objc2_foundation::NSRect) {
        self.webview.setFrame(frame);
    }

    pub(super) fn focus(&self) -> bool {
        self.webview.window().is_some_and(|window| {
            window.makeFirstResponder(Some(&self.webview))
        })
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
        if let Some(pending) = self.pending.borrow_mut().as_mut() {
            pending.push(PendingScript {
                source: source.to_owned(),
                callback,
            });
        } else {
            evaluate_now(&self.webview, source, callback);
        }
        Ok(())
    }
}

impl Drop for NativeWebView {
    fn drop(&mut self) {
        unsafe {
            self.manager.removeScriptMessageHandlerForName(
                &NSString::from_str(IPC_HANDLER),
            );
            self.webview.setNavigationDelegate(None);
        }
        self.webview.removeFromSuperview();
    }
}

fn evaluate_now(
    webview: &WKWebView,
    source: &str,
    callback: Option<ScriptCallback>,
) {
    unsafe {
        if let Some(callback) = callback {
            let callback = Rc::new(RefCell::new(Some(callback)));
            let completion = RcBlock::new(
                move |value: *mut AnyObject, _error: *mut NSError| {
                    if let Some(callback) = callback.borrow_mut().take() {
                        callback(json_string(value));
                    }
                },
            );
            webview.evaluateJavaScript_completionHandler(
                &NSString::from_str(source),
                Some(&completion),
            );
        } else {
            webview.evaluateJavaScript_completionHandler(
                &NSString::from_str(source),
                None,
            );
        }
    }
}

fn json_string(value: *mut AnyObject) -> String {
    let Some(value) = (unsafe { value.as_ref() }) else {
        return String::new();
    };
    unsafe {
        let Ok(data) = NSJSONSerialization::dataWithJSONObject_options_error(
            value,
            NSJSONWritingOptions::FragmentsAllowed,
        ) else {
            return String::new();
        };
        NSString::initWithData_encoding(
            NSString::alloc(),
            &data,
            NSUTF8StringEncoding,
        )
        .map(|value| value.to_string())
        .unwrap_or_default()
    }
}

pub(super) fn webview_version() -> Result<String, String> {
    use objc2_foundation::{NSBundle, NSDictionary};

    let bundle =
        NSBundle::bundleWithIdentifier(&NSString::from_str("com.apple.WebKit"))
            .ok_or_else(|| {
                "WebKit framework bundle is unavailable".to_owned()
            })?;
    let info: Retained<NSDictionary<NSString, AnyObject>> =
        bundle.infoDictionary().ok_or_else(|| {
            "WebKit bundle has no information dictionary".to_owned()
        })?;
    let key = NSString::from_str("CFBundleVersion");
    let value = info
        .objectForKey(&key)
        .ok_or_else(|| "WebKit bundle has no version".to_owned())?;
    value
        .downcast::<NSString>()
        .map(|value| value.to_string())
        .map_err(|_| "WebKit bundle version is not a string".to_owned())
}
