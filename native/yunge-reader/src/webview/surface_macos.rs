// SPDX-FileCopyrightText: 2020-2024 Tauri Programme within The Commons Conservancy
// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use http::Request as HttpRequest;
use objc2::rc::Retained;
use objc2::{MainThreadMarker, MainThreadOnly};
use objc2_app_kit::{NSApplication, NSResponder, NSScreen, NSView, NSWindow};
use objc2_foundation::{NSPoint, NSRect, NSSize};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

use super::protocol::ServiceError;
use super::renderer::RendererOrigin;

#[path = "surface_macos/webview.rs"]
mod webview;
use webview::NativeWebView;

const MAX_VIEW_EXTENT: u32 = 32_768;

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct Bounds {
    pub(super) x: i32,
    pub(super) y: i32,
    pub(super) width: u32,
    pub(super) height: u32,
}

#[derive(Clone)]
pub(super) struct ParentWindow {
    window: Retained<NSWindow>,
    content: Retained<NSView>,
    responder: Option<Retained<NSResponder>>,
}

#[derive(Clone)]
struct HostView(Retained<NSView>);

pub(super) struct NativeSurface {
    // Keep the webview before its native host so it is released first.
    webview: NativeWebView,
    host: HostView,
    parent: ParentWindow,
    bounds: Bounds,
    visible: bool,
}

pub(super) struct SurfaceRuntime;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[allow(dead_code)]
pub(super) enum SurfaceEvent {
    Accelerator {
        view: u64,
        key: &'static str,
        repeat: bool,
    },
    FocusGained {
        view: u64,
    },
    FocusLost {
        view: u64,
    },
}

type EventHandler = Arc<dyn Fn(SurfaceEvent) + Send + Sync>;
type IpcHandler = Arc<dyn Fn(HttpRequest<String>) + Send + Sync>;

pub(super) struct SurfaceCallbacks {
    ipc: IpcHandler,
    event: EventHandler,
}

impl SurfaceCallbacks {
    pub(super) fn new(
        on_ipc: impl Fn(HttpRequest<String>) + Send + Sync + 'static,
        on_event: impl Fn(SurfaceEvent) + Send + Sync + 'static,
    ) -> Self {
        Self {
            ipc: Arc::new(on_ipc),
            event: Arc::new(on_event),
        }
    }
}

impl Bounds {
    fn validate(self) -> Result<Self, ServiceError> {
        if self.x < -(MAX_VIEW_EXTENT as i32)
            || self.y < -(MAX_VIEW_EXTENT as i32)
        {
            return Err(ServiceError::new(
                "invalid-view-bounds",
                "view position lies outside the supported desktop extent",
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

    fn webview_rect(self) -> NSRect {
        NSRect::new(
            NSPoint::new(0.0, 0.0),
            NSSize::new(f64::from(self.width), f64::from(self.height)),
        )
    }

    fn embedded_rect(self, content: &NSView) -> NSRect {
        let content_height = content.bounds().size.height;
        NSRect::new(
            NSPoint::new(
                f64::from(self.x),
                content_height - f64::from(self.y) - f64::from(self.height),
            ),
            NSSize::new(f64::from(self.width), f64::from(self.height)),
        )
    }
}

impl ParentWindow {
    pub(super) fn current(
        _value: u64,
        frame: Option<Bounds>,
    ) -> Result<Self, ServiceError> {
        let mtm = MainThreadMarker::new().ok_or_else(|| {
            ServiceError::new(
                "view-create-failed",
                "WKWebView surfaces must be created on the main thread",
            )
        })?;
        let application = NSApplication::sharedApplication(mtm);
        let windows = application.windows();
        let candidates: Vec<_> = windows
            .iter()
            .filter(|window| {
                window.canBecomeMainWindow() && window.contentView().is_some()
            })
            .collect();
        let matched = frame.and_then(|expected| {
            let primary = NSScreen::screens(mtm).firstObject()?;
            let primary_frame = primary.frame();
            let primary_top =
                primary_frame.origin.y + primary_frame.size.height;
            let mut matching = candidates
                .iter()
                .filter(|window| frame_matches(window, expected, primary_top));
            let candidate = matching.next()?.clone();
            matching.next().is_none().then_some(candidate)
        });
        let window = matched
            .or_else(|| {
                (frame.is_none()).then(|| application.keyWindow()).flatten()
            })
            .or_else(|| {
                (frame.is_none())
                    .then(|| application.mainWindow())
                    .flatten()
            })
            .or_else(|| (candidates.len() == 1).then(|| candidates[0].clone()))
            .ok_or_else(|| {
                ServiceError::new(
                    "invalid-parent-window",
                    concat!(
                        "Emacs has no unambiguous frame for the EPUB ",
                        "surface"
                    ),
                )
            })?;
        let content = window.contentView().ok_or_else(|| {
            ServiceError::new(
                "invalid-parent-window",
                "the selected Emacs frame has no content view",
            )
        })?;
        let responder = window.firstResponder();
        Ok(Self {
            window,
            content,
            responder,
        })
    }
}

fn frame_matches(
    window: &NSWindow,
    expected: Bounds,
    primary_top: f64,
) -> bool {
    let Some(content) = window.contentView() else {
        return false;
    };
    let content_size = content.bounds().size;
    let frame = window.frame();
    let top = primary_top - frame.origin.y - frame.size.height;
    close_to(content_size.width, f64::from(expected.width), 4.0)
        && close_to(content_size.height, f64::from(expected.height), 4.0)
        && close_to(frame.origin.x, f64::from(expected.x), 4.0)
        && close_to(top, f64::from(expected.y), 64.0)
}

fn close_to(left: f64, right: f64, tolerance: f64) -> bool {
    (left - right).abs() <= tolerance
}

impl NativeSurface {
    fn create(
        parent: &ParentWindow,
        view: u64,
        bounds: Bounds,
        visible: bool,
        renderer: RendererOrigin,
        callbacks: SurfaceCallbacks,
    ) -> Result<Self, ServiceError> {
        let bounds = bounds.validate()?;
        let mtm = MainThreadMarker::new().ok_or_else(|| {
            ServiceError::new(
                "view-create-failed",
                "WKWebView surfaces must be created on the main thread",
            )
        })?;
        let host = HostView(NSView::initWithFrame(
            NSView::alloc(mtm),
            bounds.embedded_rect(&parent.content),
        ));
        host.0.setHidden(!visible);
        parent.content.addSubview(&host.0);
        let ipc = callbacks.ipc;
        let event = callbacks.event;
        let webview = NativeWebView::create(
            &host.0,
            view,
            renderer,
            move |request| ipc(request),
            move |surface_event| event(surface_event),
        )?;
        webview.set_frame(bounds.webview_rect());
        Ok(Self {
            webview,
            host,
            parent: parent.clone(),
            bounds,
            visible,
        })
    }

    pub(super) fn evaluate_script(&self, script: &str) -> Result<(), String> {
        self.webview.evaluate_script(script)
    }

    pub(super) fn evaluate_script_with_callback(
        &self,
        script: &str,
        callback: impl FnOnce(String) + Send + 'static,
    ) -> Result<(), String> {
        self.webview.evaluate_script_with_callback(script, callback)
    }

    pub(super) fn set_bounds(
        &mut self,
        bounds: Bounds,
    ) -> Result<Bounds, ServiceError> {
        let bounds = bounds.validate()?;
        self.host
            .0
            .setFrame(bounds.embedded_rect(&self.parent.content));
        self.webview.set_frame(bounds.webview_rect());
        self.bounds = bounds;
        Ok(bounds)
    }

    pub(super) fn set_visible(
        &mut self,
        visible: bool,
    ) -> Result<(), ServiceError> {
        self.host.0.setHidden(!visible);
        self.visible = visible;
        Ok(())
    }

    pub(super) fn focus(&self) -> Result<(), ServiceError> {
        if self.webview.focus() {
            Ok(())
        } else {
            Err(ServiceError::new(
                "view-focus-failed",
                "macOS could not focus the EPUB WKWebView",
            ))
        }
    }

    pub(super) fn focus_parent(&self) -> Result<(), ServiceError> {
        if self
            .parent
            .window
            .makeFirstResponder(self.parent.responder.as_deref())
        {
            Ok(())
        } else {
            Err(ServiceError::new(
                "view-focus-failed",
                "macOS could not restore the Emacs first responder",
            ))
        }
    }

    pub(super) fn loaded(&self) -> bool {
        self.webview.loaded()
    }

    pub(super) fn bounds(&self) -> Bounds {
        self.bounds
    }

    pub(super) fn visible(&self) -> bool {
        self.visible
    }
}

impl SurfaceRuntime {
    pub(super) fn new() -> Self {
        Self
    }

    pub(super) fn create(
        &mut self,
        parent: &ParentWindow,
        view: u64,
        bounds: Bounds,
        visible: bool,
        renderer: RendererOrigin,
        callbacks: SurfaceCallbacks,
    ) -> Result<NativeSurface, ServiceError> {
        NativeSurface::create(
            parent, view, bounds, visible, renderer, callbacks,
        )
    }
}

pub(super) fn webview_version() -> Result<String, String> {
    webview::webview_version()
}

impl Drop for NativeSurface {
    fn drop(&mut self) {
        self.host.0.removeFromSuperview();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn bounds_reject_empty_and_extreme_rectangles() {
        for bounds in [
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
            Bounds {
                x: -(MAX_VIEW_EXTENT as i32) - 1,
                y: 0,
                width: 1,
                height: 1,
            },
        ] {
            assert!(bounds.validate().is_err());
        }
    }
}
