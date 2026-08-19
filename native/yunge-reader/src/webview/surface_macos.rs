// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use objc2::rc::Retained;
use objc2::{MainThreadMarker, MainThreadOnly};
use objc2_app_kit::{
    NSApplication, NSApplicationActivationOptions,
    NSApplicationActivationPolicy, NSBackingStoreType, NSEventMask, NSPanel,
    NSRunningApplication, NSScreen, NSView, NSWindowCollectionBehavior,
    NSWindowStyleMask,
};
use objc2_foundation::{NSDate, NSDefaultRunLoopMode, NSPoint, NSRect, NSSize};
use serde::{Deserialize, Serialize};
use std::ffi::c_void;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::ptr::NonNull;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use wry::dpi::{LogicalPosition, LogicalSize};
use wry::raw_window_handle::{
    AppKitWindowHandle, HandleError, HasWindowHandle, RawWindowHandle,
    WindowHandle,
};
use wry::{PageLoadEvent, Rect, WebView, WebViewBuilder};

use super::protocol::ServiceError;

const MAX_VIEW_EXTENT: u32 = 32_768;

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct Bounds {
    pub(super) x: i32,
    pub(super) y: i32,
    pub(super) width: u32,
    pub(super) height: u32,
}

pub(super) struct ParentWindow {
    application: Retained<NSRunningApplication>,
}

struct HostView(Retained<NSView>);

pub(super) struct NativeSurface {
    // Keep the webview before its host panel so it is released first.
    webview: WebView,
    panel: Retained<NSPanel>,
    parent: ParentWindow,
    loaded: Arc<AtomicBool>,
    bounds: Bounds,
    visible: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[allow(dead_code)]
pub(super) enum SurfaceEvent {
    Accelerator { view: u64, key: &'static str },
    FocusGained { view: u64 },
    FocusLost { view: u64 },
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

    fn webview_rect(self) -> Rect {
        Rect {
            position: LogicalPosition::new(0, 0).into(),
            size: LogicalSize::new(self.width, self.height).into(),
        }
    }

    fn panel_rect(self, mtm: MainThreadMarker) -> Result<NSRect, ServiceError> {
        let primary =
            NSScreen::screens(mtm).firstObject().ok_or_else(|| {
                ServiceError::new(
                    "view-create-failed",
                    "macOS did not report a primary screen",
                )
            })?;
        let primary_frame = primary.frame();
        let primary_top = primary_frame.origin.y + primary_frame.size.height;
        Ok(NSRect::new(
            NSPoint::new(
                f64::from(self.x),
                primary_top - f64::from(self.y) - f64::from(self.height),
            ),
            NSSize::new(f64::from(self.width), f64::from(self.height)),
        ))
    }
}

impl ParentWindow {
    pub(super) fn new(value: u64) -> Result<Self, ServiceError> {
        let pid = i32::try_from(value).map_err(|_| {
            ServiceError::new(
                "invalid-parent-window",
                "parent application PID does not fit this process",
            )
        })?;
        if pid <= 0 {
            return Err(ServiceError::new(
                "invalid-parent-window",
                "parent application PID must be positive",
            ));
        }
        let application =
            NSRunningApplication::runningApplicationWithProcessIdentifier(pid)
                .ok_or_else(|| {
                    ServiceError::new(
                        "invalid-parent-window",
                        "parent application PID is not running",
                    )
                })?;
        Ok(Self { application })
    }
}

impl HasWindowHandle for HostView {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        let pointer = NonNull::new(
            Retained::<NSView>::as_ptr(&self.0)
                .cast::<c_void>()
                .cast_mut(),
        )
        .expect("retained NSView is non-null");
        let raw = RawWindowHandle::AppKit(AppKitWindowHandle::new(pointer));
        // SAFETY: The retained NSView outlives the borrowed handle and Wry
        // copies the pointer while constructing the child WKWebView.
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

impl NativeSurface {
    pub(super) fn create<'a>(
        builder: WebViewBuilder<'a>,
        parent: &'a ParentWindow,
        _view: u64,
        bounds: Bounds,
        visible: bool,
        _on_event: impl Fn(SurfaceEvent) + Send + Sync + 'static,
    ) -> Result<Self, ServiceError> {
        let bounds = bounds.validate()?;
        let mtm = MainThreadMarker::new().ok_or_else(|| {
            ServiceError::new(
                "view-create-failed",
                "WKWebView surfaces must be created on the main thread",
            )
        })?;
        let application = NSApplication::sharedApplication(mtm);
        let _ = application
            .setActivationPolicy(NSApplicationActivationPolicy::Accessory);
        let panel = NSPanel::initWithContentRect_styleMask_backing_defer(
            NSPanel::alloc(mtm),
            bounds.panel_rect(mtm)?,
            NSWindowStyleMask::Borderless
                | NSWindowStyleMask::NonactivatingPanel,
            NSBackingStoreType::Buffered,
            false,
        );
        panel.setFloatingPanel(true);
        panel.setBecomesKeyOnlyIfNeeded(true);
        panel.setHasShadow(false);
        panel.setCollectionBehavior(
            NSWindowCollectionBehavior::Transient
                | NSWindowCollectionBehavior::FullScreenAuxiliary,
        );
        panel.setAcceptsMouseMovedEvents(true);
        let host = HostView(panel.contentView().ok_or_else(|| {
            ServiceError::new(
                "view-create-failed",
                "macOS did not create a content view for the EPUB panel",
            )
        })?);
        let loaded = Arc::new(AtomicBool::new(false));
        let load_state = Arc::clone(&loaded);
        let build = || {
            builder
                .with_bounds(bounds.webview_rect())
                .with_focused(false)
                .with_visible(visible)
                .with_on_page_load_handler(move |event, _url| {
                    if matches!(event, PageLoadEvent::Finished) {
                        load_state.store(true, Ordering::Release);
                    }
                })
                .build_as_child(&host)
        };
        let webview = catch_unwind(AssertUnwindSafe(build))
            .map_err(|_| {
                ServiceError::new(
                    "view-create-failed",
                    "WKWebView creation panicked for the EPUB panel",
                )
            })?
            .map_err(|error| {
                ServiceError::new("view-create-failed", error.to_string())
            })?;
        if visible {
            panel.orderFrontRegardless();
        } else {
            panel.orderOut(None);
        }
        Ok(Self {
            webview,
            panel,
            parent: ParentWindow {
                application: parent.application.clone(),
            },
            loaded,
            bounds,
            visible,
        })
    }

    pub(super) fn webview(&self) -> &WebView {
        &self.webview
    }

    pub(super) fn set_bounds(
        &mut self,
        bounds: Bounds,
    ) -> Result<Bounds, ServiceError> {
        let bounds = bounds.validate()?;
        let mtm = MainThreadMarker::new().ok_or_else(|| {
            ServiceError::new(
                "view-update-failed",
                "WKWebView bounds must be changed on the main thread",
            )
        })?;
        self.panel.setFrame_display(bounds.panel_rect(mtm)?, true);
        self.webview
            .set_bounds(bounds.webview_rect())
            .map_err(|error| {
                ServiceError::new("view-update-failed", error.to_string())
            })?;
        if self.visible {
            self.panel.orderFrontRegardless();
        }
        self.bounds = bounds;
        Ok(bounds)
    }

    pub(super) fn set_visible(
        &mut self,
        visible: bool,
    ) -> Result<(), ServiceError> {
        self.webview.set_visible(visible).map_err(|error| {
            ServiceError::new("view-update-failed", error.to_string())
        })?;
        if visible {
            self.panel.orderFrontRegardless();
        } else {
            self.panel.orderOut(None);
        }
        self.visible = visible;
        Ok(())
    }

    pub(super) fn focus(&self) -> Result<(), ServiceError> {
        self.panel.makeKeyAndOrderFront(None);
        self.webview.focus().map_err(|error| {
            ServiceError::new("view-focus-failed", error.to_string())
        })
    }

    pub(super) fn focus_parent(&self) -> Result<(), ServiceError> {
        self.webview.focus_parent().map_err(|error| {
            ServiceError::new("view-focus-failed", error.to_string())
        })?;
        let _ = self
            .parent
            .application
            .activateWithOptions(NSApplicationActivationOptions::empty());
        Ok(())
    }

    pub(super) fn loaded(&self) -> bool {
        self.loaded.load(Ordering::Acquire)
    }

    pub(super) fn bounds(&self) -> Bounds {
        self.bounds
    }

    pub(super) fn visible(&self) -> bool {
        self.visible
    }
}

impl Drop for NativeSurface {
    fn drop(&mut self) {
        self.panel.orderOut(None);
    }
}

pub(super) fn pump_messages() {
    let Some(mtm) = MainThreadMarker::new() else {
        return;
    };
    let application = NSApplication::sharedApplication(mtm);
    let expiration = NSDate::distantPast();
    // SAFETY: Foundation exposes this process-lifetime run-loop mode as an
    // immutable external NSString.
    let run_loop_mode = unsafe { NSDefaultRunLoopMode };
    while let Some(event) = application
        .nextEventMatchingMask_untilDate_inMode_dequeue(
            NSEventMask::Any,
            Some(&expiration),
            run_loop_mode,
            true,
        )
    {
        application.sendEvent(&event);
    }
    application.updateWindows();
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
