// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use serde::{Deserialize, Serialize};
use std::num::NonZeroIsize;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use webview2_com::Microsoft::Web::WebView2::Win32::{
    COREWEBVIEW2_KEY_EVENT_KIND, COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN,
    COREWEBVIEW2_KEY_EVENT_KIND_SYSTEM_KEY_DOWN,
};
use webview2_com::{
    AcceleratorKeyPressedEventHandler, FocusChangedEventHandler,
};
use windows::Win32::Foundation::HWND;
use windows::Win32::UI::Input::KeyboardAndMouse::{
    GetKeyState, VIRTUAL_KEY, VK_CONTROL, VK_MENU, VK_NEXT, VK_PRIOR, VK_SHIFT,
    VK_SPACE,
};
use windows::Win32::UI::WindowsAndMessaging::IsWindow;
use wry::dpi::{PhysicalPosition, PhysicalSize};
use wry::raw_window_handle::{
    HandleError, HasWindowHandle, RawWindowHandle, Win32WindowHandle,
    WindowHandle,
};
use wry::{PageLoadEvent, Rect, WebView, WebViewBuilder, WebViewExtWindows};

use super::protocol::{ACCELERATORS, ServiceError};

const ESCAPE_VIRTUAL_KEY: u32 = 0x1b;
const MAX_VIEW_EXTENT: u32 = 32_768;

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct Bounds {
    pub(super) x: i32,
    pub(super) y: i32,
    pub(super) width: u32,
    pub(super) height: u32,
}

pub(super) struct ParentWindow(NonZeroIsize);

pub(super) struct NativeSurface {
    webview: WebView,
    accelerator_token: i64,
    got_focus_token: i64,
    lost_focus_token: i64,
    loaded: Arc<AtomicBool>,
    bounds: Bounds,
    visible: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum SurfaceEvent {
    Accelerator { view: u64, key: &'static str },
    FocusGained { view: u64 },
    FocusLost { view: u64 },
}

type EventHandler = Arc<dyn Fn(SurfaceEvent) + Send + Sync>;

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

impl ParentWindow {
    pub(super) fn new(value: u64) -> Result<Self, ServiceError> {
        let value = isize::try_from(value).map_err(|_| {
            ServiceError::new(
                "invalid-parent-window",
                "parent window handle does not fit this process",
            )
        })?;
        let value = NonZeroIsize::new(value).ok_or_else(|| {
            ServiceError::new(
                "invalid-parent-window",
                "parent window handle must be nonzero",
            )
        })?;
        let hwnd = HWND(value.get() as _);
        if !unsafe { IsWindow(Some(hwnd)) }.as_bool() {
            return Err(ServiceError::new(
                "invalid-parent-window",
                "parent window handle does not name a live window",
            ));
        }
        Ok(Self(value))
    }
}

impl HasWindowHandle for ParentWindow {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        let handle = Win32WindowHandle::new(self.0);
        let raw = RawWindowHandle::Win32(handle);
        // SAFETY: `ParentWindow::new' checks that the HWND names a live
        // window. Pinned Wry copies only the HWND on its Windows child path;
        // this private adapter is not exposed to other raw-handle consumers.
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

impl NativeSurface {
    pub(super) fn create<'a>(
        builder: WebViewBuilder<'a>,
        parent: &'a ParentWindow,
        view: u64,
        bounds: Bounds,
        visible: bool,
        on_event: impl Fn(SurfaceEvent) + Send + Sync + 'static,
    ) -> Result<Self, ServiceError> {
        let bounds = bounds.validate()?;
        let loaded = Arc::new(AtomicBool::new(false));
        let load_state = Arc::clone(&loaded);
        let build = || {
            builder
                .with_bounds(bounds.rect())
                .with_focused(false)
                .with_visible(visible)
                .with_on_page_load_handler(move |event, _url| {
                    if matches!(event, PageLoadEvent::Finished) {
                        load_state.store(true, Ordering::Release);
                    }
                })
                .build_as_child(parent)
        };
        let webview = catch_unwind(AssertUnwindSafe(build))
            .map_err(|_| {
                ServiceError::new(
                    "view-create-failed",
                    "WebView creation panicked for the supplied parent window",
                )
            })?
            .map_err(|error| {
                ServiceError::new("view-create-failed", error.to_string())
            })?;
        let on_event: EventHandler = Arc::new(on_event);
        let accelerator_token =
            install_accelerator_handler(&webview, view, Arc::clone(&on_event))?;
        let (got_focus_token, lost_focus_token) =
            install_focus_handlers(&webview, view, on_event)?;
        Ok(Self {
            webview,
            accelerator_token,
            got_focus_token,
            lost_focus_token,
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
        self.webview.set_bounds(bounds.rect()).map_err(|error| {
            ServiceError::new("view-update-failed", error.to_string())
        })?;
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
        self.visible = visible;
        Ok(())
    }

    pub(super) fn focus(&self) -> Result<(), ServiceError> {
        self.webview.focus().map_err(|error| {
            ServiceError::new("view-focus-failed", error.to_string())
        })
    }

    pub(super) fn focus_parent(&self) -> Result<(), ServiceError> {
        self.webview.focus_parent().map_err(|error| {
            ServiceError::new("view-focus-failed", error.to_string())
        })
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
        // SAFETY: The tokens were registered on this controller, and all
        // WebView operations run on the service's single UI thread.
        unsafe {
            let controller = self.webview.controller();
            let _ =
                controller.remove_AcceleratorKeyPressed(self.accelerator_token);
            let _ = controller.remove_GotFocus(self.got_focus_token);
            let _ = controller.remove_LostFocus(self.lost_focus_token);
        }
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
    let routed = if key == ESCAPE_VIRTUAL_KEY {
        Some("<escape>")
    } else if shift {
        None
    } else if alt {
        (!control && key == u32::from(b'M')).then_some("M-m")
    } else {
        match (key, control) {
            (key, false) if key == u32::from(VK_SPACE.0) => Some("SPC"),
            (key, true) if key == u32::from(b'G') => Some("C-g"),
            (key, true) if key == u32::from(b'D') => Some("C-d"),
            (key, true) if key == u32::from(b'U') => Some("C-u"),
            (key, false) if key == u32::from(VK_NEXT.0) => Some("<next>"),
            (key, false) if key == u32::from(VK_PRIOR.0) => Some("<prior>"),
            _ => None,
        }
    };
    debug_assert!(routed.is_none_or(|key| ACCELERATORS.contains(&key)));
    routed
}

fn key_state(key: VIRTUAL_KEY) -> bool {
    // SAFETY: `GetKeyState' accepts every Win32 virtual-key value and has no
    // pointer or lifetime requirements.
    unsafe { GetKeyState(i32::from(key.0)) < 0 }
}

fn install_accelerator_handler(
    webview: &WebView,
    view: u64,
    on_event: EventHandler,
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
                    on_event(SurfaceEvent::Accelerator { view, key });
                }
            }
            Ok(())
        },
    ));
    let mut token = 0;
    // SAFETY: The callback remains owned by the controller until its token
    // is removed when `NativeSurface' is dropped.
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

fn install_focus_handlers(
    webview: &WebView,
    view: u64,
    on_event: EventHandler,
) -> Result<(i64, i64), ServiceError> {
    let got_events = Arc::clone(&on_event);
    let got_handler = FocusChangedEventHandler::create(Box::new(
        move |_controller, _args| {
            got_events(SurfaceEvent::FocusGained { view });
            Ok(())
        },
    ));
    let lost_handler = FocusChangedEventHandler::create(Box::new(
        move |_controller, _args| {
            on_event(SurfaceEvent::FocusLost { view });
            Ok(())
        },
    ));
    let controller = webview.controller();
    let mut got_token = 0;
    let mut lost_token = 0;
    // SAFETY: The controller owns both callbacks until their returned tokens
    // are removed when `NativeSurface' is dropped.
    unsafe {
        controller
            .add_GotFocus(&got_handler, &mut got_token)
            .map_err(|error| {
                ServiceError::new("view-create-failed", error.to_string())
            })?;
        if let Err(error) =
            controller.add_LostFocus(&lost_handler, &mut lost_token)
        {
            let _ = controller.remove_GotFocus(got_token);
            return Err(ServiceError::new(
                "view-create-failed",
                error.to_string(),
            ));
        }
    }
    Ok((got_token, lost_token))
}

#[cfg(test)]
mod tests {
    use webview2_com::Microsoft::Web::WebView2::Win32 as WebView2;

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
    fn native_reader_control_and_leader_keys_are_normalized() {
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
                u32::from(VK_SPACE.0),
                false,
                false,
                false,
            ),
            Some("SPC")
        );
        assert_eq!(
            routed_key(
                COREWEBVIEW2_KEY_EVENT_KIND_SYSTEM_KEY_DOWN,
                u32::from(b'M'),
                false,
                true,
                false,
            ),
            Some("M-m")
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
