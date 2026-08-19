// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use emacs::{Env, Result, Value, defun};
#[cfg(any(target_os = "macos", target_os = "windows"))]
use std::cell::RefCell;
#[cfg(any(target_os = "macos", target_os = "windows"))]
use yunge_reader::webview::EmbeddedService;

emacs::plugin_is_GPL_compatible!();

#[cfg(any(target_os = "macos", target_os = "windows"))]
thread_local! {
    static SERVICE: RefCell<Option<EmbeddedService>> = const {
        RefCell::new(None)
    };
}

#[emacs::module(name = "yunge-reader-module")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

fn failure(message: impl Into<String>) -> emacs::Error {
    emacs::Error::msg(message.into())
}

#[defun]
fn start(env: &Env, pipe_process: Value<'_>) -> Result<bool> {
    #[cfg(any(target_os = "macos", target_os = "windows"))]
    {
        let writer = env.open_channel(pipe_process)?;
        SERVICE.with(|slot| {
            let mut slot = slot
                .try_borrow_mut()
                .map_err(|error| failure(error.to_string()))?;
            if slot.is_some() {
                return Ok(false);
            }
            *slot = Some(
                EmbeddedService::start(writer)
                    .map_err(|error| failure(error.to_string()))?,
            );
            Ok(true)
        })
    }
    #[cfg(not(any(target_os = "macos", target_os = "windows")))]
    {
        let _ = (env, pipe_process);
        Err(failure(
            "Yunge Reader EPUB WebViews require macOS or Windows",
        ))
    }
}

#[defun]
fn request(line: String) -> Result<bool> {
    #[cfg(any(target_os = "macos", target_os = "windows"))]
    {
        SERVICE.with(|slot| {
            let mut slot = slot
                .try_borrow_mut()
                .map_err(|error| failure(error.to_string()))?;
            let service = slot
                .as_mut()
                .ok_or_else(|| failure("Yunge Reader module is not running"))?;
            service
                .request(&line)
                .map_err(|error| failure(error.to_string()))?;
            Ok(true)
        })
    }
    #[cfg(not(any(target_os = "macos", target_os = "windows")))]
    {
        let _ = line;
        Err(failure(
            "Yunge Reader EPUB WebViews require macOS or Windows",
        ))
    }
}

#[defun]
fn pump() -> Result<bool> {
    #[cfg(any(target_os = "macos", target_os = "windows"))]
    {
        SERVICE.with(|slot| {
            let mut slot = slot
                .try_borrow_mut()
                .map_err(|error| failure(error.to_string()))?;
            let Some(service) = slot.as_mut() else {
                return Ok(false);
            };
            service.pump().map_err(|error| failure(error.to_string()))?;
            Ok(true)
        })
    }
    #[cfg(not(any(target_os = "macos", target_os = "windows")))]
    {
        Ok(false)
    }
}

#[defun]
fn stop() -> Result<bool> {
    #[cfg(any(target_os = "macos", target_os = "windows"))]
    {
        SERVICE.with(|slot| {
            let mut slot = slot
                .try_borrow_mut()
                .map_err(|error| failure(error.to_string()))?;
            Ok(slot.take().is_some())
        })
    }
    #[cfg(not(any(target_os = "macos", target_os = "windows")))]
    {
        Ok(false)
    }
}

#[defun]
fn running_p() -> Result<bool> {
    #[cfg(any(target_os = "macos", target_os = "windows"))]
    {
        SERVICE.with(|slot| {
            let slot = slot
                .try_borrow()
                .map_err(|error| failure(error.to_string()))?;
            Ok(slot.is_some())
        })
    }
    #[cfg(not(any(target_os = "macos", target_os = "windows")))]
    {
        Ok(false)
    }
}
