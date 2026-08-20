// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

pub mod broker;
pub mod epub;

#[cfg(any(target_os = "windows", target_os = "macos"))]
pub mod webview;

pub(crate) type Error = Box<dyn std::error::Error>;

pub(crate) const BUILD_ID: &str = env!("YUNGE_READER_BUILD_ID");
