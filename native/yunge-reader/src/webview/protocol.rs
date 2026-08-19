// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use serde::{Deserialize, Serialize};
use serde_json::Value;

use super::ViewEvent;

pub(super) const PROTOCOL_VERSION: u32 = 1;
pub(super) const ACCELERATORS: [&str; 20] = [
    "'", "+", "-", "=", "<escape>", "<next>", "<prior>", "C-d", "C-g", "C-u",
    "G", "J", "K", "M-m", "SPC", "g", "j", "k", "m", "y",
];
pub(super) const RENDERER_ACCELERATORS: [&str; 20] = ACCELERATORS;

#[cfg(any(target_os = "windows", test))]
pub(super) fn control_accelerator(key: u8) -> Option<&'static str> {
    ACCELERATORS.iter().copied().find(|accelerator| {
        let bytes = accelerator.as_bytes();
        bytes.len() == 3
            && bytes[0] == b'C'
            && bytes[1] == b'-'
            && bytes[2].eq_ignore_ascii_case(&key)
    })
}
pub(super) const CAPABILITIES: [&str; 26] = [
    "publication-close",
    "publication-info",
    "publication-open",
    "publication-resources",
    "view-bounds",
    "view-appearance",
    "view-clear-selection",
    "view-create",
    "view-destroy",
    "view-events",
    "view-focus",
    "view-focus-parent",
    "view-info",
    "view-navigate",
    "view-open-publication",
    "view-parent",
    "view-search",
    "view-search-result",
    "view-current-selection",
    "view-selection-text",
    "view-set-selection",
    "view-scroll-bars",
    "view-status",
    "view-style",
    "view-visible",
    "view-zoom",
];

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct Request {
    pub(super) id: u64,
    pub(super) op: String,
    #[serde(default)]
    pub(super) params: Value,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum Operation {
    Shutdown,
    PublicationOpen,
    PublicationInfo,
    PublicationClose,
    ViewInfo,
    ViewCreate,
    ViewBounds,
    ViewAppearance,
    ViewClearSelection,
    ViewNavigate,
    ViewSearch,
    ViewSearchResult,
    ViewCurrentSelection,
    ViewSelectionText,
    ViewSetSelection,
    ViewOpenPublication,
    ViewParent,
    ViewStyle,
    ViewScrollBars,
    ViewVisible,
    ViewFocus,
    ViewFocusParent,
    ViewStatus,
    ViewDestroy,
    ViewZoom,
}

impl Request {
    pub(super) fn decode(line: &str) -> Result<Self, String> {
        serde_json::from_str(line).map_err(|error| error.to_string())
    }

    pub(super) fn operation(&self) -> Result<Operation, ServiceError> {
        let operation = match self.op.as_str() {
            "shutdown" => Operation::Shutdown,
            "publication-open" => Operation::PublicationOpen,
            "publication-info" => Operation::PublicationInfo,
            "publication-close" => Operation::PublicationClose,
            "view-info" => Operation::ViewInfo,
            "view-create" => Operation::ViewCreate,
            "view-bounds" => Operation::ViewBounds,
            "view-appearance" => Operation::ViewAppearance,
            "view-clear-selection" => Operation::ViewClearSelection,
            "view-navigate" => Operation::ViewNavigate,
            "view-search" => Operation::ViewSearch,
            "view-search-result" => Operation::ViewSearchResult,
            "view-current-selection" => Operation::ViewCurrentSelection,
            "view-selection-text" => Operation::ViewSelectionText,
            "view-set-selection" => Operation::ViewSetSelection,
            "view-open-publication" => Operation::ViewOpenPublication,
            "view-parent" => Operation::ViewParent,
            "view-style" => Operation::ViewStyle,
            "view-scroll-bars" => Operation::ViewScrollBars,
            "view-visible" => Operation::ViewVisible,
            "view-focus" => Operation::ViewFocus,
            "view-focus-parent" => Operation::ViewFocusParent,
            "view-status" => Operation::ViewStatus,
            "view-destroy" => Operation::ViewDestroy,
            "view-zoom" => Operation::ViewZoom,
            _ => {
                return Err(ServiceError::new(
                    "unsupported-operation",
                    format!("unsupported operation: {}", self.op),
                ));
            }
        };
        Ok(operation)
    }
}

#[derive(Debug, Serialize)]
pub(super) struct ProtocolError {
    pub(super) code: &'static str,
    pub(super) message: String,
}

#[derive(Debug, Serialize)]
pub(super) struct Response {
    pub(super) id: Option<u64>,
    pub(super) ok: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) error: Option<ProtocolError>,
}

impl Response {
    pub(super) fn success(id: u64, result: Value) -> Self {
        Self {
            id: Some(id),
            ok: true,
            result: Some(result),
            error: None,
        }
    }

    pub(super) fn failure(
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

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub(super) enum Outgoing {
    Response(Response),
    Event(ViewEvent),
    Wake,
}

#[derive(Debug)]
pub(super) struct ServiceError {
    pub(super) code: &'static str,
    pub(super) message: String,
}

impl ServiceError {
    pub(super) fn new(code: &'static str, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum Control {
    Continue,
    Shutdown,
}

pub(super) fn response(
    id: u64,
    result: Result<Value, ServiceError>,
) -> Response {
    match result {
        Ok(value) => Response::success(id, value),
        Err(error) => Response::failure(Some(id), error.code, error.message),
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::{
        ACCELERATORS, Operation, RENDERER_ACCELERATORS, Request,
        control_accelerator,
    };

    #[test]
    fn renderer_accelerators_are_in_the_public_contract() {
        assert!(
            RENDERER_ACCELERATORS
                .iter()
                .all(|key| ACCELERATORS.contains(key))
        );
    }

    #[test]
    fn control_accelerators_are_derived_from_the_public_contract() {
        assert_eq!(control_accelerator(b'd'), Some("C-d"));
        assert_eq!(control_accelerator(b'G'), Some("C-g"));
        assert_eq!(control_accelerator(b'u'), Some("C-u"));
        assert_eq!(control_accelerator(b'x'), None);
    }

    #[test]
    fn requests_decode_and_classify_operations() {
        let request = Request::decode(
            r#"{"id":7,"op":"view-search","params":{"view":3}}"#,
        )
        .unwrap();
        assert_eq!(request.id, 7);
        assert_eq!(request.params, json!({ "view": 3 }));
        assert_eq!(request.operation().unwrap(), Operation::ViewSearch);

        let request = Request::decode(r#"{"id":8,"op":"view-info"}"#).unwrap();
        assert!(request.params.is_null());
        assert_eq!(request.operation().unwrap(), Operation::ViewInfo);

        let request =
            Request::decode(r#"{"id":9,"op":"view-zoom","params":{"view":3}}"#)
                .unwrap();
        assert_eq!(request.operation().unwrap(), Operation::ViewZoom);

        let request = Request::decode(
            r#"{"id":10,"op":"view-set-selection","params":{"view":3}}"#,
        )
        .unwrap();
        assert_eq!(request.operation().unwrap(), Operation::ViewSetSelection);

        let request = Request::decode(
            r#"{"id":11,"op":"view-appearance","params":{"view":3}}"#,
        )
        .unwrap();
        assert_eq!(request.operation().unwrap(), Operation::ViewAppearance);

        let request = Request::decode(
            r#"{"id":12,"op":"view-parent","params":{"view":3}}"#,
        )
        .unwrap();
        assert_eq!(request.operation().unwrap(), Operation::ViewParent);
    }

    #[test]
    fn requests_reject_unknown_fields_and_operations() {
        assert!(
            Request::decode(
                r#"{"id":7,"op":"view-info","params":null,"extra":true}"#,
            )
            .is_err()
        );
        let request = Request::decode(r#"{"id":7,"op":"unknown"}"#).unwrap();
        let error = request.operation().unwrap_err();
        assert_eq!(error.code, "unsupported-operation");
        assert_eq!(error.message, "unsupported operation: unknown");
    }
}
