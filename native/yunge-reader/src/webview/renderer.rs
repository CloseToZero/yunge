// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use http::Request as HttpRequest;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use super::protocol::{PROTOCOL_VERSION, RENDERER_ACCELERATORS, Response};
use super::{
    EpubAppearance, EpubLocator, EpubNavigationTarget, EpubOutline,
    EpubSearchCursor, EpubSearchMatch, EpubSelection, EpubStyle, EpubZoom,
    MAX_EPUB_EXTERNAL_URI_BYTES, MAX_EPUB_OUTLINE_ITEMS,
    MAX_EPUB_SEARCH_RESULT_BYTES, MAX_EPUB_SELECTION_CHARACTERS,
    MAX_EPUB_SELECTION_RESULT_BYTES, MAX_RENDERER_ERROR_BYTES,
    MAX_RENDERER_MESSAGE_BYTES, NavigationCommand, SearchDirection, ViewEvent,
    ViewEventPayload, ViewSearchParams, ViewSelectionTextParams,
};

#[cfg(test)]
const TEST_RENDERER_URL: &str = concat!(
    "http://127.0.0.1:32123/",
    "0123456789abcdef0123456789abcdef/app/index.html"
);

#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) struct RendererOrigin {
    url: String,
    blob_root: String,
    resource_prefix: String,
}

impl RendererOrigin {
    pub(super) fn parse(
        url: &str,
    ) -> Result<Self, super::protocol::ServiceError> {
        let uri: http::Uri = url.parse().map_err(|_| {
            super::protocol::ServiceError::new(
                "invalid-renderer-url",
                "EPUB renderer URL is invalid",
            )
        })?;
        let authority = uri.authority().ok_or_else(|| {
            super::protocol::ServiceError::new(
                "invalid-renderer-url",
                "EPUB renderer URL has no authority",
            )
        })?;
        let path = uri.path();
        let token = path
            .strip_prefix('/')
            .and_then(|path| path.strip_suffix("/app/index.html"));
        if uri.scheme_str() != Some("http")
            || authority.host() != "127.0.0.1"
            || authority.port_u16().is_none()
            || uri.query().is_some()
            || !token.is_some_and(valid_broker_token)
        {
            return Err(super::protocol::ServiceError::new(
                "invalid-renderer-url",
                "EPUB renderer URL is not a local broker URL",
            ));
        }
        let origin = format!("http://{authority}");
        let resource_prefix =
            format!("{origin}/{}/", token.unwrap_or_default());
        Ok(Self {
            url: url.to_owned(),
            blob_root: format!("blob:{origin}/"),
            resource_prefix,
        })
    }

    pub(super) fn url(&self) -> &str {
        &self.url
    }

    pub(super) fn source_allowed(&self, url: &str) -> bool {
        url == self.url
    }

    pub(super) fn navigation_allowed(&self, url: &str) -> bool {
        self.source_allowed(url) || self.blob_navigation_allowed(url)
    }

    pub(super) fn resource_root_allowed(&self, url: &str) -> bool {
        let Some(path) = url.strip_prefix(&self.resource_prefix) else {
            return false;
        };
        let Some(token) = path
            .strip_prefix("book/")
            .and_then(|path| path.strip_suffix('/'))
        else {
            return false;
        };
        valid_broker_token(token)
    }

    fn blob_navigation_allowed(&self, url: &str) -> bool {
        let Some(identifier) = url.strip_prefix(&self.blob_root) else {
            return false;
        };
        !identifier.is_empty()
            && identifier.len() <= 128
            && identifier
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-')
    }
}

fn valid_broker_token(value: &str) -> bool {
    value.len() == 32
        && value
            .bytes()
            .all(|byte| byte.is_ascii_digit() || matches!(byte, b'a'..=b'f'))
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct RendererSelectionTextEnvelope {
    ok: bool,
    #[serde(default)]
    result: Option<RendererSelectionTextResult>,
    #[serde(default)]
    error: Option<RendererError>,
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
struct RendererError {
    code: String,
    message: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct RendererSearchMessage {
    protocol: u32,
    event: String,
    request: u64,
    response: Value,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct RendererSearchEnvelope {
    ok: bool,
    #[serde(default)]
    result: Option<RendererSearchResult>,
    #[serde(default)]
    error: Option<RendererError>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct RendererSearchResult {
    matches: Vec<EpubSearchMatch>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    cursor: Option<EpubSearchCursor>,
    done: bool,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "event", rename_all = "kebab-case", deny_unknown_fields)]
enum RendererMessage {
    Accelerator {
        protocol: u32,
        key: String,
        repeat: bool,
    },
    AppearanceError {
        protocol: u32,
        message: String,
    },
    ExternalLink {
        protocol: u32,
        uri: String,
    },
    FocusGained {
        protocol: u32,
    },
    FocusLost {
        protocol: u32,
    },
    Location {
        protocol: u32,
        location: EpubLocator,
        #[serde(default, rename = "outline-index")]
        outline_index: Option<u32>,
        user: bool,
    },
    NavigationError {
        protocol: u32,
        message: String,
    },
    PublicationError {
        protocol: u32,
        message: String,
    },
    PublicationReady {
        protocol: u32,
        location: EpubLocator,
        outline: EpubOutline,
        #[serde(default, rename = "outline-index")]
        outline_index: Option<u32>,
    },
    ScrollBarsError {
        protocol: u32,
        message: String,
    },
    ShellReady {
        protocol: u32,
    },
    Selection {
        protocol: u32,
        #[serde(default, deserialize_with = "deserialize_present_option")]
        selection: Option<Option<EpubSelection>>,
    },
    StyleError {
        protocol: u32,
        message: String,
    },
    ZoomChanged {
        protocol: u32,
        scale: f64,
    },
    ZoomError {
        protocol: u32,
        message: String,
    },
}

#[cfg(test)]
pub(super) fn shell_ready(request: &HttpRequest<String>) -> bool {
    if !app_renderer_source_allowed(&request.uri().to_string())
        || request.body().len() > MAX_RENDERER_MESSAGE_BYTES
    {
        return false;
    }
    matches!(
        serde_json::from_str(request.body()),
        Ok(RendererMessage::ShellReady {
            protocol: PROTOCOL_VERSION
        })
    )
}

pub(super) fn shell_ready_for(
    origin: &RendererOrigin,
    request: &HttpRequest<String>,
) -> bool {
    if !origin.source_allowed(&request.uri().to_string())
        || request.body().len() > MAX_RENDERER_MESSAGE_BYTES
    {
        return false;
    }
    matches!(
        serde_json::from_str(request.body()),
        Ok(RendererMessage::ShellReady {
            protocol: PROTOCOL_VERSION
        })
    )
}

fn valid_external_uri(value: &str) -> bool {
    let Some((scheme, _rest)) = value.split_once(':') else {
        return false;
    };
    !value.is_empty()
        && value.len() <= MAX_EPUB_EXTERNAL_URI_BYTES
        && !value.chars().any(|character| {
            character.is_whitespace() || character.is_control()
        })
        && !scheme.is_empty()
        && scheme.bytes().enumerate().all(|(index, byte)| match byte {
            b'A'..=b'Z' | b'a'..=b'z' => true,
            b'0'..=b'9' | b'+' | b'.' | b'-' => index > 0,
            _ => false,
        })
}

#[derive(Debug)]
pub(super) struct RendererSearchCallback {
    pub(super) view: u64,
    pub(super) request: u64,
    pub(super) response: Value,
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

fn invalid_selection_result(id: u64, detail: impl Into<String>) -> Response {
    Response::failure(Some(id), "invalid-renderer-result", detail)
}

pub(super) fn selection_text_response(
    id: u64,
    offset: u32,
    character_limit: u32,
    value: &str,
) -> Response {
    if value.len() > MAX_EPUB_SELECTION_RESULT_BYTES {
        return invalid_selection_result(
            id,
            "EPUB selection text result exceeds its byte limit",
        );
    }
    let envelope: RendererSelectionTextEnvelope =
        match serde_json::from_str(value) {
            Ok(envelope) => envelope,
            Err(error) => {
                return invalid_selection_result(
                    id,
                    format!("invalid EPUB selection text result: {error}"),
                );
            }
        };
    if envelope.ok {
        let Some(result) = envelope.result else {
            return invalid_selection_result(
                id,
                "successful EPUB selection text result has no payload",
            );
        };
        if envelope.error.is_some() {
            return invalid_selection_result(
                id,
                "successful EPUB selection text result contains an error",
            );
        }
        let characters = match u32::try_from(result.text.chars().count()) {
            Ok(characters) => characters,
            Err(_) => {
                return invalid_selection_result(
                    id,
                    "EPUB selection text chunk is too large",
                );
            }
        };
        let next = match offset.checked_add(characters) {
            Some(next) => next,
            None => {
                return invalid_selection_result(
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
            return invalid_selection_result(
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
        return invalid_selection_result(
            id,
            "failed EPUB selection text result contains a payload",
        );
    }
    let Some(error) = envelope.error else {
        return invalid_selection_result(
            id,
            "failed EPUB selection text result has no error",
        );
    };
    if error.message.is_empty()
        || error.message.len() > MAX_RENDERER_ERROR_BYTES
    {
        return invalid_selection_result(
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
            return invalid_selection_result(
                id,
                "EPUB selection text error code is invalid",
            );
        }
    };
    Response::failure(Some(id), code, error.message)
}

pub(super) fn current_selection_response(id: u64, value: &str) -> Response {
    if value.len() > MAX_EPUB_SELECTION_RESULT_BYTES {
        return invalid_selection_result(
            id,
            "EPUB current selection exceeds its byte limit",
        );
    }
    let selection: Option<EpubSelection> = match serde_json::from_str(value) {
        Ok(selection) => selection,
        Err(error) => {
            return invalid_selection_result(
                id,
                format!("invalid EPUB current selection: {error}"),
            );
        }
    };
    let selection = match selection.map(EpubSelection::validate).transpose() {
        Ok(selection) => selection,
        Err(error) => return invalid_selection_result(id, error.message),
    };
    Response::success(id, json!(selection))
}

fn invalid_search_result(id: u64, detail: impl Into<String>) -> Response {
    Response::failure(Some(id), "invalid-renderer-result", detail)
}

pub(super) fn search_response(
    id: u64,
    params: &ViewSearchParams,
    value: &str,
) -> Response {
    if value.len() > MAX_EPUB_SEARCH_RESULT_BYTES {
        return invalid_search_result(
            id,
            "EPUB search result exceeds its byte limit",
        );
    }
    let envelope: RendererSearchEnvelope = match serde_json::from_str(value) {
        Ok(envelope) => envelope,
        Err(error) => {
            return invalid_search_result(
                id,
                format!("invalid EPUB search result: {error}"),
            );
        }
    };
    if envelope.ok {
        let Some(mut result) = envelope.result else {
            return invalid_search_result(
                id,
                "successful EPUB search result has no payload",
            );
        };
        if envelope.error.is_some()
            || result.matches.len() > params.match_limit as usize
        {
            return invalid_search_result(
                id,
                "successful EPUB search result is inconsistent",
            );
        }
        result.cursor =
            match result.cursor.map(EpubSearchCursor::validate).transpose() {
                Ok(cursor) => cursor,
                Err(error) => {
                    return invalid_search_result(id, error.message);
                }
            };
        let cursor_is_valid = if result.done {
            result.cursor.is_none()
        } else if let Some(cursor) = result.cursor.as_ref() {
            params.cursor.as_ref().is_none_or(|old| {
                if cursor.href != old.href {
                    return true;
                }
                match (params.direction, old.offset, cursor.offset) {
                    (SearchDirection::Forward, None, Some(_)) => true,
                    (SearchDirection::Forward, Some(old), Some(new)) => {
                        new > old
                    }
                    (SearchDirection::Backward, None, Some(_)) => true,
                    (SearchDirection::Backward, Some(old), Some(new)) => {
                        new < old
                    }
                    _ => false,
                }
            })
        } else {
            false
        };
        if !cursor_is_valid {
            return invalid_search_result(
                id,
                "EPUB search cursor did not advance",
            );
        }
        let matches = result
            .matches
            .into_iter()
            .map(EpubSearchMatch::validate)
            .collect::<Result<Vec<_>, _>>();
        result.matches = match matches {
            Ok(matches) => matches,
            Err(error) => {
                return invalid_search_result(id, error.message);
            }
        };
        return Response::success(
            id,
            serde_json::to_value(result)
                .expect("validated search result is serializable"),
        );
    }
    if envelope.result.is_some() {
        return invalid_search_result(
            id,
            "failed EPUB search result contains a payload",
        );
    }
    let Some(error) = envelope.error else {
        return invalid_search_result(
            id,
            "failed EPUB search result has no error",
        );
    };
    if error.message.is_empty()
        || error.message.len() > MAX_RENDERER_ERROR_BYTES
    {
        return invalid_search_result(
            id,
            "EPUB search error message is invalid",
        );
    }
    let code = match error.code.as_str() {
        "invalid-search-cursor" => "invalid-search-cursor",
        "search-result-too-large" => "search-result-too-large",
        "search-unavailable" => "search-unavailable",
        _ => {
            return invalid_search_result(
                id,
                "EPUB search error code is invalid",
            );
        }
    };
    Response::failure(Some(id), code, error.message)
}

#[cfg(test)]
pub(super) fn app_navigation_allowed(url: String) -> bool {
    app_renderer_source_allowed(&url) || app_blob_navigation_allowed(&url)
}

pub(super) fn search_callback_for(
    view: u64,
    origin: &RendererOrigin,
    request: &HttpRequest<String>,
) -> Option<RendererSearchCallback> {
    if !origin.source_allowed(&request.uri().to_string())
        || request.body().len() > MAX_RENDERER_MESSAGE_BYTES
    {
        return None;
    }
    search_callback_body(view, request)
}

#[cfg(test)]
pub(super) fn app_renderer_source_allowed(url: &str) -> bool {
    url == TEST_RENDERER_URL
}

#[cfg(test)]
fn app_blob_navigation_allowed(url: &str) -> bool {
    const ROOTS: &[&str] = &["blob:http://127.0.0.1:32123/"];
    ROOTS.iter().any(|root| {
        let Some(identifier) = url.strip_prefix(root) else {
            return false;
        };
        !identifier.is_empty()
            && identifier.len() <= 128
            && identifier
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-')
    })
}

#[cfg(test)]
pub(super) fn search_callback(
    view: u64,
    request: &HttpRequest<String>,
) -> Option<RendererSearchCallback> {
    if !app_renderer_source_allowed(&request.uri().to_string())
        || request.body().len() > MAX_RENDERER_MESSAGE_BYTES
    {
        return None;
    }
    search_callback_body(view, request)
}

fn search_callback_body(
    view: u64,
    request: &HttpRequest<String>,
) -> Option<RendererSearchCallback> {
    let message: RendererSearchMessage =
        serde_json::from_str(request.body()).ok()?;
    if message.protocol != PROTOCOL_VERSION
        || message.event != "search-result"
        || message.request == 0
    {
        return None;
    }
    Some(RendererSearchCallback {
        view,
        request: message.request,
        response: message.response,
    })
}

fn checked_renderer_error(protocol: u32, message: String) -> Option<String> {
    (protocol == PROTOCOL_VERSION
        && !message.is_empty()
        && message.len() <= MAX_RENDERER_ERROR_BYTES)
        .then_some(message)
}

#[cfg(test)]
pub(super) fn event(
    view: u64,
    request: &HttpRequest<String>,
) -> Option<ViewEvent> {
    if !app_renderer_source_allowed(&request.uri().to_string())
        || request.body().len() > MAX_RENDERER_MESSAGE_BYTES
    {
        return None;
    }
    event_body(view, request)
}

fn event_body(view: u64, request: &HttpRequest<String>) -> Option<ViewEvent> {
    let message: RendererMessage = serde_json::from_str(request.body()).ok()?;
    let payload = match message {
        RendererMessage::Accelerator {
            protocol,
            key,
            repeat,
        } => {
            if protocol != PROTOCOL_VERSION
                || !RENDERER_ACCELERATORS.contains(&key.as_str())
            {
                return None;
            }
            ViewEventPayload::Accelerator { key, repeat }
        }
        RendererMessage::AppearanceError { protocol, message } => {
            ViewEventPayload::AppearanceError {
                message: checked_renderer_error(protocol, message)?,
            }
        }
        RendererMessage::ExternalLink { protocol, uri } => {
            if protocol != PROTOCOL_VERSION || !valid_external_uri(&uri) {
                return None;
            }
            ViewEventPayload::ExternalLink { uri }
        }
        RendererMessage::FocusGained { protocol } => {
            if protocol != PROTOCOL_VERSION {
                return None;
            }
            ViewEventPayload::FocusGained
        }
        RendererMessage::FocusLost { protocol } => {
            if protocol != PROTOCOL_VERSION {
                return None;
            }
            ViewEventPayload::FocusLost
        }
        RendererMessage::Location {
            protocol,
            location,
            outline_index,
            user,
        } => {
            if protocol != PROTOCOL_VERSION
                || outline_index.is_some_and(|index| {
                    index as usize >= MAX_EPUB_OUTLINE_ITEMS
                })
            {
                return None;
            }
            ViewEventPayload::Location {
                location: location.validate().ok()?,
                outline_index,
                user,
            }
        }
        RendererMessage::NavigationError { protocol, message } => {
            ViewEventPayload::NavigationError {
                message: checked_renderer_error(protocol, message)?,
            }
        }
        RendererMessage::PublicationError { protocol, message } => {
            ViewEventPayload::PublicationError {
                message: checked_renderer_error(protocol, message)?,
            }
        }
        RendererMessage::PublicationReady {
            protocol,
            location,
            outline,
            outline_index,
        } => {
            if protocol != PROTOCOL_VERSION {
                return None;
            }
            let outline = outline.validate().ok()?;
            if outline_index
                .is_some_and(|index| index as usize >= outline.items.len())
            {
                return None;
            }
            ViewEventPayload::PublicationReady {
                location: location.validate().ok()?,
                outline,
                outline_index,
            }
        }
        RendererMessage::ScrollBarsError { protocol, message } => {
            ViewEventPayload::ScrollBarsError {
                message: checked_renderer_error(protocol, message)?,
            }
        }
        RendererMessage::ShellReady { .. } => return None,
        RendererMessage::Selection {
            protocol,
            selection,
        } => {
            if protocol != PROTOCOL_VERSION {
                return None;
            }
            let selection =
                selection?.map(EpubSelection::validate).transpose().ok()?;
            ViewEventPayload::Selection { selection }
        }
        RendererMessage::StyleError { protocol, message } => {
            ViewEventPayload::StyleError {
                message: checked_renderer_error(protocol, message)?,
            }
        }
        RendererMessage::ZoomChanged { protocol, scale } => {
            if protocol != PROTOCOL_VERSION
                || !scale.is_finite()
                || scale <= 0.0
            {
                return None;
            }
            ViewEventPayload::ZoomChanged { scale }
        }
        RendererMessage::ZoomError { protocol, message } => {
            ViewEventPayload::ZoomError {
                message: checked_renderer_error(protocol, message)?,
            }
        }
    };
    Some(ViewEvent::new(view, payload))
}

pub(super) fn event_for(
    view: u64,
    origin: &RendererOrigin,
    request: &HttpRequest<String>,
) -> Option<ViewEvent> {
    if !origin.source_allowed(&request.uri().to_string())
        || request.body().len() > MAX_RENDERER_MESSAGE_BYTES
    {
        return None;
    }
    event_body(view, request)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum RendererMethod {
    ClearSelection,
    CurrentSelection,
    Navigate,
    Open,
    Search,
    SelectionText,
    SetAppearance,
    SetScrollBars,
    SetSearchResult,
    SetSelection,
    SetStyle,
    SetZoom,
}

impl RendererMethod {
    fn name(self) -> &'static str {
        match self {
            Self::ClearSelection => "clearSelection",
            Self::CurrentSelection => "currentSelection",
            Self::Navigate => "navigate",
            Self::Open => "open",
            Self::Search => "search",
            Self::SelectionText => "selectionText",
            Self::SetAppearance => "setAppearance",
            Self::SetScrollBars => "setScrollBars",
            Self::SetSearchResult => "setSearchResult",
            Self::SetSelection => "setSelection",
            Self::SetStyle => "setStyle",
            Self::SetZoom => "setZoom",
        }
    }

    fn discards_result(self) -> bool {
        !matches!(
            self,
            Self::CurrentSelection | Self::Search | Self::SelectionText
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
struct RendererCall {
    method: RendererMethod,
    payload: Value,
}

impl RendererCall {
    fn new(method: RendererMethod, payload: Value) -> Self {
        Self { method, payload }
    }

    fn script(&self) -> String {
        let payload = serde_json::to_string(&self.payload)
            .expect("renderer call payload is serializable");
        let discard = if self.method.discards_result() {
            "void "
        } else {
            ""
        };
        format!(
            "{discard}globalThis.yungeReader.{}({payload});",
            self.method.name()
        )
    }
}

fn open_call(
    view: u64,
    resource_root: &str,
    location: Option<&EpubLocator>,
    appearance: &EpubAppearance,
    style: Option<&EpubStyle>,
    zoom: Option<&EpubZoom>,
    scroll_bars: bool,
) -> RendererCall {
    RendererCall::new(
        RendererMethod::Open,
        json!({
            "view": view,
            "resourceRoot": resource_root,
            "location": location,
            "appearance": appearance,
            "style": style,
            "zoom": zoom,
            "scrollBars": scroll_bars,
            "rendererAccelerators": RENDERER_ACCELERATORS,
        }),
    )
}

pub(super) fn open_script(
    view: u64,
    resource_root: &str,
    location: Option<&EpubLocator>,
    appearance: &EpubAppearance,
    style: Option<&EpubStyle>,
    zoom: Option<&EpubZoom>,
    scroll_bars: bool,
) -> String {
    open_call(
        view,
        resource_root,
        location,
        appearance,
        style,
        zoom,
        scroll_bars,
    )
    .script()
}

fn appearance_call(view: u64, appearance: &EpubAppearance) -> RendererCall {
    RendererCall::new(
        RendererMethod::SetAppearance,
        json!({
            "view": view,
            "appearance": appearance,
        }),
    )
}

pub(super) fn appearance_script(
    view: u64,
    appearance: &EpubAppearance,
) -> String {
    appearance_call(view, appearance).script()
}

fn navigation_call(
    view: u64,
    command: NavigationCommand,
    location: Option<&EpubNavigationTarget>,
) -> RendererCall {
    RendererCall::new(
        RendererMethod::Navigate,
        json!({
            "view": view,
            "command": command,
            "location": location,
        }),
    )
}

pub(super) fn navigation_script(
    view: u64,
    command: NavigationCommand,
    location: Option<&EpubNavigationTarget>,
) -> String {
    navigation_call(view, command, location).script()
}

fn style_call(view: u64, style: &EpubStyle) -> RendererCall {
    RendererCall::new(
        RendererMethod::SetStyle,
        json!({
            "view": view,
            "style": style,
        }),
    )
}

pub(super) fn style_script(view: u64, style: &EpubStyle) -> String {
    style_call(view, style).script()
}

fn zoom_call(view: u64, zoom: &EpubZoom) -> RendererCall {
    RendererCall::new(
        RendererMethod::SetZoom,
        json!({
            "view": view,
            "zoom": zoom,
        }),
    )
}

pub(super) fn zoom_script(view: u64, zoom: &EpubZoom) -> String {
    zoom_call(view, zoom).script()
}

fn scroll_bars_call(view: u64, visible: bool) -> RendererCall {
    RendererCall::new(
        RendererMethod::SetScrollBars,
        json!({
            "view": view,
            "visible": visible,
        }),
    )
}

pub(super) fn scroll_bars_script(view: u64, visible: bool) -> String {
    scroll_bars_call(view, visible).script()
}

fn clear_selection_call(view: u64) -> RendererCall {
    RendererCall::new(RendererMethod::ClearSelection, json!({ "view": view }))
}

pub(super) fn clear_selection_script(view: u64) -> String {
    clear_selection_call(view).script()
}

fn set_selection_call(view: u64, selection: &EpubSelection) -> RendererCall {
    RendererCall::new(
        RendererMethod::SetSelection,
        json!({
            "view": view,
            "selection": selection,
        }),
    )
}

pub(super) fn set_selection_script(
    view: u64,
    selection: &EpubSelection,
) -> String {
    set_selection_call(view, selection).script()
}

fn current_selection_call(view: u64) -> RendererCall {
    RendererCall::new(RendererMethod::CurrentSelection, json!({ "view": view }))
}

pub(super) fn current_selection_script(view: u64) -> String {
    current_selection_call(view).script()
}

fn selection_text_call(params: &ViewSelectionTextParams) -> RendererCall {
    RendererCall::new(
        RendererMethod::SelectionText,
        serde_json::to_value(params)
            .expect("selection text payload is serializable"),
    )
}

pub(super) fn selection_text_script(
    params: &ViewSelectionTextParams,
) -> String {
    selection_text_call(params).script()
}

fn search_call(id: u64, params: &ViewSearchParams) -> RendererCall {
    let mut payload =
        serde_json::to_value(params).expect("search payload is serializable");
    payload
        .as_object_mut()
        .expect("search payload is an object")
        .insert("request".into(), json!(id));
    RendererCall::new(RendererMethod::Search, payload)
}

pub(super) fn search_script(id: u64, params: &ViewSearchParams) -> String {
    search_call(id, params).script()
}

fn search_result_call(
    view: u64,
    selection: Option<&EpubSelection>,
    reveal: bool,
) -> RendererCall {
    RendererCall::new(
        RendererMethod::SetSearchResult,
        json!({
            "view": view,
            "selection": selection,
            "reveal": reveal,
        }),
    )
}

pub(super) fn search_result_script(
    view: u64,
    selection: Option<&EpubSelection>,
    reveal: bool,
) -> String {
    search_result_call(view, selection, reveal).script()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::webview::EpubZoomMode;

    fn recorded_script(script: &str) -> (bool, &str, Value) {
        let (discards_result, script) =
            if let Some(script) = script.strip_prefix("void ") {
                (true, script)
            } else {
                (false, script)
            };
        let invocation = script
            .strip_prefix("globalThis.yungeReader.")
            .and_then(|script| script.strip_suffix(");"))
            .expect("renderer script is a direct static invocation");
        let (method, payload) = invocation
            .split_once('(')
            .expect("renderer invocation has one payload");
        let payload = serde_json::from_str(payload)
            .expect("renderer invocation payload is JSON");
        (discards_result, method, payload)
    }

    fn appearance() -> EpubAppearance {
        serde_json::from_value(json!({
            "mode": "follow-emacs",
            "foreground": "#112233",
            "background": "#f4f5f6",
            "link": "#2244aa",
            "selection-foreground": "#ffffff",
            "selection-background": "#335577",
            "search-background": "#aa5500",
        }))
        .unwrap()
    }

    fn location() -> EpubLocator {
        EpubLocator {
            cfi: "epubcfi(/6/4!/4/2)".into(),
            href: "OPS/chapter.xhtml".into(),
            fraction: Some(0.25),
            x: Some(12.5),
            y: Some(30.0),
        }
    }

    fn selection() -> EpubSelection {
        EpubSelection {
            href: "OPS/chapter.xhtml".into(),
            start: "epubcfi(/6/4!/4/2/1:0)".into(),
            end: "epubcfi(/6/4!/4/2/1:7)".into(),
        }
    }

    #[test]
    fn open_call_preserves_typed_renderer_inputs() {
        let location = location();
        let style = EpubStyle::default();
        assert_eq!(
            open_call(
                4,
                "https://yunge-reader-book.localhost/token/",
                Some(&location),
                &appearance(),
                Some(&style),
                None,
                false,
            ),
            RendererCall::new(
                RendererMethod::Open,
                json!({
                    "view": 4,
                    "resourceRoot":
                        "https://yunge-reader-book.localhost/token/",
                    "location": location,
                    "appearance": appearance(),
                    "style": style,
                    "zoom": null,
                    "scrollBars": false,
                    "rendererAccelerators": RENDERER_ACCELERATORS,
                }),
            )
        );

        assert_eq!(
            open_call(
                4,
                "https://yunge-reader-book.localhost/token/",
                None,
                &EpubAppearance::Original,
                None,
                Some(&EpubZoom::Mode(EpubZoomMode::FitPage)),
                true,
            )
            .payload["zoom"],
            "fit-page"
        );
    }

    #[test]
    fn action_calls_preserve_methods_and_payloads() {
        let location = location();
        let target = EpubNavigationTarget {
            cfi: Some(location.cfi),
            href: location.href,
            fraction: location.fraction,
            x: location.x,
            y: location.y,
        };
        let selection = selection();
        let cases = [
            appearance_call(4, &appearance()),
            navigation_call(4, NavigationCommand::GoTo, Some(&target)),
            style_call(4, &EpubStyle::default()),
            zoom_call(4, &EpubZoom::Mode(EpubZoomMode::FitWidth)),
            scroll_bars_call(4, true),
            clear_selection_call(4),
            set_selection_call(4, &selection),
            search_result_call(4, Some(&selection), true),
        ];
        assert_eq!(
            cases.each_ref().map(|call| call.method),
            [
                RendererMethod::SetAppearance,
                RendererMethod::Navigate,
                RendererMethod::SetStyle,
                RendererMethod::SetZoom,
                RendererMethod::SetScrollBars,
                RendererMethod::ClearSelection,
                RendererMethod::SetSelection,
                RendererMethod::SetSearchResult,
            ]
        );
        assert_eq!(cases[1].payload["command"], "go-to");
        assert_eq!(cases[1].payload["location"]["x"], 12.5);
        assert_eq!(cases[3].payload["zoom"], "fit-width");
        assert_eq!(cases[4].payload, json!({"view": 4, "visible": true}));
        assert_eq!(cases[6].payload["selection"], json!(selection));
        assert_eq!(cases[7].payload["reveal"], true);
        assert!(cases.iter().all(|call| call.method.discards_result()));
    }

    #[test]
    fn query_calls_preserve_methods_and_payloads() {
        let selection = selection();
        let current = current_selection_call(4);
        let text = selection_text_call(&ViewSelectionTextParams {
            view: 4,
            selection: selection.clone(),
            offset: 2,
            character_limit: 16_384,
        });
        let search = search_call(
            27,
            &ViewSearchParams {
                view: 4,
                query: "Chapter".into(),
                case_sensitive: true,
                direction: SearchDirection::Forward,
                origin: None,
                cursor: Some(EpubSearchCursor {
                    href: selection.href,
                    offset: Some(2),
                }),
                match_limit: 32,
                section_limit: 8,
            },
        );

        assert_eq!(current.method, RendererMethod::CurrentSelection);
        assert_eq!(current.payload, json!({"view": 4}));
        assert_eq!(text.method, RendererMethod::SelectionText);
        assert_eq!(text.payload["character-limit"], 16_384);
        assert_eq!(text.payload["offset"], 2);
        assert_eq!(search.method, RendererMethod::Search);
        assert_eq!(search.payload["request"], 27);
        assert_eq!(search.payload["query"], "Chapter");
        assert_eq!(search.payload["case-sensitive"], true);
        assert_eq!(search.payload["match-limit"], 32);
        assert_eq!(search.payload["section-limit"], 8);
        assert_eq!(search.payload["cursor"]["offset"], 2);
        assert!(
            [&current, &text, &search]
                .into_iter()
                .all(|call| !call.method.discards_result())
        );
    }

    #[test]
    fn renderer_call_serialization_uses_static_methods_and_json_data() {
        let call = RendererCall::new(
            RendererMethod::Search,
            json!({"query": "\"); globalThis.injected = true; //"}),
        );
        let script = call.script();
        let (discards_result, method, payload) = recorded_script(&script);
        assert!(!discards_result);
        assert_eq!(method, "search");
        assert_eq!(
            payload,
            json!({"query": "\"); globalThis.injected = true; //"})
        );
    }
}
