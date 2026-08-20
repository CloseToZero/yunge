// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use http::Request as HttpRequest;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use super::protocol::{PROTOCOL_VERSION, RENDERER_ACCELERATORS, Response};
use super::{
    EpubAppearance, EpubLocator, EpubNavigationTarget, EpubOutline,
    EpubSearchCursor, EpubSearchMatch, EpubSelection, EpubStyle, EpubZoom,
    MAX_EPUB_EXTERNAL_URI_BYTES, MAX_EPUB_SEARCH_RESULT_BYTES,
    MAX_EPUB_SELECTION_CHARACTERS, MAX_EPUB_SELECTION_RESULT_BYTES,
    MAX_RENDERER_ERROR_BYTES, MAX_RENDERER_MESSAGE_BYTES, NavigationCommand,
    SearchDirection, ViewEvent, ViewEventPayload, ViewSearchParams,
    ViewSelectionTextParams,
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
            user,
        } => {
            if protocol != PROTOCOL_VERSION {
                return None;
            }
            ViewEventPayload::Location {
                location: location.validate().ok()?,
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
        } => {
            if protocol != PROTOCOL_VERSION {
                return None;
            }
            ViewEventPayload::PublicationReady {
                location: location.validate().ok()?,
                outline: outline.validate().ok()?,
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

pub(super) fn open_script(
    view: u64,
    resource_root: &str,
    location: Option<&EpubLocator>,
    appearance: &EpubAppearance,
    style: Option<&EpubStyle>,
    zoom: Option<&EpubZoom>,
    scroll_bars: bool,
) -> String {
    let payload = serde_json::to_string(&json!({
        "view": view,
        "resourceRoot": resource_root,
        "location": location,
        "appearance": appearance,
        "style": style,
        "zoom": zoom,
        "scrollBars": scroll_bars,
        "rendererAccelerators": RENDERER_ACCELERATORS,
    }))
    .expect("publication open payload is serializable");
    format!("void globalThis.yungeReader.open({payload});")
}

pub(super) fn appearance_script(
    view: u64,
    appearance: &EpubAppearance,
) -> String {
    let payload = serde_json::to_string(&json!({
        "view": view,
        "appearance": appearance,
    }))
    .expect("publication appearance payload is serializable");
    format!("void globalThis.yungeReader.setAppearance({payload});")
}

pub(super) fn navigation_script(
    view: u64,
    command: NavigationCommand,
    location: Option<&EpubNavigationTarget>,
) -> String {
    let payload = serde_json::to_string(&json!({
        "view": view,
        "command": command,
        "location": location,
    }))
    .expect("publication navigation payload is serializable");
    format!("void globalThis.yungeReader.navigate({payload});")
}

pub(super) fn style_script(view: u64, style: &EpubStyle) -> String {
    let payload = serde_json::to_string(&json!({
        "view": view,
        "style": style,
    }))
    .expect("publication style payload is serializable");
    format!("void globalThis.yungeReader.setStyle({payload});")
}

pub(super) fn zoom_script(view: u64, zoom: &EpubZoom) -> String {
    let payload = serde_json::to_string(&json!({
        "view": view,
        "zoom": zoom,
    }))
    .expect("fixed-layout zoom payload is serializable");
    format!("void globalThis.yungeReader.setZoom({payload});")
}

pub(super) fn scroll_bars_script(view: u64, visible: bool) -> String {
    let payload = serde_json::to_string(&json!({
        "view": view,
        "visible": visible,
    }))
    .expect("scroll bar payload is serializable");
    format!("void globalThis.yungeReader.setScrollBars({payload});")
}

pub(super) fn clear_selection_script(view: u64) -> String {
    let payload = serde_json::to_string(&json!({ "view": view }))
        .expect("selection payload is serializable");
    format!("void globalThis.yungeReader.clearSelection({payload});")
}

pub(super) fn set_selection_script(
    view: u64,
    selection: &EpubSelection,
) -> String {
    let payload = serde_json::to_string(&json!({
        "view": view,
        "selection": selection,
    }))
    .expect("selection payload is serializable");
    format!("void globalThis.yungeReader.setSelection({payload});")
}

pub(super) fn current_selection_script(view: u64) -> String {
    let payload = serde_json::to_string(&json!({ "view": view }))
        .expect("current selection payload is serializable");
    format!("globalThis.yungeReader.currentSelection({payload});")
}

pub(super) fn selection_text_script(
    params: &ViewSelectionTextParams,
) -> String {
    let payload = serde_json::to_string(params)
        .expect("selection text payload is serializable");
    format!("globalThis.yungeReader.selectionText({payload});")
}

pub(super) fn search_script(id: u64, params: &ViewSearchParams) -> String {
    let mut payload =
        serde_json::to_value(params).expect("search payload is serializable");
    payload
        .as_object_mut()
        .expect("search payload is an object")
        .insert("request".into(), json!(id));
    let payload = serde_json::to_string(&payload)
        .expect("search payload remains serializable");
    format!("globalThis.yungeReader.search({payload});")
}

pub(super) fn search_result_script(
    view: u64,
    selection: Option<&EpubSelection>,
    reveal: bool,
) -> String {
    let payload = serde_json::to_string(&json!({
        "view": view,
        "selection": selection,
        "reveal": reveal,
    }))
    .expect("search result payload is serializable");
    format!("void globalThis.yungeReader.setSearchResult({payload});")
}
