// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use wry::http::Request as HttpRequest;

use super::protocol::{PROTOCOL_VERSION, Response};
use super::resources::{APP_BROWSER_URL, APP_URL};
use super::{
    EpubLocator, EpubNavigationTarget, EpubOutline, EpubSearchCursor,
    EpubSearchMatch, EpubSelection, EpubStyle, MAX_EPUB_SEARCH_RESULT_BYTES,
    MAX_EPUB_SELECTION_CHARACTERS, MAX_EPUB_SELECTION_RESULT_BYTES,
    MAX_RENDERER_ERROR_BYTES, MAX_RENDERER_MESSAGE_BYTES, NavigationCommand,
    SearchDirection, ViewEvent, ViewSearchParams, ViewSelectionTextParams,
};

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
#[serde(deny_unknown_fields)]
struct RendererMessage {
    protocol: u32,
    event: RendererEvent,
    #[serde(default)]
    message: Option<String>,
    #[serde(default)]
    location: Option<EpubLocator>,
    #[serde(default)]
    outline: Option<EpubOutline>,
    #[serde(default, deserialize_with = "deserialize_present_option")]
    selection: Option<Option<EpubSelection>>,
    #[serde(default)]
    key: Option<String>,
    #[serde(default)]
    user: Option<bool>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
enum RendererEvent {
    Accelerator,
    Location,
    NavigationError,
    PublicationError,
    PublicationReady,
    ScrollBarsError,
    Selection,
    StyleError,
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

pub(super) fn app_navigation_allowed(url: String) -> bool {
    matches!(url.as_str(), APP_URL | APP_BROWSER_URL)
}

pub(super) fn search_callback(
    view: u64,
    request: &HttpRequest<String>,
) -> Option<RendererSearchCallback> {
    if !app_navigation_allowed(request.uri().to_string())
        || request.body().len() > MAX_RENDERER_MESSAGE_BYTES
    {
        return None;
    }
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

pub(super) fn event(
    view: u64,
    request: &HttpRequest<String>,
) -> Option<ViewEvent> {
    if !app_navigation_allowed(request.uri().to_string())
        || request.body().len() > MAX_RENDERER_MESSAGE_BYTES
    {
        return None;
    }
    let message: RendererMessage = serde_json::from_str(request.body()).ok()?;
    if message.protocol != PROTOCOL_VERSION
        || message
            .message
            .as_ref()
            .is_some_and(|value| value.len() > MAX_RENDERER_ERROR_BYTES)
    {
        return None;
    }
    let user = match &message.event {
        RendererEvent::Location => Some(message.user?),
        _ => {
            if message.user.is_some() {
                return None;
            }
            None
        }
    };
    let (event, detail, location, outline, selection, key) = match message.event
    {
        RendererEvent::Accelerator => {
            if message.message.is_some()
                || message.location.is_some()
                || message.outline.is_some()
                || message.selection.is_some()
            {
                return None;
            }
            let key = message.key.filter(|key| {
                matches!(
                    key.as_str(),
                    "J" | "K" | "+" | "-" | "=" | "y" | "SPC"
                )
            })?;
            ("accelerator", None, None, None, None, Some(key))
        }
        RendererEvent::Location => {
            if message.message.is_some()
                || message.outline.is_some()
                || message.selection.is_some()
                || message.key.is_some()
            {
                return None;
            }
            let location = message.location?.validate().ok()?;
            ("location", None, Some(location), None, None, None)
        }
        RendererEvent::NavigationError => {
            if message.location.is_some()
                || message.outline.is_some()
                || message.selection.is_some()
                || message.key.is_some()
            {
                return None;
            }
            let detail = message.message.filter(|value| !value.is_empty())?;
            ("navigation-error", Some(detail), None, None, None, None)
        }
        RendererEvent::PublicationReady => {
            if message.message.is_some()
                || message.selection.is_some()
                || message.key.is_some()
            {
                return None;
            }
            let location = message.location?.validate().ok()?;
            let outline = message.outline?.validate().ok()?;
            (
                "publication-ready",
                None,
                Some(location),
                Some(outline),
                None,
                None,
            )
        }
        RendererEvent::PublicationError => {
            if message.location.is_some()
                || message.outline.is_some()
                || message.selection.is_some()
                || message.key.is_some()
            {
                return None;
            }
            let detail = message.message.filter(|value| !value.is_empty())?;
            ("publication-error", Some(detail), None, None, None, None)
        }
        RendererEvent::Selection => {
            if message.message.is_some()
                || message.location.is_some()
                || message.outline.is_some()
                || message.key.is_some()
            {
                return None;
            }
            let selection = message
                .selection?
                .map(EpubSelection::validate)
                .transpose()
                .ok()?;
            ("selection", None, None, None, Some(selection), None)
        }
        RendererEvent::ScrollBarsError => {
            if message.location.is_some()
                || message.outline.is_some()
                || message.selection.is_some()
                || message.key.is_some()
            {
                return None;
            }
            let detail = message.message.filter(|value| !value.is_empty())?;
            ("scroll-bars-error", Some(detail), None, None, None, None)
        }
        RendererEvent::StyleError => {
            if message.location.is_some()
                || message.outline.is_some()
                || message.selection.is_some()
                || message.key.is_some()
            {
                return None;
            }
            let detail = message.message.filter(|value| !value.is_empty())?;
            ("style-error", Some(detail), None, None, None, None)
        }
    };
    Some(ViewEvent {
        kind: "event",
        event,
        view,
        message: detail,
        location,
        outline,
        selection,
        key,
        user,
    })
}

pub(super) fn open_script(
    view: u64,
    resource_root: &str,
    location: Option<&EpubLocator>,
    style: &EpubStyle,
    scroll_bars: bool,
) -> String {
    let payload = serde_json::to_string(&json!({
        "view": view,
        "resourceRoot": resource_root,
        "location": location,
        "style": style,
        "scrollBars": scroll_bars,
    }))
    .expect("publication open payload is serializable");
    format!("void globalThis.yungeReader.open({payload});")
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
