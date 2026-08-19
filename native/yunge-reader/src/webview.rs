// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError, Sender};
use std::thread;
use std::time::Duration;
use windows::Win32::UI::WindowsAndMessaging::{
    DispatchMessageW, MSG, PM_REMOVE, PeekMessageW, TranslateMessage,
};
use wry::{
    NewWindowResponse, PermissionResponse, WebViewBuilder,
    WebViewBuilderExtWindows,
};
use yunge_reader::epub::{Publication, PublicationLayout};

use super::{BUILD_ID, Error};

mod protocol;
mod renderer;
mod resources;
mod surface;

#[cfg(test)]
use protocol::RENDERER_ACCELERATORS;
use protocol::{
    ACCELERATORS, CAPABILITIES, Control, Operation, Outgoing, PROTOCOL_VERSION,
    Request, Response, ServiceError, response,
};
use renderer::{
    RendererSearchCallback, app_navigation_allowed,
    appearance_script as publication_appearance_script,
    clear_selection_script as publication_clear_selection_script,
    event as renderer_event,
    navigation_script as publication_navigation_script,
    open_script as publication_open_script,
    scroll_bars_script as publication_scroll_bars_script,
    search_callback as renderer_search_callback,
    search_response as renderer_search_response,
    search_result_script as publication_search_result_script,
    search_script as publication_search_script,
    selection_text_response as renderer_selection_text_response,
    selection_text_script as publication_selection_text_script,
    set_selection_script as publication_set_selection_script,
    style_script as publication_style_script,
    zoom_script as publication_zoom_script,
};
#[cfg(test)]
use resources::{
    APP_BROWSER_ORIGIN, APP_BROWSER_URL, MAX_RESOURCE_REQUESTS,
    RESOURCE_CATALOG_PATH, ResourcePermit, app_asset, resource_request_target,
};
use resources::{
    APP_PROTOCOL, APP_URL, BOOK_PROTOCOL, ResourceService, app_response,
};
use surface::{Bounds, NativeSurface, ParentWindow, SurfaceEvent};

const MAX_EPUB_LOCATOR_TEXT_BYTES: usize = 3_072;
const MAX_EPUB_OUTLINE_DEPTH: u32 = 256;
const MAX_EPUB_OUTLINE_ITEMS: usize = 4_096;
const MAX_EPUB_OUTLINE_TITLE_BYTES: usize = 1_024;
const MAX_EPUB_OUTLINE_TEXT_BYTES: usize = 384 * 1_024;
const MAX_EPUB_SELECTION_CHARACTERS: u32 = 1_048_576;
const MAX_EPUB_SELECTION_CHARACTER_LIMIT: u32 = 65_536;
const MAX_EPUB_SELECTION_RESULT_BYTES: usize = 512 * 1_024;
const MAX_EPUB_SEARCH_QUERY_CHARACTERS: usize = 256;
const MAX_EPUB_SEARCH_MATCH_LIMIT: u32 = 200;
const MAX_EPUB_SEARCH_SECTION_LIMIT: u32 = 64;
const MAX_EPUB_SEARCH_CURSOR_OFFSET: u32 = 1_048_576;
const MAX_EPUB_SEARCH_RESULT_BYTES: usize = 512 * 1_024;
const MAX_EPUB_SEARCH_MATCH_TEXT_BYTES: usize = 16 * 1_024;
const MAX_EPUB_SEARCH_CONTEXT_BYTES: usize = 4 * 1_024;
const MAX_EPUB_EXTERNAL_URI_BYTES: usize = 4_096;
const MIN_EPUB_FONT_SCALE: f64 = 0.5;
const MAX_EPUB_FONT_SCALE: f64 = 3.0;
const MIN_EPUB_LINE_HEIGHT: f64 = 1.0;
const MAX_EPUB_LINE_HEIGHT: f64 = 3.0;
const MIN_EPUB_CONTENT_WIDTH: u32 = 320;
const MAX_EPUB_CONTENT_WIDTH: u32 = 1_600;
const MAX_EPUB_SIDE_PADDING: f64 = 20.0;
const MIN_EPUB_FIXED_SCALE: f64 = 0.25;
const MAX_EPUB_FIXED_SCALE: f64 = 8.0;
const MAX_EPUB_VIEWPORT_COORDINATE: f64 = 1_000_000.0;
const MAX_RENDERER_MESSAGE_BYTES: usize = 1_024 * 1_024;
const MAX_RENDERER_ERROR_BYTES: usize = 4 * 1_024;
const MESSAGE_PUMP_INTERVAL: Duration = Duration::from_millis(8);

#[derive(Debug, Serialize)]
struct ViewEvent {
    kind: &'static str,
    event: &'static str,
    view: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    message: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    location: Option<EpubLocator>,
    #[serde(skip_serializing_if = "Option::is_none")]
    outline: Option<EpubOutline>,
    #[serde(skip_serializing_if = "Option::is_none")]
    selection: Option<Option<EpubSelection>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    key: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    uri: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    user: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    scale: Option<f64>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct CreateParams {
    view: u64,
    parent: u64,
    bounds: Bounds,
    #[serde(default = "default_visible")]
    visible: bool,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ViewParams {
    view: u64,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct PublicationOpenParams {
    path: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct PublicationParams {
    publication: u64,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ViewPublicationParams {
    view: u64,
    publication: u64,
    appearance: EpubAppearance,
    #[serde(default)]
    location: Option<EpubLocator>,
    #[serde(default)]
    style: Option<EpubStyle>,
    #[serde(default)]
    zoom: Option<EpubZoom>,
    #[serde(rename = "scroll-bars")]
    scroll_bars: bool,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ViewStyleParams {
    view: u64,
    style: EpubStyle,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ViewAppearanceParams {
    view: u64,
    appearance: EpubAppearance,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ViewZoomParams {
    view: u64,
    zoom: EpubZoom,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ViewScrollBarsParams {
    view: u64,
    visible: bool,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct ViewSelectionTextParams {
    view: u64,
    selection: EpubSelection,
    offset: u32,
    character_limit: u32,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ViewSetSelectionParams {
    view: u64,
    selection: EpubSelection,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct ViewSearchParams {
    view: u64,
    query: String,
    case_sensitive: bool,
    direction: SearchDirection,
    #[serde(default)]
    origin: Option<EpubLocator>,
    #[serde(default)]
    cursor: Option<EpubSearchCursor>,
    match_limit: u32,
    section_limit: u32,
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
enum SearchDirection {
    Forward,
    Backward,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ViewSearchResultParams {
    view: u64,
    selection: Value,
    reveal: bool,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct EpubSearchCursor {
    href: String,
    #[serde(default)]
    offset: Option<u32>,
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct EpubStyle {
    font_scale: f64,
    line_height: f64,
    content_width: u32,
    side_padding: f64,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
enum EpubAppearance {
    Original,
    FollowEmacs,
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
enum EpubZoomMode {
    FitPage,
    FitWidth,
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize)]
#[serde(untagged)]
enum EpubZoom {
    Scale(f64),
    Mode(EpubZoomMode),
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct EpubLocator {
    cfi: String,
    href: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    fraction: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    x: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    y: Option<f64>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct EpubNavigationTarget {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    cfi: Option<String>,
    href: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    fraction: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    x: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    y: Option<f64>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct EpubOutlineItem {
    title: String,
    depth: u32,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    href: Option<String>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct EpubOutline {
    items: Vec<EpubOutlineItem>,
    truncated: bool,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct EpubSelection {
    href: String,
    start: String,
    end: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct EpubSearchMatch {
    href: String,
    start: String,
    end: String,
    text: String,
    before: String,
    after: String,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
enum NavigationCommand {
    PreviousPage,
    NextPage,
    PreviousScreen,
    NextScreen,
    PreviousLine,
    NextLine,
    First,
    Last,
    GoTo,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ViewNavigateParams {
    view: u64,
    command: NavigationCommand,
    #[serde(default)]
    location: Option<EpubNavigationTarget>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct BoundsParams {
    view: u64,
    bounds: Bounds,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct VisibleParams {
    view: u64,
    visible: bool,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct EmptyParams {}

enum Incoming {
    Request(Request),
    Invalid(String),
    RendererSearch(RendererSearchCallback),
}

struct NativeView {
    surface: NativeSurface,
    publication: Option<u64>,
}

struct Service {
    views: HashMap<u64, NativeView>,
    resources: ResourceService,
    next_publication: u64,
    version: Result<String, String>,
    outgoing_sender: Sender<Outgoing>,
    incoming_sender: Sender<Incoming>,
    pending_searches: HashMap<u64, ViewSearchParams>,
}

impl EpubLocator {
    fn validate(self) -> Result<Self, ServiceError> {
        for (name, value) in [("CFI", &self.cfi), ("href", &self.href)] {
            if value.is_empty()
                || value.len() > MAX_EPUB_LOCATOR_TEXT_BYTES
                || value.chars().any(char::is_control)
            {
                return Err(ServiceError::new(
                    "invalid-epub-location",
                    format!("EPUB locator {name} is invalid"),
                ));
            }
        }
        if !self.cfi.starts_with("epubcfi(") || !self.cfi.ends_with(')') {
            return Err(ServiceError::new(
                "invalid-epub-location",
                "EPUB locator CFI is invalid",
            ));
        }
        if !valid_epub_href(&self.href, false) {
            return Err(ServiceError::new(
                "invalid-epub-location",
                "EPUB locator href is not a canonical relative path",
            ));
        }
        if self.fraction.is_some_and(|value| {
            !value.is_finite() || !(0.0..=1.0).contains(&value)
        }) {
            return Err(ServiceError::new(
                "invalid-epub-location",
                "EPUB locator fraction must be between zero and one",
            ));
        }
        match (self.x, self.y) {
            (None, None) => {}
            (Some(x), Some(y))
                if [x, y].into_iter().all(|value| {
                    value.is_finite()
                        && (0.0..=MAX_EPUB_VIEWPORT_COORDINATE).contains(&value)
                }) => {}
            _ => {
                return Err(ServiceError::new(
                    "invalid-epub-location",
                    "EPUB viewport coordinates are invalid",
                ));
            }
        }
        Ok(self)
    }
}

impl Default for EpubStyle {
    fn default() -> Self {
        Self {
            font_scale: 1.0,
            line_height: 1.6,
            content_width: 720,
            side_padding: 7.0,
        }
    }
}

impl EpubStyle {
    fn validate(self) -> Result<Self, ServiceError> {
        if !self.font_scale.is_finite()
            || !(MIN_EPUB_FONT_SCALE..=MAX_EPUB_FONT_SCALE)
                .contains(&self.font_scale)
            || !self.line_height.is_finite()
            || !(MIN_EPUB_LINE_HEIGHT..=MAX_EPUB_LINE_HEIGHT)
                .contains(&self.line_height)
            || !(MIN_EPUB_CONTENT_WIDTH..=MAX_EPUB_CONTENT_WIDTH)
                .contains(&self.content_width)
            || !self.side_padding.is_finite()
            || !(0.0..=MAX_EPUB_SIDE_PADDING).contains(&self.side_padding)
        {
            return Err(ServiceError::new(
                "invalid-epub-style",
                "EPUB reading style is outside its supported bounds",
            ));
        }
        Ok(self)
    }
}

impl Default for EpubZoom {
    fn default() -> Self {
        Self::Mode(EpubZoomMode::FitPage)
    }
}

impl EpubZoom {
    fn validate(self) -> Result<Self, ServiceError> {
        if let Self::Scale(scale) = self
            && (!scale.is_finite()
                || !(MIN_EPUB_FIXED_SCALE..=MAX_EPUB_FIXED_SCALE)
                    .contains(&scale))
        {
            return Err(ServiceError::new(
                "invalid-epub-zoom",
                "EPUB fixed-layout scale is outside its supported bounds",
            ));
        }
        Ok(self)
    }
}

fn view_layout_options(
    layout: PublicationLayout,
    style: Option<EpubStyle>,
    zoom: Option<EpubZoom>,
) -> Result<(Option<EpubStyle>, Option<EpubZoom>), ServiceError> {
    match layout {
        PublicationLayout::Reflowable => {
            if zoom.is_some() {
                return Err(ServiceError::new(
                    "invalid-epub-view-layout",
                    "reflowable EPUB views do not accept fixed zoom",
                ));
            }
            Ok((Some(style.unwrap_or_default()), None))
        }
        PublicationLayout::PrePaginated => {
            if style.is_some() {
                return Err(ServiceError::new(
                    "invalid-epub-view-layout",
                    "fixed-layout EPUB views do not accept reflow style",
                ));
            }
            Ok((None, Some(zoom.unwrap_or_default())))
        }
    }
}

impl EpubNavigationTarget {
    fn validate(self) -> Result<Self, ServiceError> {
        if let Some(cfi) = self.cfi.as_ref() {
            EpubLocator {
                cfi: cfi.clone(),
                href: self.href.clone(),
                fraction: self.fraction,
                x: self.x,
                y: self.y,
            }
            .validate()?;
            return Ok(self);
        }
        if self.href.is_empty()
            || self.href.len() > MAX_EPUB_LOCATOR_TEXT_BYTES
            || self.href.chars().any(char::is_control)
            || !valid_epub_href(&self.href, true)
        {
            return Err(ServiceError::new(
                "invalid-epub-target",
                "EPUB navigation href is not a bounded internal target",
            ));
        }
        if self.fraction.is_some() || self.x.is_some() || self.y.is_some() {
            return Err(ServiceError::new(
                "invalid-epub-target",
                "EPUB href-only targets do not accept position data",
            ));
        }
        Ok(self)
    }
}

impl EpubOutline {
    fn validate(self) -> Result<Self, ServiceError> {
        if self.items.len() > MAX_EPUB_OUTLINE_ITEMS {
            return Err(ServiceError::new(
                "invalid-epub-outline",
                "EPUB outline contains too many items",
            ));
        }
        let mut text_bytes = 0_usize;
        for item in &self.items {
            text_bytes = text_bytes
                .checked_add(item.title.len())
                .and_then(|value| {
                    value.checked_add(item.href.as_ref().map_or(0, String::len))
                })
                .ok_or_else(|| {
                    ServiceError::new(
                        "invalid-epub-outline",
                        "EPUB outline text exceeds its aggregate limit",
                    )
                })?;
            if item.title.trim().is_empty()
                || item.title.len() > MAX_EPUB_OUTLINE_TITLE_BYTES
                || text_bytes > MAX_EPUB_OUTLINE_TEXT_BYTES
                || item.title.chars().any(char::is_control)
                || item.depth > MAX_EPUB_OUTLINE_DEPTH
                || item.href.as_ref().is_some_and(|href| {
                    href.is_empty()
                        || href.len() > MAX_EPUB_LOCATOR_TEXT_BYTES
                        || href.chars().any(char::is_control)
                        || !valid_epub_href(href, true)
                })
            {
                return Err(ServiceError::new(
                    "invalid-epub-outline",
                    "EPUB outline contains an invalid item",
                ));
            }
        }
        Ok(self)
    }
}

impl EpubSelection {
    fn validate(self) -> Result<Self, ServiceError> {
        let start = EpubLocator {
            cfi: self.start,
            href: self.href,
            fraction: None,
            x: None,
            y: None,
        }
        .validate()?;
        let end = EpubLocator {
            cfi: self.end,
            href: start.href.clone(),
            fraction: None,
            x: None,
            y: None,
        }
        .validate()?;
        if start.cfi == end.cfi {
            return Err(ServiceError::new(
                "invalid-epub-selection",
                "EPUB selection endpoints must differ",
            ));
        }
        Ok(Self {
            href: start.href,
            start: start.cfi,
            end: end.cfi,
        })
    }
}

impl ViewSelectionTextParams {
    fn validate(mut self) -> Result<Self, ServiceError> {
        self.selection = self.selection.validate()?;
        if !(1..=MAX_EPUB_SELECTION_CHARACTER_LIMIT)
            .contains(&self.character_limit)
        {
            return Err(ServiceError::new(
                "invalid-selection-limit",
                format!(
                    concat!(
                        "EPUB selection character limit must be between ",
                        "1 and {}"
                    ),
                    MAX_EPUB_SELECTION_CHARACTER_LIMIT
                ),
            ));
        }
        if self.offset > MAX_EPUB_SELECTION_CHARACTERS {
            return Err(ServiceError::new(
                "invalid-selection-offset",
                format!(
                    "EPUB selection offset must not exceed {}",
                    MAX_EPUB_SELECTION_CHARACTERS
                ),
            ));
        }
        Ok(self)
    }
}

impl EpubSearchCursor {
    fn validate(self) -> Result<Self, ServiceError> {
        if !valid_epub_href(&self.href, false)
            || self.href.len() > MAX_EPUB_LOCATOR_TEXT_BYTES
            || self.href.chars().any(char::is_control)
            || self
                .offset
                .is_some_and(|value| value > MAX_EPUB_SEARCH_CURSOR_OFFSET)
        {
            return Err(ServiceError::new(
                "invalid-search-cursor",
                "EPUB search cursor is invalid",
            ));
        }
        Ok(self)
    }
}

impl ViewSearchParams {
    fn validate(mut self) -> Result<Self, ServiceError> {
        let query_characters = self.query.chars().count();
        if query_characters == 0
            || query_characters > MAX_EPUB_SEARCH_QUERY_CHARACTERS
            || self.query.chars().any(char::is_control)
        {
            return Err(ServiceError::new(
                "invalid-search-query",
                format!(
                    "EPUB search query must contain 1 to {} characters",
                    MAX_EPUB_SEARCH_QUERY_CHARACTERS
                ),
            ));
        }
        if !(1..=MAX_EPUB_SEARCH_MATCH_LIMIT).contains(&self.match_limit) {
            return Err(ServiceError::new(
                "invalid-search-limit",
                format!(
                    "EPUB search match limit must be between 1 and {}",
                    MAX_EPUB_SEARCH_MATCH_LIMIT
                ),
            ));
        }
        if !(1..=MAX_EPUB_SEARCH_SECTION_LIMIT).contains(&self.section_limit) {
            return Err(ServiceError::new(
                "invalid-search-limit",
                format!(
                    "EPUB search section limit must be between 1 and {}",
                    MAX_EPUB_SEARCH_SECTION_LIMIT
                ),
            ));
        }
        self.cursor =
            self.cursor.map(EpubSearchCursor::validate).transpose()?;
        self.origin = self.origin.map(EpubLocator::validate).transpose()?;
        if self.cursor.is_some() && self.origin.is_some() {
            return Err(ServiceError::new(
                "invalid-search-cursor",
                "EPUB search origin and cursor are mutually exclusive",
            ));
        }
        Ok(self)
    }
}

impl EpubSearchMatch {
    fn validate(self) -> Result<Self, ServiceError> {
        let selection = EpubSelection {
            href: self.href,
            start: self.start,
            end: self.end,
        }
        .validate()?;
        if self.text.is_empty()
            || self.text.len() > MAX_EPUB_SEARCH_MATCH_TEXT_BYTES
            || self.before.len() > MAX_EPUB_SEARCH_CONTEXT_BYTES
            || self.after.len() > MAX_EPUB_SEARCH_CONTEXT_BYTES
            || self.text.contains('\0')
            || self.before.contains('\0')
            || self.after.contains('\0')
        {
            return Err(ServiceError::new(
                "invalid-search-result",
                "EPUB search match text is invalid",
            ));
        }
        Ok(Self {
            href: selection.href,
            start: selection.start,
            end: selection.end,
            text: self.text,
            before: self.before,
            after: self.after,
        })
    }
}

fn valid_epub_href(value: &str, allow_fragment: bool) -> bool {
    let path = if let Some((path, fragment)) = value.split_once('#') {
        if !allow_fragment || fragment.contains('#') {
            return false;
        }
        path
    } else {
        value
    };
    !path.is_empty()
        && !path.starts_with('/')
        && !value.contains(['\\', '?'])
        && !path.contains(':')
        && !path
            .split('/')
            .any(|part| part.is_empty() || matches!(part, "." | ".."))
}

impl Service {
    fn new(
        outgoing_sender: Sender<Outgoing>,
        incoming_sender: Sender<Incoming>,
    ) -> Self {
        Self {
            views: HashMap::new(),
            resources: ResourceService::default(),
            next_publication: 1,
            version: wry::webview_version().map_err(|error| error.to_string()),
            outgoing_sender,
            incoming_sender,
            pending_searches: HashMap::new(),
        }
    }

    fn parse<T: for<'de> Deserialize<'de>>(
        value: Value,
    ) -> Result<T, ServiceError> {
        let value = if value.is_null() { json!({}) } else { value };
        serde_json::from_value(value).map_err(|error| {
            ServiceError::new("invalid-params", error.to_string())
        })
    }

    fn info(&self, params: Value) -> Result<Value, ServiceError> {
        Self::parse::<EmptyParams>(params)?;
        Ok(info_result(&self.version))
    }

    fn open_publication(
        &mut self,
        params: Value,
    ) -> Result<Value, ServiceError> {
        let params: PublicationOpenParams = Self::parse(params)?;
        let path = std::path::Path::new(&params.path);
        if !path.is_absolute() {
            return Err(ServiceError::new(
                "invalid-publication-path",
                "publication path must be absolute",
            ));
        }
        let publication =
            Publication::open(path).map_err(ServiceError::from)?;
        let id = self.next_publication;
        self.next_publication = id.checked_add(1).ok_or_else(|| {
            ServiceError::new(
                "publication-id-exhausted",
                "no publication IDs remain",
            )
        })?;
        self.resources.insert(id, publication)
    }

    fn publication_info(&self, params: Value) -> Result<Value, ServiceError> {
        let params: PublicationParams = Self::parse(params)?;
        self.resources.info(params.publication)
    }

    fn close_publication(
        &mut self,
        params: Value,
    ) -> Result<Value, ServiceError> {
        let params: PublicationParams = Self::parse(params)?;
        if self
            .views
            .values()
            .any(|view| view.publication == Some(params.publication))
        {
            return Err(ServiceError::new(
                "publication-in-use",
                format!(
                    "publication {} is attached to a live view",
                    params.publication
                ),
            ));
        }
        self.resources.remove(params.publication)?;
        Ok(json!({
            "publication": params.publication,
            "closed": true,
        }))
    }

    fn create_view(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: CreateParams = Self::parse(params)?;
        if self.views.contains_key(&params.view) {
            return Err(ServiceError::new(
                "duplicate-view",
                format!("view {} already exists", params.view),
            ));
        }
        if let Err(message) = &self.version {
            return Err(ServiceError::new(
                "webview-unavailable",
                dependency_message(message),
            ));
        }
        let parent = ParentWindow::new(params.parent)?;
        let resources = self.resources.clone();
        let renderer_events = self.outgoing_sender.clone();
        let renderer_callbacks = self.incoming_sender.clone();
        let view_id = params.view;
        let builder = WebViewBuilder::new()
            .with_custom_protocol(
                APP_PROTOCOL.into(),
                move |_webview_id, request| app_response(request),
            )
            .with_asynchronous_custom_protocol(
                BOOK_PROTOCOL.into(),
                move |_webview_id, request, responder| {
                    resources.respond(request, responder);
                },
            )
            .with_https_scheme(true)
            .with_url(APP_URL)
            .with_navigation_handler(app_navigation_allowed)
            .with_ipc_handler(move |request| {
                if let Some(callback) =
                    renderer_search_callback(view_id, &request)
                {
                    let _ = renderer_callbacks
                        .send(Incoming::RendererSearch(callback));
                } else if let Some(event) = renderer_event(view_id, &request) {
                    let _ = renderer_events.send(Outgoing::Event(event));
                }
            })
            .with_permission_handler(|_| PermissionResponse::Deny)
            .with_new_window_req_handler(|_, _| NewWindowResponse::Deny)
            .with_download_started_handler(|_, _| false);
        let surface_events = self.outgoing_sender.clone();
        let surface = NativeSurface::create(
            builder,
            &parent,
            params.view,
            params.bounds,
            params.visible,
            move |event| {
                let _ = surface_events
                    .send(Outgoing::Event(surface_view_event(event)));
            },
        )?;
        let bounds = surface.bounds();
        self.views.insert(
            params.view,
            NativeView {
                surface,
                publication: None,
            },
        );
        Ok(json!({
            "view": params.view,
            "bounds": bounds,
            "visible": params.visible,
        }))
    }

    fn set_bounds(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: BoundsParams = Self::parse(params)?;
        let bounds = self
            .view_mut(params.view)?
            .surface
            .set_bounds(params.bounds)?;
        Ok(json!({ "view": params.view, "bounds": bounds }))
    }

    fn set_visible(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: VisibleParams = Self::parse(params)?;
        self.view_mut(params.view)?
            .surface
            .set_visible(params.visible)?;
        Ok(json!({ "view": params.view, "visible": params.visible }))
    }

    fn focus(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: ViewParams = Self::parse(params)?;
        self.view(params.view)?.surface.focus()?;
        Ok(json!({ "view": params.view, "focused": true }))
    }

    fn focus_parent(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: ViewParams = Self::parse(params)?;
        self.view(params.view)?.surface.focus_parent()?;
        Ok(json!({ "view": params.view, "focused": false }))
    }

    fn clear_selection(
        &mut self,
        params: Value,
    ) -> Result<Value, ServiceError> {
        let params: ViewParams = Self::parse(params)?;
        let script = publication_clear_selection_script(params.view);
        self.view(params.view)?
            .surface
            .webview()
            .evaluate_script(&script)
            .map_err(|error| {
                ServiceError::new("view-update-failed", error.to_string())
            })?;
        Ok(json!({ "view": params.view, "selection": false }))
    }

    fn set_selection(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params = Self::parse::<ViewSetSelectionParams>(params)?;
        let selection = params.selection.validate()?;
        let view = self.view(params.view)?;
        if view.publication.is_none() {
            return Err(ServiceError::new(
                "view-has-no-publication",
                format!("view {} has no attached publication", params.view),
            ));
        }
        let script = publication_set_selection_script(params.view, &selection);
        view.surface
            .webview()
            .evaluate_script(&script)
            .map_err(|error| {
                ServiceError::new("view-update-failed", error.to_string())
            })?;
        Ok(json!({ "view": params.view, "selection": true }))
    }

    fn selection_text(
        &self,
        id: u64,
        params: Value,
    ) -> Result<(), ServiceError> {
        let params =
            Self::parse::<ViewSelectionTextParams>(params)?.validate()?;
        let view = self.view(params.view)?;
        if view.publication.is_none() {
            return Err(ServiceError::new(
                "view-has-no-publication",
                format!("view {} has no attached publication", params.view),
            ));
        }
        let offset = params.offset;
        let character_limit = params.character_limit;
        let script = publication_selection_text_script(&params);
        let sender = self.outgoing_sender.clone();
        view.surface
            .webview()
            .evaluate_script_with_callback(&script, move |value| {
                let response = renderer_selection_text_response(
                    id,
                    offset,
                    character_limit,
                    &value,
                );
                let _ = sender.send(Outgoing::Response(response));
            })
            .map_err(|error| {
                ServiceError::new("selection-text-failed", error.to_string())
            })?;
        Ok(())
    }

    fn search(&mut self, id: u64, params: Value) -> Result<(), ServiceError> {
        let params = Self::parse::<ViewSearchParams>(params)?.validate()?;
        let view = self.view(params.view)?;
        if view.publication.is_none() {
            return Err(ServiceError::new(
                "view-has-no-publication",
                format!("view {} has no attached publication", params.view),
            ));
        }
        if self.pending_searches.contains_key(&id) {
            return Err(ServiceError::new(
                "duplicate-request",
                format!("search request {id} is already pending"),
            ));
        }
        let view_id = params.view;
        let script = publication_search_script(id, &params);
        self.pending_searches.insert(id, params);
        if let Err(error) = self
            .view(view_id)?
            .surface
            .webview()
            .evaluate_script(&script)
        {
            self.pending_searches.remove(&id);
            return Err(ServiceError::new("search-failed", error.to_string()));
        }
        Ok(())
    }

    fn set_search_result(
        &mut self,
        params: Value,
    ) -> Result<Value, ServiceError> {
        let params: ViewSearchResultParams = Self::parse(params)?;
        let selection = if params.selection.is_null() {
            None
        } else {
            Some(Self::parse::<EpubSelection>(params.selection)?.validate()?)
        };
        let view = self.view(params.view)?;
        if view.publication.is_none() {
            return Err(ServiceError::new(
                "view-has-no-publication",
                format!("view {} has no attached publication", params.view),
            ));
        }
        let script = publication_search_result_script(
            params.view,
            selection.as_ref(),
            params.reveal,
        );
        view.surface
            .webview()
            .evaluate_script(&script)
            .map_err(|error| {
                ServiceError::new("view-update-failed", error.to_string())
            })?;
        Ok(json!({
            "view": params.view,
            "selection": selection.is_some(),
        }))
    }

    fn complete_search(
        &mut self,
        callback: RendererSearchCallback,
    ) -> Option<Response> {
        let params = self.pending_searches.get(&callback.request)?;
        if params.view != callback.view {
            return None;
        }
        let params = self.pending_searches.remove(&callback.request)?;
        let value = serde_json::to_string(&callback.response).ok()?;
        Some(renderer_search_response(callback.request, &params, &value))
    }

    fn open_view_publication(
        &mut self,
        params: Value,
    ) -> Result<Value, ServiceError> {
        let params: ViewPublicationParams = Self::parse(params)?;
        let location =
            params.location.map(EpubLocator::validate).transpose()?;
        let style = params.style.map(EpubStyle::validate).transpose()?;
        let zoom = params.zoom.map(EpubZoom::validate).transpose()?;
        let view = self.view(params.view)?;
        if !view.surface.loaded() {
            return Err(ServiceError::new(
                "view-not-ready",
                format!("view {} has not finished loading", params.view),
            ));
        }
        let layout = self.resources.layout(params.publication)?;
        let (style, zoom) = view_layout_options(layout, style, zoom)?;
        let resource_root = self.resources.resource_root(params.publication)?;
        let script = publication_open_script(
            params.view,
            &resource_root,
            location.as_ref(),
            params.appearance,
            style.as_ref(),
            zoom.as_ref(),
            params.scroll_bars,
        );
        self.view(params.view)?
            .surface
            .webview()
            .evaluate_script(&script)
            .map_err(|error| {
                ServiceError::new(
                    "publication-render-failed",
                    error.to_string(),
                )
            })?;
        self.view_mut(params.view)?.publication = Some(params.publication);
        Ok(json!({
            "view": params.view,
            "publication": params.publication,
            "opening": true,
        }))
    }

    fn set_view_appearance(
        &mut self,
        params: Value,
    ) -> Result<Value, ServiceError> {
        let params: ViewAppearanceParams = Self::parse(params)?;
        let view = self.view(params.view)?;
        if view.publication.is_none() {
            return Err(ServiceError::new(
                "view-has-no-publication",
                format!("view {} has no attached publication", params.view),
            ));
        }
        let script =
            publication_appearance_script(params.view, params.appearance);
        view.surface
            .webview()
            .evaluate_script(&script)
            .map_err(|error| {
                ServiceError::new("view-update-failed", error.to_string())
            })?;
        Ok(json!({
            "view": params.view,
            "appearance": params.appearance,
        }))
    }

    fn navigate_view(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: ViewNavigateParams = Self::parse(params)?;
        let location = params
            .location
            .map(EpubNavigationTarget::validate)
            .transpose()?;
        match (params.command, location.as_ref()) {
            (NavigationCommand::GoTo, None) => {
                return Err(ServiceError::new(
                    "invalid-params",
                    "go-to navigation requires an EPUB location",
                ));
            }
            (NavigationCommand::GoTo, Some(_)) => {}
            (_, Some(_)) => {
                return Err(ServiceError::new(
                    "invalid-params",
                    "non-go-to navigation does not accept an EPUB location",
                ));
            }
            (_, None) => {}
        }
        let view = self.view(params.view)?;
        if view.publication.is_none() {
            return Err(ServiceError::new(
                "view-has-no-publication",
                format!("view {} has no attached publication", params.view),
            ));
        }
        let script = publication_navigation_script(
            params.view,
            params.command,
            location.as_ref(),
        );
        view.surface
            .webview()
            .evaluate_script(&script)
            .map_err(|error| {
                ServiceError::new("view-update-failed", error.to_string())
            })?;
        Ok(json!({
            "view": params.view,
            "command": params.command,
            "navigating": true,
        }))
    }

    fn set_view_style(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: ViewStyleParams = Self::parse(params)?;
        let style = params.style.validate()?;
        let view = self.view(params.view)?;
        let publication = view.publication.ok_or_else(|| {
            ServiceError::new(
                "view-has-no-publication",
                format!("view {} has no attached publication", params.view),
            )
        })?;
        if self.resources.layout(publication)? != PublicationLayout::Reflowable
        {
            return Err(ServiceError::new(
                "invalid-epub-view-layout",
                "fixed-layout EPUB views do not accept reflow style",
            ));
        }
        let script = publication_style_script(params.view, &style);
        view.surface
            .webview()
            .evaluate_script(&script)
            .map_err(|error| {
                ServiceError::new("view-update-failed", error.to_string())
            })?;
        Ok(json!({
            "view": params.view,
            "style": style,
        }))
    }

    fn set_view_zoom(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: ViewZoomParams = Self::parse(params)?;
        let zoom = params.zoom.validate()?;
        let view = self.view(params.view)?;
        let publication = view.publication.ok_or_else(|| {
            ServiceError::new(
                "view-has-no-publication",
                format!("view {} has no attached publication", params.view),
            )
        })?;
        if self.resources.layout(publication)?
            != PublicationLayout::PrePaginated
        {
            return Err(ServiceError::new(
                "invalid-epub-view-layout",
                "reflowable EPUB views do not accept fixed zoom",
            ));
        }
        let script = publication_zoom_script(params.view, &zoom);
        view.surface
            .webview()
            .evaluate_script(&script)
            .map_err(|error| {
                ServiceError::new("view-update-failed", error.to_string())
            })?;
        Ok(json!({
            "view": params.view,
            "zoom": zoom,
        }))
    }

    fn set_view_scroll_bars(
        &mut self,
        params: Value,
    ) -> Result<Value, ServiceError> {
        let params: ViewScrollBarsParams = Self::parse(params)?;
        let view = self.view(params.view)?;
        if view.publication.is_none() {
            return Err(ServiceError::new(
                "view-has-no-publication",
                format!("view {} has no attached publication", params.view),
            ));
        }
        let script =
            publication_scroll_bars_script(params.view, params.visible);
        view.surface
            .webview()
            .evaluate_script(&script)
            .map_err(|error| {
                ServiceError::new("view-update-failed", error.to_string())
            })?;
        Ok(json!({
            "view": params.view,
            "visible": params.visible,
        }))
    }

    fn destroy(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: ViewParams = Self::parse(params)?;
        if self.views.remove(&params.view).is_none() {
            return Err(unknown_view(params.view));
        }
        let pending = self
            .pending_searches
            .iter()
            .filter_map(|(id, search)| {
                (search.view == params.view).then_some(*id)
            })
            .collect::<Vec<_>>();
        for id in pending {
            self.pending_searches.remove(&id);
            let _ = self.outgoing_sender.send(Outgoing::Response(
                Response::failure(
                    Some(id),
                    "search-unavailable",
                    "EPUB view was destroyed during search",
                ),
            ));
        }
        Ok(json!({ "view": params.view, "destroyed": true }))
    }

    fn status(&self, params: Value) -> Result<Value, ServiceError> {
        let params: ViewParams = Self::parse(params)?;
        let view = self.view(params.view)?;
        Ok(json!({
            "view": params.view,
            "loaded": view.surface.loaded(),
            "bounds": view.surface.bounds(),
            "visible": view.surface.visible(),
            "publication": view.publication,
        }))
    }

    fn view(&self, id: u64) -> Result<&NativeView, ServiceError> {
        self.views.get(&id).ok_or_else(|| unknown_view(id))
    }

    fn view_mut(&mut self, id: u64) -> Result<&mut NativeView, ServiceError> {
        self.views.get_mut(&id).ok_or_else(|| unknown_view(id))
    }

    fn handle(&mut self, request: Request) -> (Option<Response>, Control) {
        let operation = match request.operation() {
            Ok(operation) => operation,
            Err(error) => {
                return (
                    Some(Response::failure(
                        Some(request.id),
                        error.code,
                        error.message,
                    )),
                    Control::Continue,
                );
            }
        };
        if operation == Operation::Shutdown {
            let result =
                Self::parse::<EmptyParams>(request.params).and_then(|_| {
                    self.views.clear();
                    self.resources.clear()?;
                    Ok(json!({ "stopped": true }))
                });
            let control = if result.is_ok() {
                Control::Shutdown
            } else {
                Control::Continue
            };
            return (Some(response(request.id, result)), control);
        }
        if matches!(
            operation,
            Operation::ViewSearch | Operation::ViewSelectionText
        ) {
            let result = match operation {
                Operation::ViewSearch => {
                    self.search(request.id, request.params)
                }
                Operation::ViewSelectionText => {
                    self.selection_text(request.id, request.params)
                }
                _ => unreachable!("matched asynchronous operation"),
            };
            let response = result.err().map(|error| {
                Response::failure(Some(request.id), error.code, error.message)
            });
            return (response, Control::Continue);
        }
        let result = match operation {
            Operation::PublicationOpen => self.open_publication(request.params),
            Operation::PublicationInfo => self.publication_info(request.params),
            Operation::PublicationClose => {
                self.close_publication(request.params)
            }
            Operation::ViewInfo => self.info(request.params),
            Operation::ViewCreate => self.create_view(request.params),
            Operation::ViewBounds => self.set_bounds(request.params),
            Operation::ViewAppearance => {
                self.set_view_appearance(request.params)
            }
            Operation::ViewClearSelection => {
                self.clear_selection(request.params)
            }
            Operation::ViewNavigate => self.navigate_view(request.params),
            Operation::ViewSearchResult => {
                self.set_search_result(request.params)
            }
            Operation::ViewSetSelection => self.set_selection(request.params),
            Operation::ViewOpenPublication => {
                self.open_view_publication(request.params)
            }
            Operation::ViewStyle => self.set_view_style(request.params),
            Operation::ViewZoom => self.set_view_zoom(request.params),
            Operation::ViewScrollBars => {
                self.set_view_scroll_bars(request.params)
            }
            Operation::ViewVisible => self.set_visible(request.params),
            Operation::ViewFocus => self.focus(request.params),
            Operation::ViewFocusParent => self.focus_parent(request.params),
            Operation::ViewStatus => self.status(request.params),
            Operation::ViewDestroy => self.destroy(request.params),
            Operation::Shutdown
            | Operation::ViewSearch
            | Operation::ViewSelectionText => {
                unreachable!("handled protocol operation")
            }
        };
        (Some(response(request.id, result)), Control::Continue)
    }
}

fn default_visible() -> bool {
    true
}

fn unknown_view(id: u64) -> ServiceError {
    ServiceError::new("unknown-view", format!("view {id} does not exist"))
}

fn surface_view_event(surface: SurfaceEvent) -> ViewEvent {
    let (event, view, key) = match surface {
        SurfaceEvent::Accelerator { view, key } => {
            ("accelerator", view, Some(key.to_owned()))
        }
        SurfaceEvent::FocusGained { view } => ("focus-gained", view, None),
        SurfaceEvent::FocusLost { view } => ("focus-lost", view, None),
    };
    ViewEvent {
        kind: "event",
        event,
        view,
        message: None,
        location: None,
        outline: None,
        selection: None,
        key,
        uri: None,
        user: None,
        scale: None,
    }
}

fn dependency_message(detail: &str) -> String {
    format!(
        concat!(
            "Microsoft Edge WebView2 Runtime is unavailable; ",
            "install it and restart Yunge Reader ({})"
        ),
        detail
    )
}

fn info_result(version: &Result<String, String>) -> Value {
    match version {
        Ok(version) => json!({
            "platform": "windows",
            "engine": "webview2",
            "available": true,
            "version": version,
        }),
        Err(error) => json!({
            "platform": "windows",
            "engine": "webview2",
            "available": false,
            "message": dependency_message(error),
        }),
    }
}

fn ready_message(version: &Result<String, String>) -> Value {
    let mut ready = info_result(version);
    let object = ready.as_object_mut().expect("info is an object");
    object.insert("kind".into(), json!("webview-ready"));
    object.insert("protocol".into(), json!(PROTOCOL_VERSION));
    object.insert("build-id".into(), json!(BUILD_ID));
    object.insert("capabilities".into(), json!(CAPABILITIES));
    object.insert("accelerators".into(), json!(ACCELERATORS));
    ready
}

fn write_message(
    mut output: impl Write,
    message: &impl Serialize,
) -> Result<(), Error> {
    serde_json::to_writer(&mut output, message)?;
    output.write_all(b"\n")?;
    output.flush()?;
    Ok(())
}

fn pump_messages() {
    let mut message = MSG::default();
    while unsafe { PeekMessageW(&mut message, None, 0, 0, PM_REMOVE) }.as_bool()
    {
        unsafe {
            let _ = TranslateMessage(&message);
            DispatchMessageW(&message);
        }
    }
}

fn write_outgoing(
    receiver: &Receiver<Outgoing>,
    output: &mut impl Write,
) -> Result<(), Error> {
    while let Ok(message) = receiver.try_recv() {
        write_message(&mut *output, &message)?;
    }
    Ok(())
}

pub(super) fn serve() -> Result<(), Error> {
    let (sender, receiver) = mpsc::channel();
    let (outgoing_sender, outgoing_receiver) = mpsc::channel();
    let service_sender = sender.clone();
    thread::spawn(move || {
        for line in io::stdin().lock().lines() {
            let incoming = match line {
                Ok(line) if line.trim().is_empty() => continue,
                Ok(line) => match Request::decode(&line) {
                    Ok(request) => Incoming::Request(request),
                    Err(error) => Incoming::Invalid(error.to_string()),
                },
                Err(error) => Incoming::Invalid(error.to_string()),
            };
            if sender.send(incoming).is_err() {
                break;
            }
        }
    });

    let mut service = Service::new(outgoing_sender, service_sender);
    let stdout = io::stdout();
    let mut output = stdout.lock();
    write_message(&mut output, &ready_message(&service.version))?;
    loop {
        pump_messages();
        write_outgoing(&outgoing_receiver, &mut output)?;
        let incoming = match receiver.recv_timeout(MESSAGE_PUMP_INTERVAL) {
            Ok(incoming) => incoming,
            Err(RecvTimeoutError::Timeout) => continue,
            Err(RecvTimeoutError::Disconnected) => break,
        };
        let (response, control) = match incoming {
            Incoming::Request(request) => service.handle(request),
            Incoming::Invalid(message) => (
                Some(Response::failure(None, "invalid-request", message)),
                Control::Continue,
            ),
            Incoming::RendererSearch(callback) => {
                (service.complete_search(callback), Control::Continue)
            }
        };
        if let Some(response) = response {
            write_message(&mut output, &response)?;
        }
        write_outgoing(&outgoing_receiver, &mut output)?;
        if control == Control::Shutdown {
            break;
        }
    }
    service.views.clear();
    let _ = service.resources.clear();
    pump_messages();
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::{self, File};
    use std::io::Write;
    use std::path::PathBuf;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
    use wry::http::{Method, Request as HttpRequest};
    use zip::write::SimpleFileOptions;
    use zip::{CompressionMethod, ZipWriter};

    static TEMPORARY_ID: AtomicU64 = AtomicU64::new(1);

    struct TemporaryEpub(PathBuf);

    impl Drop for TemporaryEpub {
        fn drop(&mut self) {
            let _ = fs::remove_file(&self.0);
        }
    }

    fn test_epub() -> TemporaryEpub {
        let id = TEMPORARY_ID.fetch_add(1, Ordering::Relaxed);
        let path = std::env::temp_dir().join(format!(
            "yunge-reader-webview-{}-{id}.epub",
            std::process::id()
        ));
        let container = concat!(
            "<container xmlns=\"",
            "urn:oasis:names:tc:opendocument:xmlns:container",
            "\" version=\"1.0\"><rootfiles>",
            "<rootfile full-path=\"OPS/package.opf\" media-type=\"",
            "application/oebps-package+xml",
            "\"/></rootfiles></container>"
        );
        let package = concat!(
            "<package xmlns=\"http://www.idpf.org/2007/opf\" ",
            "version=\"3.0\"><metadata xmlns:dc=\"",
            "http://purl.org/dc/elements/1.1/",
            "\"><dc:title>Protocol Book</dc:title>",
            "</metadata><manifest>",
            "<item id=\"chapter\" href=\"chapter.xhtml\" media-type=\"",
            "application/xhtml+xml\"/>",
            "</manifest></package>"
        );
        let file = File::create(&path).unwrap();
        let mut archive = ZipWriter::new(file);
        for (name, contents, method) in [
            (
                "mimetype",
                "application/epub+zip",
                CompressionMethod::Stored,
            ),
            (
                "META-INF/container.xml",
                container,
                CompressionMethod::Deflated,
            ),
            ("OPS/package.opf", package, CompressionMethod::Deflated),
            (
                "OPS/chapter.xhtml",
                "<html><body>Chapter</body></html>",
                CompressionMethod::Deflated,
            ),
        ] {
            let options =
                SimpleFileOptions::default().compression_method(method);
            archive.start_file(name, options).unwrap();
            archive.write_all(contents.as_bytes()).unwrap();
        }
        archive.finish().unwrap();
        TemporaryEpub(path)
    }

    fn handle_immediate(
        service: &mut Service,
        request: Request,
    ) -> (Response, Control) {
        let (response, control) = service.handle(request);
        (
            response.expect("operation returns an immediate response"),
            control,
        )
    }

    #[test]
    fn renderer_assets_are_bounded_and_script_safe() {
        let response = app_response(
            HttpRequest::builder()
                .uri(APP_URL)
                .body(Vec::new())
                .unwrap(),
        );
        assert_eq!(response.status(), 200);
        assert_eq!(response.headers()["Cache-Control"], "no-store");
        assert!(
            response.headers()["Content-Security-Policy"]
                .to_str()
                .unwrap()
                .contains("script-src 'self'")
        );
        let csp = response.headers()["Content-Security-Policy"]
            .to_str()
            .unwrap();
        assert!(csp.contains("style-src 'self' blob: 'unsafe-inline'"));
        assert!(csp.contains("img-src blob: data:"));
        assert!(csp.contains("font-src blob: data:"));
        assert!(csp.contains("media-src blob: data:"));
        assert!(!csp.contains("script-src 'self' blob:"));
        assert!(app_asset("foliate-js/search.js").is_some());
        assert!(app_asset("foliate-js/view.js").is_some());
        assert!(app_asset("foliate-js/vendor/zip.js").is_none());
        let adapter =
            std::str::from_utf8(app_asset("yunge-reader.js").unwrap().1)
                .unwrap();
        assert!(adapter.contains("post('accelerator', { key })"));
        assert!(adapter.contains("const READER_CHARACTER_KEYS"));
        assert!(adapter.contains("checkedRendererAccelerators"));
        assert!(adapter.contains("event.code === 'Space'"));
        assert!(adapter.contains("key === 'SPC' || key === 'g'"));
        assert!(adapter.contains("case 'previous-page':"));
        assert!(adapter.contains("case 'next-page':"));
        assert!(adapter.contains("case 'previous-line':"));
        assert!(adapter.contains("case 'first':"));
        assert!(adapter.contains("case 'last':"));
        assert!(adapter.contains("const showBoundary"));
        assert!(adapter.contains("section.linear !== 'no'"));
        assert!(adapter.contains("[location.cfi, true]"));
        assert!(adapter.contains("location CFI and href do not match"));
        assert!(adapter.contains("session.location = location"));
        assert!(adapter.contains("lineDistance(session), false"));
        assert!(adapter.contains("moveFixedViewport(session, -40)"));
        assert!(adapter.contains("moveFixedViewport(session, 40)"));
        assert!(adapter.contains("post('external-link'"));
        assert!(adapter.contains("checkedExternalURI(event.detail?.href)"));
        assert!(adapter.contains("checkedAppearance(appearance)"));
        assert!(adapter.contains("post('appearance-error'"));
        assert!(adapter.contains("applyReadingStyle(view, style)"));
        assert!(adapter.contains("if (view.isFixedLayout) return"));
        assert!(adapter.contains("collapseCFI(relocation.cfi)"));
        assert!(adapter.contains("session.commandNavigation"));
        assert!(adapter.contains("view.renderer.setAttribute('animated', '')"));
        assert!(adapter.contains("pendingNavigation"));
        assert!(adapter.contains("navigationRunning"));
        assert!(adapter.contains("case 'show-selection':"));
        assert!(adapter.contains("selected.addRange(range)"));
        assert!(adapter.contains("'boundary-scroll', event =>"));
        assert!(adapter.contains("scheduleNavigation(session, { command })"));
        assert!(
            adapter
                .contains("scheduleNavigation(session, { command, location })")
        );
        assert!(!adapter.contains("session.navigation = session.navigation"));
        assert!(adapter.contains("if (session.opening) return"));
        assert!(adapter.contains("view.renderer.addEventListener('relocate'"));
        assert!(adapter.contains("pendingStyle"));
        assert!(adapter.contains("requestAnimationFrame("));
        assert!(adapter.contains("post('style-error'"));
        assert!(adapter.contains("post('zoom-changed', { scale })"));
        assert!(adapter.contains("installSelectionTracking"));
        assert!(adapter.contains(
            "clearSelection, navigate, open, search, selectionText, \
             setScrollBars"
        ));
        assert!(adapter.contains("selectedRange"));
        assert!(adapter.contains("Array.from(text)"));
        assert!(adapter.contains("searchMatcher(textWalker"));
        assert!(adapter.contains("search, selectionText"));
        assert!(adapter.contains("fromRangeEndpoints("));
        assert!(adapter.contains("searchResultRevision"));
        assert!(
            adapter
                .contains("setSearchResult, setSelection, setStyle, setZoom")
        );
        let search =
            std::str::from_utf8(app_asset("foliate-js/search.js").unwrap().1)
                .unwrap();
        assert!(search.contains("startIndex === endIndex"));
        assert!(search.contains("strs.slice(startIndex + 1, endIndex)"));
        assert!(search.contains("while (sum < end)"));
        let paginator = std::str::from_utf8(
            app_asset("foliate-js/paginator.js").unwrap().1,
        )
        .unwrap();
        assert!(paginator.contains("prev(distance, smooth = true)"));
        assert!(paginator.contains("#scrollPrev(distance, smooth = true)"));
        let view =
            std::str::from_utf8(app_asset("foliate-js/view.js").unwrap().1)
                .unwrap();
        assert!(view.contains("'external-link', { a, href, href_ }, true"));
        let cfi_asset = app_asset("foliate-js/epubcfi.js").unwrap().1;
        let cfi = std::str::from_utf8(cfi_asset).unwrap();
        assert!(cfi.contains("export const fromRangeEndpoints"));
        let view =
            std::str::from_utf8(app_asset("foliate-js/view.js").unwrap().1)
                .unwrap();
        assert!(view.contains("setSearchResult(cfi, options = {})"));
        assert!(view.contains("options.draw ?? Overlayer.highlight"));
        for path in ["foliate-js/paginator.js", "foliate-js/fixed-layout.js"] {
            let source =
                std::str::from_utf8(app_asset(path).unwrap().1).unwrap();
            assert!(!source.contains("allow-same-origin allow-scripts"));
        }
        let fixed = std::str::from_utf8(
            app_asset("foliate-js/fixed-layout.js").unwrap().1,
        )
        .unwrap();
        assert!(fixed.contains("new CustomEvent('zoom'"));
        assert!(fixed.contains("detail: { scale }"));
        assert!(fixed.contains("align-items: safe center"));
        assert!(fixed.contains("new CustomEvent('boundary-scroll'"));
        assert!(fixed.contains("if (this.#wheelLatched)"));
        assert!(fixed.contains("async moveBy(distance, smooth = true)"));
        assert!(fixed.contains("setViewport(x, y, end = false)"));
        assert!(fixed.contains("x: this.scrollLeft / this.#scale"));
        assert!(fixed.contains("y: this.scrollTop / this.#scale"));
        let paginator = std::str::from_utf8(
            app_asset("foliate-js/paginator.js").unwrap().1,
        )
        .unwrap();
        assert!(paginator.contains(
            "const index = shouldGo ? this.#adjacentIndex(dir) : null"
        ));
        assert!(paginator.contains("if (index != null) await this.#goTo({"));
        assert!(
            paginator.contains("finally {\n            this.#locked = false")
        );
    }

    #[test]
    fn ready_reports_the_exact_accelerator_contract() {
        let message = ready_message(&Ok("test-version".into()));
        assert_eq!(message["accelerators"], json!(ACCELERATORS));
    }

    #[test]
    fn omitted_empty_parameters_are_accepted() {
        Service::parse::<EmptyParams>(Value::Null).unwrap();
    }

    #[test]
    fn publication_operations_own_query_and_release_one_epub() {
        let epub = test_epub();
        let (sender, _receiver) = mpsc::channel();
        let (incoming, _incoming_receiver) = mpsc::channel();
        let mut service = Service::new(sender, incoming);
        let path = epub.0.to_string_lossy().into_owned();
        let (opened, control) = handle_immediate(
            &mut service,
            Request {
                id: 1,
                op: "publication-open".into(),
                params: json!({ "path": path }),
            },
        );

        assert_eq!(control, Control::Continue);
        assert!(opened.ok);
        let result = opened.result.unwrap();
        assert_eq!(result["publication"], 1);
        assert_eq!(result["metadata"]["package-path"], "OPS/package.opf");
        assert_eq!(result["metadata"]["layout"], "reflowable");
        assert_eq!(result["metadata"]["title"], "Protocol Book");
        assert_eq!(result["entry-count"], 4);
        assert!(result["expanded-bytes"].as_u64().unwrap() > 0);
        let resource_root =
            result["resource-root"].as_str().unwrap().to_owned();
        assert!(resource_root.starts_with("yunge-reader-book://"));

        let catalog = service.resources.response(
            HttpRequest::builder()
                .uri(format!("{resource_root}{RESOURCE_CATALOG_PATH}"))
                .body(Vec::new())
                .unwrap(),
        );
        assert_eq!(catalog.status(), 200);
        assert_eq!(
            catalog.headers()["Content-Type"],
            "application/json; charset=utf-8"
        );
        let catalog: Value = serde_json::from_slice(catalog.body()).unwrap();
        assert_eq!(catalog["resources"][0]["path"], "OPS/chapter.xhtml");
        assert_eq!(catalog["resources"][0]["size"], 33);

        let resource = service.resources.response(
            HttpRequest::builder()
                .uri(format!("{resource_root}OPS/chapter.xhtml"))
                .body(Vec::new())
                .unwrap(),
        );
        assert_eq!(resource.status(), 200);
        assert_eq!(resource.headers()["Content-Type"], "application/xhtml+xml");
        assert_eq!(resource.headers()["Cache-Control"], "no-store");
        assert_eq!(
            resource.headers()["Access-Control-Allow-Origin"],
            APP_BROWSER_ORIGIN
        );
        assert_eq!(resource.headers()["X-Content-Type-Options"], "nosniff");
        assert!(
            resource.headers()["Content-Security-Policy"]
                .to_str()
                .unwrap()
                .contains("script-src 'none'")
        );
        assert_eq!(resource.body(), b"<html><body>Chapter</body></html>");

        let head = service.resources.response(
            HttpRequest::builder()
                .method(Method::HEAD)
                .uri(format!("{resource_root}OPS/chapter.xhtml"))
                .body(Vec::new())
                .unwrap(),
        );
        assert_eq!(head.status(), 200);
        assert_eq!(head.headers()["Content-Length"], "33");
        assert!(head.body().is_empty());

        let (info, _) = handle_immediate(
            &mut service,
            Request {
                id: 2,
                op: "publication-info".into(),
                params: json!({ "publication": 1 }),
            },
        );
        let info = info.result.unwrap();
        assert_eq!(info["metadata"]["title"], "Protocol Book");
        assert_eq!(info["resource-root"], resource_root);

        let (closed, _) = handle_immediate(
            &mut service,
            Request {
                id: 3,
                op: "publication-close".into(),
                params: json!({ "publication": 1 }),
            },
        );
        assert_eq!(closed.result.unwrap()["closed"], true);

        let released = service.resources.response(
            HttpRequest::builder()
                .uri(format!("{resource_root}OPS/chapter.xhtml"))
                .body(Vec::new())
                .unwrap(),
        );
        assert_eq!(released.status(), 404);

        let (missing, _) = handle_immediate(
            &mut service,
            Request {
                id: 4,
                op: "publication-info".into(),
                params: json!({ "publication": 1 }),
            },
        );
        assert_eq!(missing.error.unwrap().code, "unknown-publication");
    }

    #[test]
    fn publication_open_requires_an_absolute_strict_path() {
        let (sender, _receiver) = mpsc::channel();
        let (incoming, _incoming_receiver) = mpsc::channel();
        let mut service = Service::new(sender, incoming);
        let (relative, _) = handle_immediate(
            &mut service,
            Request {
                id: 1,
                op: "publication-open".into(),
                params: json!({ "path": "book.epub" }),
            },
        );
        assert_eq!(relative.error.unwrap().code, "invalid-publication-path");

        let (unknown, _) = handle_immediate(
            &mut service,
            Request {
                id: 2,
                op: "publication-open".into(),
                params: json!({ "path": "book.epub", "extra": true }),
            },
        );
        assert_eq!(unknown.error.unwrap().code, "invalid-params");
    }

    #[test]
    fn resource_protocol_rejects_unsafe_targets_and_methods() {
        let token = "0123456789abcdef0123456789abcdef";
        let valid =
            format!("{BOOK_PROTOCOL}://localhost/{token}/OPS/chapter.xhtml");
        assert_eq!(
            resource_request_target(&valid.parse().unwrap()).unwrap(),
            (token.into(), "OPS/chapter.xhtml".into())
        );
        for uri in [
            format!("{BOOK_PROTOCOL}://wrong/{token}/OPS/chapter.xhtml"),
            format!("{BOOK_PROTOCOL}://localhost/short/OPS/chapter.xhtml"),
            format!("{BOOK_PROTOCOL}://localhost/{token}/"),
            format!(
                "{BOOK_PROTOCOL}://localhost/{token}/OPS/%2e%2e/chapter.xhtml"
            ),
            format!(
                "{BOOK_PROTOCOL}://localhost/{token}/OPS/a%2fchapter.xhtml"
            ),
            format!(
                "{BOOK_PROTOCOL}://localhost/{token}/OPS/chapter.xhtml?x=1"
            ),
        ] {
            let parsed = uri.parse().unwrap();
            assert!(
                resource_request_target(&parsed).is_err(),
                "accepted {uri}"
            );
        }

        let (sender, _receiver) = mpsc::channel();
        let (incoming, _incoming_receiver) = mpsc::channel();
        let service = Service::new(sender, incoming);
        let response = service.resources.response(
            HttpRequest::builder()
                .method(Method::POST)
                .uri(valid)
                .body(Vec::new())
                .unwrap(),
        );
        assert_eq!(response.status(), 405);
    }

    #[test]
    fn renderer_ipc_accepts_only_owned_bounded_messages() {
        let ready = HttpRequest::builder()
            .uri(APP_BROWSER_URL)
            .body(
                concat!(
                    r#"{"protocol":1,"event":"publication-ready","#,
                    r#""location":{"cfi":"epubcfi(/6/4)","#,
                    r#""href":"OPS/chapter.xhtml","fraction":0.25},"#,
                    r#""outline":{"items":[{"title":"Chapter","#,
                    r#""depth":0,"href":"OPS/chapter.xhtml#start"}],"#,
                    r#""truncated":false}}"#,
                )
                .into(),
            )
            .unwrap();
        let event = renderer_event(7, &ready).unwrap();
        assert_eq!(event.event, "publication-ready");
        assert_eq!(event.view, 7);
        assert!(event.message.is_none());
        assert!(event.selection.is_none());
        assert!(event.key.is_none());
        assert!(event.uri.is_none());
        assert_eq!(
            event.outline,
            Some(EpubOutline {
                items: vec![EpubOutlineItem {
                    title: "Chapter".into(),
                    depth: 0,
                    href: Some("OPS/chapter.xhtml#start".into()),
                }],
                truncated: false,
            })
        );
        assert_eq!(
            event.location,
            Some(EpubLocator {
                cfi: "epubcfi(/6/4)".into(),
                href: "OPS/chapter.xhtml".into(),
                fraction: Some(0.25),
                x: None,
                y: None,
            })
        );

        let location = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                concat!(
                    r#"{"protocol":1,"event":"location","user":true,"#,
                    r#""location":{"#,
                    r#""cfi":"epubcfi(/6/6)","#,
                    r#""href":"OPS/next.xhtml"}}"#,
                )
                .into(),
            )
            .unwrap();
        let event = renderer_event(7, &location).unwrap();
        assert_eq!(event.event, "location");
        assert_eq!(event.user, Some(true));
        assert_eq!(event.location.unwrap().fraction, None);
        assert!(event.outline.is_none());
        assert!(event.selection.is_none());
        assert!(event.uri.is_none());

        let external_link = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                r#"{"protocol":1,"event":"external-link",
                    "uri":"https://example.com/reference"}"#
                    .into(),
            )
            .unwrap();
        let event = renderer_event(7, &external_link).unwrap();
        assert_eq!(event.event, "external-link");
        assert_eq!(event.uri.as_deref(), Some("https://example.com/reference"));
        assert!(event.location.is_none());
        assert!(event.selection.is_none());

        let selection = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                concat!(
                    r#"{"protocol":1,"event":"selection","selection":{"#,
                    r#""href":"OPS/chapter.xhtml","#,
                    r#""start":"epubcfi(/6/4!/4/2/1:0)","#,
                    r#""end":"epubcfi(/6/4!/4/2/1:7)"}}"#,
                )
                .into(),
            )
            .unwrap();
        let event = renderer_event(7, &selection).unwrap();
        assert_eq!(event.event, "selection");
        assert_eq!(
            event.selection,
            Some(Some(EpubSelection {
                href: "OPS/chapter.xhtml".into(),
                start: "epubcfi(/6/4!/4/2/1:0)".into(),
                end: "epubcfi(/6/4!/4/2/1:7)".into(),
            }))
        );

        let selection_clear = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                r#"{"protocol":1,"event":"selection","selection":null}"#.into(),
            )
            .unwrap();
        let event = renderer_event(7, &selection_clear).unwrap();
        assert_eq!(event.selection, Some(None));

        let error = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                r#"{"protocol":1,"event":"publication-error",
                    "message":"bad EPUB"}"#
                    .into(),
            )
            .unwrap();
        let event = renderer_event(8, &error).unwrap();
        assert_eq!(event.event, "publication-error");
        assert_eq!(event.message.as_deref(), Some("bad EPUB"));
        assert!(event.location.is_none());
        assert!(event.outline.is_none());
        assert!(event.selection.is_none());
        assert!(event.key.is_none());

        let appearance_error = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                r#"{"protocol":1,"event":"appearance-error",
                    "message":"bad appearance"}"#
                    .into(),
            )
            .unwrap();
        let event = renderer_event(8, &appearance_error).unwrap();
        assert_eq!(event.event, "appearance-error");
        assert_eq!(event.message.as_deref(), Some("bad appearance"));
        assert!(event.location.is_none());
        assert!(event.selection.is_none());

        let style_error = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                r#"{"protocol":1,"event":"style-error",
                    "message":"bad style"}"#
                    .into(),
            )
            .unwrap();
        let event = renderer_event(8, &style_error).unwrap();
        assert_eq!(event.event, "style-error");
        assert_eq!(event.message.as_deref(), Some("bad style"));
        assert!(event.location.is_none());
        assert!(event.outline.is_none());
        assert!(event.selection.is_none());
        assert!(event.key.is_none());

        let zoom_changed = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                r#"{"protocol":1,"event":"zoom-changed","scale":1.25}"#.into(),
            )
            .unwrap();
        let event = renderer_event(8, &zoom_changed).unwrap();
        assert_eq!(event.event, "zoom-changed");
        assert_eq!(event.scale, Some(1.25));
        assert!(event.message.is_none());
        assert!(event.location.is_none());

        let zoom_error = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                r#"{"protocol":1,"event":"zoom-error",
                    "message":"bad zoom"}"#
                    .into(),
            )
            .unwrap();
        let event = renderer_event(8, &zoom_error).unwrap();
        assert_eq!(event.event, "zoom-error");
        assert_eq!(event.message.as_deref(), Some("bad zoom"));
        assert!(event.scale.is_none());
        assert!(event.location.is_none());

        let scroll_bars_error = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                r#"{"protocol":1,"event":"scroll-bars-error",
                    "message":"bad scroll bars"}"#
                    .into(),
            )
            .unwrap();
        let event = renderer_event(8, &scroll_bars_error).unwrap();
        assert_eq!(event.event, "scroll-bars-error");
        assert_eq!(event.message.as_deref(), Some("bad scroll bars"));
        assert!(event.location.is_none());
        assert!(event.outline.is_none());
        assert!(event.selection.is_none());
        assert!(event.key.is_none());

        for key in RENDERER_ACCELERATORS {
            let payload = json!({
                "protocol": 1,
                "event": "accelerator",
                "key": key,
            })
            .to_string();
            let accelerator = HttpRequest::builder()
                .uri(APP_URL)
                .body(payload.into())
                .unwrap();
            let event = renderer_event(8, &accelerator).unwrap();
            assert_eq!(event.event, "accelerator");
            assert_eq!(event.key.as_deref(), Some(key));
            assert!(event.location.is_none());
            assert!(event.outline.is_none());
            assert!(event.selection.is_none());
            assert!(event.message.is_none());
        }

        for request in [
            HttpRequest::builder()
                .uri("https://example.invalid/")
                .body(ready.body().clone())
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(r#"{"protocol":2,"event":"publication-ready"}"#.into())
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(r#"{"protocol":1,"event":"other"}"#.into())
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(r#"{"protocol":1,"event":"publication-ready"}"#.into())
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(
                    r#"{"protocol":1,"event":"accelerator","key":"C-d"}"#
                        .into(),
                )
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(
                    concat!(
                        r#"{"protocol":1,"event":"location","location":{"#,
                        r#""cfi":"epubcfi(/6/6)","#,
                        r#""href":"OPS/next.xhtml"}}"#,
                    )
                    .into(),
                )
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(
                    concat!(
                        r#"{"protocol":1,"event":"location","location":{"#,
                        r#""cfi":"bad","href":"../chapter.xhtml"}}"#,
                    )
                    .into(),
                )
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(r#"{"protocol":1,"event":"selection"}"#.into())
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(
                    r#"{"protocol":1,"event":"external-link",
                        "uri":"relative/path"}"#
                        .into(),
                )
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(
                    r#"{"protocol":1,"event":"external-link",
                        "uri":"https://bad uri"}"#
                        .into(),
                )
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(
                    concat!(
                        r#"{"protocol":1,"event":"selection","#,
                        r#""selection":{"href":"OPS/chapter.xhtml","#,
                        r#""start":"epubcfi(/6/4)","#,
                        r#""end":"epubcfi(/6/4)"}}"#,
                    )
                    .into(),
                )
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(
                    concat!(
                        r#"{"protocol":1,"event":"location","#,
                        r#""selection":null,"location":{"#,
                        r#""cfi":"epubcfi(/6/6)","#,
                        r#""href":"OPS/next.xhtml"}}"#,
                    )
                    .into(),
                )
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(r#"{"protocol":1,"event":"zoom-changed"}"#.into())
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(
                    r#"{"protocol":1,"event":"zoom-changed","scale":0}"#.into(),
                )
                .unwrap(),
            HttpRequest::builder()
                .uri(APP_URL)
                .body(
                    concat!(
                        r#"{"protocol":1,"event":"location","scale":1,"#,
                        r#""location":{"cfi":"epubcfi(/6/6)","#,
                        r#""href":"OPS/next.xhtml"}}"#,
                    )
                    .into(),
                )
                .unwrap(),
        ] {
            assert!(renderer_event(9, &request).is_none());
        }

        let oversized_link = HttpRequest::builder()
            .uri(APP_URL)
            .body(
                json!({
                    "protocol": 1,
                    "event": "external-link",
                    "uri": format!(
                        "https:{}",
                        "a".repeat(MAX_EPUB_EXTERNAL_URI_BYTES)
                    ),
                })
                .to_string(),
            )
            .unwrap();
        assert!(renderer_event(9, &oversized_link).is_none());
    }

    #[test]
    fn renderer_search_callbacks_keep_request_identity() {
        let body = json!({
            "protocol": 1,
            "event": "search-result",
            "request": 27,
            "response": {
                "ok": true,
                "result": { "matches": [], "done": true },
            },
        })
        .to_string();
        let request = HttpRequest::builder().uri(APP_URL).body(body).unwrap();
        let callback = renderer_search_callback(4, &request).unwrap();
        assert_eq!(callback.view, 4);
        assert_eq!(callback.request, 27);
        assert_eq!(callback.response["ok"], true);
        assert!(renderer_event(4, &request).is_none());
    }

    #[test]
    fn service_completes_only_matching_search_callbacks() {
        let (outgoing, _outgoing_receiver) = mpsc::channel();
        let (incoming, _incoming_receiver) = mpsc::channel();
        let mut service = Service::new(outgoing, incoming);
        service.pending_searches.insert(
            27,
            ViewSearchParams {
                view: 4,
                query: "Chapter".into(),
                case_sensitive: false,
                direction: SearchDirection::Forward,
                origin: None,
                cursor: None,
                match_limit: 2,
                section_limit: 1,
            },
        );
        let response = json!({
            "ok": true,
            "result": { "matches": [], "done": true },
        });
        assert!(
            service
                .complete_search(RendererSearchCallback {
                    view: 5,
                    request: 27,
                    response: response.clone(),
                })
                .is_none()
        );
        assert!(service.pending_searches.contains_key(&27));
        let completed = service
            .complete_search(RendererSearchCallback {
                view: 4,
                request: 27,
                response,
            })
            .unwrap();
        assert!(completed.ok);
        assert!(!service.pending_searches.contains_key(&27));
    }

    #[test]
    fn publication_script_serializes_renderer_inputs() {
        let location = EpubLocator {
            cfi: "epubcfi(/6/4!/4/2)".into(),
            href: "OPS/chapter.xhtml".into(),
            fraction: Some(0.25),
            x: Some(12.5),
            y: Some(30.0),
        };
        let script = publication_open_script(
            4,
            "https://yunge-reader-book.localhost/token/",
            Some(&location),
            EpubAppearance::FollowEmacs,
            Some(&EpubStyle::default()),
            None,
            false,
        );
        assert!(script.starts_with("void globalThis.yungeReader.open("));
        assert!(script.contains(r#""view":4"#));
        assert!(script.contains(r#""resourceRoot":"https://"#));
        assert!(script.contains(r#""cfi":"epubcfi(/6/4!/4/2)"#));
        assert!(script.contains(r#""x":12.5"#));
        assert!(script.contains(r#""y":30.0"#));
        assert!(script.contains(r#""appearance":"follow-emacs""#));
        assert!(script.contains(r#""font-scale":1.0"#));
        assert!(script.contains(r#""line-height":1.6"#));
        assert!(script.contains(r#""content-width":720"#));
        assert!(script.contains(r#""side-padding":7.0"#));
        assert!(script.contains(r#""scrollBars":false"#));
        assert!(script.contains(r#""rendererAccelerators":["+","-","=""#));
        assert!(!script.contains("eval"));

        let fixed_script = publication_open_script(
            4,
            "https://yunge-reader-book.localhost/token/",
            None,
            EpubAppearance::Original,
            None,
            Some(&EpubZoom::Mode(EpubZoomMode::FitPage)),
            false,
        );
        assert!(fixed_script.contains(r#""style":null"#));
        assert!(fixed_script.contains(r#""zoom":"fit-page""#));

        let appearance_script =
            publication_appearance_script(4, EpubAppearance::FollowEmacs);
        assert!(
            appearance_script
                .starts_with("void globalThis.yungeReader.setAppearance(")
        );
        assert!(appearance_script.contains(r#""view":4"#));
        assert!(appearance_script.contains(r#""appearance":"follow-emacs""#));
        assert!(!appearance_script.contains("eval"));

        let style_script = publication_style_script(4, &EpubStyle::default());
        assert!(
            style_script.starts_with("void globalThis.yungeReader.setStyle(")
        );
        assert!(style_script.contains(r#""view":4"#));
        assert!(style_script.contains(r#""font-scale":1.0"#));
        assert!(!style_script.contains("eval"));

        let zoom_script =
            publication_zoom_script(4, &EpubZoom::Mode(EpubZoomMode::FitWidth));
        assert!(
            zoom_script.starts_with("void globalThis.yungeReader.setZoom(")
        );
        assert!(zoom_script.contains(r#""zoom":"fit-width""#));
        assert!(!zoom_script.contains("eval"));

        let scroll_bars = publication_scroll_bars_script(4, true);
        assert!(
            scroll_bars
                .starts_with("void globalThis.yungeReader.setScrollBars(")
        );
        assert!(scroll_bars.contains(r#""view":4"#));
        assert!(scroll_bars.contains(r#""visible":true"#));
        assert!(!scroll_bars.contains("eval"));

        let target = EpubNavigationTarget {
            cfi: Some(location.cfi.clone()),
            href: location.href.clone(),
            fraction: location.fraction,
            x: location.x,
            y: location.y,
        };
        let navigation = publication_navigation_script(
            4,
            NavigationCommand::GoTo,
            Some(&target),
        );
        assert!(
            navigation.starts_with("void globalThis.yungeReader.navigate(")
        );
        assert!(navigation.contains(r#""command":"go-to"#));
        assert!(navigation.contains(r#""x":12.5"#));
        assert!(navigation.contains(r#""y":30.0"#));
        assert!(!navigation.contains("eval"));

        let first =
            publication_navigation_script(4, NavigationCommand::First, None);
        assert!(first.contains(r#""command":"first""#));
        assert!(first.contains(r#""location":null"#));

        let clear = publication_clear_selection_script(4);
        assert!(
            clear.starts_with("void globalThis.yungeReader.clearSelection(")
        );
        assert!(clear.contains(r#""view":4"#));
        assert!(!clear.contains("eval"));

        let selection = EpubSelection {
            href: "OPS/chapter.xhtml".into(),
            start: "epubcfi(/6/4!/4/2/1:0)".into(),
            end: "epubcfi(/6/4!/4/2/1:7)".into(),
        };
        let set_selection = publication_set_selection_script(4, &selection);
        assert!(
            set_selection
                .starts_with("void globalThis.yungeReader.setSelection(")
        );
        assert!(set_selection.contains(r#""selection":{"#));
        assert!(set_selection.contains(r#""href":"OPS/chapter.xhtml""#));
        assert!(!set_selection.contains("eval"));

        let selection_text =
            publication_selection_text_script(&ViewSelectionTextParams {
                view: 4,
                selection: selection.clone(),
                offset: 0,
                character_limit: 16_384,
            });
        assert!(
            selection_text.starts_with("globalThis.yungeReader.selectionText(")
        );
        assert!(selection_text.contains(r#""character-limit":16384"#));
        assert!(selection_text.contains(r#""offset":0"#));
        assert!(!selection_text.contains("eval"));

        let search = publication_search_script(
            27,
            &ViewSearchParams {
                view: 4,
                query: "Chapter".into(),
                case_sensitive: true,
                direction: SearchDirection::Forward,
                origin: None,
                cursor: Some(EpubSearchCursor {
                    href: "OPS/chapter.xhtml".into(),
                    offset: Some(2),
                }),
                match_limit: 32,
                section_limit: 8,
            },
        );
        assert!(search.starts_with("globalThis.yungeReader.search("));
        assert!(search.contains(r#""query":"Chapter""#));
        assert!(search.contains(r#""request":27"#));
        assert!(search.contains(r#""case-sensitive":true"#));
        assert!(search.contains(r#""match-limit":32"#));
        assert!(search.contains(r#""section-limit":8"#));
        assert!(search.contains(r#""offset":2"#));
        assert!(!search.contains("eval"));

        let search_result =
            publication_search_result_script(4, Some(&selection), true);
        assert!(
            search_result
                .starts_with("void globalThis.yungeReader.setSearchResult(")
        );
        assert!(search_result.contains(r#""selection":{"#));
        assert!(search_result.contains(r#""href":"OPS/chapter.xhtml""#));
        assert!(search_result.contains(r#""start":"epubcfi("#));
        assert!(search_result.contains(r#""reveal":true"#));
        assert!(!search_result.contains("eval"));
        let cleared = publication_search_result_script(4, None, false);
        assert!(cleared.contains(r#""selection":null"#));
        assert!(cleared.contains(r#""reveal":false"#));
    }

    #[test]
    fn epub_locations_are_bounded_and_canonical() {
        let valid = EpubLocator {
            cfi: "epubcfi(/6/4!/4/2)".into(),
            href: "OPS/chapter.xhtml".into(),
            fraction: Some(1.0),
            x: Some(MAX_EPUB_VIEWPORT_COORDINATE),
            y: Some(0.0),
        };
        assert_eq!(valid.clone().validate().unwrap(), valid);

        for invalid in [
            EpubLocator {
                cfi: "bad".into(),
                href: "OPS/chapter.xhtml".into(),
                fraction: None,
                x: None,
                y: None,
            },
            EpubLocator {
                cfi: "epubcfi(/6/4)".into(),
                href: "../chapter.xhtml".into(),
                fraction: None,
                x: None,
                y: None,
            },
            EpubLocator {
                cfi: "epubcfi(/6/4)".into(),
                href: "https:chapter.xhtml".into(),
                fraction: None,
                x: None,
                y: None,
            },
            EpubLocator {
                cfi: "epubcfi(/6/4)".into(),
                href: "OPS/chapter.xhtml".into(),
                fraction: Some(1.1),
                x: None,
                y: None,
            },
            EpubLocator {
                cfi: "epubcfi(/6/4)".into(),
                href: "OPS/chapter.xhtml".into(),
                fraction: None,
                x: Some(1.0),
                y: None,
            },
            EpubLocator {
                cfi: "epubcfi(/6/4)".into(),
                href: "OPS/chapter.xhtml".into(),
                fraction: None,
                x: Some(MAX_EPUB_VIEWPORT_COORDINATE + 1.0),
                y: Some(0.0),
            },
        ] {
            assert_eq!(
                invalid.validate().unwrap_err().code,
                "invalid-epub-location"
            );
        }
    }

    #[test]
    fn epub_selections_are_same_spine_distinct_ranges() {
        let valid = EpubSelection {
            href: "OPS/chapter.xhtml".into(),
            start: "epubcfi(/6/4!/4/2/1:0)".into(),
            end: "epubcfi(/6/4!/4/2/1:7)".into(),
        };
        assert_eq!(valid.clone().validate().unwrap(), valid);

        for invalid in [
            EpubSelection {
                href: "OPS/chapter.xhtml#part".into(),
                start: "epubcfi(/6/4!/4/2/1:0)".into(),
                end: "epubcfi(/6/4!/4/2/1:7)".into(),
            },
            EpubSelection {
                href: "OPS/chapter.xhtml".into(),
                start: "bad".into(),
                end: "epubcfi(/6/4!/4/2/1:7)".into(),
            },
            EpubSelection {
                href: "OPS/chapter.xhtml".into(),
                start: "epubcfi(/6/4!/4/2/1:0)".into(),
                end: "epubcfi(/6/4!/4/2/1:0)".into(),
            },
        ] {
            assert!(invalid.validate().is_err());
        }
        assert!(
            EpubSelection {
                href: "OPS/chapter.xhtml".into(),
                start: "epubcfi(/6/4!/4/2/1:0)".into(),
                end: format!("epubcfi({})", "x".repeat(3_072)),
            }
            .validate()
            .is_err()
        );
        assert!(
            Service::parse::<ViewSearchResultParams>(json!({
                "view": 4,
                "reveal": true,
            }))
            .is_err()
        );
        assert!(
            Service::parse::<ViewSetSelectionParams>(json!({
                "view": 4,
                "selection": valid,
                "extra": true,
            }))
            .is_err()
        );
        assert!(
            Service::parse::<ViewSearchResultParams>(json!({
                "view": 4,
                "selection": null,
                "reveal": false,
                "extra": true,
            }))
            .is_err()
        );
    }

    #[test]
    fn epub_selection_text_requests_are_strictly_bounded() {
        let selection = EpubSelection {
            href: "OPS/chapter.xhtml".into(),
            start: "epubcfi(/6/4!/4/2/1:0)".into(),
            end: "epubcfi(/6/4!/4/2/1:7)".into(),
        };
        let valid = ViewSelectionTextParams {
            view: 4,
            selection: selection.clone(),
            offset: MAX_EPUB_SELECTION_CHARACTERS,
            character_limit: MAX_EPUB_SELECTION_CHARACTER_LIMIT,
        };
        assert_eq!(valid.validate().unwrap().view, 4);
        assert!(
            ViewSearchParams {
                view: 4,
                query: "Chapter".into(),
                case_sensitive: false,
                direction: SearchDirection::Backward,
                origin: Some(EpubLocator {
                    cfi: "epubcfi(/6/4!/4/2/1:7)".into(),
                    href: "OPS/chapter.xhtml".into(),
                    fraction: None,
                    x: None,
                    y: None,
                }),
                cursor: None,
                match_limit: 1,
                section_limit: 1,
            }
            .validate()
            .is_ok()
        );

        let invalid_limit = ViewSelectionTextParams {
            view: 4,
            selection: selection.clone(),
            offset: 0,
            character_limit: 0,
        };
        assert_eq!(
            invalid_limit.validate().unwrap_err().code,
            "invalid-selection-limit"
        );
        let invalid_offset = ViewSelectionTextParams {
            view: 4,
            selection,
            offset: MAX_EPUB_SELECTION_CHARACTERS + 1,
            character_limit: 1,
        };
        assert_eq!(
            invalid_offset.validate().unwrap_err().code,
            "invalid-selection-offset"
        );
        assert!(
            Service::parse::<ViewSelectionTextParams>(json!({
                "view": 4,
                "selection": {
                    "href": "OPS/chapter.xhtml",
                    "start": "epubcfi(/6/4!/4/2/1:0)",
                    "end": "epubcfi(/6/4!/4/2/1:7)",
                },
                "offset": 0,
                "character-limit": 16,
                "extra": true,
            }))
            .is_err()
        );
    }

    #[test]
    fn renderer_selection_text_batches_are_independently_validated() {
        let response = renderer_selection_text_response(
            7,
            0,
            2,
            concat!(
                r#"{"ok":true,"result":{"text":"A😀","total":4,"#,
                r#""next-offset":2,"done":false}}"#,
            ),
        );
        assert!(response.ok);
        let result = response.result.unwrap();
        assert_eq!(result["text"], "A😀");
        assert_eq!(result["next-offset"], 2);
        assert_eq!(result["total"], 4);

        let done = renderer_selection_text_response(
            8,
            2,
            8,
            r#"{"ok":true,"result":{"text":"bc","total":4,
                "done":true}}"#,
        );
        assert!(done.ok);
        assert!(done.result.unwrap().get("next-offset").is_none());

        for invalid in [
            r#"null"#,
            r#"{"ok":true,"result":{"text":"abc","total":4,
                "next-offset":2,"done":false}}"#,
            r#"{"ok":true,"result":{"text":"","total":4,
                "next-offset":0,"done":false}}"#,
            r#"{"ok":true,"result":{"text":"a","total":1,
                "next-offset":1,"done":true}}"#,
        ] {
            let response = renderer_selection_text_response(9, 0, 2, invalid);
            assert!(!response.ok);
            assert_eq!(response.error.unwrap().code, "invalid-renderer-result");
        }

        let stale = renderer_selection_text_response(
            10,
            0,
            2,
            concat!(
                r#"{"ok":false,"error":{"code":"#,
                r#""selection-no-longer-current","message":"stale"}}"#,
            ),
        );
        assert_eq!(stale.error.unwrap().code, "selection-no-longer-current");
    }

    #[test]
    fn epub_search_requests_are_strictly_bounded() {
        let valid = ViewSearchParams {
            view: 4,
            query: "Chapter".into(),
            case_sensitive: false,
            direction: SearchDirection::Forward,
            origin: None,
            cursor: Some(EpubSearchCursor {
                href: "OPS/chapter.xhtml".into(),
                offset: Some(MAX_EPUB_SEARCH_CURSOR_OFFSET),
            }),
            match_limit: MAX_EPUB_SEARCH_MATCH_LIMIT,
            section_limit: MAX_EPUB_SEARCH_SECTION_LIMIT,
        };
        assert_eq!(valid.validate().unwrap().view, 4);

        for invalid in [
            ViewSearchParams {
                view: 4,
                query: String::new(),
                case_sensitive: false,
                direction: SearchDirection::Forward,
                origin: None,
                cursor: None,
                match_limit: 1,
                section_limit: 1,
            },
            ViewSearchParams {
                view: 4,
                query: "Chapter".into(),
                case_sensitive: false,
                direction: SearchDirection::Forward,
                origin: None,
                cursor: None,
                match_limit: 0,
                section_limit: 1,
            },
            ViewSearchParams {
                view: 4,
                query: "Chapter".into(),
                case_sensitive: false,
                direction: SearchDirection::Forward,
                origin: None,
                cursor: None,
                match_limit: 1,
                section_limit: 0,
            },
        ] {
            assert!(invalid.validate().is_err());
        }
        assert!(
            EpubSearchCursor {
                href: "../chapter.xhtml".into(),
                offset: Some(0),
            }
            .validate()
            .is_err()
        );
    }

    #[test]
    fn renderer_search_batches_are_independently_validated() {
        let params = ViewSearchParams {
            view: 4,
            query: "Chapter".into(),
            case_sensitive: false,
            direction: SearchDirection::Forward,
            origin: None,
            cursor: None,
            match_limit: 2,
            section_limit: 1,
        };
        let value = json!({
            "ok": true,
            "result": {
                "matches": [{
                    "href": "OPS/chapter.xhtml",
                    "start": "epubcfi(/6/4!/4/2/1:0)",
                    "end": "epubcfi(/6/4!/4/2/1:7)",
                    "text": "Chapter",
                    "before": "A ",
                    "after": " title",
                }],
                "cursor": {
                    "href": "OPS/chapter.xhtml",
                    "offset": 1,
                },
                "done": false,
            },
        })
        .to_string();
        let response = renderer_search_response(11, &params, &value);
        assert!(
            response.ok,
            "{}",
            response
                .error
                .as_ref()
                .map_or("missing error", |error| error.message.as_str())
        );
        let result = response.result.unwrap();
        assert_eq!(result["matches"][0]["text"], "Chapter");
        assert_eq!(result["cursor"]["offset"], 1);

        let invalid = [
            "null".into(),
            json!({
                "ok": true,
                "result": { "matches": [], "done": false },
            })
            .to_string(),
            json!({
                "ok": true,
                "result": {
                    "matches": [],
                    "cursor": {
                        "href": "../chapter.xhtml",
                        "offset": 0,
                    },
                    "done": false,
                },
            })
            .to_string(),
            json!({
                "ok": true,
                "result": {
                    "matches": [{
                        "href": "OPS/chapter.xhtml",
                        "start": "epubcfi(/6/4)",
                        "end": "epubcfi(/6/4)",
                        "text": "Chapter",
                        "before": "",
                        "after": "",
                    }],
                    "done": true,
                },
            })
            .to_string(),
        ];
        for invalid in &invalid {
            let response = renderer_search_response(12, &params, invalid);
            assert!(!response.ok);
            assert_eq!(response.error.unwrap().code, "invalid-renderer-result");
        }

        let unavailable_value = json!({
            "ok": false,
            "error": {
                "code": "search-unavailable",
                "message": "unavailable",
            },
        })
        .to_string();
        let unavailable =
            renderer_search_response(13, &params, &unavailable_value);
        assert_eq!(unavailable.error.unwrap().code, "search-unavailable");
    }

    #[test]
    fn backward_renderer_search_cursors_decrease() {
        let params = ViewSearchParams {
            view: 4,
            query: "Chapter".into(),
            case_sensitive: false,
            direction: SearchDirection::Backward,
            origin: None,
            cursor: Some(EpubSearchCursor {
                href: "OPS/chapter.xhtml".into(),
                offset: Some(5),
            }),
            match_limit: 2,
            section_limit: 1,
        };
        let decreasing = json!({
            "ok": true,
            "result": {
                "matches": [],
                "cursor": {
                    "href": "OPS/chapter.xhtml",
                    "offset": 2,
                },
                "done": false,
            },
        })
        .to_string();
        assert!(renderer_search_response(20, &params, &decreasing).ok);

        let increasing = decreasing.replace("\"offset\":2", "\"offset\":7");
        assert!(!renderer_search_response(21, &params, &increasing).ok);
    }

    #[test]
    fn outgoing_messages_preserve_the_public_ndjson_shapes() {
        let response = serde_json::to_value(Outgoing::Response(
            Response::success(3, json!({ "scheduled": true })),
        ))
        .unwrap();
        assert_eq!(response["id"], 3);
        assert_eq!(response["ok"], true);
        assert!(response.get("kind").is_none());

        let event = serde_json::to_value(Outgoing::Event(surface_view_event(
            SurfaceEvent::Accelerator {
                view: 4,
                key: "<escape>",
            },
        )))
        .unwrap();
        assert_eq!(event["kind"], "event");
        assert_eq!(event["event"], "accelerator");
        assert_eq!(event["view"], 4);
        assert_eq!(event["key"], "<escape>");
        assert!(event.get("id").is_none());

        let focus = serde_json::to_value(Outgoing::Event(surface_view_event(
            SurfaceEvent::FocusGained { view: 4 },
        )))
        .unwrap();
        assert_eq!(focus["kind"], "event");
        assert_eq!(focus["event"], "focus-gained");
        assert_eq!(focus["view"], 4);
        assert!(focus.get("key").is_none());
    }

    #[test]
    fn epub_styles_are_bounded_semantic_values() {
        let default = EpubStyle::default();
        assert_eq!(default.validate().unwrap(), default);

        let parsed = Service::parse::<ViewPublicationParams>(json!({
            "view": 4,
            "publication": 7,
            "appearance": "original",
            "scroll-bars": false,
        }))
        .unwrap();
        assert_eq!(parsed.style, None);
        assert_eq!(parsed.zoom, None);
        assert_eq!(parsed.appearance, EpubAppearance::Original);
        assert!(!parsed.scroll_bars);

        assert!(
            Service::parse::<ViewPublicationParams>(json!({
                "view": 4,
                "publication": 7,
                "appearance": "original",
            }))
            .is_err()
        );
        assert!(
            Service::parse::<ViewPublicationParams>(json!({
                "view": 4,
                "publication": 7,
                "scroll-bars": false,
            }))
            .is_err()
        );

        for zoom in [
            EpubZoom::Mode(EpubZoomMode::FitPage),
            EpubZoom::Mode(EpubZoomMode::FitWidth),
            EpubZoom::Scale(1.5),
        ] {
            assert_eq!(zoom.validate().unwrap(), zoom);
        }
        for scale in [0.24, 8.01, f64::NAN] {
            assert_eq!(
                EpubZoom::Scale(scale).validate().unwrap_err().code,
                "invalid-epub-zoom"
            );
        }

        let (style, zoom) =
            view_layout_options(PublicationLayout::Reflowable, None, None)
                .unwrap();
        assert_eq!(style, Some(EpubStyle::default()));
        assert_eq!(zoom, None);
        let (style, zoom) =
            view_layout_options(PublicationLayout::PrePaginated, None, None)
                .unwrap();
        assert_eq!(style, None);
        assert_eq!(zoom, Some(EpubZoom::default()));
        assert_eq!(
            view_layout_options(
                PublicationLayout::Reflowable,
                None,
                Some(EpubZoom::default()),
            )
            .unwrap_err()
            .code,
            "invalid-epub-view-layout"
        );
        assert_eq!(
            view_layout_options(
                PublicationLayout::PrePaginated,
                Some(EpubStyle::default()),
                None,
            )
            .unwrap_err()
            .code,
            "invalid-epub-view-layout"
        );

        let parsed = Service::parse::<ViewZoomParams>(json!({
            "view": 4,
            "zoom": "fit-width",
        }))
        .unwrap();
        assert_eq!(parsed.zoom, EpubZoom::Mode(EpubZoomMode::FitWidth));

        let parsed = Service::parse::<ViewAppearanceParams>(json!({
            "view": 4,
            "appearance": "follow-emacs",
        }))
        .unwrap();
        assert_eq!(parsed.view, 4);
        assert_eq!(parsed.appearance, EpubAppearance::FollowEmacs);
        assert!(
            Service::parse::<ViewAppearanceParams>(json!({
                "view": 4,
                "appearance": "sepia",
            }))
            .is_err()
        );

        let parsed = Service::parse::<ViewStyleParams>(json!({
            "view": 4,
            "style": {
                "font-scale": 1.25,
                "line-height": 1.8,
                "content-width": 640,
                "side-padding": 10.0,
            },
        }))
        .unwrap();
        assert_eq!(parsed.view, 4);
        assert_eq!(parsed.style.font_scale, 1.25);

        for invalid in [
            EpubStyle {
                font_scale: 0.49,
                ..default
            },
            EpubStyle {
                line_height: 3.1,
                ..default
            },
            EpubStyle {
                content_width: 319,
                ..default
            },
            EpubStyle {
                side_padding: 20.1,
                ..default
            },
            EpubStyle {
                font_scale: f64::NAN,
                ..default
            },
        ] {
            assert_eq!(
                invalid.validate().unwrap_err().code,
                "invalid-epub-style"
            );
        }

        assert!(
            Service::parse::<ViewPublicationParams>(json!({
                "view": 4,
                "publication": 7,
                "appearance": "original",
                "scroll-bars": true,
                "style": {
                    "font-scale": 1.0,
                    "line-height": 1.6,
                    "content-width": 720,
                    "side-padding": 7.0,
                    "color": "red",
                },
            }))
            .is_err()
        );
    }

    #[test]
    fn epub_navigation_accepts_bounded_internal_href_targets() {
        let target = EpubNavigationTarget {
            cfi: None,
            href: "OPS/chapter.xhtml#section-2".into(),
            fraction: None,
            x: None,
            y: None,
        };
        assert_eq!(target.clone().validate().unwrap(), target);

        for href in [
            "../chapter.xhtml#section",
            "/OPS/chapter.xhtml",
            "https:chapter.xhtml",
            "OPS/chapter.xhtml?query",
            "OPS/chapter.xhtml#one#two",
        ] {
            let invalid = EpubNavigationTarget {
                cfi: None,
                href: href.into(),
                fraction: None,
                x: None,
                y: None,
            };
            assert_eq!(
                invalid.validate().unwrap_err().code,
                "invalid-epub-target"
            );
        }
    }

    #[test]
    fn epub_navigation_accepts_relative_scales_and_boundaries() {
        for command in [
            "previous-page",
            "next-page",
            "previous-line",
            "next-line",
            "previous-screen",
            "next-screen",
            "first",
            "last",
        ] {
            let params = Service::parse::<ViewNavigateParams>(json!({
                "view": 4,
                "command": command,
            }))
            .unwrap();
            assert_eq!(params.view, 4);
            assert!(params.location.is_none());
        }

        assert!(
            Service::parse::<ViewNavigateParams>(json!({
                "view": 4,
                "command": "next-paragraph",
            }))
            .is_err()
        );
    }

    #[test]
    fn epub_outlines_are_bounded_and_internal() {
        let outline = EpubOutline {
            items: vec![EpubOutlineItem {
                title: "Part One".into(),
                depth: 0,
                href: Some("OPS/chapter.xhtml#part-one".into()),
            }],
            truncated: false,
        };
        assert_eq!(outline.clone().validate().unwrap(), outline);

        let invalid = EpubOutline {
            items: vec![EpubOutlineItem {
                title: "External".into(),
                depth: 0,
                href: Some("https:example.invalid".into()),
            }],
            truncated: false,
        };
        assert_eq!(
            invalid.validate().unwrap_err().code,
            "invalid-epub-outline"
        );

        let oversized = EpubOutline {
            items: (0..385)
                .map(|_| EpubOutlineItem {
                    title: "x".repeat(MAX_EPUB_OUTLINE_TITLE_BYTES),
                    depth: 0,
                    href: None,
                })
                .collect(),
            truncated: false,
        };
        assert_eq!(
            oversized.validate().unwrap_err().code,
            "invalid-epub-outline"
        );
    }

    #[test]
    fn resource_request_permits_are_bounded_and_released() {
        let active = Arc::new(AtomicUsize::new(0));
        let permits: Vec<_> = (0..MAX_RESOURCE_REQUESTS)
            .map(|_| ResourcePermit::acquire(Arc::clone(&active)).unwrap())
            .collect();
        assert!(ResourcePermit::acquire(Arc::clone(&active)).is_none());
        drop(permits);
        assert!(ResourcePermit::acquire(active).is_some());
    }
}
