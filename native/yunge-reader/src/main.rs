// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use image::{DynamicImage, GenericImageView};
use pdfium_render::prelude::*;
use regex::{Regex, RegexBuilder};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::{self, File};
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

#[cfg(target_os = "windows")]
mod webview;

type Error = Box<dyn std::error::Error>;

const PROTOCOL_VERSION: u32 = 1;
const PDFIUM_API: &str = "7881";
const BUILD_ID: &str = env!("YUNGE_READER_BUILD_ID");
const CAPABILITIES: [&str; 7] = [
    "cache-maintenance",
    "lifecycle",
    "pdf-links",
    "pdf-outline",
    "pdf-render",
    "pdf-search",
    "pdf-text",
];
const PAGE_LINK_MAX_ITEMS: usize = 4_096;
const PAGE_LINK_MAX_LABEL_CHARACTERS: usize = 256;
const PAGE_LINK_MAX_URI_BYTES: usize = 4_096;
const OUTLINE_MAX_ITEMS: usize = 10_000;
const OUTLINE_MAX_TITLE_CHARACTERS: usize = 1_024;
const SEARCH_CONTEXT_CHARACTERS: usize = 24;
const SEARCH_MAX_MATCHES: u32 = 200;
const SEARCH_MAX_PAGES: u32 = 64;
const SEARCH_MAX_QUERY_CHARACTERS: usize = 256;
const SELECTION_MAX_CHARACTERS: u32 = 65_536;
const SELECTION_MAX_PAGES: u32 = 64;

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Request {
    id: u64,
    op: String,
    #[serde(default)]
    params: Value,
}

#[derive(Debug, Serialize)]
struct Ready<'a> {
    kind: &'static str,
    protocol: u32,
    #[serde(rename = "build-id")]
    build_id: &'a str,
    #[serde(rename = "pdfium-api")]
    pdfium_api: &'static str,
    capabilities: [&'static str; 7],
}

#[derive(Debug, Serialize)]
struct ProtocolError {
    code: &'static str,
    message: String,
}

#[derive(Debug, Serialize)]
struct Response {
    id: Option<u64>,
    ok: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<ProtocolError>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Control {
    Continue,
    Shutdown,
}

#[derive(Debug)]
struct ServiceError {
    code: &'static str,
    message: String,
}

struct Service {
    pdfium: Option<&'static Pdfium>,
    pdfium_library: Option<PathBuf>,
    cache_directory: Option<PathBuf>,
    documents: HashMap<u64, OpenDocument>,
    next_document: u64,
}

struct OpenDocument {
    value: PdfDocument<'static>,
    pages: Vec<PageGeometry>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct EmptyParams {}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct OpenParams {
    path: String,
    #[serde(default)]
    password: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct DocumentParams {
    document: u64,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct PageParams {
    document: u64,
    page: u32,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct RenderParams {
    document: u64,
    page: u32,
    width: i32,
    appearance: PdfAppearance,
    #[serde(rename = "cache-key")]
    cache_key: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ThemeColor([u8; 3]);

impl<'de> Deserialize<'de> for ThemeColor {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        let valid = value.len() == 7
            && value.starts_with('#')
            && value.as_bytes()[1..].iter().all(|byte| {
                byte.is_ascii_digit() || (b'a'..=b'f').contains(byte)
            });
        if !valid {
            return Err(serde::de::Error::custom(
                "PDF colors must be lowercase #rrggbb values",
            ));
        }
        let mut channels = [0; 3];
        for (index, channel) in channels.iter_mut().enumerate() {
            let start = 1 + index * 2;
            *channel = u8::from_str_radix(&value[start..start + 2], 16)
                .map_err(serde::de::Error::custom)?;
        }
        Ok(Self(channels))
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PdfAppearance {
    Original,
    FollowEmacs {
        foreground: ThemeColor,
        background: ThemeColor,
    },
}

impl<'de> Deserialize<'de> for PdfAppearance {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = Value::deserialize(deserializer)?;
        let mut fields = value.as_object().cloned().ok_or_else(|| {
            serde::de::Error::custom("PDF appearance must be an object")
        })?;
        let mode = fields
            .remove("mode")
            .and_then(|value| value.as_str().map(str::to_owned))
            .ok_or_else(|| {
                serde::de::Error::custom("PDF appearance mode must be a string")
            })?;
        match mode.as_str() {
            "original" if fields.is_empty() => Ok(Self::Original),
            "follow-emacs" if fields.len() == 2 => {
                let mut color = |key| {
                    fields
                        .remove(key)
                        .ok_or_else(|| format!("missing PDF color {key}"))
                        .and_then(|value| {
                            serde_json::from_value(value)
                                .map_err(|error| error.to_string())
                        })
                };
                let appearance = Self::FollowEmacs {
                    foreground: color("foreground")
                        .map_err(serde::de::Error::custom)?,
                    background: color("background")
                        .map_err(serde::de::Error::custom)?,
                };
                if fields.is_empty() {
                    Ok(appearance)
                } else {
                    Err(serde::de::Error::custom(
                        "unknown PDF appearance color",
                    ))
                }
            }
            _ => Err(serde::de::Error::custom(
                "invalid PDF appearance mode or fields",
            )),
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct CachePruneParams {
    #[serde(rename = "max-bytes")]
    max_bytes: u64,
    #[serde(rename = "target-bytes")]
    target_bytes: u64,
}

#[derive(Debug)]
struct CacheEntry {
    path: PathBuf,
    size: u64,
    modified: Option<SystemTime>,
}

#[derive(
    Clone, Copy, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize,
)]
#[serde(deny_unknown_fields)]
struct SelectionPosition {
    page: u32,
    offset: u32,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct SelectionParams {
    document: u64,
    start: SelectionPosition,
    end: SelectionPosition,
    #[serde(default)]
    cursor: Option<SelectionPosition>,
    #[serde(
        default = "default_selection_character_limit",
        rename = "character-limit"
    )]
    character_limit: u32,
    #[serde(default = "default_selection_page_limit", rename = "page-limit")]
    page_limit: u32,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct SearchParams {
    document: u64,
    query: String,
    #[serde(default, rename = "case-sensitive")]
    case_sensitive: bool,
    direction: SearchDirection,
    #[serde(default)]
    origin: Option<SearchPosition>,
    #[serde(default)]
    cursor: Option<SearchPosition>,
    #[serde(default = "default_search_match_limit", rename = "match-limit")]
    match_limit: u32,
    #[serde(default = "default_search_page_limit", rename = "page-limit")]
    page_limit: u32,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "kebab-case")]
enum SearchDirection {
    Forward,
    Backward,
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct SearchPosition {
    page: u32,
    #[serde(default)]
    offset: Option<u32>,
}

#[derive(Debug)]
struct SearchCharacter {
    index: u32,
    text: String,
    start: usize,
    end: usize,
}

#[derive(Debug, Serialize)]
struct SearchMatch {
    start: SelectionPosition,
    end: SelectionPosition,
    text: String,
    before: String,
    after: String,
}

#[derive(Debug, Serialize)]
struct OutlineDestination {
    page: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    x: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    y: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    zoom: Option<f32>,
    view: &'static str,
}

#[derive(Debug, Serialize)]
struct OutlineItem {
    title: String,
    depth: usize,
    destination: Option<OutlineDestination>,
}

#[derive(Debug, Serialize)]
struct OutlineResult {
    items: Vec<OutlineItem>,
    truncated: bool,
}

#[derive(Clone, Copy, Debug, Serialize)]
struct PageLinkBounds {
    left: f32,
    bottom: f32,
    right: f32,
    top: f32,
}

impl PageLinkBounds {
    fn from_pdfium(bounds: PdfRect) -> Option<Self> {
        let result = Self {
            left: bounds.left().value,
            bottom: bounds.bottom().value,
            right: bounds.right().value,
            top: bounds.top().value,
        };
        (result.left.is_finite()
            && result.bottom.is_finite()
            && result.right.is_finite()
            && result.top.is_finite()
            && result.right > result.left
            && result.top > result.bottom)
            .then_some(result)
    }
}

#[derive(Debug, Serialize)]
#[serde(tag = "type", rename_all = "kebab-case")]
enum PageLinkAction {
    Location { destination: OutlineDestination },
    Uri { uri: String },
}

#[derive(Debug, Serialize)]
struct PageLink {
    bounds: PageLinkBounds,
    action: PageLinkAction,
    #[serde(skip_serializing_if = "Option::is_none")]
    label: Option<String>,
}

#[derive(Debug, Serialize)]
struct PageLinksResult {
    page: u32,
    links: Vec<PageLink>,
    truncated: bool,
}

fn finite_point(value: Option<PdfPoints>) -> Option<f32> {
    value
        .map(|point| point.value)
        .filter(|value| value.is_finite())
}

fn outline_title(value: Option<String>) -> String {
    let trimmed = value.as_deref().unwrap_or("").trim();
    let title: String =
        trimmed.chars().take(OUTLINE_MAX_TITLE_CHARACTERS).collect();
    if title.is_empty() {
        "(untitled)".to_owned()
    } else {
        title
    }
}

fn outline_destination_view(
    settings: PdfDestinationViewSettings,
) -> (Option<f32>, Option<f32>, Option<f32>, &'static str) {
    match settings {
        PdfDestinationViewSettings::SpecificCoordinatesAndZoom(x, y, zoom) => (
            finite_point(x),
            finite_point(y),
            zoom.filter(|value| value.is_finite() && *value > 0.0),
            "xyz",
        ),
        PdfDestinationViewSettings::FitPageToWindow => {
            (None, None, None, "fit")
        }
        PdfDestinationViewSettings::FitPageHorizontallyToWindow(y) => {
            (None, finite_point(y), None, "fit-horizontal")
        }
        PdfDestinationViewSettings::FitPageVerticallyToWindow(x) => {
            (finite_point(x), None, None, "fit-vertical")
        }
        PdfDestinationViewSettings::FitPageToRectangle(rectangle) => (
            Some(rectangle.left().value),
            Some(rectangle.top().value),
            None,
            "fit-rectangle",
        ),
        PdfDestinationViewSettings::FitBoundsToWindow => {
            (None, None, None, "fit-bounds")
        }
        PdfDestinationViewSettings::FitBoundsHorizontallyToWindow(y) => {
            (None, finite_point(y), None, "fit-bounds-horizontal")
        }
        PdfDestinationViewSettings::FitBoundsVerticallyToWindow(x) => {
            (finite_point(x), None, None, "fit-bounds-vertical")
        }
        PdfDestinationViewSettings::Unknown => (None, None, None, "unknown"),
    }
}

fn outline_destination(
    destination: PdfDestination<'_>,
    pages: &[PageGeometry],
) -> Option<OutlineDestination> {
    let page = u32::try_from(destination.page_index().ok()?).ok()?;
    let settings = destination
        .view_settings()
        .unwrap_or(PdfDestinationViewSettings::Unknown);
    let (x, y, zoom, view) = outline_destination_view(settings);
    let (x, y) = pages.get(page as usize)?.normalize_optional_point(x, y);
    Some(OutlineDestination {
        page,
        x,
        y,
        zoom,
        view,
    })
}

fn page_link_label(value: String) -> Option<String> {
    let normalized = value.split_whitespace().collect::<Vec<_>>().join(" ");
    let label: String = normalized
        .chars()
        .take(PAGE_LINK_MAX_LABEL_CHARACTERS)
        .collect();
    (!label.is_empty()).then_some(label)
}

fn page_link_uri(value: String) -> Option<String> {
    if value.is_empty()
        || value.len() > PAGE_LINK_MAX_URI_BYTES
        || value.chars().any(|character| {
            character.is_control() || character.is_whitespace()
        })
    {
        return None;
    }
    let (scheme, _) = value.split_once(':')?;
    let mut characters = scheme.chars();
    if !characters.next()?.is_ascii_alphabetic()
        || !characters.all(|character| {
            character.is_ascii_alphanumeric()
                || matches!(character, '+' | '-' | '.')
        })
    {
        return None;
    }
    Some(value)
}

fn pdf_open_error_code(error: &PdfiumError) -> &'static str {
    if matches!(
        error,
        PdfiumError::PdfiumLibraryInternalError(
            PdfiumInternalError::PasswordError
        )
    ) {
        "pdf-password-error"
    } else {
        "pdf-open-failed"
    }
}

fn page_link_action(
    link: &PdfLink<'_>,
    pages: &[PageGeometry],
) -> Option<PageLinkAction> {
    match link.action() {
        Some(action) => {
            if let Some(local) = action.as_local_destination_action() {
                local
                    .destination()
                    .ok()
                    .and_then(|destination| {
                        outline_destination(destination, pages)
                    })
                    .map(|destination| PageLinkAction::Location { destination })
            } else if let Some(uri) = action.as_uri_action() {
                uri.uri()
                    .ok()
                    .and_then(page_link_uri)
                    .map(|uri| PageLinkAction::Uri { uri })
            } else {
                None
            }
        }
        None => link
            .destination()
            .and_then(|destination| outline_destination(destination, pages))
            .map(|destination| PageLinkAction::Location { destination }),
    }
}

fn default_search_match_limit() -> u32 {
    64
}

fn default_search_page_limit() -> u32 {
    8
}

fn default_selection_character_limit() -> u32 {
    16_384
}

fn default_selection_page_limit() -> u32 {
    8
}

fn compile_search_pattern(
    query: &str,
    case_sensitive: bool,
) -> Result<Regex, ServiceError> {
    if query.is_empty() {
        return Err(ServiceError::new(
            "invalid-search-query",
            "search query must not be empty",
        ));
    }
    if query.chars().count() > SEARCH_MAX_QUERY_CHARACTERS {
        return Err(ServiceError::new(
            "invalid-search-query",
            format!(
                concat!("search query must not exceed ", "{} characters"),
                SEARCH_MAX_QUERY_CHARACTERS
            ),
        ));
    }
    RegexBuilder::new(&regex::escape(query))
        .case_insensitive(!case_sensitive)
        .unicode(true)
        .build()
        .map_err(|error| {
            ServiceError::new(
                "invalid-search-query",
                format!("could not compile search query: {error}"),
            )
        })
}

fn search_context(characters: &[SearchCharacter]) -> String {
    let mut context = String::new();
    for character in characters {
        context.push_str(&character.text);
    }
    context
}

fn search_character_range(
    characters: &[SearchCharacter],
    start: usize,
    end: usize,
) -> Option<(usize, usize)> {
    let first = characters
        .iter()
        .position(|character| character.end > start)?;
    let last = characters
        .iter()
        .rposition(|character| character.start < end)?;
    (first <= last).then_some((first, last))
}

fn search_page_text(
    page: u32,
    text: &str,
    characters: &[SearchCharacter],
    pattern: &Regex,
    direction: SearchDirection,
    boundary: Option<u32>,
    limit: usize,
) -> Vec<SearchMatch> {
    let mut matches = Vec::new();
    for found in pattern.find_iter(text) {
        let Some((first, last)) =
            search_character_range(characters, found.start(), found.end())
        else {
            continue;
        };
        let start_offset = characters[first].index;
        let outside = match (direction, boundary) {
            (SearchDirection::Forward, Some(minimum)) => start_offset < minimum,
            (SearchDirection::Backward, Some(maximum)) => {
                start_offset >= maximum
            }
            _ => false,
        };
        if outside {
            continue;
        }
        let before_start = first.saturating_sub(SEARCH_CONTEXT_CHARACTERS);
        let after_end =
            characters.len().min(last + 1 + SEARCH_CONTEXT_CHARACTERS);
        let start = SelectionPosition {
            page,
            offset: characters[first].index,
        };
        let end = SelectionPosition {
            page,
            offset: characters[last].index,
        };
        if matches.last().is_some_and(|previous: &SearchMatch| {
            previous.start == start && previous.end == end
        }) {
            continue;
        }
        matches.push(SearchMatch {
            start,
            end,
            text: found.as_str().to_owned(),
            before: search_context(&characters[before_start..first]),
            after: search_context(&characters[last + 1..after_end]),
        });
        if direction == SearchDirection::Forward && matches.len() >= limit {
            break;
        }
    }
    if direction == SearchDirection::Backward {
        matches.reverse();
        matches.truncate(limit);
    }
    matches
}

#[derive(Clone, Copy, Debug, Serialize)]
struct TextPoint {
    x: f32,
    y: f32,
}

#[derive(Clone, Copy, Debug, Serialize)]
struct TextBounds {
    left: f32,
    bottom: f32,
    right: f32,
    top: f32,
}

impl TextBounds {
    fn from_pdfium(bounds: PdfRect) -> Self {
        Self {
            left: bounds.left().value,
            bottom: bounds.bottom().value,
            right: bounds.right().value,
            top: bounds.top().value,
        }
    }

    fn width(self) -> f32 {
        self.right - self.left
    }

    fn height(self) -> f32 {
        self.top - self.bottom
    }

    fn intersection(self, other: Self) -> Option<Self> {
        let intersection = Self {
            left: self.left.max(other.left),
            bottom: self.bottom.max(other.bottom),
            right: self.right.min(other.right),
            top: self.top.min(other.top),
        };
        (intersection.width() > 0.0 && intersection.height() > 0.0)
            .then_some(intersection)
    }
}

#[derive(Clone, Copy, Debug)]
struct PageGeometry {
    left: f32,
    bottom: f32,
    right: f32,
    top: f32,
    width: f32,
    height: f32,
    rotation: PdfPageRenderRotation,
}

impl PageGeometry {
    fn from_page(page: &PdfPage<'_>) -> Option<Self> {
        let boundaries = page.boundaries();
        let crop = boundaries
            .crop()
            .ok()
            .map(|boundary| TextBounds::from_pdfium(boundary.bounds));
        let media = boundaries
            .media()
            .ok()
            .map(|boundary| TextBounds::from_pdfium(boundary.bounds));
        Self::from_boxes(
            crop,
            media,
            page.width().value,
            page.height().value,
            page.rotation().ok()?,
        )
    }

    fn from_boxes(
        crop: Option<TextBounds>,
        media: Option<TextBounds>,
        width: f32,
        height: f32,
        rotation: PdfPageRenderRotation,
    ) -> Option<Self> {
        let intersection = crop
            .zip(media)
            .and_then(|(crop, media)| crop.intersection(media));
        intersection
            .into_iter()
            .chain(crop)
            .chain(media)
            .find_map(|bounds| Self::new(bounds, width, height, rotation))
    }

    fn new(
        bounds: TextBounds,
        width: f32,
        height: f32,
        rotation: PdfPageRenderRotation,
    ) -> Option<Self> {
        let geometry = Self {
            left: bounds.left,
            bottom: bounds.bottom,
            right: bounds.right,
            top: bounds.top,
            width,
            height,
            rotation,
        };
        geometry.valid().then_some(geometry)
    }

    fn valid(self) -> bool {
        if ![
            self.left,
            self.bottom,
            self.right,
            self.top,
            self.width,
            self.height,
        ]
        .iter()
        .all(|value| value.is_finite())
            || self.right <= self.left
            || self.top <= self.bottom
            || self.width <= 0.0
            || self.height <= 0.0
        {
            return false;
        }
        let raw_width = self.right - self.left;
        let raw_height = self.top - self.bottom;
        let (expected_width, expected_height) = match self.rotation {
            PdfPageRenderRotation::None | PdfPageRenderRotation::Degrees180 => {
                (raw_width, raw_height)
            }
            PdfPageRenderRotation::Degrees90
            | PdfPageRenderRotation::Degrees270 => (raw_height, raw_width),
        };
        Self::close_dimension(self.width, expected_width)
            && Self::close_dimension(self.height, expected_height)
    }

    fn close_dimension(actual: f32, expected: f32) -> bool {
        (actual - expected).abs() <= 0.01_f32.max(expected.abs() * 0.001)
    }

    fn normalize_point(self, point: TextPoint) -> TextPoint {
        match self.rotation {
            PdfPageRenderRotation::None => TextPoint {
                x: point.x - self.left,
                y: point.y - self.bottom,
            },
            PdfPageRenderRotation::Degrees90 => TextPoint {
                x: point.y - self.bottom,
                y: self.right - point.x,
            },
            PdfPageRenderRotation::Degrees180 => TextPoint {
                x: self.right - point.x,
                y: self.top - point.y,
            },
            PdfPageRenderRotation::Degrees270 => TextPoint {
                x: self.top - point.y,
                y: point.x - self.left,
            },
        }
    }

    fn normalize_optional_point(
        self,
        x: Option<f32>,
        y: Option<f32>,
    ) -> (Option<f32>, Option<f32>) {
        match self.rotation {
            PdfPageRenderRotation::None => (
                x.map(|value| value - self.left),
                y.map(|value| value - self.bottom),
            ),
            PdfPageRenderRotation::Degrees90 => (
                y.map(|value| value - self.bottom),
                x.map(|value| self.right - value),
            ),
            PdfPageRenderRotation::Degrees180 => (
                x.map(|value| self.right - value),
                y.map(|value| self.top - value),
            ),
            PdfPageRenderRotation::Degrees270 => (
                y.map(|value| self.top - value),
                x.map(|value| value - self.left),
            ),
        }
    }

    fn normalize_bounds(self, bounds: TextBounds) -> Option<TextBounds> {
        let points = [
            TextPoint {
                x: bounds.left,
                y: bounds.bottom,
            },
            TextPoint {
                x: bounds.right,
                y: bounds.bottom,
            },
            TextPoint {
                x: bounds.right,
                y: bounds.top,
            },
            TextPoint {
                x: bounds.left,
                y: bounds.top,
            },
        ]
        .map(|point| self.normalize_point(point));
        let normalized = TextBounds {
            left: points
                .iter()
                .map(|point| point.x)
                .fold(f32::INFINITY, f32::min),
            bottom: points
                .iter()
                .map(|point| point.y)
                .fold(f32::INFINITY, f32::min),
            right: points
                .iter()
                .map(|point| point.x)
                .fold(f32::NEG_INFINITY, f32::max),
            top: points
                .iter()
                .map(|point| point.y)
                .fold(f32::NEG_INFINITY, f32::max),
        };
        (normalized.left.is_finite()
            && normalized.bottom.is_finite()
            && normalized.right.is_finite()
            && normalized.top.is_finite()
            && normalized.right > normalized.left
            && normalized.top > normalized.bottom)
            .then_some(normalized)
    }

    fn normalize_quad(self, quad: [TextPoint; 4]) -> [TextPoint; 4] {
        quad.map(|point| self.normalize_point(point))
    }

    fn normalize_link_bounds(
        self,
        bounds: PageLinkBounds,
    ) -> Option<PageLinkBounds> {
        self.normalize_bounds(TextBounds {
            left: bounds.left,
            bottom: bounds.bottom,
            right: bounds.right,
            top: bounds.top,
        })
        .map(|bounds| PageLinkBounds {
            left: bounds.left,
            bottom: bounds.bottom,
            right: bounds.right,
            top: bounds.top,
        })
    }
}

fn character_font_height(character: &PdfPageTextChar<'_>) -> f32 {
    let font_size = character.unscaled_font_size();
    character
        .text_object()
        .ok()
        .and_then(|object| {
            let font = object.font();
            let ascent = font.ascent(font_size).ok()?.value;
            let descent = font.descent(font_size).ok()?.value;
            let height = ascent - descent;
            (height.is_finite() && height > 0.0).then_some(height)
        })
        .unwrap_or(font_size.value.abs())
}

fn regular_dimensions(
    bounds: TextBounds,
    matrix: PdfMatrix,
) -> Option<(f32, f32)> {
    let a = matrix.a().abs();
    let b = matrix.b().abs();
    let c = matrix.c().abs();
    let d = matrix.d().abs();
    let determinant = a * d - b * c;
    let scale = a * d + b * c;
    if determinant.abs() <= f32::EPSILON.max(scale * 0.0001) {
        return None;
    }
    let width = (bounds.width() * d - c * bounds.height()) / determinant;
    let height = (a * bounds.height() - b * bounds.width()) / determinant;
    (width.is_finite()
        && height.is_finite()
        && width > f32::EPSILON
        && height > f32::EPSILON)
        .then_some((width, height))
}

fn singular_dimensions(
    bounds: TextBounds,
    matrix: PdfMatrix,
    preferred_height: f32,
) -> Option<(f32, f32)> {
    let a = matrix.a().abs();
    let b = matrix.b().abs();
    let c = matrix.c().abs();
    let d = matrix.d().abs();
    let mut maximum_height = f32::INFINITY;
    if c > f32::EPSILON {
        maximum_height = maximum_height.min(bounds.width() / c);
    }
    if d > f32::EPSILON {
        maximum_height = maximum_height.min(bounds.height() / d);
    }
    let height = preferred_height.abs().min(maximum_height);
    if !height.is_finite() || height <= f32::EPSILON {
        return None;
    }
    let mut widths = Vec::with_capacity(2);
    if a > f32::EPSILON {
        widths.push((bounds.width() - c * height) / a);
    }
    if b > f32::EPSILON {
        widths.push((bounds.height() - d * height) / b);
    }
    widths.retain(|width| width.is_finite() && *width > f32::EPSILON);
    if widths.is_empty() {
        None
    } else {
        Some((widths.iter().sum::<f32>() / widths.len() as f32, height))
    }
}

fn character_quad(
    bounds: TextBounds,
    matrix: PdfMatrix,
    preferred_height: impl FnOnce() -> f32,
) -> Option<[TextPoint; 4]> {
    if ![
        bounds.left,
        bounds.bottom,
        bounds.right,
        bounds.top,
        matrix.a(),
        matrix.b(),
        matrix.c(),
        matrix.d(),
    ]
    .iter()
    .all(|value| value.is_finite())
        || bounds.width() <= f32::EPSILON
        || bounds.height() <= f32::EPSILON
    {
        return None;
    }
    let determinant = matrix.a() * matrix.d() - matrix.b() * matrix.c();
    if determinant.abs() <= f32::EPSILON {
        return None;
    }
    let (width, height) = regular_dimensions(bounds, matrix)
        .or_else(|| singular_dimensions(bounds, matrix, preferred_height()))?;
    let point = |x: f32, y: f32| TextPoint {
        x: matrix.a() * x + matrix.c() * y,
        y: matrix.b() * x + matrix.d() * y,
    };
    let lower_left = point(0.0, 0.0);
    let lower_right = point(width, 0.0);
    let upper_right = point(width, height);
    let upper_left = point(0.0, height);
    let mut points = if determinant > 0.0 {
        [lower_left, lower_right, upper_right, upper_left]
    } else {
        [lower_left, upper_left, upper_right, lower_right]
    };
    let minimum_x = points
        .iter()
        .map(|point| point.x)
        .fold(f32::INFINITY, f32::min);
    let minimum_y = points
        .iter()
        .map(|point| point.y)
        .fold(f32::INFINITY, f32::min);
    for point in &mut points {
        point.x += bounds.left - minimum_x;
        point.y += bounds.bottom - minimum_y;
    }
    Some(points)
}

fn ready_message() -> Ready<'static> {
    Ready {
        kind: "ready",
        protocol: PROTOCOL_VERSION,
        build_id: BUILD_ID,
        pdfium_api: PDFIUM_API,
        capabilities: CAPABILITIES,
    }
}

fn valid_cache_key(value: &str) -> bool {
    value.len() == 64
        && value
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
}

fn themed_channel(foreground: u8, background: u8, luminance: u16) -> u8 {
    let ink = 255 - luminance;
    let value = u32::from(foreground) * u32::from(ink)
        + u32::from(background) * u32::from(luminance)
        + 127;
    (value / 255) as u8
}

fn apply_pdf_appearance(
    image: DynamicImage,
    appearance: PdfAppearance,
) -> DynamicImage {
    let PdfAppearance::FollowEmacs {
        foreground,
        background,
    } = appearance
    else {
        return image;
    };
    let mut pixels = image.to_rgba8();
    for pixel in pixels.pixels_mut() {
        let [red, green, blue, alpha] = pixel.0;
        let luminance = (54 * u16::from(red)
            + 183 * u16::from(green)
            + 19 * u16::from(blue)
            + 128)
            / 256;
        for (channel, value) in pixel.0[..3].iter_mut().enumerate() {
            *value = themed_channel(
                foreground.0[channel],
                background.0[channel],
                luminance,
            );
        }
        pixel.0[3] = alpha;
    }
    DynamicImage::ImageRgba8(pixels)
}

fn valid_cache_file_name(value: &str) -> bool {
    value.strip_suffix(".png").is_some_and(valid_cache_key)
}

impl Response {
    fn success(id: u64, result: Value) -> Self {
        Self {
            id: Some(id),
            ok: true,
            result: Some(result),
            error: None,
        }
    }

    fn failure(
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

impl ServiceError {
    fn new(code: &'static str, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
        }
    }
}

impl Service {
    fn new() -> Self {
        Self {
            pdfium: None,
            pdfium_library: env::var_os("YUNGE_READER_PDFIUM")
                .map(PathBuf::from),
            cache_directory: env::var_os("YUNGE_READER_CACHE")
                .map(PathBuf::from),
            documents: HashMap::new(),
            next_document: 1,
        }
    }

    fn pdfium(&mut self) -> Result<&'static Pdfium, ServiceError> {
        if let Some(pdfium) = self.pdfium {
            return Ok(pdfium);
        }
        let library = self.pdfium_library.as_ref().ok_or_else(|| {
            ServiceError::new(
                "pdfium-unavailable",
                "YUNGE_READER_PDFIUM is not set",
            )
        })?;
        if !library.is_absolute() || !library.is_file() {
            return Err(ServiceError::new(
                "pdfium-unavailable",
                format!("PDFium library is unavailable: {}", library.display()),
            ));
        }
        let bindings = Pdfium::bind_to_library(library).map_err(|error| {
            ServiceError::new(
                "pdfium-unavailable",
                format!("could not load PDFium: {error}"),
            )
        })?;
        // PdfDocument borrows Pdfium.  The helper owns one process-lifetime
        // Pdfium instance, while documents are still closed deterministically.
        let pdfium = Box::leak(Box::new(Pdfium::new(bindings)));
        self.pdfium = Some(pdfium);
        Ok(pdfium)
    }

    fn parse<T: DeserializeOwned>(params: Value) -> Result<T, ServiceError> {
        let params = if params.is_null() { json!({}) } else { params };
        serde_json::from_value(params).map_err(|error| {
            ServiceError::new("invalid-params", error.to_string())
        })
    }

    fn document(
        &self,
        document: u64,
    ) -> Result<&PdfDocument<'static>, ServiceError> {
        Ok(&self.open_document(document)?.value)
    }

    fn open_document(
        &self,
        document: u64,
    ) -> Result<&OpenDocument, ServiceError> {
        self.documents.get(&document).ok_or_else(|| {
            ServiceError::new(
                "unknown-document",
                format!("unknown document handle: {document}"),
            )
        })
    }

    fn page_geometry(
        &self,
        document: u64,
        page: u32,
    ) -> Result<PageGeometry, ServiceError> {
        self.open_document(document)?
            .pages
            .get(page as usize)
            .copied()
            .ok_or_else(|| {
                ServiceError::new(
                    "invalid-page",
                    format!("page {page} has no visible geometry"),
                )
            })
    }

    fn page_index(
        document: &PdfDocument<'_>,
        page: u32,
    ) -> Result<i32, ServiceError> {
        let page_count = document.pages().len();
        let index = i32::try_from(page).map_err(|_| {
            ServiceError::new("invalid-page", "page index is too large")
        })?;
        if index < 0 || index >= page_count {
            return Err(ServiceError::new(
                "invalid-page",
                format!("page {page} is outside a {page_count}-page document"),
            ));
        }
        Ok(index)
    }

    fn pdfium_info(&mut self, params: Value) -> Result<Value, ServiceError> {
        let _: EmptyParams = Self::parse(params)?;
        self.pdfium()?;
        let library = self.pdfium_library.as_ref().unwrap();
        Ok(json!({
            "backend": "pdfium",
            "pdfium-api": PDFIUM_API,
            "library": library,
        }))
    }

    fn open(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: OpenParams = Self::parse(params)?;
        let path = PathBuf::from(&params.path);
        if !path.is_absolute() || !path.is_file() {
            return Err(ServiceError::new(
                "pdf-open-failed",
                format!(
                    "PDF is not an absolute readable file: {}",
                    path.display()
                ),
            ));
        }
        let pdfium = self.pdfium()?;
        let document = pdfium
            .load_pdf_from_file(&path, params.password.as_deref())
            .map_err(|error| {
                ServiceError::new(
                    pdf_open_error_code(&error),
                    format!("could not open {}: {error}", path.display()),
                )
            })?;
        let page_count = document.pages().len();
        let mut pages = Vec::with_capacity(page_count as usize);
        let mut page_geometries = Vec::with_capacity(page_count as usize);
        for index in 0..page_count {
            let page = document.pages().get(index).map_err(|error| {
                ServiceError::new(
                    "pdf-open-failed",
                    format!("could not inspect page {index}: {error}"),
                )
            })?;
            let geometry = PageGeometry::from_page(&page).ok_or_else(|| {
                ServiceError::new(
                    "pdf-open-failed",
                    format!(
                        "could not resolve visible geometry for page {index}"
                    ),
                )
            })?;
            let label = page
                .label()
                .map(str::to_owned)
                .unwrap_or_else(|| (index + 1).to_string());
            pages.push(json!({
                "page": index,
                "width": geometry.width,
                "height": geometry.height,
                "label": label,
            }));
            page_geometries.push(geometry);
        }
        let handle = self.next_document;
        self.next_document =
            self.next_document.checked_add(1).ok_or_else(|| {
                ServiceError::new(
                    "pdf-open-failed",
                    "document handle space exhausted",
                )
            })?;
        self.documents.insert(
            handle,
            OpenDocument {
                value: document,
                pages: page_geometries,
            },
        );
        Ok(json!({
            "document": handle,
            "layout": "fixed",
            "page-count": page_count,
            "pages": pages,
        }))
    }

    fn close(&mut self, params: Value) -> Result<Value, ServiceError> {
        let params: DocumentParams = Self::parse(params)?;
        if self.documents.remove(&params.document).is_none() {
            return Err(ServiceError::new(
                "unknown-document",
                format!("unknown document handle: {}", params.document),
            ));
        }
        Ok(json!({ "closed": true }))
    }

    fn outline(&self, params: Value) -> Result<Value, ServiceError> {
        let params: DocumentParams = Self::parse(params)?;
        let open_document = self.open_document(params.document)?;
        let document = &open_document.value;
        let mut stack = Vec::new();
        let mut visited = HashSet::new();
        let mut items = Vec::new();
        let mut truncated = false;
        if let Some(root) = document.bookmarks().root() {
            stack.push((root, 0));
        }
        while let Some((bookmark, depth)) = stack.pop() {
            if items.len() == OUTLINE_MAX_ITEMS {
                truncated = true;
                break;
            }
            if !visited.insert(bookmark.clone()) {
                truncated = true;
                continue;
            }
            if let Some(sibling) = bookmark.next_sibling() {
                stack.push((sibling, depth));
            }
            if let Some(child) = bookmark.first_child() {
                stack.push((child, depth + 1));
            }
            items.push(OutlineItem {
                title: outline_title(bookmark.title()),
                depth,
                destination: bookmark.destination().and_then(|destination| {
                    outline_destination(destination, &open_document.pages)
                }),
            });
        }
        serde_json::to_value(OutlineResult { items, truncated }).map_err(
            |error| {
                ServiceError::new(
                    "outline-failed",
                    format!("could not encode document outline: {error}"),
                )
            },
        )
    }

    fn page_info(&self, params: Value) -> Result<Value, ServiceError> {
        let params: PageParams = Self::parse(params)?;
        let document = self.document(params.document)?;
        let index = Self::page_index(document, params.page)?;
        let page = document.pages().get(index).map_err(|error| {
            ServiceError::new(
                "invalid-page",
                format!("could not load page {}: {error}", params.page),
            )
        })?;
        let geometry = self.page_geometry(params.document, params.page)?;
        let label = page
            .label()
            .map(str::to_owned)
            .unwrap_or_else(|| (params.page + 1).to_string());
        Ok(json!({
            "page": params.page,
            "width": geometry.width,
            "height": geometry.height,
            "label": label,
        }))
    }

    fn page_links(&self, params: Value) -> Result<Value, ServiceError> {
        let params: PageParams = Self::parse(params)?;
        let document = self.document(params.document)?;
        let index = Self::page_index(document, params.page)?;
        let page = document.pages().get(index).map_err(|error| {
            ServiceError::new(
                "invalid-page",
                format!("could not load page {}: {error}", params.page),
            )
        })?;
        let geometry = self.page_geometry(params.document, params.page)?;
        let pages = &self.open_document(params.document)?.pages;
        let text = page.text().ok();
        let mut links = Vec::new();
        let mut truncated = false;
        for (link_index, link) in page.links().iter().enumerate() {
            if link_index == PAGE_LINK_MAX_ITEMS {
                truncated = true;
                break;
            }
            let Some(raw_bounds) =
                link.rect().ok().and_then(PageLinkBounds::from_pdfium)
            else {
                continue;
            };
            let Some(bounds) = geometry.normalize_link_bounds(raw_bounds)
            else {
                continue;
            };
            let Some(action) = page_link_action(&link, pages) else {
                continue;
            };
            links.push(PageLink {
                bounds,
                action,
                label: text.as_ref().and_then(|text| {
                    page_link_label(text.inside_rect(PdfRect::new_from_values(
                        raw_bounds.bottom,
                        raw_bounds.left,
                        raw_bounds.top,
                        raw_bounds.right,
                    )))
                }),
            });
        }
        serde_json::to_value(PageLinksResult {
            page: params.page,
            links,
            truncated,
        })
        .map_err(|error| {
            ServiceError::new(
                "page-links-failed",
                format!("could not encode page links: {error}"),
            )
        })
    }

    fn page_text(&self, params: Value) -> Result<Value, ServiceError> {
        let params: PageParams = Self::parse(params)?;
        let document = self.document(params.document)?;
        let index = Self::page_index(document, params.page)?;
        let page = document.pages().get(index).map_err(|error| {
            ServiceError::new(
                "invalid-page",
                format!("could not load page {}: {error}", params.page),
            )
        })?;
        let geometry = self.page_geometry(params.document, params.page)?;
        let text = page.text().map_err(|error| {
            ServiceError::new(
                "text-unavailable",
                format!("could not load page text: {error}"),
            )
        })?;
        let chars = text.chars();
        let mut complete_text = String::new();
        let mut characters = Vec::with_capacity(chars.len());
        for character in chars.iter() {
            let value = character.unicode_string().unwrap_or_default();
            complete_text.push_str(&value);
            let raw_bounds = character
                .loose_bounds()
                .or_else(|_| character.tight_bounds())
                .ok()
                .map(TextBounds::from_pdfium);
            let quad = raw_bounds
                .and_then(|bounds| {
                    character.matrix().ok().and_then(|matrix| {
                        character_quad(bounds, matrix, || {
                            character_font_height(&character)
                        })
                    })
                })
                .map(|quad| geometry.normalize_quad(quad));
            let bounds =
                raw_bounds.and_then(|bounds| geometry.normalize_bounds(bounds));
            characters.push(json!({
                "index": character.index(),
                "text": value,
                "bounds": bounds,
                "quad": quad,
                "generated": character.is_generated().unwrap_or(false),
                "hyphen": character.is_hyphen().unwrap_or(false),
            }));
        }
        Ok(json!({
            "page": params.page,
            "text": complete_text,
            "characters": characters,
        }))
    }

    fn search(&self, params: Value) -> Result<Value, ServiceError> {
        let params: SearchParams = Self::parse(params)?;
        if params.origin.is_some() && params.cursor.is_some() {
            return Err(ServiceError::new(
                "invalid-search-cursor",
                "search origin and cursor are mutually exclusive",
            ));
        }
        if !(1..=SEARCH_MAX_MATCHES).contains(&params.match_limit) {
            return Err(ServiceError::new(
                "invalid-search-limit",
                format!(
                    concat!("search match limit must be between 1 and ", "{}"),
                    SEARCH_MAX_MATCHES
                ),
            ));
        }
        if !(1..=SEARCH_MAX_PAGES).contains(&params.page_limit) {
            return Err(ServiceError::new(
                "invalid-search-limit",
                format!(
                    "search page limit must be between 1 and {SEARCH_MAX_PAGES}"
                ),
            ));
        }
        let pattern =
            compile_search_pattern(&params.query, params.case_sensitive)?;
        let document = self.document(params.document)?;
        let page_count = document.pages().len() as u32;
        if page_count == 0 {
            return Ok(json!({
                "matches": [],
                "cursor": null,
                "done": true,
            }));
        }
        let initial = params.cursor.or(params.origin);
        let mut page_number = initial.map_or_else(
            || match params.direction {
                SearchDirection::Forward => 0,
                SearchDirection::Backward => page_count - 1,
            },
            |position| position.page,
        );
        if page_number >= page_count {
            return Err(ServiceError::new(
                "invalid-search-cursor",
                "search position is outside the document",
            ));
        }
        let mut boundary = initial.and_then(|position| position.offset);
        if params.cursor.is_none()
            && params.direction == SearchDirection::Forward
        {
            boundary = boundary.map(|offset| offset.saturating_add(1));
        }
        let mut matches = Vec::new();
        let mut pages_scanned = 0;
        let mut exhausted = false;
        while pages_scanned < params.page_limit {
            let index = Self::page_index(document, page_number)?;
            let page = document.pages().get(index).map_err(|error| {
                ServiceError::new(
                    "invalid-page",
                    format!("could not load page {page_number}: {error}"),
                )
            })?;
            let page_text = page.text().map_err(|error| {
                ServiceError::new(
                    "text-unavailable",
                    format!("could not load page {page_number} text: {error}"),
                )
            })?;
            let chars = page_text.chars();
            if boundary.is_some_and(|offset| offset > chars.len() as u32) {
                return Err(ServiceError::new(
                    "invalid-search-cursor",
                    format!(
                        concat!(
                            "search offset {} exceeds {} characters ",
                            "on page {}"
                        ),
                        boundary.expect("checked search boundary"),
                        chars.len(),
                        page_number
                    ),
                ));
            }
            let mut complete_text = String::new();
            let mut characters = Vec::with_capacity(chars.len());
            for character in chars.iter() {
                let start = complete_text.len();
                let value = character.unicode_string().unwrap_or_default();
                complete_text.push_str(&value);
                characters.push(SearchCharacter {
                    index: character.index() as u32,
                    text: value,
                    start,
                    end: complete_text.len(),
                });
            }
            let remaining = params.match_limit as usize - matches.len();
            let page_matches = search_page_text(
                page_number,
                &complete_text,
                &characters,
                &pattern,
                params.direction,
                boundary,
                remaining,
            );
            matches.extend(page_matches);
            pages_scanned += 1;
            if matches.len() >= params.match_limit as usize {
                let last = matches.last().expect("search limit is positive");
                let next = match params.direction {
                    SearchDirection::Forward => {
                        let offset = last.start.offset.saturating_add(1);
                        if offset < chars.len() as u32 {
                            Some(SearchPosition {
                                page: page_number,
                                offset: Some(offset),
                            })
                        } else if page_number + 1 < page_count {
                            Some(SearchPosition {
                                page: page_number + 1,
                                offset: None,
                            })
                        } else {
                            None
                        }
                    }
                    SearchDirection::Backward => {
                        let offset = last.start.offset;
                        if offset > 0 {
                            Some(SearchPosition {
                                page: page_number,
                                offset: Some(offset),
                            })
                        } else if page_number > 0 {
                            Some(SearchPosition {
                                page: page_number - 1,
                                offset: None,
                            })
                        } else {
                            None
                        }
                    }
                };
                return Ok(json!({
                    "matches": matches,
                    "cursor": next,
                    "done": next.is_none(),
                }));
            }
            boundary = None;
            match params.direction {
                SearchDirection::Forward if page_number + 1 < page_count => {
                    page_number += 1;
                }
                SearchDirection::Backward if page_number > 0 => {
                    page_number -= 1;
                }
                _ => {
                    exhausted = true;
                    break;
                }
            }
        }
        let cursor = (!exhausted).then_some(SearchPosition {
            page: page_number,
            offset: None,
        });
        Ok(json!({
            "matches": matches,
            "cursor": cursor,
            "done": exhausted,
        }))
    }

    fn selection_text(&self, params: Value) -> Result<Value, ServiceError> {
        let params: SelectionParams = Self::parse(params)?;
        if !(1..=SELECTION_MAX_CHARACTERS).contains(&params.character_limit) {
            return Err(ServiceError::new(
                "invalid-selection-limit",
                format!(
                    concat!(
                        "selection character limit must be between 1 and ",
                        "{}"
                    ),
                    SELECTION_MAX_CHARACTERS
                ),
            ));
        }
        if !(1..=SELECTION_MAX_PAGES).contains(&params.page_limit) {
            return Err(ServiceError::new(
                "invalid-selection-limit",
                format!(
                    concat!(
                        "selection page limit must be between 1 and ",
                        "{}"
                    ),
                    SELECTION_MAX_PAGES
                ),
            ));
        }
        let (start, end) = ordered_positions(params.start, params.end);
        let cursor = params.cursor.unwrap_or(start);
        if cursor < start || cursor > end {
            return Err(ServiceError::new(
                "invalid-selection-cursor",
                "selection cursor lies outside the selected range",
            ));
        }
        let document = self.document(params.document)?;
        let mut text = String::new();
        let mut page_number = cursor.page;
        let mut minimum_offset = cursor.offset;
        let mut pages_read = 0;
        let mut characters_read = 0;
        let mut done = false;
        while pages_read < params.page_limit
            && characters_read < params.character_limit
        {
            let index = Self::page_index(document, page_number)?;
            let page = document.pages().get(index).map_err(|error| {
                ServiceError::new(
                    "invalid-page",
                    format!("could not load page {page_number}: {error}"),
                )
            })?;
            let page_text = page.text().map_err(|error| {
                ServiceError::new(
                    "text-unavailable",
                    format!("could not load page {page_number} text: {error}"),
                )
            })?;
            let chars = page_text.chars();
            let range =
                page_selection_range(chars.len(), page_number, start, end)?;
            if let Some(range) = range {
                let first = *range.start();
                let last = *range.end();
                if page_number == cursor.page
                    && !(first..=last).contains(&(minimum_offset as usize))
                {
                    return Err(ServiceError::new(
                        "invalid-selection-cursor",
                        format!(
                            concat!(
                                "selection cursor offset {} is outside ",
                                "the selected characters on page {}"
                            ),
                            minimum_offset, page_number
                        ),
                    ));
                }
                if page_number > start.page && minimum_offset == 0 {
                    text.push('\n');
                }
                let mut char_index = minimum_offset as usize;
                while char_index <= last
                    && characters_read < params.character_limit
                {
                    let character = chars.get(char_index).map_err(|error| {
                        ServiceError::new(
                            "invalid-selection",
                            format!(
                                "could not read selected character: {error}"
                            ),
                        )
                    })?;
                    if let Some(value) = character.unicode_string() {
                        text.push_str(&value);
                    }
                    char_index += 1;
                    characters_read += 1;
                }
                if char_index <= last {
                    return Ok(json!({
                        "start": start,
                        "end": end,
                        "text": text,
                        "cursor": SelectionPosition {
                            page: page_number,
                            offset: char_index as u32,
                        },
                        "done": false,
                    }));
                }
            } else {
                if page_number == cursor.page && minimum_offset != 0 {
                    return Err(ServiceError::new(
                        "invalid-selection-cursor",
                        format!(
                            "selection cursor exceeds empty page {page_number}"
                        ),
                    ));
                }
                if page_number > start.page {
                    text.push('\n');
                }
            }
            pages_read += 1;
            if page_number == end.page {
                done = true;
                break;
            }
            page_number += 1;
            minimum_offset = 0;
        }
        Ok(json!({
            "start": start,
            "end": end,
            "text": text,
            "cursor": (!done).then_some(SelectionPosition {
                page: page_number,
                offset: minimum_offset,
            }),
            "done": done,
        }))
    }

    fn cache_prune(&self, params: Value) -> Result<Value, ServiceError> {
        let params: CachePruneParams = Self::parse(params)?;
        if params.max_bytes == 0 || params.target_bytes > params.max_bytes {
            return Err(ServiceError::new(
                "invalid-cache-limit",
                "cache target must not exceed a positive maximum",
            ));
        }
        if !self.documents.is_empty() {
            return Err(ServiceError::new(
                "cache-in-use",
                "cache pruning requires all documents to be closed",
            ));
        }
        let cache = self.cache_directory.as_ref().ok_or_else(|| {
            ServiceError::new(
                "cache-unavailable",
                "YUNGE_READER_CACHE is not set",
            )
        })?;
        if !cache.is_absolute() {
            return Err(ServiceError::new(
                "cache-unavailable",
                "YUNGE_READER_CACHE must be an absolute path",
            ));
        }
        let root_metadata = match fs::symlink_metadata(cache) {
            Ok(metadata) => metadata,
            Err(error) if error.kind() == io::ErrorKind::NotFound => {
                return Ok(json!({
                    "scanned": 0,
                    "before-bytes": 0,
                    "after-bytes": 0,
                    "removed-files": 0,
                    "removed-bytes": 0,
                    "failed-files": 0,
                    "over-budget": false,
                }));
            }
            Err(error) => {
                return Err(ServiceError::new(
                    "cache-unavailable",
                    format!("could not inspect render cache: {error}"),
                ));
            }
        };
        if root_metadata.file_type().is_symlink() || !root_metadata.is_dir() {
            return Err(ServiceError::new(
                "cache-unavailable",
                "YUNGE_READER_CACHE must be a real directory",
            ));
        }

        let directory = fs::read_dir(cache).map_err(|error| {
            ServiceError::new(
                "cache-unavailable",
                format!("could not read render cache: {error}"),
            )
        })?;
        let mut entries = Vec::new();
        let mut failed_files = 0_u64;
        let mut before_bytes = 0_u64;
        for entry in directory {
            let entry = match entry {
                Ok(entry) => entry,
                Err(_) => {
                    failed_files += 1;
                    continue;
                }
            };
            let Some(name) = entry.file_name().to_str().map(str::to_owned)
            else {
                continue;
            };
            if !valid_cache_file_name(&name) {
                continue;
            }
            let file_type = match entry.file_type() {
                Ok(file_type) => file_type,
                Err(_) => {
                    failed_files += 1;
                    continue;
                }
            };
            if !file_type.is_file() {
                continue;
            }
            let metadata = match entry.metadata() {
                Ok(metadata) => metadata,
                Err(_) => {
                    failed_files += 1;
                    continue;
                }
            };
            let size = metadata.len();
            before_bytes = before_bytes.saturating_add(size);
            entries.push(CacheEntry {
                path: entry.path(),
                size,
                modified: metadata.modified().ok(),
            });
        }
        let scanned = entries.len() as u64;
        let mut after_bytes = before_bytes;
        let mut removed_files = 0_u64;
        let mut removed_bytes = 0_u64;
        if before_bytes > params.max_bytes {
            entries.sort_by(|left, right| {
                left.modified
                    .cmp(&right.modified)
                    .then_with(|| left.path.cmp(&right.path))
            });
            for entry in entries {
                if after_bytes <= params.target_bytes {
                    break;
                }
                match fs::remove_file(entry.path) {
                    Ok(()) => {
                        after_bytes = after_bytes.saturating_sub(entry.size);
                        removed_bytes =
                            removed_bytes.saturating_add(entry.size);
                        removed_files += 1;
                    }
                    Err(_) => failed_files += 1,
                }
            }
        }
        Ok(json!({
            "scanned": scanned,
            "before-bytes": before_bytes,
            "after-bytes": after_bytes,
            "removed-files": removed_files,
            "removed-bytes": removed_bytes,
            "failed-files": failed_files,
            "over-budget": after_bytes > params.max_bytes,
        }))
    }

    fn render_page(&self, params: Value) -> Result<Value, ServiceError> {
        let params: RenderParams = Self::parse(params)?;
        if !(16..=8192).contains(&params.width) {
            return Err(ServiceError::new(
                "invalid-render-size",
                "render width must be between 16 and 8192 pixels",
            ));
        }
        if !valid_cache_key(&params.cache_key) {
            return Err(ServiceError::new(
                "invalid-cache-key",
                "cache key must be 64 lowercase hexadecimal characters",
            ));
        }
        let document = self.document(params.document)?;
        let index = Self::page_index(document, params.page)?;
        let page = document.pages().get(index).map_err(|error| {
            ServiceError::new(
                "invalid-page",
                format!("could not load page {}: {error}", params.page),
            )
        })?;
        let cache = self.cache_directory.as_ref().ok_or_else(|| {
            ServiceError::new(
                "cache-unavailable",
                "YUNGE_READER_CACHE is not set",
            )
        })?;
        if !cache.is_absolute() {
            return Err(ServiceError::new(
                "cache-unavailable",
                "YUNGE_READER_CACHE must be an absolute path",
            ));
        }
        fs::create_dir_all(cache).map_err(|error| {
            ServiceError::new(
                "cache-unavailable",
                format!("could not create render cache: {error}"),
            )
        })?;
        let output = cache.join(format!("{}.png", params.cache_key));
        if output.is_file() {
            let (width, height) =
                image::image_dimensions(&output).map_err(|error| {
                    ServiceError::new(
                        "render-failed",
                        format!("could not inspect cached page: {error}"),
                    )
                })?;
            let _ = File::options()
                .write(true)
                .open(&output)
                .and_then(|file| file.set_modified(SystemTime::now()));
            return Ok(render_result(output, width, height, true));
        }
        let config = PdfRenderConfig::new().set_target_width(params.width);
        let image = page
            .render_with_config(&config)
            .and_then(|bitmap| bitmap.as_image())
            .map_err(|error| {
                ServiceError::new(
                    "render-failed",
                    format!("could not render page {}: {error}", params.page),
                )
            })?;
        let image = apply_pdf_appearance(image, params.appearance);
        let (width, height) = image.dimensions();
        let temporary =
            output.with_extension(format!("{}.tmp", std::process::id()));
        image
            .save_with_format(&temporary, image::ImageFormat::Png)
            .map_err(|error| {
                ServiceError::new(
                    "render-failed",
                    format!("could not write rendered page: {error}"),
                )
            })?;
        if let Err(error) = fs::rename(&temporary, &output) {
            let _ = fs::remove_file(&temporary);
            return Err(ServiceError::new(
                "render-failed",
                format!("could not publish rendered page: {error}"),
            ));
        }
        Ok(render_result(output, width, height, false))
    }

    fn handle(&mut self, request: Request) -> (Response, Control) {
        if request.op == "ping" {
            let result = Self::parse::<EmptyParams>(request.params).map(|_| {
                json!({
                    "protocol": PROTOCOL_VERSION,
                    "build-id": BUILD_ID,
                    "backend": "pdfium",
                    "capabilities": CAPABILITIES,
                })
            });
            return response(request.id, result, Control::Continue);
        }
        if request.op == "shutdown" {
            let result = Self::parse::<EmptyParams>(request.params).map(|_| {
                self.documents.clear();
                json!({ "stopped": true })
            });
            let control = if result.is_ok() {
                Control::Shutdown
            } else {
                Control::Continue
            };
            return response(request.id, result, control);
        }
        let result = match request.op.as_str() {
            "pdfium-info" => self.pdfium_info(request.params),
            "open" => self.open(request.params),
            "close" => self.close(request.params),
            "outline" => self.outline(request.params),
            "page-info" => self.page_info(request.params),
            "page-links" => self.page_links(request.params),
            "page-text" => self.page_text(request.params),
            "cache-prune" => self.cache_prune(request.params),
            "search" => self.search(request.params),
            "render-page" => self.render_page(request.params),
            "selection-text" => self.selection_text(request.params),
            _ => Err(ServiceError::new(
                "unsupported-operation",
                format!("unsupported operation: {}", request.op),
            )),
        };
        response(request.id, result, Control::Continue)
    }
}

fn ordered_positions(
    start: SelectionPosition,
    end: SelectionPosition,
) -> (SelectionPosition, SelectionPosition) {
    if start <= end {
        (start, end)
    } else {
        (end, start)
    }
}

fn page_selection_range(
    length: usize,
    page: u32,
    start: SelectionPosition,
    end: SelectionPosition,
) -> Result<Option<std::ops::RangeInclusive<usize>>, ServiceError> {
    if page < start.page || page > end.page {
        return Ok(None);
    }
    if length == 0 {
        if page == start.page || page == end.page {
            return Err(ServiceError::new(
                "invalid-selection",
                format!("selection endpoint page {page} has no characters"),
            ));
        }
        return Ok(None);
    }
    let first = if page == start.page {
        start.offset as usize
    } else {
        0
    };
    let last = if page == end.page {
        end.offset as usize
    } else {
        length - 1
    };
    if first >= length || last >= length {
        return Err(ServiceError::new(
            "invalid-selection",
            format!(
                concat!(
                    "character range {} through {} exceeds ",
                    "{} characters on page {}"
                ),
                first, last, length, page
            ),
        ));
    }
    Ok(Some(first..=last))
}

fn response(
    id: u64,
    result: Result<Value, ServiceError>,
    control: Control,
) -> (Response, Control) {
    match result {
        Ok(value) => (Response::success(id, value), control),
        Err(error) => (
            Response::failure(Some(id), error.code, error.message),
            Control::Continue,
        ),
    }
}

fn render_result(
    path: impl AsRef<Path>,
    width: u32,
    height: u32,
    cached: bool,
) -> Value {
    json!({
        "path": path.as_ref(),
        "pixel-width": width,
        "pixel-height": height,
        "cached": cached,
    })
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

fn serve(input: impl BufRead, mut output: impl Write) -> Result<(), Error> {
    let mut service = Service::new();
    write_message(&mut output, &ready_message())?;
    for line in input.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let (response, control) = match serde_json::from_str(&line) {
            Ok(request) => service.handle(request),
            Err(error) => (
                Response::failure(None, "invalid-request", error.to_string()),
                Control::Continue,
            ),
        };
        write_message(&mut output, &response)?;
        if control == Control::Shutdown {
            break;
        }
    }
    Ok(())
}

fn main() {
    #[cfg(target_os = "windows")]
    if env::args().nth(1).as_deref() == Some("--webview") {
        if let Err(error) = webview::serve() {
            eprintln!("yunge-reader webview: {error}");
            std::process::exit(1);
        }
        return;
    }
    if let Err(error) = serve(io::stdin().lock(), io::stdout().lock()) {
        eprintln!("yunge-reader: {error}");
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;
    use std::time::{Duration, UNIX_EPOCH};

    struct TempDirectory(PathBuf);

    impl TempDirectory {
        fn new(name: &str) -> Self {
            let unique = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos();
            let path = env::temp_dir().join(format!(
                "yunge-reader-{name}-{}-{unique}",
                std::process::id(),
            ));
            fs::create_dir(&path).unwrap();
            Self(path)
        }
    }

    impl Drop for TempDirectory {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    fn messages(input: &str) -> Vec<Value> {
        let mut output = Vec::new();
        serve(Cursor::new(input), &mut output).unwrap();
        String::from_utf8(output)
            .unwrap()
            .lines()
            .map(|line| serde_json::from_str(line).unwrap())
            .collect()
    }

    #[test]
    fn pdf_appearances_are_strictly_validated() {
        let original = serde_json::from_value::<PdfAppearance>(json!({
            "mode": "original",
        }))
        .unwrap();
        assert_eq!(original, PdfAppearance::Original);

        let following = serde_json::from_value::<PdfAppearance>(json!({
            "mode": "follow-emacs",
            "foreground": "#112233",
            "background": "#f4f5f6",
        }))
        .unwrap();
        assert_eq!(
            following,
            PdfAppearance::FollowEmacs {
                foreground: ThemeColor([0x11, 0x22, 0x33]),
                background: ThemeColor([0xf4, 0xf5, 0xf6]),
            }
        );

        for appearance in [
            json!({ "mode": "follow-emacs" }),
            json!({
                "mode": "follow-emacs",
                "foreground": "#AABBCC",
                "background": "#ffffff",
            }),
            json!({
                "mode": "original",
                "foreground": "#112233",
            }),
        ] {
            assert!(
                serde_json::from_value::<PdfAppearance>(appearance).is_err()
            );
        }
    }

    #[test]
    fn pdf_theme_maps_tones_between_frame_colors() {
        let source = image::RgbaImage::from_vec(
            3,
            1,
            vec![0, 0, 0, 255, 255, 255, 255, 240, 255, 0, 0, 128],
        )
        .unwrap();
        let foreground = ThemeColor([10, 20, 30]);
        let background = ThemeColor([210, 220, 230]);
        let result = apply_pdf_appearance(
            DynamicImage::ImageRgba8(source),
            PdfAppearance::FollowEmacs {
                foreground,
                background,
            },
        )
        .to_rgba8();
        assert_eq!(result.get_pixel(0, 0).0, [10, 20, 30, 255]);
        assert_eq!(result.get_pixel(1, 0).0, [210, 220, 230, 240]);
        let red_luminance = 54;
        assert_eq!(
            result.get_pixel(2, 0).0,
            [
                themed_channel(10, 210, red_luminance),
                themed_channel(20, 220, red_luminance),
                themed_channel(30, 230, red_luminance),
                128,
            ]
        );
    }

    #[test]
    fn ready_reports_protocol_and_exact_source_build() {
        let value = serde_json::to_value(ready_message()).unwrap();
        assert_eq!(BUILD_ID, include_str!("../source.sha256").trim());
        assert_eq!(value["kind"], "ready");
        assert_eq!(value["protocol"], PROTOCOL_VERSION);
        assert_eq!(value["build-id"], BUILD_ID);
        assert_eq!(value["pdfium-api"], PDFIUM_API);
        assert_eq!(value["capabilities"], json!(CAPABILITIES));
    }

    #[test]
    fn ping_does_not_load_pdfium() {
        let output = messages(r#"{"id":7,"op":"ping","params":{}}"#);
        assert_eq!(output.len(), 2);
        assert_eq!(output[1]["id"], 7);
        assert_eq!(output[1]["ok"], true);
        assert_eq!(output[1]["result"]["backend"], "pdfium");
    }

    #[test]
    fn shutdown_replies_and_ignores_later_input() {
        let output = messages(concat!(
            r#"{"id":1,"op":"shutdown"}"#,
            "\n",
            r#"{"id":2,"op":"ping"}"#,
        ));
        assert_eq!(output.len(), 2);
        assert_eq!(output[1]["id"], 1);
        assert_eq!(output[1]["result"]["stopped"], true);
    }

    #[test]
    fn malformed_and_unknown_requests_return_protocol_errors() {
        let malformed = messages("not-json");
        assert_eq!(malformed[1]["id"], Value::Null);
        assert_eq!(malformed[1]["error"]["code"], "invalid-request");

        let unknown = messages(r#"{"id":3,"op":"render"}"#);
        assert_eq!(unknown[1]["id"], 3);
        assert_eq!(unknown[1]["error"]["code"], "unsupported-operation");
    }

    #[test]
    fn operation_parameters_are_strictly_validated() {
        let output = messages(concat!(
            r#"{"id":1,"op":"close","params":{}}"#,
            "\n",
            r#"{"id":2,"op":"render-page","params":{"#,
            r#""document":1,"page":0,"width":1,"#,
            r#""appearance":{"mode":"original"},"cache-key":"x"}}"#,
            "\n",
            r#"{"id":3,"op":"ping","params":{"extra":true}}"#,
            "\n",
            r#"{"id":4,"op":"search","params":{"document":1,"#,
            r#""query":"","direction":"forward"}}"#,
            "\n",
            r#"{"id":5,"op":"search","params":{"document":1,"#,
            r#""query":"x","direction":"forward","page-limit":0}}"#,
            "\n",
            r#"{"id":6,"op":"cache-prune","params":{"#,
            r#""max-bytes":0,"target-bytes":0}}"#,
            "\n",
            r#"{"id":7,"op":"cache-prune","params":{"#,
            r#""max-bytes":10,"target-bytes":5,"extra":true}}"#,
            "\n",
            r#"{"id":8,"op":"outline","params":{"document":1,"#,
            r#""extra":true}}"#,
            "\n",
            r#"{"id":9,"op":"outline","params":{"document":1}}"#,
            "\n",
            r#"{"id":10,"op":"page-links","params":{"document":1,"#,
            r#""page":0,"extra":true}}"#,
            "\n",
            r#"{"id":11,"op":"page-links","params":{"document":1,"#,
            r#""page":0}}"#,
            "\n",
            r#"{"id":12,"op":"selection-text","params":{"#,
            r#""document":1,"start":{"page":0,"offset":0},"#,
            r#""end":{"page":0,"offset":1},"character-limit":0}}"#,
            "\n",
            r#"{"id":13,"op":"selection-text","params":{"#,
            r#""document":1,"start":{"page":0,"offset":0},"#,
            r#""end":{"page":0,"offset":1},"page-limit":65}}"#,
            "\n",
            r#"{"id":14,"op":"selection-text","params":{"#,
            r#""document":1,"start":{"page":0,"offset":0},"#,
            r#""end":{"page":0,"offset":1},"extra":true}}"#,
            "\n",
            r#"{"id":15,"op":"selection-text","params":{"#,
            r#""document":1,"start":{"page":0,"offset":0},"#,
            r#""end":{"page":0,"offset":1},"#,
            r#""cursor":{"page":1,"offset":0}}}"#,
        ));
        assert_eq!(output[1]["error"]["code"], "invalid-params");
        assert_eq!(output[2]["error"]["code"], "invalid-render-size");
        assert_eq!(output[3]["error"]["code"], "invalid-params");
        assert_eq!(output[4]["error"]["code"], "invalid-search-query");
        assert_eq!(output[5]["error"]["code"], "invalid-search-limit");
        assert_eq!(output[6]["error"]["code"], "invalid-cache-limit");
        assert_eq!(output[7]["error"]["code"], "invalid-params");
        assert_eq!(output[8]["error"]["code"], "invalid-params");
        assert_eq!(output[9]["error"]["code"], "unknown-document");
        assert_eq!(output[10]["error"]["code"], "invalid-params");
        assert_eq!(output[11]["error"]["code"], "unknown-document");
        assert_eq!(output[12]["error"]["code"], "invalid-selection-limit");
        assert_eq!(output[13]["error"]["code"], "invalid-selection-limit");
        assert_eq!(output[14]["error"]["code"], "invalid-params");
        assert_eq!(output[15]["error"]["code"], "invalid-selection-cursor");
    }

    #[test]
    fn outline_titles_are_nonempty_and_bounded() {
        assert_eq!(outline_title(None), "(untitled)");
        assert_eq!(outline_title(Some("   ".to_owned())), "(untitled)");
        let value =
            format!("  {}  ", "章".repeat(OUTLINE_MAX_TITLE_CHARACTERS + 2));
        let title = outline_title(Some(value));
        assert_eq!(title.chars().count(), OUTLINE_MAX_TITLE_CHARACTERS);
        assert!(title.chars().all(|character| character == '章'));
    }

    #[test]
    fn outline_views_preserve_coordinates_and_zoom_hints() {
        let (x, y, zoom, view) = outline_destination_view(
            PdfDestinationViewSettings::SpecificCoordinatesAndZoom(
                Some(PdfPoints::new(12.0)),
                Some(PdfPoints::new(34.0)),
                Some(1.5),
            ),
        );
        assert_eq!(
            (x, y, zoom, view),
            (Some(12.0), Some(34.0), Some(1.5), "xyz")
        );
        let (x, y, zoom, view) = outline_destination_view(
            PdfDestinationViewSettings::FitPageHorizontallyToWindow(Some(
                PdfPoints::new(500.0),
            )),
        );
        assert_eq!(
            (x, y, zoom, view),
            (None, Some(500.0), None, "fit-horizontal")
        );
    }

    #[test]
    fn page_link_labels_are_compact_and_bounded() {
        assert_eq!(
            page_link_label("  Read\n  more  ".to_owned()).unwrap(),
            "Read more"
        );
        let value = "链".repeat(PAGE_LINK_MAX_LABEL_CHARACTERS + 2);
        let label = page_link_label(value).unwrap();
        assert_eq!(label.chars().count(), PAGE_LINK_MAX_LABEL_CHARACTERS);
        assert_eq!(page_link_label(" \n\t ".to_owned()), None);
    }

    #[test]
    fn page_link_uris_require_bounded_explicit_schemes() {
        let https = "HTTPS://example.com/path".to_owned();
        assert_eq!(page_link_uri(https.clone()), Some(https));
        assert_eq!(
            page_link_uri("mailto:user@example.com".to_owned()),
            Some("mailto:user@example.com".to_owned())
        );
        assert_eq!(page_link_uri("relative/path".to_owned()), None);
        assert_eq!(page_link_uri("https://example.com/a b".to_owned()), None);
        assert_eq!(page_link_uri("1https://example.com".to_owned()), None);
        assert_eq!(page_link_uri("javascript:\nalert(1)".to_owned()), None);
        assert_eq!(
            page_link_uri("x".repeat(PAGE_LINK_MAX_URI_BYTES + 1)),
            None
        );
    }

    #[test]
    fn pdf_password_errors_have_a_stable_protocol_code() {
        let password = PdfiumError::PdfiumLibraryInternalError(
            PdfiumInternalError::PasswordError,
        );
        let format = PdfiumError::PdfiumLibraryInternalError(
            PdfiumInternalError::FormatError,
        );
        assert_eq!(pdf_open_error_code(&password), "pdf-password-error");
        assert_eq!(pdf_open_error_code(&format), "pdf-open-failed");
    }

    #[test]
    fn page_link_bounds_reject_empty_or_nonfinite_rectangles() {
        let valid = PdfRect::new_from_values(2.0, 1.0, 4.0, 3.0);
        assert!(PageLinkBounds::from_pdfium(valid).is_some());
        let empty = PdfRect::new_from_values(2.0, 1.0, 2.0, 3.0);
        assert!(PageLinkBounds::from_pdfium(empty).is_none());
        let nonfinite = PdfRect::new_from_values(2.0, 1.0, f32::INFINITY, 3.0);
        assert!(PageLinkBounds::from_pdfium(nonfinite).is_none());
    }

    #[test]
    fn page_geometry_removes_the_visible_box_origin() {
        let geometry = PageGeometry {
            left: 63.0,
            bottom: 72.0,
            right: 532.0,
            top: 738.0,
            width: 469.0,
            height: 666.0,
            rotation: PdfPageRenderRotation::None,
        };
        assert!(geometry.valid());
        let point = geometry.normalize_point(TextPoint {
            x: 214.47876,
            y: 509.57025,
        });
        assert_close(point.x, 151.47876);
        assert_close(point.y, 437.57025);
        let (x, y) =
            geometry.normalize_optional_point(Some(214.47876), Some(509.57025));
        assert_close(x.unwrap(), point.x);
        assert_close(y.unwrap(), point.y);
    }

    #[test]
    fn page_geometry_intersects_crop_and_media_boxes() {
        let crop = TextBounds {
            left: -20.0,
            bottom: 50.0,
            right: 550.0,
            top: 750.0,
        };
        let media = TextBounds {
            left: 0.0,
            bottom: 0.0,
            right: 600.0,
            top: 800.0,
        };
        let geometry = PageGeometry::from_boxes(
            Some(crop),
            Some(media),
            550.0,
            700.0,
            PdfPageRenderRotation::None,
        )
        .unwrap();
        assert_close(geometry.left, 0.0);
        assert_close(geometry.bottom, 50.0);
        assert_close(geometry.right, 550.0);
        assert_close(geometry.top, 750.0);
    }

    #[test]
    fn page_geometry_applies_intrinsic_page_rotation() {
        let geometry = PageGeometry {
            left: 63.0,
            bottom: 72.0,
            right: 532.0,
            top: 738.0,
            width: 666.0,
            height: 469.0,
            rotation: PdfPageRenderRotation::Degrees90,
        };
        assert!(geometry.valid());
        let lower_right =
            geometry.normalize_point(TextPoint { x: 532.0, y: 72.0 });
        assert_close(lower_right.x, 0.0);
        assert_close(lower_right.y, 0.0);
        let upper_left =
            geometry.normalize_point(TextPoint { x: 63.0, y: 738.0 });
        assert_close(upper_left.x, 666.0);
        assert_close(upper_left.y, 469.0);
        let (x, y) = geometry.normalize_optional_point(None, Some(400.0));
        assert_close(x.unwrap(), 328.0);
        assert!(y.is_none());
    }

    #[test]
    fn cache_prune_removes_oldest_recognized_files_only() {
        let directory = TempDirectory::new("cache-prune");
        let names = [
            format!("{}.png", "a".repeat(64)),
            format!("{}.png", "b".repeat(64)),
            format!("{}.png", "c".repeat(64)),
        ];
        for (index, name) in names.iter().enumerate() {
            let path = directory.0.join(name);
            fs::write(&path, b"123456").unwrap();
            File::options()
                .write(true)
                .open(path)
                .unwrap()
                .set_modified(
                    UNIX_EPOCH
                        + Duration::from_secs(1_700_000_000 + index as u64),
                )
                .unwrap();
        }
        let unknown = directory.0.join("keep-me.png");
        let temporary =
            directory.0.join(format!("{}.123.tmp", "d".repeat(64),));
        let directory_entry =
            directory.0.join(format!("{}.png", "e".repeat(64),));
        fs::write(&unknown, b"unknown").unwrap();
        fs::write(&temporary, b"temporary").unwrap();
        fs::create_dir(&directory_entry).unwrap();

        let service = Service {
            pdfium: None,
            pdfium_library: None,
            cache_directory: Some(directory.0.clone()),
            documents: HashMap::new(),
            next_document: 1,
        };
        let result = service
            .cache_prune(json!({
                "max-bytes": 15,
                "target-bytes": 10,
            }))
            .unwrap();

        assert_eq!(result["scanned"], 3);
        assert_eq!(result["before-bytes"], 18);
        assert_eq!(result["after-bytes"], 6);
        assert_eq!(result["removed-files"], 2);
        assert_eq!(result["removed-bytes"], 12);
        assert_eq!(result["failed-files"], 0);
        assert_eq!(result["over-budget"], false);
        assert!(!directory.0.join(&names[0]).exists());
        assert!(!directory.0.join(&names[1]).exists());
        assert!(directory.0.join(&names[2]).exists());
        assert!(unknown.exists());
        assert!(temporary.exists());
        assert!(directory_entry.is_dir());

        let below_maximum = service
            .cache_prune(json!({
                "max-bytes": 7,
                "target-bytes": 0,
            }))
            .unwrap();
        assert_eq!(below_maximum["before-bytes"], 6);
        assert_eq!(below_maximum["after-bytes"], 6);
        assert_eq!(below_maximum["removed-files"], 0);
        assert!(directory.0.join(&names[2]).exists());
    }

    #[test]
    fn selection_ranges_are_inclusive_and_direction_independent() {
        let forward_start = SelectionPosition { page: 0, offset: 1 };
        let forward_end = SelectionPosition { page: 0, offset: 3 };
        let (start, end) = ordered_positions(forward_start, forward_end);
        assert_eq!(
            page_selection_range(5, 0, start, end).unwrap(),
            Some(1..=3)
        );
        let (start, end) = ordered_positions(forward_end, forward_start);
        assert_eq!(
            page_selection_range(5, 0, start, end).unwrap(),
            Some(1..=3)
        );
        let outside = SelectionPosition { page: 0, offset: 5 };
        assert_eq!(
            page_selection_range(5, 0, forward_start, outside)
                .unwrap_err()
                .code,
            "invalid-selection"
        );
    }

    #[test]
    fn selection_batches_have_bounded_defaults() {
        let params: SelectionParams = Service::parse(json!({
            "document": 1,
            "start": {"page": 0, "offset": 2},
            "end": {"page": 1, "offset": 3},
        }))
        .unwrap();
        assert_eq!(params.cursor, None);
        assert_eq!(params.character_limit, 16_384);
        assert_eq!(params.page_limit, 8);
        assert!(params.character_limit <= SELECTION_MAX_CHARACTERS);
        assert!(params.page_limit <= SELECTION_MAX_PAGES);
    }

    #[test]
    fn page_selection_ranges_cover_cross_page_interiors() {
        let start = SelectionPosition { page: 2, offset: 3 };
        let end = SelectionPosition { page: 4, offset: 5 };
        assert_eq!(
            page_selection_range(10, 2, start, end).unwrap(),
            Some(3..=9)
        );
        assert_eq!(
            page_selection_range(8, 3, start, end).unwrap(),
            Some(0..=7)
        );
        assert_eq!(
            page_selection_range(10, 4, start, end).unwrap(),
            Some(0..=5)
        );
        assert_eq!(page_selection_range(8, 1, start, end).unwrap(), None);
        assert_eq!(page_selection_range(0, 3, start, end).unwrap(), None);
    }

    #[test]
    fn document_positions_are_ordered_by_page_then_offset() {
        let later = SelectionPosition { page: 4, offset: 1 };
        let earlier = SelectionPosition {
            page: 2,
            offset: 99,
        };
        assert_eq!(ordered_positions(later, earlier), (earlier, later));
    }

    fn searchable_characters(
        values: &[(u32, &str)],
    ) -> (String, Vec<SearchCharacter>) {
        let mut text = String::new();
        let mut characters = Vec::new();
        for (index, value) in values {
            let start = text.len();
            text.push_str(value);
            characters.push(SearchCharacter {
                index: *index,
                text: (*value).to_owned(),
                start,
                end: text.len(),
            });
        }
        (text, characters)
    }

    #[test]
    fn literal_search_maps_unicode_bytes_to_pdfium_indices() {
        let (text, characters) = searchable_characters(&[
            (7, "A"),
            (8, "你"),
            (9, "好"),
            (10, "ffi"),
            (11, "."),
        ]);
        let pattern = compile_search_pattern("好ff", true).unwrap();
        let matches = search_page_text(
            3,
            &text,
            &characters,
            &pattern,
            SearchDirection::Forward,
            Some(0),
            10,
        );
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].start, SelectionPosition { page: 3, offset: 9 });
        assert_eq!(
            matches[0].end,
            SelectionPosition {
                page: 3,
                offset: 10
            }
        );
        assert_eq!(matches[0].text, "好ff");
        assert_eq!(matches[0].before, "A你");
        assert_eq!(matches[0].after, ".");
        let ligature_pattern = compile_search_pattern("f", true).unwrap();
        let ligature_matches = search_page_text(
            3,
            &text,
            &characters,
            &ligature_pattern,
            SearchDirection::Forward,
            Some(0),
            10,
        );
        assert_eq!(ligature_matches.len(), 1);
        assert_eq!(
            ligature_matches[0].start,
            SelectionPosition {
                page: 3,
                offset: 10
            }
        );
        assert_eq!(ligature_matches[0].end, ligature_matches[0].start);
    }

    #[test]
    fn literal_search_supports_case_folding_and_escaped_syntax() {
        let (text, characters) = searchable_characters(&[
            (0, "I"),
            (1, "n"),
            (2, "t"),
            (3, "r"),
            (4, "o"),
            (5, "."),
            (6, "*"),
        ]);
        let folded = compile_search_pattern("intro", false).unwrap();
        let folded_matches = search_page_text(
            0,
            &text,
            &characters,
            &folded,
            SearchDirection::Forward,
            Some(0),
            10,
        );
        assert_eq!(folded_matches.len(), 1);
        let literal = compile_search_pattern(".*", true).unwrap();
        let literal_matches = search_page_text(
            0,
            &text,
            &characters,
            &literal,
            SearchDirection::Forward,
            Some(0),
            10,
        );
        assert_eq!(literal_matches.len(), 1);
        assert_eq!(literal_matches[0].start.offset, 5);
        assert_eq!(literal_matches[0].end.offset, 6);
    }

    #[test]
    fn literal_search_respects_cursor_and_match_limits() {
        let (text, characters) = searchable_characters(&[
            (0, "a"),
            (1, " "),
            (2, "a"),
            (3, " "),
            (4, "a"),
        ]);
        let pattern = compile_search_pattern("a", true).unwrap();
        let matches = search_page_text(
            0,
            &text,
            &characters,
            &pattern,
            SearchDirection::Forward,
            Some(2),
            1,
        );
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].start.offset, 2);
        let previous = search_page_text(
            0,
            &text,
            &characters,
            &pattern,
            SearchDirection::Backward,
            Some(4),
            2,
        );
        assert_eq!(previous.len(), 2);
        assert_eq!(previous[0].start.offset, 2);
        assert_eq!(previous[1].start.offset, 0);
    }

    #[test]
    fn literal_search_rejects_empty_and_oversized_queries() {
        assert_eq!(
            compile_search_pattern("", true).unwrap_err().code,
            "invalid-search-query"
        );
        let oversized = "x".repeat(SEARCH_MAX_QUERY_CHARACTERS + 1);
        assert_eq!(
            compile_search_pattern(&oversized, true).unwrap_err().code,
            "invalid-search-query"
        );
    }

    fn assert_close(actual: f32, expected: f32) {
        assert!(
            (actual - expected).abs() < 0.001,
            "expected {expected}, got {actual}"
        );
    }

    fn quad_extents(points: [TextPoint; 4]) -> TextBounds {
        TextBounds {
            left: points
                .iter()
                .map(|point| point.x)
                .fold(f32::INFINITY, f32::min),
            bottom: points
                .iter()
                .map(|point| point.y)
                .fold(f32::INFINITY, f32::min),
            right: points
                .iter()
                .map(|point| point.x)
                .fold(f32::NEG_INFINITY, f32::max),
            top: points
                .iter()
                .map(|point| point.y)
                .fold(f32::NEG_INFINITY, f32::max),
        }
    }

    #[test]
    fn character_quads_follow_rotated_matrix_axes() {
        let cosine = 30.0_f32.to_radians().cos();
        let sine = 30.0_f32.to_radians().sin();
        let bounds = TextBounds {
            left: 10.0,
            bottom: 20.0,
            right: 10.0 + 20.0 * cosine + 10.0 * sine,
            top: 20.0 + 20.0 * sine + 10.0 * cosine,
        };
        let matrix = PdfMatrix::new(cosine, sine, -sine, cosine, 0.0, 0.0);
        let mut used_font_metrics = false;
        let quad = character_quad(bounds, matrix, || {
            used_font_metrics = true;
            10.0
        })
        .unwrap();
        assert!(!used_font_metrics);
        assert_close(quad[0].x, 15.0);
        assert_close(quad[0].y, 20.0);
        assert_close(quad[1].x, 15.0 + 20.0 * cosine);
        assert_close(quad[1].y, 30.0);
        let extents = quad_extents(quad);
        assert_close(extents.left, bounds.left);
        assert_close(extents.bottom, bounds.bottom);
        assert_close(extents.right, bounds.right);
        assert_close(extents.top, bounds.top);
    }

    #[test]
    fn character_quads_use_font_height_at_singular_angles() {
        let cosine = 45.0_f32.to_radians().cos();
        let sine = 45.0_f32.to_radians().sin();
        let extent = 30.0 * cosine;
        let bounds = TextBounds {
            left: 5.0,
            bottom: 7.0,
            right: 5.0 + extent,
            top: 7.0 + extent,
        };
        let matrix = PdfMatrix::new(cosine, sine, -sine, cosine, 0.0, 0.0);
        let mut used_font_metrics = false;
        let quad = character_quad(bounds, matrix, || {
            used_font_metrics = true;
            10.0
        })
        .unwrap();
        assert!(used_font_metrics);
        let extents = quad_extents(quad);
        assert_close(extents.left, bounds.left);
        assert_close(extents.bottom, bounds.bottom);
        assert_close(extents.right, bounds.right);
        assert_close(extents.top, bounds.top);
    }

    #[test]
    fn end_of_input_exits_cleanly() {
        let output = messages("");
        assert_eq!(output.len(), 1);
        assert_eq!(output[0]["kind"], "ready");
    }
}
