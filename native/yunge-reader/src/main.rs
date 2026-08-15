// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use image::GenericImageView;
use pdfium_render::prelude::*;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};

type Error = Box<dyn std::error::Error>;

const PROTOCOL_VERSION: u32 = 1;
const PDFIUM_API: &str = "7881";
const BUILD_ID: &str = env!("YUNGE_READER_BUILD_ID");
const CAPABILITIES: [&str; 3] = ["lifecycle", "pdf-render", "pdf-text"];

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
    capabilities: [&'static str; 3],
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
    documents: HashMap<u64, PdfDocument<'static>>,
    next_document: u64,
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
    #[serde(rename = "cache-key")]
    cache_key: String,
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
        self.documents.get(&document).ok_or_else(|| {
            ServiceError::new(
                "unknown-document",
                format!("unknown document handle: {document}"),
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
                    "pdf-open-failed",
                    format!("could not open {}: {error}", path.display()),
                )
            })?;
        let page_count = document.pages().len();
        let mut pages = Vec::with_capacity(page_count as usize);
        for index in 0..page_count {
            let page = document.pages().get(index).map_err(|error| {
                ServiceError::new(
                    "pdf-open-failed",
                    format!("could not inspect page {index}: {error}"),
                )
            })?;
            let label = page
                .label()
                .map(str::to_owned)
                .unwrap_or_else(|| (index + 1).to_string());
            pages.push(json!({
                "page": index,
                "width": page.width().value,
                "height": page.height().value,
                "label": label,
            }));
        }
        let handle = self.next_document;
        self.next_document =
            self.next_document.checked_add(1).ok_or_else(|| {
                ServiceError::new(
                    "pdf-open-failed",
                    "document handle space exhausted",
                )
            })?;
        self.documents.insert(handle, document);
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
        let label = page
            .label()
            .map(str::to_owned)
            .unwrap_or_else(|| (params.page + 1).to_string());
        Ok(json!({
            "page": params.page,
            "width": page.width().value,
            "height": page.height().value,
            "label": label,
        }))
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
            let bounds = character
                .loose_bounds()
                .or_else(|_| character.tight_bounds())
                .ok()
                .map(TextBounds::from_pdfium);
            let quad = bounds.and_then(|bounds| {
                character.matrix().ok().and_then(|matrix| {
                    character_quad(bounds, matrix, || {
                        character_font_height(&character)
                    })
                })
            });
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

    fn selection_text(&self, params: Value) -> Result<Value, ServiceError> {
        let params: SelectionParams = Self::parse(params)?;
        let document = self.document(params.document)?;
        let (start, end) = ordered_positions(params.start, params.end);
        let mut text = String::new();
        for page_number in start.page..=end.page {
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
            if page_number > start.page {
                text.push('\n');
            }
            if let Some(range) =
                page_selection_range(chars.len(), page_number, start, end)?
            {
                for char_index in range {
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
                }
            }
        }
        Ok(json!({
            "start": start,
            "end": end,
            "text": text,
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
        if params.cache_key.len() != 64
            || !params.cache_key.bytes().all(|byte| {
                byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte)
            })
        {
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
            "page-info" => self.page_info(request.params),
            "page-text" => self.page_text(request.params),
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
    if let Err(error) = serve(io::stdin().lock(), io::stdout().lock()) {
        eprintln!("yunge-reader: {error}");
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

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
            r#""document":1,"page":0,"width":1,"cache-key":"x"}}"#,
            "\n",
            r#"{"id":3,"op":"ping","params":{"extra":true}}"#,
        ));
        assert_eq!(output[1]["error"]["code"], "invalid-params");
        assert_eq!(output[2]["error"]["code"], "invalid-render-size");
        assert_eq!(output[3]["error"]["code"], "invalid-params");
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
