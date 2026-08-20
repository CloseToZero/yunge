// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

//! Out-of-process EPUB publication and renderer resource broker.

use crate::epub::{EpubError, Publication, PublicationMetadata};
use getrandom::getrandom;
use percent_encoding::percent_decode_str;
use serde::Serialize;
use serde_json::json;
use std::collections::HashMap;
use std::fmt;
use std::io::{Read, Write};
use std::net::{SocketAddr, TcpListener, TcpStream};
use std::path::Path;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::Duration;

const MAX_ACTIVE_CONNECTIONS: usize = 64;
const MAX_HEADER_BYTES: usize = 16 * 1_024;
const MAX_RESOURCE_CATALOG_BYTES: usize = 16 * 1_024 * 1_024;
const MAX_RESOURCE_URI_PATH_BYTES: usize = 196_605;
const RESOURCE_CATALOG_PATH: &str = ".yunge/resources.json";

const APP_CSP: &str = concat!(
    "default-src 'none'; ",
    "script-src 'self'; ",
    "style-src 'self' blob: 'unsafe-inline'; ",
    "img-src blob: data:; ",
    "font-src blob: data:; ",
    "media-src blob: data:; ",
    "connect-src 'self'; ",
    "frame-src blob:; ",
    "object-src 'none'; ",
    "worker-src 'none'; ",
    "base-uri 'none'; ",
    "form-action 'none'"
);

const RESOURCE_CSP: &str = concat!(
    "default-src 'none'; ",
    "img-src 'self' data:; ",
    "style-src 'self' 'unsafe-inline'; ",
    "font-src 'self' data:; ",
    "media-src 'self'; ",
    "script-src 'none'; ",
    "object-src 'none'; ",
    "frame-src 'none'; ",
    "connect-src 'none'; ",
    "base-uri 'none'; ",
    "form-action 'none'"
);

#[derive(Debug)]
pub struct BrokerError {
    code: &'static str,
    message: String,
}

impl BrokerError {
    fn new(code: &'static str, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
        }
    }

    pub fn code(&self) -> &'static str {
        self.code
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for BrokerError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}: {}", self.code, self.message)
    }
}

impl std::error::Error for BrokerError {}

impl From<EpubError> for BrokerError {
    fn from(error: EpubError) -> Self {
        Self::new(error.code(), error.message())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct PublicationDescriptor {
    pub publication: u64,
    pub metadata: PublicationMetadata,
    pub entry_count: usize,
    pub expanded_size: u64,
    pub renderer_url: String,
    pub resource_root: String,
}

struct StoredPublication {
    token: String,
    publication: Publication,
}

struct BrokerState {
    service_token: String,
    publications: HashMap<u64, Arc<Mutex<StoredPublication>>>,
    tokens: HashMap<String, u64>,
}

struct HttpServer {
    address: SocketAddr,
    stopping: Arc<AtomicBool>,
    thread: Option<JoinHandle<()>>,
}

pub struct EpubBroker {
    state: Arc<Mutex<BrokerState>>,
    _server: HttpServer,
    base_url: String,
    next_publication: u64,
}

struct HttpRequest {
    method: HttpMethod,
    target: String,
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum HttpMethod {
    Get,
    Head,
}

struct HttpResponse {
    status: u16,
    media_type: String,
    content_length: usize,
    content_security_policy: &'static str,
    body: Vec<u8>,
}

struct ConnectionGuard(Arc<AtomicUsize>);

impl Drop for ConnectionGuard {
    fn drop(&mut self) {
        self.0.fetch_sub(1, Ordering::AcqRel);
    }
}

impl EpubBroker {
    pub fn start() -> Result<Self, BrokerError> {
        let service_token = random_token()?;
        let state = Arc::new(Mutex::new(BrokerState {
            service_token: service_token.clone(),
            publications: HashMap::new(),
            tokens: HashMap::new(),
        }));
        let server = HttpServer::start(Arc::clone(&state))?;
        let base_url = format!("http://{}/{service_token}/", server.address);
        Ok(Self {
            state,
            _server: server,
            base_url,
            next_publication: 1,
        })
    }

    pub fn open(
        &mut self,
        path: impl AsRef<Path>,
    ) -> Result<PublicationDescriptor, BrokerError> {
        let path = path.as_ref();
        if !path.is_absolute() {
            return Err(BrokerError::new(
                "invalid-publication-path",
                "publication path must be absolute",
            ));
        }
        let publication = Publication::open(path)?;
        let id = self.next_publication;
        self.next_publication = id.checked_add(1).ok_or_else(|| {
            BrokerError::new(
                "publication-id-exhausted",
                "no publication IDs remain",
            )
        })?;
        let token = random_token()?;
        let descriptor = self.descriptor(id, &token, &publication);
        let stored = Arc::new(Mutex::new(StoredPublication {
            token: token.clone(),
            publication,
        }));
        let mut state = self.state.lock().map_err(|_| {
            BrokerError::new(
                "publication-store-failed",
                "publication store failed",
            )
        })?;
        state.tokens.insert(token, id);
        state.publications.insert(id, stored);
        Ok(descriptor)
    }

    pub fn info(&self, id: u64) -> Result<PublicationDescriptor, BrokerError> {
        let publication = self.publication(id)?;
        let publication = publication.lock().map_err(|_| {
            BrokerError::new(
                "publication-store-failed",
                "publication store failed",
            )
        })?;
        Ok(self.descriptor(id, &publication.token, &publication.publication))
    }

    pub fn close(&mut self, id: u64) -> Result<(), BrokerError> {
        let mut state = self.state.lock().map_err(|_| {
            BrokerError::new(
                "publication-store-failed",
                "publication store failed",
            )
        })?;
        let publication = state.publications.remove(&id).ok_or_else(|| {
            BrokerError::new(
                "unknown-publication",
                format!("publication {id} does not exist"),
            )
        })?;
        let token = publication
            .lock()
            .map_err(|_| {
                BrokerError::new(
                    "publication-store-failed",
                    "publication store failed",
                )
            })?
            .token
            .clone();
        state.tokens.remove(&token);
        Ok(())
    }

    pub fn clear(&mut self) -> Result<(), BrokerError> {
        let mut state = self.state.lock().map_err(|_| {
            BrokerError::new(
                "publication-store-failed",
                "publication store failed",
            )
        })?;
        state.publications.clear();
        state.tokens.clear();
        Ok(())
    }

    fn publication(
        &self,
        id: u64,
    ) -> Result<Arc<Mutex<StoredPublication>>, BrokerError> {
        self.state
            .lock()
            .map_err(|_| {
                BrokerError::new(
                    "publication-store-failed",
                    "publication store failed",
                )
            })?
            .publications
            .get(&id)
            .cloned()
            .ok_or_else(|| {
                BrokerError::new(
                    "unknown-publication",
                    format!("publication {id} does not exist"),
                )
            })
    }

    fn descriptor(
        &self,
        id: u64,
        publication_token: &str,
        publication: &Publication,
    ) -> PublicationDescriptor {
        let base = &self.base_url;
        PublicationDescriptor {
            publication: id,
            metadata: publication.metadata().clone(),
            entry_count: publication.entry_count(),
            expanded_size: publication.expanded_size(),
            renderer_url: format!("{base}app/index.html"),
            resource_root: format!("{base}book/{publication_token}/"),
        }
    }

    #[cfg(test)]
    fn base_url(&self) -> &str {
        &self.base_url
    }
}

impl HttpServer {
    fn start(state: Arc<Mutex<BrokerState>>) -> Result<Self, BrokerError> {
        let listener =
            TcpListener::bind(("127.0.0.1", 0)).map_err(|error| {
                BrokerError::new(
                    "epub-broker-unavailable",
                    format!("could not bind EPUB broker: {error}"),
                )
            })?;
        let address = listener.local_addr().map_err(|error| {
            BrokerError::new(
                "epub-broker-unavailable",
                format!("could not read EPUB broker address: {error}"),
            )
        })?;
        let stopping = Arc::new(AtomicBool::new(false));
        let server_stopping = Arc::clone(&stopping);
        let thread = thread::Builder::new()
            .name("yunge-reader-epub-broker".into())
            .spawn(move || serve_http(listener, state, server_stopping))
            .map_err(|error| {
                BrokerError::new(
                    "epub-broker-unavailable",
                    format!("could not start EPUB broker: {error}"),
                )
            })?;
        Ok(Self {
            address,
            stopping,
            thread: Some(thread),
        })
    }
}

impl Drop for HttpServer {
    fn drop(&mut self) {
        self.stopping.store(true, Ordering::Release);
        let _ = TcpStream::connect_timeout(
            &self.address,
            Duration::from_millis(50),
        );
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
    }
}

fn serve_http(
    listener: TcpListener,
    state: Arc<Mutex<BrokerState>>,
    stopping: Arc<AtomicBool>,
) {
    let active = Arc::new(AtomicUsize::new(0));
    for connection in listener.incoming() {
        if stopping.load(Ordering::Acquire) {
            break;
        }
        let Ok(mut stream) = connection else {
            continue;
        };
        if active.fetch_add(1, Ordering::AcqRel) >= MAX_ACTIVE_CONNECTIONS {
            active.fetch_sub(1, Ordering::AcqRel);
            let _ = write_http_response(
                &mut stream,
                error_response(503, "EPUB broker is busy"),
            );
            continue;
        }
        let state = Arc::clone(&state);
        let guard = ConnectionGuard(Arc::clone(&active));
        let _ = thread::Builder::new()
            .name("yunge-reader-epub-resource".into())
            .spawn(move || {
                let _guard = guard;
                let _ = stream.set_read_timeout(Some(Duration::from_secs(5)));
                let _ = stream.set_write_timeout(Some(Duration::from_secs(10)));
                let response = match read_http_request(&mut stream) {
                    Ok(request) => route_http(&state, request),
                    Err(message) => error_response(400, message),
                };
                let _ = write_http_response(&mut stream, response);
            });
    }
}

fn read_http_request(
    stream: &mut TcpStream,
) -> Result<HttpRequest, &'static str> {
    let mut header = Vec::with_capacity(1024);
    let mut chunk = [0_u8; 1024];
    while header.len() < MAX_HEADER_BYTES {
        let read = stream
            .read(&mut chunk)
            .map_err(|_| "could not read HTTP request")?;
        if read == 0 {
            return Err("HTTP request ended before its headers");
        }
        header.extend_from_slice(&chunk[..read]);
        if header.windows(4).any(|window| window == b"\r\n\r\n") {
            break;
        }
    }
    if !header.windows(4).any(|window| window == b"\r\n\r\n") {
        return Err("HTTP request headers are too large");
    }
    let header = std::str::from_utf8(&header)
        .map_err(|_| "HTTP request headers are not UTF-8")?;
    let request_line = header
        .split("\r\n")
        .next()
        .ok_or("HTTP request line is missing")?;
    let mut fields = request_line.split(' ');
    let method = match fields.next() {
        Some("GET") => HttpMethod::Get,
        Some("HEAD") => HttpMethod::Head,
        _ => return Err("HTTP method is not supported"),
    };
    let target = fields.next().ok_or("HTTP request target is missing")?;
    let version = fields.next().ok_or("HTTP version is missing")?;
    if fields.next().is_some() || !matches!(version, "HTTP/1.0" | "HTTP/1.1") {
        return Err("HTTP request line is invalid");
    }
    if !target.starts_with('/') || target.contains(['\r', '\n', '\0']) {
        return Err("HTTP request target is invalid");
    }
    Ok(HttpRequest {
        method,
        target: target.to_owned(),
    })
}

fn route_http(
    state: &Arc<Mutex<BrokerState>>,
    request: HttpRequest,
) -> HttpResponse {
    let target = match request.target.split_once('?') {
        Some(_) => {
            return error_response(400, "resource queries are not supported");
        }
        None => request.target.as_str(),
    };
    let service_token = match state.lock() {
        Ok(state) => state.service_token.clone(),
        Err(_) => return error_response(500, "publication store failed"),
    };
    let Some(path) = target.strip_prefix(&format!("/{service_token}/")) else {
        return error_response(404, "resource not found");
    };
    if let Some(path) = path.strip_prefix("app/") {
        return app_response(request.method, path);
    }
    let Some(path) = path.strip_prefix("book/") else {
        return error_response(404, "resource not found");
    };
    let Some((token, path)) = path.split_once('/') else {
        return error_response(400, "invalid publication resource target");
    };
    if !valid_token(token) {
        return error_response(400, "invalid publication token");
    }
    let path = match decode_resource_path(path) {
        Ok(path) => path,
        Err(message) => return error_response(400, message),
    };
    let publication = match state.lock() {
        Ok(state) => {
            let Some(id) = state.tokens.get(token) else {
                return error_response(404, "publication not found");
            };
            state.publications.get(id).cloned()
        }
        Err(_) => return error_response(500, "publication store failed"),
    };
    let Some(publication) = publication else {
        return error_response(404, "publication not found");
    };
    let mut publication = match publication.lock() {
        Ok(publication) => publication,
        Err(_) => return error_response(500, "publication failed"),
    };
    if path == RESOURCE_CATALOG_PATH {
        let body = match serde_json::to_vec(&json!({
            "resources": publication.publication.resource_catalog(),
        })) {
            Ok(body) if body.len() <= MAX_RESOURCE_CATALOG_BYTES => body,
            Ok(_) => {
                return error_response(413, "resource catalog is too large");
            }
            Err(_) => {
                return error_response(
                    500,
                    "could not encode resource catalog",
                );
            }
        };
        return body_response(
            request.method,
            "application/json; charset=utf-8",
            body,
            RESOURCE_CSP,
        );
    }
    match publication.publication.read_resource(&path) {
        Ok(resource) => body_response(
            request.method,
            resource.media_type().to_owned(),
            resource.into_bytes(),
            RESOURCE_CSP,
        ),
        Err(error) => epub_error_response(error),
    }
}

fn app_response(method: HttpMethod, path: &str) -> HttpResponse {
    let Some((media_type, asset)) = app_asset(path) else {
        return error_response(404, "renderer asset not found");
    };
    body_response(method, media_type, asset.to_vec(), APP_CSP)
}

pub(crate) fn app_asset(path: &str) -> Option<(&'static str, &'static [u8])> {
    match path {
        "index.html" => Some((
            "text/html; charset=utf-8",
            include_bytes!("../renderer/index.html"),
        )),
        "style.css" => Some((
            "text/css; charset=utf-8",
            include_bytes!("../renderer/style.css"),
        )),
        "yunge-reader.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/yunge-reader.js"),
        )),
        "yunge-reader-core.mjs" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/yunge-reader-core.mjs"),
        )),
        "foliate-js/epub.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/epub.js"),
        )),
        "foliate-js/epubcfi.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/epubcfi.js"),
        )),
        "foliate-js/fixed-layout.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/fixed-layout.js"),
        )),
        "foliate-js/overlayer.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/overlayer.js"),
        )),
        "foliate-js/paginator.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/paginator.js"),
        )),
        "foliate-js/progress.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/progress.js"),
        )),
        "foliate-js/search.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/search.js"),
        )),
        "foliate-js/text-walker.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/text-walker.js"),
        )),
        "foliate-js/view.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../renderer/foliate-js/view.js"),
        )),
        _ => None,
    }
}

fn body_response(
    method: HttpMethod,
    media_type: impl Into<String>,
    body: Vec<u8>,
    content_security_policy: &'static str,
) -> HttpResponse {
    let content_length = body.len();
    HttpResponse {
        status: 200,
        media_type: media_type.into(),
        content_length,
        content_security_policy,
        body: if method == HttpMethod::Head {
            Vec::new()
        } else {
            body
        },
    }
}

fn error_response(status: u16, message: impl AsRef<str>) -> HttpResponse {
    let body = message.as_ref().as_bytes().to_vec();
    HttpResponse {
        status,
        media_type: "text/plain; charset=utf-8".into(),
        content_length: body.len(),
        content_security_policy: RESOURCE_CSP,
        body,
    }
}

fn epub_error_response(error: EpubError) -> HttpResponse {
    let status = match error.code() {
        "invalid-epub-path" => 400,
        "epub-resource-not-found" => 404,
        "epub-limit-exceeded" => 413,
        "unsupported-epub-resource" => 415,
        _ => 500,
    };
    error_response(status, error.message())
}

fn write_http_response(
    stream: &mut TcpStream,
    response: HttpResponse,
) -> std::io::Result<()> {
    let reason = match response.status {
        200 => "OK",
        400 => "Bad Request",
        404 => "Not Found",
        405 => "Method Not Allowed",
        413 => "Content Too Large",
        415 => "Unsupported Media Type",
        500 => "Internal Server Error",
        503 => "Service Unavailable",
        _ => "Error",
    };
    write!(
        stream,
        concat!(
            "HTTP/1.1 {} {}\r\n",
            "Content-Type: {}\r\n",
            "Content-Length: {}\r\n",
            "Cache-Control: no-store\r\n",
            "Connection: close\r\n",
            "X-Content-Type-Options: nosniff\r\n",
            "Referrer-Policy: no-referrer\r\n",
            "Cross-Origin-Resource-Policy: same-origin\r\n",
            "Content-Security-Policy: {}\r\n\r\n"
        ),
        response.status,
        reason,
        response.media_type,
        response.content_length,
        response.content_security_policy,
    )?;
    stream.write_all(&response.body)?;
    stream.flush()
}

fn decode_resource_path(encoded: &str) -> Result<String, &'static str> {
    if encoded.is_empty() || encoded.len() > MAX_RESOURCE_URI_PATH_BYTES {
        return Err("publication resource path is invalid");
    }
    validate_percent_encoding(encoded)?;
    let path = percent_decode_str(encoded)
        .decode_utf8()
        .map_err(|_| "publication resource path is not UTF-8")?;
    if path.starts_with('/')
        || path.contains(['\\', '\0'])
        || path.split('/').any(|component| {
            component.is_empty() || matches!(component, "." | "..")
        })
    {
        return Err("publication resource path is not normalized");
    }
    Ok(path.into_owned())
}

fn validate_percent_encoding(value: &str) -> Result<(), &'static str> {
    let bytes = value.as_bytes();
    let mut index = 0;
    while index < bytes.len() {
        if bytes[index] != b'%' {
            index += 1;
            continue;
        }
        if index + 2 >= bytes.len()
            || !bytes[index + 1].is_ascii_hexdigit()
            || !bytes[index + 2].is_ascii_hexdigit()
        {
            return Err(
                "publication resource path has invalid percent encoding",
            );
        }
        let first = bytes[index + 1].to_ascii_lowercase();
        let second = bytes[index + 2].to_ascii_lowercase();
        if matches!((first, second), (b'2', b'f') | (b'5', b'c') | (b'0', b'0'))
        {
            return Err("publication resource path encodes a separator");
        }
        index += 3;
    }
    Ok(())
}

fn valid_token(value: &str) -> bool {
    value.len() == 32
        && value
            .bytes()
            .all(|byte| byte.is_ascii_digit() || matches!(byte, b'a'..=b'f'))
}

fn random_token() -> Result<String, BrokerError> {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut bytes = [0_u8; 16];
    getrandom(&mut bytes).map_err(|error| {
        BrokerError::new(
            "publication-token-unavailable",
            format!("could not create publication token: {error}"),
        )
    })?;
    let mut token = String::with_capacity(32);
    for byte in bytes {
        token.push(HEX[(byte >> 4) as usize] as char);
        token.push(HEX[(byte & 0x0f) as usize] as char);
    }
    Ok(token)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::{self, File};
    use std::sync::atomic::{AtomicU64, Ordering};
    use zip::write::SimpleFileOptions;
    use zip::{CompressionMethod, ZipWriter};

    static TEMPORARY_ID: AtomicU64 = AtomicU64::new(1);

    struct TemporaryEpub(std::path::PathBuf);

    impl Drop for TemporaryEpub {
        fn drop(&mut self) {
            let _ = fs::remove_file(&self.0);
        }
    }

    fn test_epub() -> TemporaryEpub {
        let id = TEMPORARY_ID.fetch_add(1, Ordering::Relaxed);
        let path = std::env::temp_dir().join(format!(
            "yunge-reader-broker-{}-{id}.epub",
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
            "\"><dc:title>Broker Book</dc:title>",
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
            archive
                .start_file(
                    name,
                    SimpleFileOptions::default().compression_method(method),
                )
                .unwrap();
            archive.write_all(contents.as_bytes()).unwrap();
        }
        archive.finish().unwrap();
        TemporaryEpub(path)
    }

    fn request(url: &str, method: &str) -> Vec<u8> {
        let target = url.strip_prefix("http://").unwrap();
        let (authority, path) = target.split_once('/').unwrap();
        let mut stream = TcpStream::connect(authority).unwrap();
        write!(
            stream,
            "{method} /{path} HTTP/1.1\r\nHost: {authority}\r\n\r\n"
        )
        .unwrap();
        let mut response = Vec::new();
        stream.read_to_end(&mut response).unwrap();
        response
    }

    #[test]
    fn renderer_assets_are_served_only_below_the_secret_root() {
        let broker = EpubBroker::start().unwrap();
        let url = broker.base_url();
        let target = url.strip_prefix("http://").unwrap();
        let (authority, path) = target.split_once('/').unwrap();
        let mut stream = TcpStream::connect(authority).unwrap();
        write!(
            stream,
            "GET /{path}app/index.html HTTP/1.1\r\nHost: {authority}\r\n\r\n"
        )
        .unwrap();
        let mut response = String::new();
        stream.read_to_string(&mut response).unwrap();
        assert!(response.starts_with("HTTP/1.1 200 OK\r\n"));
        assert!(
            response.contains("Content-Security-Policy: default-src 'none'")
        );
        assert!(response.contains("<!doctype html>"));
    }

    #[test]
    fn resource_paths_reject_encoded_separators_and_traversal() {
        assert!(decode_resource_path("OPS/chapter.xhtml").is_ok());
        assert!(decode_resource_path("OPS/%2fsecret").is_err());
        assert!(decode_resource_path("OPS/../secret").is_err());
        assert!(decode_resource_path("/absolute").is_err());
    }

    #[test]
    fn publication_resources_are_brokered_and_revoked() {
        let epub = test_epub();
        let mut broker = EpubBroker::start().unwrap();
        let descriptor = broker.open(&epub.0).unwrap();
        assert_eq!(descriptor.metadata.title.as_deref(), Some("Broker Book"));
        assert_eq!(descriptor.entry_count, 4);
        assert!(descriptor.renderer_url.ends_with("/app/index.html"));

        let catalog = request(
            &format!("{}{}", descriptor.resource_root, RESOURCE_CATALOG_PATH),
            "GET",
        );
        let catalog = String::from_utf8(catalog).unwrap();
        assert!(catalog.starts_with("HTTP/1.1 200 OK\r\n"));
        assert!(catalog.contains("\"path\":\"OPS/chapter.xhtml\""));

        let resource_url =
            format!("{}OPS/chapter.xhtml", descriptor.resource_root);
        let resource =
            String::from_utf8(request(&resource_url, "GET")).unwrap();
        assert!(resource.contains("Content-Type: application/xhtml+xml"));
        assert!(resource.contains("script-src 'none'"));
        assert!(resource.ends_with("<html><body>Chapter</body></html>"));

        let head = String::from_utf8(request(&resource_url, "HEAD")).unwrap();
        assert!(head.contains("Content-Length: 33\r\n"));
        assert!(head.ends_with("\r\n\r\n"));

        broker.close(descriptor.publication).unwrap();
        let revoked = String::from_utf8(request(&resource_url, "GET")).unwrap();
        assert!(revoked.starts_with("HTTP/1.1 404 Not Found\r\n"));
    }
}
