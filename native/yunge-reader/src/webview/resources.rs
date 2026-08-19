// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use getrandom::getrandom;
use serde_json::{Value, json};
use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};
use std::thread;
use wry::RequestAsyncResponder;
use wry::http::{Method, Request as HttpRequest, Response as HttpResponse};
use yunge_reader::epub::{EpubError, Publication, PublicationLayout};

use super::protocol::ServiceError;

pub(super) const APP_PROTOCOL: &str = "yunge-reader-app";
pub(super) const APP_URL: &str = "yunge-reader-app://localhost/index.html";
pub(super) const APP_BROWSER_URL: &str =
    "https://yunge-reader-app.localhost/index.html";
pub(super) const APP_BROWSER_ORIGIN: &str =
    "https://yunge-reader-app.localhost";
pub(super) const BOOK_PROTOCOL: &str = "yunge-reader-book";
pub(super) const RESOURCE_CATALOG_PATH: &str = ".yunge/resources.json";
pub(super) const MAX_RESOURCE_REQUESTS: usize = 8;

const BOOK_BROWSER_ROOT: &str = "https://yunge-reader-book.localhost/";
const MAX_RESOURCE_CATALOG_BYTES: usize = 16 * 1_024 * 1_024;
const MAX_RESOURCE_URI_PATH_BYTES: usize = 196_605;
const APP_CSP: &str = concat!(
    "default-src 'none'; ",
    "script-src 'self'; ",
    "style-src 'self' blob: 'unsafe-inline'; ",
    "img-src blob: data:; ",
    "font-src blob: data:; ",
    "media-src blob: data:; ",
    "connect-src https://yunge-reader-book.localhost; ",
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

struct StoredPublication {
    token: String,
    publication: Publication,
}

#[derive(Default)]
struct PublicationStore {
    entries: HashMap<u64, StoredPublication>,
    tokens: HashMap<String, u64>,
}

type SharedPublications = Arc<Mutex<PublicationStore>>;

#[derive(Clone, Default)]
pub(super) struct ResourceService {
    publications: SharedPublications,
    active_requests: Arc<AtomicUsize>,
}

pub(super) struct ResourcePermit(Arc<AtomicUsize>);

impl PublicationStore {
    fn insert(
        &mut self,
        id: u64,
        publication: Publication,
    ) -> Result<&StoredPublication, ServiceError> {
        let token = loop {
            let candidate = publication_token()?;
            if !self.tokens.contains_key(&candidate) {
                break candidate;
            }
        };
        self.tokens.insert(token.clone(), id);
        self.entries
            .insert(id, StoredPublication { token, publication });
        Ok(self
            .entries
            .get(&id)
            .expect("inserted publication is present"))
    }

    fn remove(&mut self, id: u64) -> Option<StoredPublication> {
        let publication = self.entries.remove(&id)?;
        self.tokens.remove(&publication.token);
        Some(publication)
    }

    fn by_token_mut(&mut self, token: &str) -> Option<&mut StoredPublication> {
        let id = self.tokens.get(token).copied()?;
        self.entries.get_mut(&id)
    }

    fn clear(&mut self) {
        self.entries.clear();
        self.tokens.clear();
    }
}

impl ResourceService {
    pub(super) fn insert(
        &self,
        id: u64,
        publication: Publication,
    ) -> Result<Value, ServiceError> {
        let mut publications = self.lock()?;
        let publication = publications.insert(id, publication)?;
        Ok(publication_result(id, publication))
    }

    pub(super) fn info(&self, id: u64) -> Result<Value, ServiceError> {
        let publications = self.lock()?;
        let publication = publications
            .entries
            .get(&id)
            .ok_or_else(|| unknown_publication(id))?;
        Ok(publication_result(id, publication))
    }

    pub(super) fn remove(&self, id: u64) -> Result<(), ServiceError> {
        let mut publications = self.lock()?;
        if publications.remove(id).is_none() {
            return Err(unknown_publication(id));
        }
        Ok(())
    }

    pub(super) fn resource_root(
        &self,
        id: u64,
    ) -> Result<String, ServiceError> {
        let publications = self.lock()?;
        let publication = publications
            .entries
            .get(&id)
            .ok_or_else(|| unknown_publication(id))?;
        Ok(format!("{BOOK_BROWSER_ROOT}{}/", publication.token))
    }

    pub(super) fn layout(
        &self,
        id: u64,
    ) -> Result<PublicationLayout, ServiceError> {
        let publications = self.lock()?;
        let publication = publications
            .entries
            .get(&id)
            .ok_or_else(|| unknown_publication(id))?;
        Ok(publication.publication.metadata().layout)
    }

    pub(super) fn respond(
        &self,
        request: HttpRequest<Vec<u8>>,
        responder: RequestAsyncResponder,
    ) {
        let Some(permit) =
            ResourcePermit::acquire(Arc::clone(&self.active_requests))
        else {
            responder.respond(resource_error_response(
                503,
                "too many EPUB resource requests",
            ));
            return;
        };
        let publications = Arc::clone(&self.publications);
        thread::spawn(move || {
            responder.respond(resource_response(&publications, request));
            drop(permit);
        });
    }

    #[cfg(test)]
    pub(super) fn response(
        &self,
        request: HttpRequest<Vec<u8>>,
    ) -> HttpResponse<Vec<u8>> {
        resource_response(&self.publications, request)
    }

    pub(super) fn clear(&self) -> Result<(), ServiceError> {
        self.lock()?.clear();
        Ok(())
    }

    fn lock(&self) -> Result<MutexGuard<'_, PublicationStore>, ServiceError> {
        self.publications.lock().map_err(|_| {
            ServiceError::new(
                "publication-store-failed",
                "the publication store is unavailable",
            )
        })
    }
}

impl ResourcePermit {
    pub(super) fn acquire(active: Arc<AtomicUsize>) -> Option<Self> {
        let mut current = active.load(Ordering::Acquire);
        loop {
            if current >= MAX_RESOURCE_REQUESTS {
                return None;
            }
            match active.compare_exchange_weak(
                current,
                current + 1,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => return Some(Self(active)),
                Err(updated) => current = updated,
            }
        }
    }
}

impl Drop for ResourcePermit {
    fn drop(&mut self) {
        self.0.fetch_sub(1, Ordering::AcqRel);
    }
}

fn publication_token() -> Result<String, ServiceError> {
    let mut bytes = [0_u8; 16];
    getrandom(&mut bytes).map_err(|error| {
        ServiceError::new(
            "publication-token-unavailable",
            format!("could not create a publication token: {error}"),
        )
    })?;
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut token = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        token.push(HEX[(byte >> 4) as usize] as char);
        token.push(HEX[(byte & 0x0f) as usize] as char);
    }
    Ok(token)
}

pub(super) fn app_asset(path: &str) -> Option<(&'static str, &'static [u8])> {
    match path {
        "index.html" => Some((
            "text/html; charset=utf-8",
            include_bytes!("../../renderer/index.html"),
        )),
        "style.css" => Some((
            "text/css; charset=utf-8",
            include_bytes!("../../renderer/style.css"),
        )),
        "yunge-reader.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../../renderer/yunge-reader.js"),
        )),
        "yunge-reader-core.mjs" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../../renderer/yunge-reader-core.mjs"),
        )),
        "foliate-js/epub.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../../renderer/foliate-js/epub.js"),
        )),
        "foliate-js/epubcfi.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../../renderer/foliate-js/epubcfi.js"),
        )),
        "foliate-js/fixed-layout.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../../renderer/foliate-js/fixed-layout.js"),
        )),
        "foliate-js/overlayer.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../../renderer/foliate-js/overlayer.js"),
        )),
        "foliate-js/paginator.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../../renderer/foliate-js/paginator.js"),
        )),
        "foliate-js/progress.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../../renderer/foliate-js/progress.js"),
        )),
        "foliate-js/search.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../../renderer/foliate-js/search.js"),
        )),
        "foliate-js/text-walker.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../../renderer/foliate-js/text-walker.js"),
        )),
        "foliate-js/view.js" => Some((
            "text/javascript; charset=utf-8",
            include_bytes!("../../renderer/foliate-js/view.js"),
        )),
        _ => None,
    }
}

pub(super) fn app_response(
    request: HttpRequest<Vec<u8>>,
) -> HttpResponse<Cow<'static, [u8]>> {
    if request.method() != Method::GET && request.method() != Method::HEAD {
        return app_error_response(405, "method not allowed");
    }
    let uri = request.uri();
    if uri.scheme_str() != Some(APP_PROTOCOL)
        || uri.authority().map(|value| value.as_str()) != Some("localhost")
        || uri.query().is_some()
    {
        return app_error_response(400, "invalid renderer asset target");
    }
    let Some(path) = uri.path().strip_prefix('/') else {
        return app_error_response(400, "invalid renderer asset path");
    };
    let Some((media_type, asset)) = app_asset(path) else {
        return app_error_response(404, "renderer asset not found");
    };
    let body: Cow<'static, [u8]> = if request.method() == Method::HEAD {
        Cow::Borrowed(&[])
    } else {
        Cow::Borrowed(asset)
    };
    build_app_response(200, media_type, asset.len(), body)
}

fn app_error_response(
    status: u16,
    message: &'static str,
) -> HttpResponse<Cow<'static, [u8]>> {
    build_app_response(
        status,
        "text/plain; charset=utf-8",
        message.len(),
        Cow::Borrowed(message.as_bytes()),
    )
}

fn build_app_response(
    status: u16,
    media_type: &str,
    content_length: usize,
    body: Cow<'static, [u8]>,
) -> HttpResponse<Cow<'static, [u8]>> {
    HttpResponse::builder()
        .status(status)
        .header("Content-Type", media_type)
        .header("Content-Length", content_length)
        .header("Cache-Control", "no-store")
        .header("X-Content-Type-Options", "nosniff")
        .header("Referrer-Policy", "no-referrer")
        .header("Content-Security-Policy", APP_CSP)
        .body(body)
        .expect("renderer response headers are valid")
}

fn resource_response(
    publications: &SharedPublications,
    request: HttpRequest<Vec<u8>>,
) -> HttpResponse<Vec<u8>> {
    if request.method() != Method::GET && request.method() != Method::HEAD {
        return resource_error_response(405, "method not allowed");
    }
    let (token, path) = match resource_request_target(request.uri()) {
        Ok(target) => target,
        Err(message) => return resource_error_response(400, message),
    };
    let mut publications = match publications.lock() {
        Ok(publications) => publications,
        Err(_) => {
            return resource_error_response(500, "publication store failed");
        }
    };
    let Some(publication) = publications.by_token_mut(&token) else {
        return resource_error_response(404, "publication not found");
    };
    if path == RESOURCE_CATALOG_PATH {
        let body = match serde_json::to_vec(&json!({
            "resources": publication.publication.resource_catalog(),
        })) {
            Ok(body) if body.len() <= MAX_RESOURCE_CATALOG_BYTES => body,
            Ok(_) => {
                return resource_error_response(
                    413,
                    "EPUB resource catalog is too large",
                );
            }
            Err(_) => {
                return resource_error_response(
                    500,
                    "could not encode EPUB resource catalog",
                );
            }
        };
        return resource_body_response(
            request.method(),
            "application/json; charset=utf-8",
            body,
        );
    }
    let resource = match publication.publication.read_resource(&path) {
        Ok(resource) => resource,
        Err(error) => return epub_resource_error_response(error),
    };
    let media_type = resource.media_type().to_owned();
    resource_body_response(request.method(), &media_type, resource.into_bytes())
}

fn resource_body_response(
    method: &Method,
    media_type: &str,
    bytes: Vec<u8>,
) -> HttpResponse<Vec<u8>> {
    let content_length = bytes.len();
    let body = if method == Method::HEAD {
        Vec::new()
    } else {
        bytes
    };
    build_resource_response(200, media_type, content_length, body)
}

pub(super) fn resource_request_target(
    uri: &wry::http::Uri,
) -> Result<(String, String), &'static str> {
    if uri.scheme_str() != Some(BOOK_PROTOCOL) {
        return Err("invalid EPUB resource scheme");
    }
    if uri.query().is_some() {
        return Err("EPUB resource queries are not supported");
    }
    uri.authority()
        .map(|authority| authority.as_str())
        .filter(|authority| *authority == "localhost")
        .ok_or("invalid EPUB resource authority")?;
    let encoded = uri
        .path()
        .strip_prefix('/')
        .filter(|path| !path.is_empty())
        .ok_or("EPUB resource path is empty")?;
    if encoded.len() > MAX_RESOURCE_URI_PATH_BYTES {
        return Err("EPUB resource path is too long");
    }
    let (token, encoded) = encoded
        .split_once('/')
        .filter(|(token, path)| {
            token.len() == 32
                && token.bytes().all(|byte| {
                    byte.is_ascii_digit() || matches!(byte, b'a'..=b'f')
                })
                && !path.is_empty()
        })
        .ok_or("invalid EPUB publication token")?;
    validate_percent_encoding(encoded)?;
    let path = percent_encoding::percent_decode_str(encoded)
        .decode_utf8()
        .map_err(|_| "EPUB resource path is not UTF-8")?;
    if path.starts_with('/')
        || path.contains(['\\', '\0'])
        || path.split('/').any(|component| {
            component.is_empty() || matches!(component, "." | "..")
        })
    {
        return Err("EPUB resource path is not normalized");
    }
    Ok((token.to_owned(), path.into_owned()))
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
            return Err("EPUB resource path has invalid percent encoding");
        }
        let first = bytes[index + 1].to_ascii_lowercase();
        let second = bytes[index + 2].to_ascii_lowercase();
        if matches!((first, second), (b'2', b'f') | (b'5', b'c') | (b'0', b'0'))
        {
            return Err("EPUB resource path encodes a separator");
        }
        index += 3;
    }
    Ok(())
}

fn epub_resource_error_response(error: EpubError) -> HttpResponse<Vec<u8>> {
    let status = match error.code() {
        "invalid-epub-path" => 400,
        "epub-resource-not-found" => 404,
        "epub-limit-exceeded" => 413,
        "unsupported-epub-resource" => 415,
        _ => 500,
    };
    resource_error_response(status, error.message())
}

fn resource_error_response(
    status: u16,
    message: impl AsRef<str>,
) -> HttpResponse<Vec<u8>> {
    let body = message.as_ref().as_bytes().to_vec();
    build_resource_response(
        status,
        "text/plain; charset=utf-8",
        body.len(),
        body,
    )
}

fn build_resource_response(
    status: u16,
    media_type: &str,
    content_length: usize,
    body: Vec<u8>,
) -> HttpResponse<Vec<u8>> {
    HttpResponse::builder()
        .status(status)
        .header("Content-Type", media_type)
        .header("Content-Length", content_length)
        .header("Cache-Control", "no-store")
        .header("Access-Control-Allow-Origin", APP_BROWSER_ORIGIN)
        .header("X-Content-Type-Options", "nosniff")
        .header("Referrer-Policy", "no-referrer")
        .header("Content-Security-Policy", RESOURCE_CSP)
        .body(body)
        .expect("resource response headers are valid")
}

fn unknown_publication(id: u64) -> ServiceError {
    ServiceError::new(
        "unknown-publication",
        format!("publication {id} does not exist"),
    )
}

fn publication_result(id: u64, publication: &StoredPublication) -> Value {
    json!({
        "publication": id,
        "metadata": publication.publication.metadata(),
        "entry-count": publication.publication.entry_count(),
        "expanded-bytes": publication.publication.expanded_size(),
        "resource-root": format!(
            "{BOOK_PROTOCOL}://localhost/{}/",
            publication.token
        ),
    })
}

impl From<EpubError> for ServiceError {
    fn from(error: EpubError) -> Self {
        Self::new(error.code(), error.message())
    }
}
