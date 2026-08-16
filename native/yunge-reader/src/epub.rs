// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use percent_encoding::percent_decode_str;
use roxmltree::{Document, Node, ParsingOptions};
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs::File;
use std::io::{Read, Seek};
use std::path::Path;
use zip::{CompressionMethod, ZipArchive};

const CONTAINER_PATH: &str = "META-INF/container.xml";
const CONTAINER_NAMESPACE: &str =
    "urn:oasis:names:tc:opendocument:xmlns:container";
const DC_NAMESPACE: &str = "http://purl.org/dc/elements/1.1/";
const EPUB_MIMETYPE: &[u8] = b"application/epub+zip";
const OPF_MEDIA_TYPE: &str = "application/oebps-package+xml";
const OPF_NAMESPACE: &str = "http://www.idpf.org/2007/opf";
const MAX_ARCHIVE_ENTRIES: usize = 65_536;
const MAX_ARCHIVE_PATH_BYTES: usize = 65_535;
const MAX_COMPRESSION_RATIO: u64 = 1_000;
const MAX_CONTAINER_BYTES: u64 = 512 * 1_024;
const MAX_ENTRY_BYTES: u64 = 512 * 1_024 * 1_024;
const MAX_FILE_NAME_BYTES: usize = 255;
const MAX_MEDIA_TYPE_BYTES: usize = 255;
const MAX_PACKAGE_BYTES: u64 = 8 * 1_024 * 1_024;
const MAX_RESOURCE_BYTES: u64 = 128 * 1_024 * 1_024;
const MAX_TOTAL_BYTES: u64 = 4 * 1_024 * 1_024 * 1_024;
const MAX_XML_NODES: u32 = 100_000;

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct PublicationMetadata {
    pub package_path: String,
    pub title: Option<String>,
    pub language: Option<String>,
    pub identifier: Option<String>,
    pub version: Option<String>,
}

#[derive(Debug)]
pub struct EpubError {
    code: &'static str,
    message: String,
}

pub struct Publication {
    archive: ZipArchive<File>,
    entries: HashMap<String, ArchiveEntry>,
    expanded_size: u64,
    metadata: PublicationMetadata,
    resources: HashMap<String, String>,
}

#[derive(Debug)]
pub struct PublicationResource {
    bytes: Vec<u8>,
    media_type: String,
}

#[derive(Clone, Copy)]
struct ArchiveEntry {
    index: usize,
    size: u64,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct PublicationResourceMetadata {
    pub path: String,
    pub media_type: String,
    pub size: u64,
}

impl EpubError {
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

impl fmt::Display for EpubError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}: {}", self.code, self.message)
    }
}

impl std::error::Error for EpubError {}

impl Publication {
    pub fn open(path: impl AsRef<Path>) -> Result<Self, EpubError> {
        let path = path.as_ref();
        let file = File::open(path).map_err(|error| {
            EpubError::new(
                "epub-io-error",
                format!("could not open {}: {error}", path.display()),
            )
        })?;
        let mut archive = ZipArchive::new(file).map_err(|error| {
            EpubError::new(
                "invalid-epub",
                format!("could not read EPUB ZIP directory: {error}"),
            )
        })?;
        let (entries, expanded_size) = validate_archive(&mut archive)?;

        let mimetype = read_entry(
            &mut archive,
            &entries,
            "mimetype",
            EPUB_MIMETYPE.len() as u64,
        )?;
        if mimetype != EPUB_MIMETYPE {
            return Err(EpubError::new(
                "invalid-epub",
                "mimetype entry is not application/epub+zip",
            ));
        }

        let container = read_entry(
            &mut archive,
            &entries,
            CONTAINER_PATH,
            MAX_CONTAINER_BYTES,
        )?;
        let package_path = parse_container(&container)?;
        if !entries.contains_key(&package_path) {
            return Err(EpubError::new(
                "invalid-epub",
                format!("package document does not exist: {package_path}"),
            ));
        }
        let package = read_entry(
            &mut archive,
            &entries,
            &package_path,
            MAX_PACKAGE_BYTES,
        )?;
        let (metadata, resources) = parse_package(&package_path, &package)?;

        Ok(Self {
            archive,
            entries,
            expanded_size,
            metadata,
            resources,
        })
    }

    pub fn metadata(&self) -> &PublicationMetadata {
        &self.metadata
    }

    pub fn entry_count(&self) -> usize {
        self.entries.len()
    }

    pub fn expanded_size(&self) -> u64 {
        self.expanded_size
    }

    pub fn resource_catalog(&self) -> Vec<PublicationResourceMetadata> {
        let mut resources: Vec<_> = self
            .resources
            .iter()
            .filter(|(_, media_type)| browser_media_type_allowed(media_type))
            .filter_map(|(path, media_type)| {
                self.entries.get(path).map(|entry| {
                    PublicationResourceMetadata {
                        path: path.clone(),
                        media_type: media_type.clone(),
                        size: entry.size,
                    }
                })
            })
            .collect();
        resources.sort_unstable_by(|left, right| left.path.cmp(&right.path));
        resources
    }

    pub fn read_resource(
        &mut self,
        path: &str,
    ) -> Result<PublicationResource, EpubError> {
        let path = normalize_archive_path(path, false)
            .map_err(|message| EpubError::new("invalid-epub-path", message))?;
        let media_type = self.resource_media_type(&path)?;
        let bytes = read_entry(
            &mut self.archive,
            &self.entries,
            &path,
            MAX_RESOURCE_BYTES,
        )?;
        Ok(PublicationResource { bytes, media_type })
    }

    fn resource_media_type(&self, path: &str) -> Result<String, EpubError> {
        if !self.entries.contains_key(path) {
            return Err(EpubError::new(
                "epub-resource-not-found",
                format!("EPUB resource does not exist: {path}"),
            ));
        }
        let media_type = if path == CONTAINER_PATH
            || (path.starts_with("META-INF/") && path.ends_with(".xml"))
        {
            "application/xml"
        } else if path == self.metadata.package_path {
            OPF_MEDIA_TYPE
        } else {
            self.resources
                .get(path)
                .map(String::as_str)
                .ok_or_else(|| {
                    EpubError::new(
                        "unsupported-epub-resource",
                        format!("EPUB resource is not in the manifest: {path}"),
                    )
                })?
        };
        if !browser_media_type_allowed(media_type) {
            return Err(EpubError::new(
                "unsupported-epub-resource",
                format!("EPUB resource type is not allowed: {media_type}"),
            ));
        }
        Ok(media_type.to_owned())
    }
}

impl PublicationResource {
    pub fn media_type(&self) -> &str {
        &self.media_type
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }
}

fn validate_archive(
    archive: &mut ZipArchive<File>,
) -> Result<(HashMap<String, ArchiveEntry>, u64), EpubError> {
    if archive.len() > MAX_ARCHIVE_ENTRIES {
        return Err(limit_error(format!(
            "archive has {} entries; limit is {MAX_ARCHIVE_ENTRIES}",
            archive.len()
        )));
    }
    let mut entries = HashMap::new();
    let mut names = HashSet::new();
    let mut expanded_size = 0_u64;

    for index in 0..archive.len() {
        let entry = archive.by_index(index).map_err(|error| {
            EpubError::new(
                "invalid-epub",
                format!("could not inspect ZIP entry {index}: {error}"),
            )
        })?;
        let name = std::str::from_utf8(entry.name_raw()).map_err(|_| {
            EpubError::new(
                "invalid-epub",
                format!("ZIP entry {index} does not have a UTF-8 name"),
            )
        })?;
        let normalized = normalize_archive_path(name, entry.is_dir())
            .map_err(|message| EpubError::new("invalid-epub", message))?;
        record_archive_name(&mut names, &normalized)?;
        if entry.is_symlink() {
            return Err(EpubError::new(
                "invalid-epub",
                format!("symbolic link ZIP entry is not allowed: {name}"),
            ));
        }
        if entry.encrypted() {
            return Err(EpubError::new(
                "invalid-epub",
                format!("encrypted ZIP entry is not supported: {name}"),
            ));
        }
        if !matches!(
            entry.compression(),
            CompressionMethod::Stored | CompressionMethod::Deflated
        ) {
            return Err(EpubError::new(
                "invalid-epub",
                format!("unsupported ZIP compression for entry: {name}"),
            ));
        }
        validate_entry_sizes(name, entry.size(), entry.compressed_size())?;
        expanded_size = expanded_size
            .checked_add(entry.size())
            .ok_or_else(|| limit_error("expanded archive size overflowed"))?;
        if expanded_size > MAX_TOTAL_BYTES {
            return Err(limit_error(format!(
                "expanded archive exceeds {MAX_TOTAL_BYTES} bytes"
            )));
        }
        if !entry.is_dir() {
            entries.insert(
                normalized,
                ArchiveEntry {
                    index,
                    size: entry.size(),
                },
            );
        }
    }
    Ok((entries, expanded_size))
}

fn record_archive_name(
    names: &mut HashSet<String>,
    name: &str,
) -> Result<(), EpubError> {
    if !names.insert(name.to_owned()) {
        return Err(EpubError::new(
            "invalid-epub",
            format!("duplicate ZIP entry path: {name}"),
        ));
    }
    Ok(())
}

fn validate_entry_sizes(
    name: &str,
    size: u64,
    compressed_size: u64,
) -> Result<(), EpubError> {
    if size > MAX_ENTRY_BYTES {
        return Err(limit_error(format!(
            "ZIP entry exceeds {MAX_ENTRY_BYTES} bytes: {name}"
        )));
    }
    if size > 0
        && (compressed_size == 0
            || size > compressed_size.saturating_mul(MAX_COMPRESSION_RATIO))
    {
        return Err(limit_error(format!(
            concat!("ZIP entry compression ratio exceeds ", "{}: {}"),
            MAX_COMPRESSION_RATIO, name
        )));
    }
    Ok(())
}

fn normalize_archive_path(
    name: &str,
    directory: bool,
) -> Result<String, String> {
    if name.is_empty() {
        return Err("ZIP entry path is empty".into());
    }
    if name.len() > MAX_ARCHIVE_PATH_BYTES {
        return Err(format!(
            "ZIP entry path exceeds {MAX_ARCHIVE_PATH_BYTES} bytes"
        ));
    }
    if name.contains(['\\', '\0']) {
        return Err(format!(
            "ZIP entry path has a forbidden character: {name}"
        ));
    }
    if name.starts_with('/') {
        return Err(format!("absolute ZIP entry path is not allowed: {name}"));
    }
    let path = if directory {
        name.strip_suffix('/').ok_or_else(|| {
            format!("ZIP directory entry lacks a trailing slash: {name}")
        })?
    } else {
        if name.ends_with('/') {
            return Err(format!("ZIP file entry ends with a slash: {name}"));
        }
        name
    };
    if path.is_empty() {
        return Err("ZIP entry path has no components".into());
    }
    for component in path.split('/') {
        if component.is_empty() || component == "." || component == ".." {
            return Err(format!("ZIP entry path is not normalized: {name}"));
        }
        if component.len() > MAX_FILE_NAME_BYTES {
            return Err(format!(
                "ZIP file name exceeds {MAX_FILE_NAME_BYTES} bytes: {component}"
            ));
        }
        if component.ends_with('.')
            || component.contains(['"', '*', ':', '<', '>', '?'])
        {
            return Err(format!(
                "ZIP file name has a forbidden character: {name}"
            ));
        }
    }
    Ok(path.to_owned())
}

fn resolve_archive_reference(
    base_path: &str,
    reference: &str,
) -> Result<Option<String>, EpubError> {
    if reference.is_empty() {
        return Err(EpubError::new(
            "invalid-epub",
            "manifest item href is empty",
        ));
    }
    let path = reference.split(['?', '#']).next().unwrap_or_default();
    if path.is_empty() {
        return Err(EpubError::new(
            "invalid-epub",
            format!("manifest item href has no path: {reference}"),
        ));
    }
    if path.starts_with("//") || uri_scheme(path).is_some() {
        return Ok(None);
    }
    if path.starts_with('/') || path.contains(['\\', '\0']) {
        return Err(EpubError::new(
            "invalid-epub",
            format!("manifest item href is not a safe relative URI: {path}"),
        ));
    }
    reject_encoded_separator(path)?;
    let decoded = percent_decode_str(path).decode_utf8().map_err(|_| {
        EpubError::new(
            "invalid-epub",
            format!("manifest item href is not UTF-8: {path}"),
        )
    })?;
    let mut components: Vec<&str> = base_path.split('/').collect();
    components.pop();
    for component in decoded.split('/') {
        match component {
            "" => {
                return Err(EpubError::new(
                    "invalid-epub",
                    format!(
                        "manifest item href has an empty component: {path}"
                    ),
                ));
            }
            "." => {}
            ".." => {
                if components.pop().is_none() {
                    return Err(EpubError::new(
                        "invalid-epub",
                        format!(
                            "manifest item href escapes the archive: {path}"
                        ),
                    ));
                }
            }
            value => components.push(value),
        }
    }
    let resolved = components.join("/");
    normalize_archive_path(&resolved, false)
        .map(Some)
        .map_err(|message| EpubError::new("invalid-epub", message))
}

fn uri_scheme(value: &str) -> Option<&str> {
    let end = value.find(':')?;
    let scheme = &value[..end];
    (!scheme.is_empty()
        && scheme.bytes().enumerate().all(|(index, byte)| {
            byte.is_ascii_alphabetic()
                || (index > 0
                    && (byte.is_ascii_digit()
                        || matches!(byte, b'+' | b'-' | b'.')))
        }))
    .then_some(scheme)
}

fn reject_encoded_separator(value: &str) -> Result<(), EpubError> {
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
            return Err(EpubError::new(
                "invalid-epub",
                format!("manifest item href has invalid escaping: {value}"),
            ));
        }
        let first = bytes[index + 1].to_ascii_lowercase();
        let second = bytes[index + 2].to_ascii_lowercase();
        if matches!((first, second), (b'2', b'f') | (b'5', b'c') | (b'0', b'0'))
        {
            return Err(EpubError::new(
                "invalid-epub",
                format!("manifest item href encodes a separator: {value}"),
            ));
        }
        index += 3;
    }
    Ok(())
}

fn browser_media_type_allowed(media_type: &str) -> bool {
    matches!(
        media_type,
        "application/font-sfnt"
            | "application/oebps-package+xml"
            | "application/smil+xml"
            | "application/ttml+xml"
            | "application/vnd.ms-opentype"
            | "application/x-dtbncx+xml"
            | "application/xhtml+xml"
            | "application/xml"
            | "audio/aac"
            | "audio/mp4"
            | "audio/mpeg"
            | "audio/ogg"
            | "audio/opus"
            | "font/otf"
            | "font/ttf"
            | "font/woff"
            | "font/woff2"
            | "image/avif"
            | "image/gif"
            | "image/jpeg"
            | "image/png"
            | "image/svg+xml"
            | "image/webp"
            | "text/css"
            | "text/html"
            | "text/vtt"
            | "video/mp4"
            | "video/ogg"
            | "video/webm"
    )
}

fn valid_media_type(media_type: &str) -> bool {
    if media_type.is_empty() || media_type.len() > MAX_MEDIA_TYPE_BYTES {
        return false;
    }
    let Some((kind, subtype)) = media_type.split_once('/') else {
        return false;
    };
    !kind.is_empty()
        && !subtype.is_empty()
        && !subtype.contains('/')
        && media_type.bytes().all(|byte| {
            byte.is_ascii_alphanumeric()
                || byte == b'/'
                || matches!(
                    byte,
                    b'!' | b'#'
                        | b'$'
                        | b'%'
                        | b'&'
                        | b'\''
                        | b'*'
                        | b'+'
                        | b'-'
                        | b'.'
                        | b'^'
                        | b'_'
                        | b'`'
                        | b'|'
                        | b'~'
                )
        })
}

fn read_entry<R: Read + Seek>(
    archive: &mut ZipArchive<R>,
    entries: &HashMap<String, ArchiveEntry>,
    path: &str,
    limit: u64,
) -> Result<Vec<u8>, EpubError> {
    let entry = entries.get(path).copied().ok_or_else(|| {
        EpubError::new(
            "epub-resource-not-found",
            format!("EPUB resource does not exist: {path}"),
        )
    })?;
    let mut entry = archive.by_index(entry.index).map_err(|error| {
        EpubError::new(
            "invalid-epub",
            format!("could not open EPUB resource {path}: {error}"),
        )
    })?;
    if entry.size() > limit {
        return Err(limit_error(format!(
            "EPUB resource exceeds {limit} bytes: {path}"
        )));
    }
    let capacity = usize::try_from(entry.size()).map_err(|_| {
        limit_error(format!(
            "EPUB resource is too large for this platform: {path}"
        ))
    })?;
    let mut bytes = Vec::with_capacity(capacity);
    entry
        .by_ref()
        .take(limit.saturating_add(1))
        .read_to_end(&mut bytes)
        .map_err(|error| {
            EpubError::new(
                "invalid-epub",
                format!("could not read EPUB resource {path}: {error}"),
            )
        })?;
    if bytes.len() as u64 > limit {
        return Err(limit_error(format!(
            "EPUB resource exceeds {limit} bytes: {path}"
        )));
    }
    Ok(bytes)
}

fn parse_container(bytes: &[u8]) -> Result<String, EpubError> {
    let text = std::str::from_utf8(bytes).map_err(|_| {
        EpubError::new("invalid-epub", "container.xml is not UTF-8")
    })?;
    let document = parse_xml(text, "container.xml")?;
    let root = document.root_element();
    if root.tag_name().name() != "container"
        || root.tag_name().namespace() != Some(CONTAINER_NAMESPACE)
    {
        return Err(EpubError::new(
            "invalid-epub",
            "container.xml has an invalid root element",
        ));
    }
    if root.attribute("version") != Some("1.0") {
        return Err(EpubError::new(
            "invalid-epub",
            "container.xml version must be 1.0",
        ));
    }
    let rootfile = root
        .descendants()
        .find(|node| {
            node.is_element()
                && node.tag_name().name() == "rootfile"
                && node.tag_name().namespace() == Some(CONTAINER_NAMESPACE)
        })
        .ok_or_else(|| {
            EpubError::new(
                "invalid-epub",
                "container.xml has no EPUB package rootfile",
            )
        })?;
    if rootfile.attribute("media-type") != Some(OPF_MEDIA_TYPE) {
        return Err(EpubError::new(
            "invalid-epub",
            "first container.xml rootfile has an invalid media-type",
        ));
    }
    let path = rootfile.attribute("full-path").ok_or_else(|| {
        EpubError::new(
            "invalid-epub",
            "container.xml rootfile has no full-path",
        )
    })?;
    normalize_archive_path(path, false)
        .map_err(|message| EpubError::new("invalid-epub", message))
}

fn parse_package(
    package_path: &str,
    bytes: &[u8],
) -> Result<(PublicationMetadata, HashMap<String, String>), EpubError> {
    let text = std::str::from_utf8(bytes).map_err(|_| {
        EpubError::new(
            "invalid-epub",
            format!("package document is not UTF-8: {package_path}"),
        )
    })?;
    let document = parse_xml(text, package_path)?;
    let root = document.root_element();
    if root.tag_name().name() != "package"
        || root.tag_name().namespace() != Some(OPF_NAMESPACE)
    {
        return Err(EpubError::new(
            "invalid-epub",
            format!("package document has an invalid root: {package_path}"),
        ));
    }
    let metadata = root
        .children()
        .find(|node| {
            node.is_element()
                && node.tag_name().name() == "metadata"
                && node.tag_name().namespace() == Some(OPF_NAMESPACE)
        })
        .ok_or_else(|| {
            EpubError::new(
                "invalid-epub",
                format!("package document has no metadata: {package_path}"),
            )
        })?;
    let unique_id = root.attribute("unique-identifier");
    let identifiers: Vec<_> = metadata
        .descendants()
        .filter(|node| is_dc_element(*node, "identifier"))
        .collect();
    let identifier = unique_id
        .and_then(|id| {
            identifiers
                .iter()
                .find(|node| node.attribute("id") == Some(id))
        })
        .or_else(|| identifiers.first())
        .and_then(|node| normalized_text(*node));

    let manifest = root
        .children()
        .find(|node| is_opf_element(*node, "manifest"))
        .ok_or_else(|| {
            EpubError::new(
                "invalid-epub",
                format!("package document has no manifest: {package_path}"),
            )
        })?;
    let resources = parse_manifest(package_path, manifest)?;
    Ok((
        PublicationMetadata {
            package_path: package_path.to_owned(),
            title: first_dc_text(metadata, "title"),
            language: first_dc_text(metadata, "language"),
            identifier,
            version: root.attribute("version").map(str::to_owned),
        },
        resources,
    ))
}

fn parse_manifest(
    package_path: &str,
    manifest: Node<'_, '_>,
) -> Result<HashMap<String, String>, EpubError> {
    let mut resources = HashMap::new();
    let mut ids = HashSet::new();
    for item in manifest
        .children()
        .filter(|node| is_opf_element(*node, "item"))
    {
        let id = required_attribute(item, "id", package_path)?;
        if !ids.insert(id) {
            return Err(EpubError::new(
                "invalid-epub",
                format!("duplicate manifest item ID: {id}"),
            ));
        }
        let href = required_attribute(item, "href", package_path)?;
        let media_type = required_attribute(item, "media-type", package_path)?;
        if !valid_media_type(media_type) {
            return Err(EpubError::new(
                "invalid-epub",
                format!("invalid manifest media type: {media_type}"),
            ));
        }
        let Some(path) = resolve_archive_reference(package_path, href)? else {
            continue;
        };
        if let Some(previous) =
            resources.insert(path.clone(), media_type.into())
            && previous != media_type
        {
            return Err(EpubError::new(
                "invalid-epub",
                format!("conflicting media types for manifest path: {path}"),
            ));
        }
    }
    Ok(resources)
}

fn required_attribute<'a>(
    node: Node<'a, '_>,
    name: &str,
    package_path: &str,
) -> Result<&'a str, EpubError> {
    node.attribute(name)
        .filter(|value| !value.is_empty())
        .ok_or_else(|| {
            EpubError::new(
                "invalid-epub",
                format!("manifest item lacks {name}: {package_path}"),
            )
        })
}

fn parse_xml<'a>(text: &'a str, name: &str) -> Result<Document<'a>, EpubError> {
    let options = ParsingOptions {
        allow_dtd: false,
        nodes_limit: MAX_XML_NODES,
        entity_resolver: None,
    };
    Document::parse_with_options(text, options).map_err(|error| {
        EpubError::new(
            "invalid-epub",
            format!("could not parse {name}: {error}"),
        )
    })
}

fn is_dc_element(node: Node<'_, '_>, name: &str) -> bool {
    node.is_element()
        && node.tag_name().name() == name
        && node.tag_name().namespace() == Some(DC_NAMESPACE)
}

fn is_opf_element(node: Node<'_, '_>, name: &str) -> bool {
    node.is_element()
        && node.tag_name().name() == name
        && node.tag_name().namespace() == Some(OPF_NAMESPACE)
}

fn first_dc_text(metadata: Node<'_, '_>, name: &str) -> Option<String> {
    metadata
        .descendants()
        .find(|node| is_dc_element(*node, name))
        .and_then(normalized_text)
}

fn normalized_text(node: Node<'_, '_>) -> Option<String> {
    let value = node
        .descendants()
        .filter(|node| node.is_text())
        .filter_map(|node| node.text())
        .flat_map(str::split_whitespace)
        .collect::<Vec<_>>()
        .join(" ");
    (!value.is_empty()).then_some(value)
}

fn limit_error(message: impl Into<String>) -> EpubError {
    EpubError::new("epub-limit-exceeded", message)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::io::Write;
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicU64, Ordering};
    use zip::ZipWriter;
    use zip::write::SimpleFileOptions;

    static TEMPORARY_ID: AtomicU64 = AtomicU64::new(1);

    const CONTAINER: &str = r#"<?xml version="1.0"?>
<container xmlns="urn:oasis:names:tc:opendocument:xmlns:container"
           version="1.0">
  <rootfiles>
    <rootfile full-path="OPS/package.opf"
              media-type="application/oebps-package+xml"/>
  </rootfiles>
</container>"#;

    const PACKAGE: &str = r#"<?xml version="1.0"?>
<package xmlns="http://www.idpf.org/2007/opf"
         unique-identifier="book-id" version="3.0">
  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
    <dc:identifier id="other-id">ignored</dc:identifier>
    <dc:identifier id="book-id">urn:uuid:123</dc:identifier>
    <dc:title>  Test   Book  </dc:title>
    <dc:language>en</dc:language>
  </metadata>
  <manifest>
    <item id="chapter" href="chapter.xhtml"
          media-type="application/xhtml+xml"/>
  </manifest>
</package>"#;

    struct TemporaryFile(PathBuf);

    impl Drop for TemporaryFile {
        fn drop(&mut self) {
            let _ = fs::remove_file(&self.0);
        }
    }

    fn valid_entries() -> Vec<(String, Vec<u8>)> {
        vec![
            ("mimetype".into(), EPUB_MIMETYPE.to_vec()),
            (CONTAINER_PATH.into(), CONTAINER.as_bytes().to_vec()),
            ("OPS/package.opf".into(), PACKAGE.as_bytes().to_vec()),
            (
                "OPS/chapter.xhtml".into(),
                b"<html><body>Chapter</body></html>".to_vec(),
            ),
        ]
    }

    fn write_archive(
        name: &str,
        entries: &[(String, Vec<u8>)],
    ) -> TemporaryFile {
        let id = TEMPORARY_ID.fetch_add(1, Ordering::Relaxed);
        let path = std::env::temp_dir().join(format!(
            "yunge-reader-epub-{}-{id}-{name}.epub",
            std::process::id()
        ));
        let file = File::create(&path).unwrap();
        let mut archive = ZipWriter::new(file);
        for (entry_name, bytes) in entries {
            let method = if entry_name == "mimetype" {
                CompressionMethod::Stored
            } else {
                CompressionMethod::Deflated
            };
            let options =
                SimpleFileOptions::default().compression_method(method);
            archive.start_file(entry_name, options).unwrap();
            archive.write_all(bytes).unwrap();
        }
        archive.finish().unwrap();
        TemporaryFile(path)
    }

    #[test]
    fn opens_valid_epub_and_reads_metadata_and_resources() {
        let file = write_archive("valid", &valid_entries());
        let mut publication = Publication::open(&file.0).unwrap();

        assert_eq!(publication.entry_count(), 4);
        assert!(publication.expanded_size() > 0);
        assert_eq!(publication.metadata().package_path, "OPS/package.opf");
        assert_eq!(publication.metadata().title.as_deref(), Some("Test Book"));
        assert_eq!(publication.metadata().language.as_deref(), Some("en"));
        assert_eq!(
            publication.metadata().identifier.as_deref(),
            Some("urn:uuid:123")
        );
        assert_eq!(publication.metadata().version.as_deref(), Some("3.0"));
        let resource = publication.read_resource("OPS/chapter.xhtml").unwrap();
        assert_eq!(resource.media_type(), "application/xhtml+xml");
        assert_eq!(resource.bytes(), b"<html><body>Chapter</body></html>");
        assert_eq!(
            publication.resource_catalog(),
            vec![PublicationResourceMetadata {
                path: "OPS/chapter.xhtml".into(),
                media_type: "application/xhtml+xml".into(),
                size: 33,
            }]
        );
    }

    #[test]
    fn archive_paths_must_be_canonical_relative_utf8_paths() {
        for path in [
            "",
            "/OPS/book.opf",
            "C:/OPS/book.opf",
            "OPS\\book.opf",
            "OPS//book.opf",
            "OPS/./book.opf",
            "OPS/../book.opf",
            "OPS/book.opf/",
            "OPS/book?.opf",
            "OPS/book.",
        ] {
            assert!(
                normalize_archive_path(path, false).is_err(),
                "accepted {path:?}"
            );
        }
        assert_eq!(
            normalize_archive_path("OPS/章.xhtml", false).unwrap(),
            "OPS/章.xhtml"
        );
        assert_eq!(
            normalize_archive_path("OPS/images/", true).unwrap(),
            "OPS/images"
        );
        let long_name = format!("OPS/{}", "x".repeat(MAX_FILE_NAME_BYTES + 1));
        assert!(normalize_archive_path(&long_name, false).is_err());
    }

    #[test]
    fn rejects_archive_with_traversal_entry() {
        let mut entries = valid_entries();
        entries.push(("../escape.xhtml".into(), b"escape".to_vec()));
        let file = write_archive("traversal", &entries);
        let error = Publication::open(&file.0).err().unwrap();

        assert_eq!(error.code(), "invalid-epub");
        assert!(error.message().contains("not normalized"));
    }

    #[test]
    fn rejects_duplicate_archive_paths() {
        let mut names = HashSet::new();
        record_archive_name(&mut names, "OPS/chapter.xhtml").unwrap();
        let error =
            record_archive_name(&mut names, "OPS/chapter.xhtml").unwrap_err();

        assert_eq!(error.code(), "invalid-epub");
        assert!(error.message().contains("duplicate"));
    }

    #[test]
    fn rejects_wrong_mimetype() {
        let mut entries = valid_entries();
        entries[0].1 = b"application/zip".to_vec();
        let file = write_archive("mimetype", &entries);
        let error = Publication::open(&file.0).err().unwrap();

        assert_eq!(error.code(), "invalid-epub");
        assert!(error.message().contains("mimetype"));
    }

    #[test]
    fn accepts_mimetype_that_is_not_the_first_entry() {
        let mut entries = valid_entries();
        entries.swap(0, 1);
        let file = write_archive("mimetype-order", &entries);
        let publication = Publication::open(&file.0).unwrap();

        assert_eq!(publication.metadata().title.as_deref(), Some("Test Book"));
    }

    #[test]
    fn rejects_missing_package_document() {
        let mut entries = valid_entries();
        entries.retain(|(name, _)| name != "OPS/package.opf");
        let file = write_archive("missing-package", &entries);
        let error = Publication::open(&file.0).err().unwrap();

        assert_eq!(error.code(), "invalid-epub");
        assert!(error.message().contains("does not exist"));
    }

    #[test]
    fn rejects_unsafe_resource_requests() {
        let file = write_archive("resource", &valid_entries());
        let mut publication = Publication::open(&file.0).unwrap();
        let error = publication.read_resource("../outside").unwrap_err();

        assert_eq!(error.code(), "invalid-epub-path");
    }

    #[test]
    fn resolves_manifest_hrefs_without_leaving_the_archive() {
        assert_eq!(
            resolve_archive_reference(
                "OPS/package.opf",
                "./Text/a%20b.xhtml#section"
            )
            .unwrap()
            .as_deref(),
            Some("OPS/Text/a b.xhtml")
        );
        assert_eq!(
            resolve_archive_reference("OPS/package.opf", "https://host/book")
                .unwrap(),
            None
        );
        for href in ["../../book", "/book", "a%2fbook", "a%5Cbook", "a%zzbook"]
        {
            assert!(
                resolve_archive_reference("OPS/package.opf", href).is_err(),
                "accepted {href:?}"
            );
        }
    }

    #[test]
    fn browser_resources_require_manifested_safe_media_types() {
        let mut entries = valid_entries();
        entries.push(("OPS/script.js".into(), b"alert(1)".to_vec()));
        entries.push(("OPS/unlisted.css".into(), b"body {}".to_vec()));
        entries[2].1 = PACKAGE
            .replace(
                "</manifest>",
                concat!(
                    "<item id=\"script\" href=\"script.js\" ",
                    "media-type=\"text/javascript\"/></manifest>"
                ),
            )
            .into_bytes();
        let file = write_archive("browser-resources", &entries);
        let mut publication = Publication::open(&file.0).unwrap();

        assert_eq!(
            publication
                .read_resource("OPS/script.js")
                .unwrap_err()
                .code(),
            "unsupported-epub-resource"
        );
        assert_eq!(
            publication
                .read_resource("OPS/unlisted.css")
                .unwrap_err()
                .code(),
            "unsupported-epub-resource"
        );
    }

    #[test]
    fn browser_resources_expose_epub_two_navigation() {
        let mut entries = valid_entries();
        entries.push(("OPS/toc.ncx".into(), b"<ncx/>".to_vec()));
        entries[2].1 = PACKAGE
            .replace(
                "</manifest>",
                concat!(
                    "<item id=\"ncx\" href=\"toc.ncx\" ",
                    "media-type=\"application/x-dtbncx+xml\"/>",
                    "</manifest>"
                ),
            )
            .into_bytes();
        let file = write_archive("epub-two-navigation", &entries);
        let mut publication = Publication::open(&file.0).unwrap();

        let resource = publication.read_resource("OPS/toc.ncx").unwrap();
        assert_eq!(resource.media_type(), "application/x-dtbncx+xml");
        assert!(publication.resource_catalog().iter().any(|resource| {
            resource.path == "OPS/toc.ncx"
                && resource.media_type == "application/x-dtbncx+xml"
        }));
    }

    #[test]
    fn rejects_entry_and_compression_limits() {
        assert_eq!(
            validate_entry_sizes("large", MAX_ENTRY_BYTES + 1, 1)
                .unwrap_err()
                .code(),
            "epub-limit-exceeded"
        );
        assert_eq!(
            validate_entry_sizes("bomb", MAX_COMPRESSION_RATIO + 1, 1)
                .unwrap_err()
                .code(),
            "epub-limit-exceeded"
        );
    }

    #[test]
    fn xml_metadata_rejects_dtd_and_excessive_nodes() {
        let dtd = "<!DOCTYPE package [<!ENTITY title 'bad'>]><package/>";
        let error = parse_xml(dtd, "package.opf").unwrap_err();
        assert!(error.message().contains("DTD"));

        let mut nodes = String::from("<metadata>");
        for _ in 0..MAX_XML_NODES {
            nodes.push_str("<meta/>");
        }
        nodes.push_str("</metadata>");
        let error = parse_xml(&nodes, "package.opf").unwrap_err();
        assert!(error.message().contains("nodes limit"));
    }

    #[test]
    fn container_uses_the_first_declared_rendition() {
        let container = format!(
            concat!(
                "<container xmlns=\"{}\" version=\"1.0\">",
                "<rootfiles>",
                "<rootfile full-path=\"bad.opf\" media-type=\"text/xml\"/>",
                "<rootfile full-path=\"good.opf\" media-type=\"{}\"/>",
                "</rootfiles></container>"
            ),
            CONTAINER_NAMESPACE, OPF_MEDIA_TYPE
        );
        let error = parse_container(container.as_bytes()).unwrap_err();

        assert!(error.message().contains("first"));
    }
}
