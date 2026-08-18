// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use std::error::Error;
use std::fs::OpenOptions;
use std::io::{Cursor, Seek, Write};
use std::path::Path;
use zip::write::SimpleFileOptions;
use zip::{CompressionMethod, ZipWriter};

const MIMETYPE: &[u8] = b"application/epub+zip";

const CONTAINER: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
<container xmlns="urn:oasis:names:tc:opendocument:xmlns:container"
           version="1.0">
  <rootfiles>
    <rootfile full-path="OPS/package.opf"
              media-type="application/oebps-package+xml"/>
  </rootfiles>
</container>
"#;

pub type EpubEntry = (String, Vec<u8>);

fn add_entry<W>(
    archive: &mut ZipWriter<W>,
    name: &str,
    bytes: &[u8],
    method: CompressionMethod,
) -> Result<(), Box<dyn Error>>
where
    W: Write + Seek,
{
    let options = SimpleFileOptions::default().compression_method(method);
    archive.start_file(name, options)?;
    archive.write_all(bytes)?;
    Ok(())
}

pub fn build_epub(entries: Vec<EpubEntry>) -> Result<Vec<u8>, Box<dyn Error>> {
    let mut archive = ZipWriter::new(Cursor::new(Vec::new()));
    add_entry(
        &mut archive,
        "mimetype",
        MIMETYPE,
        CompressionMethod::Stored,
    )?;
    add_entry(
        &mut archive,
        "META-INF/container.xml",
        CONTAINER.as_bytes(),
        CompressionMethod::Deflated,
    )?;
    for (name, contents) in entries {
        add_entry(&mut archive, &name, &contents, CompressionMethod::Deflated)?;
    }
    Ok(archive.finish()?.into_inner())
}

pub fn write_epub(
    path: &Path,
    entries: Vec<EpubEntry>,
) -> Result<(), Box<dyn Error>> {
    let bytes = build_epub(entries)?;
    let mut output =
        OpenOptions::new().write(true).create_new(true).open(path)?;
    output.write_all(&bytes)?;
    output.flush()?;
    Ok(())
}
