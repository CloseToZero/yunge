// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use std::env;
use std::error::Error;
use std::ffi::OsString;
use std::path::Path;

mod support;

#[cfg(test)]
use support::epub_fixture::build_epub;
use support::epub_fixture::{EpubEntry, write_epub};

const PACKAGE: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
<package xmlns="http://www.idpf.org/2007/opf"
         unique-identifier="book-id" version="3.0">
  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
    <dc:identifier id="book-id">urn:yunge-reader:reflow-fixture</dc:identifier>
    <dc:title>Yunge Reader Reflowable Fixture</dc:title>
    <dc:language>en</dc:language>
    <meta property="dcterms:modified">2026-01-01T00:00:00Z</meta>
  </metadata>
  <manifest>
    <item id="nav" href="nav.xhtml" properties="nav"
          media-type="application/xhtml+xml"/>
    <item id="chapter-1" href="chapter-1.xhtml"
          media-type="application/xhtml+xml"/>
    <item id="chapter-2" href="chapter-2.xhtml"
          media-type="application/xhtml+xml"/>
    <item id="chapter-3" href="chapter-3.xhtml"
          media-type="application/xhtml+xml"/>
  </manifest>
  <spine>
    <itemref idref="chapter-1"/>
    <itemref idref="chapter-2"/>
    <itemref idref="chapter-3"/>
  </spine>
</package>
"#;

const NAVIGATION: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml"
      xmlns:epub="http://www.idpf.org/2007/ops">
<head><title>Contents</title></head>
<body>
  <nav epub:type="toc">
    <h1>Contents</h1>
    <ol>
      <li><a href="chapter-1.xhtml">First chapter</a></li>
      <li><a href="chapter-2.xhtml">Second chapter</a></li>
      <li><a href="chapter-3.xhtml">Third chapter</a></li>
    </ol>
  </nav>
</body>
</html>
"#;

const USAGE: &str = "usage: reflow_epub_fixture OUTPUT.epub";

struct FixtureChapter {
    number: u8,
    name: &'static str,
    marker: &'static str,
}

const CHAPTERS: [FixtureChapter; 3] = [
    FixtureChapter {
        number: 1,
        name: "FIRST",
        marker: "amber",
    },
    FixtureChapter {
        number: 2,
        name: "SECOND",
        marker: "cedar",
    },
    FixtureChapter {
        number: 3,
        name: "THIRD",
        marker: "indigo",
    },
];

fn chapter_xhtml(chapter: &FixtureChapter) -> String {
    let paragraphs = (1..=24)
        .map(|paragraph| {
            format!(
                r#"<p id="p-{paragraph}">Chapter {number}, paragraph
{paragraph}, carries the {marker} marker through a deterministic stream of
reader text.  Its repeated sentences make line and screen movement observable
without relying on an installed font or an external resource.</p>"#,
                number = chapter.number,
                marker = chapter.marker,
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    format!(
        r#"<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
  <title>{name} chapter</title>
  <style>
    body {{ font-family: serif; margin: 0; }}
    main {{ margin: 2rem auto; max-width: 42rem; padding: 0 2rem; }}
    h1 {{ color: #264f78; }}
    p {{ line-height: 1.6; text-align: justify; }}
  </style>
</head>
<body>
  <main>
    <h1>CHAPTER {number} · {name}</h1>
    {paragraphs}
    <p id="search-beacon">cross chapter beacon {number}</p>
  </main>
</body>
</html>
"#,
        number = chapter.number,
        name = chapter.name,
    )
}

fn fixture_entries() -> Vec<EpubEntry> {
    let mut entries = vec![
        ("OPS/package.opf".to_owned(), PACKAGE.as_bytes().to_vec()),
        ("OPS/nav.xhtml".to_owned(), NAVIGATION.as_bytes().to_vec()),
    ];
    for chapter in &CHAPTERS {
        entries.push((
            format!("OPS/chapter-{}.xhtml", chapter.number),
            chapter_xhtml(chapter).into_bytes(),
        ));
    }
    entries
}

#[cfg(test)]
fn build_fixture() -> Result<Vec<u8>, Box<dyn Error>> {
    build_epub(fixture_entries())
}

fn write_fixture(path: &Path) -> Result<(), Box<dyn Error>> {
    write_epub(path, fixture_entries())
}

fn parse_arguments() -> Result<OsString, Box<dyn Error>> {
    let mut arguments = env::args_os().skip(1);
    let output = arguments.next().ok_or(USAGE)?;
    if arguments.next().is_some() {
        return Err(USAGE.into());
    }
    Ok(output)
}

fn main() -> Result<(), Box<dyn Error>> {
    let output = parse_arguments()?;
    write_fixture(Path::new(&output))?;
    println!("Wrote {} (reflowable)", Path::new(&output).display());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::io::{Cursor, Read};
    use std::time::{SystemTime, UNIX_EPOCH};
    use yunge_reader::epub::{Publication, PublicationLayout};
    use zip::{CompressionMethod, ZipArchive};

    #[test]
    fn fixture_has_reflowable_chapters() {
        let bytes = build_fixture().unwrap();
        assert_eq!(bytes, build_fixture().unwrap());
        let mut archive = ZipArchive::new(Cursor::new(bytes)).unwrap();
        assert_eq!(archive.len(), 7);

        let first = archive.by_index(0).unwrap();
        assert_eq!(first.name(), "mimetype");
        assert_eq!(first.compression(), CompressionMethod::Stored);
        drop(first);

        let mut package = String::new();
        archive
            .by_name("OPS/package.opf")
            .unwrap()
            .read_to_string(&mut package)
            .unwrap();
        assert!(!package.contains("rendition:layout"));
        for chapter in &CHAPTERS {
            let mut contents = String::new();
            archive
                .by_name(&format!("OPS/chapter-{}.xhtml", chapter.number))
                .unwrap()
                .read_to_string(&mut contents)
                .unwrap();
            assert!(contents.contains(&format!("CHAPTER {}", chapter.number)));
            assert!(contents.contains(chapter.marker));
            assert!(contents.contains("id=\"p-24\""));
            assert!(
                contents.contains(&format!(
                    "cross chapter beacon {}",
                    chapter.number
                ))
            );
        }
    }

    #[test]
    fn fixture_opens_as_a_reflowable_publication() {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let path = env::temp_dir().join(format!(
            "yunge-reader-reflow-{}-{nonce}.epub",
            std::process::id()
        ));
        write_fixture(&path).unwrap();
        let publication = Publication::open(&path).unwrap();
        assert_eq!(
            publication.metadata().layout,
            PublicationLayout::Reflowable
        );
        assert_eq!(publication.entry_count(), 7);
        drop(publication);
        fs::remove_file(path).unwrap();
    }
}
