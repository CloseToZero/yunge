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

const PACKAGE_TEMPLATE: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
<package xmlns="http://www.idpf.org/2007/opf"
         unique-identifier="book-id" version="3.0">
  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
    <dc:identifier id="book-id">urn:yunge-reader:fixed-fixture</dc:identifier>
    <dc:title>Yunge Reader Fixed Layout Fixture</dc:title>
    <dc:language>{{LANGUAGE}}</dc:language>
    <meta property="dcterms:modified">2026-01-01T00:00:00Z</meta>
    <meta property="rendition:layout">pre-paginated</meta>
    <meta property="rendition:spread">none</meta>
  </metadata>
  <manifest>
    <item id="nav" href="nav.xhtml" properties="nav"
          media-type="application/xhtml+xml"/>
    <item id="page-1" href="page-1.xhtml"
          media-type="application/xhtml+xml"/>
    <item id="page-2" href="page-2.xhtml"
          media-type="application/xhtml+xml"/>
    <item id="page-3" href="page-3.xhtml"
          media-type="application/xhtml+xml"/>
  </manifest>
  <spine page-progression-direction="{{PROGRESSION}}">
    <itemref idref="page-1"/>
    <itemref idref="page-2"/>
    <itemref idref="page-3"/>
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
      <li><a href="page-1.xhtml">Red page</a></li>
      <li><a href="page-2.xhtml">Green page</a></li>
      <li><a href="page-3.xhtml">Blue page</a></li>
    </ol>
  </nav>
</body>
</html>
"#;

const USAGE: &str = concat!(
    "usage: fixed_epub_fixture ",
    "[--variant ltr|rtl|vertical-rl] OUTPUT.epub"
);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Variant {
    Ltr,
    Rtl,
    VerticalRl,
}

impl Variant {
    #[cfg(test)]
    const ALL: [Self; 3] = [Self::Ltr, Self::Rtl, Self::VerticalRl];

    fn parse(value: &OsString) -> Result<Self, Box<dyn Error>> {
        match value.to_str() {
            Some("ltr") => Ok(Self::Ltr),
            Some("rtl") => Ok(Self::Rtl),
            Some("vertical-rl") => Ok(Self::VerticalRl),
            _ => Err(USAGE.into()),
        }
    }

    fn slug(self) -> &'static str {
        match self {
            Self::Ltr => "ltr",
            Self::Rtl => "rtl",
            Self::VerticalRl => "vertical-rl",
        }
    }

    fn language(self) -> &'static str {
        match self {
            Self::Rtl => "he",
            Self::VerticalRl => "ja",
            Self::Ltr => "en",
        }
    }

    fn progression(self) -> &'static str {
        match self {
            Self::Ltr => "ltr",
            Self::Rtl | Self::VerticalRl => "rtl",
        }
    }

    fn content_direction(self) -> &'static str {
        match self {
            Self::Rtl => "rtl",
            Self::Ltr | Self::VerticalRl => "ltr",
        }
    }

    fn writing_mode(self) -> &'static str {
        match self {
            Self::VerticalRl => "vertical-rl",
            Self::Ltr | Self::Rtl => "horizontal-tb",
        }
    }

    fn sample(self) -> &'static str {
        match self {
            Self::Ltr => "FIRST · SECOND · THIRD",
            Self::Rtl => "ראשון · שני · שלישי",
            Self::VerticalRl => "天地玄黃　宇宙洪荒",
        }
    }
}

fn package_document(variant: Variant) -> String {
    PACKAGE_TEMPLATE
        .replace("{{LANGUAGE}}", variant.language())
        .replace("{{PROGRESSION}}", variant.progression())
}

struct FixturePage {
    number: u8,
    name: &'static str,
    background: &'static str,
    foreground: &'static str,
}

const PAGES: [FixturePage; 3] = [
    FixturePage {
        number: 1,
        name: "RED",
        background: "#fff0f0",
        foreground: "#8a1515",
    },
    FixturePage {
        number: 2,
        name: "GREEN",
        background: "#effbef",
        foreground: "#145c2b",
    },
    FixturePage {
        number: 3,
        name: "BLUE",
        background: "#eef5ff",
        foreground: "#174f88",
    },
];

fn page_xhtml(page: &FixturePage, variant: Variant) -> String {
    format!(
        r#"<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" lang="{language}"
      dir="{direction}">
<head>
  <title>{name} page</title>
  <meta name="viewport" content="width=800, height=1200"/>
  <style>
    html, body {{ margin: 0; width: 800px; height: 1200px; }}
    body {{ background: {background}; color: {foreground};
            font-family: sans-serif; overflow: hidden; }}
    .frame {{ position: absolute; inset: 40px;
              border: 8px solid currentColor; }}
    .title {{ position: absolute; inset: 510px 0 auto;
              text-align: center; font-size: 72px; font-weight: bold; }}
    .axis-x {{ position: absolute; left: 40px; right: 40px; top: 600px;
               border-top: 2px dashed currentColor; }}
    .axis-y {{ position: absolute; top: 40px; bottom: 40px; left: 400px;
               border-left: 2px dashed currentColor; }}
    .mark {{ position: absolute; font: bold 28px monospace; }}
    .direction {{ position: absolute; inset: 760px 180px 180px;
                  display: flex; align-items: center; justify-content: center;
                  border: 3px dotted currentColor; font-size: 38px;
                  writing-mode: {writing_mode}; direction: {direction}; }}
    .tl {{ left: 58px; top: 58px; }}
    .tr {{ right: 58px; top: 58px; }}
    .bl {{ left: 58px; bottom: 58px; }}
    .br {{ right: 58px; bottom: 58px; }}
  </style>
</head>
<body>
  <div class="frame"></div>
  <div class="axis-x"></div><div class="axis-y"></div>
  <div class="mark tl">0,0</div>
  <div class="mark tr">800,0</div>
  <div class="mark bl">0,1200</div>
  <div class="mark br">800,1200</div>
  <div class="title">PAGE {number} · {name}</div>
  <div class="direction">{sample}</div>
</body>
</html>
"#,
        number = page.number,
        name = page.name,
        background = page.background,
        foreground = page.foreground,
        language = variant.language(),
        direction = variant.content_direction(),
        writing_mode = variant.writing_mode(),
        sample = variant.sample(),
    )
}

fn fixture_entries(variant: Variant) -> Vec<EpubEntry> {
    let mut entries = vec![
        (
            "OPS/package.opf".to_owned(),
            package_document(variant).into_bytes(),
        ),
        ("OPS/nav.xhtml".to_owned(), NAVIGATION.as_bytes().to_vec()),
    ];
    for page in &PAGES {
        entries.push((
            format!("OPS/page-{}.xhtml", page.number),
            page_xhtml(page, variant).into_bytes(),
        ));
    }
    entries
}

#[cfg(test)]
fn build_fixture(variant: Variant) -> Result<Vec<u8>, Box<dyn Error>> {
    build_epub(fixture_entries(variant))
}

fn write_fixture(path: &Path, variant: Variant) -> Result<(), Box<dyn Error>> {
    write_epub(path, fixture_entries(variant))
}

fn parse_arguments() -> Result<(Variant, OsString), Box<dyn Error>> {
    let mut arguments = env::args_os().skip(1);
    let first = arguments.next().ok_or(USAGE)?;
    let (variant, output) = if first == "--variant" {
        let variant = arguments.next().ok_or(USAGE)?;
        let output = arguments.next().ok_or(USAGE)?;
        (Variant::parse(&variant)?, output)
    } else {
        (Variant::Ltr, first)
    };
    if arguments.next().is_some() {
        return Err(USAGE.into());
    }
    Ok((variant, output))
}

fn main() -> Result<(), Box<dyn Error>> {
    let (variant, output) = parse_arguments()?;
    write_fixture(Path::new(&output), variant)?;
    println!(
        "Wrote {} ({})",
        Path::new(&output).display(),
        variant.slug()
    );
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
    fn fixture_has_fixed_layout_structure_and_geometry() {
        let bytes = build_fixture(Variant::Ltr).unwrap();
        assert_eq!(bytes, build_fixture(Variant::Ltr).unwrap());
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
        assert!(package.contains("pre-paginated"));
        assert!(package.contains("rendition:spread\">none"));
        assert!(package.contains("page-progression-direction=\"ltr\""));

        for page in &PAGES {
            let mut contents = String::new();
            archive
                .by_name(&format!("OPS/page-{}.xhtml", page.number))
                .unwrap()
                .read_to_string(&mut contents)
                .unwrap();
            assert!(contents.contains("width=800, height=1200"));
            assert!(contents.contains(&format!("PAGE {}", page.number)));
            assert!(contents.contains("800,1200"));
        }
    }

    #[test]
    fn fixture_variants_encode_progression_and_writing_mode() {
        for variant in Variant::ALL {
            let bytes = build_fixture(variant).unwrap();
            let mut archive = ZipArchive::new(Cursor::new(bytes)).unwrap();
            let mut package = String::new();
            archive
                .by_name("OPS/package.opf")
                .unwrap()
                .read_to_string(&mut package)
                .unwrap();
            assert!(package.contains(&format!(
                "page-progression-direction=\"{}\"",
                variant.progression()
            )));
            assert!(package.contains(&format!(
                "<dc:language>{}</dc:language>",
                variant.language()
            )));

            let mut page = String::new();
            archive
                .by_name("OPS/page-1.xhtml")
                .unwrap()
                .read_to_string(&mut page)
                .unwrap();
            assert!(page.contains(&format!(
                "writing-mode: {}",
                variant.writing_mode()
            )));
            assert!(
                page.contains(&format!(
                    "dir=\"{}\"",
                    variant.content_direction()
                ))
            );
            assert!(page.contains(variant.sample()));
        }
    }

    #[test]
    fn fixture_variants_open_as_fixed_publications() {
        for variant in Variant::ALL {
            let nonce = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos();
            let path = env::temp_dir().join(format!(
                "yunge-reader-fixed-{}-{}-{nonce}.epub",
                variant.slug(),
                std::process::id()
            ));
            write_fixture(&path, variant).unwrap();
            let publication = Publication::open(&path).unwrap();
            assert_eq!(
                publication.metadata().layout,
                PublicationLayout::PrePaginated
            );
            assert_eq!(publication.entry_count(), 7);
            drop(publication);
            fs::remove_file(path).unwrap();
        }
    }
}
