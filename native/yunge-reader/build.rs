// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use sha2::{Digest, Sha256};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

fn source_files(directory: &Path, files: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(directory).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_dir() {
            source_files(&path, files);
        } else {
            files.push(path);
        }
    }
}

fn normalized_contents(path: &Path) -> String {
    fs::read_to_string(path)
        .unwrap()
        .replace("\r\n", "\n")
        .replace('\r', "\n")
}

fn main() {
    let root = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
    let source = root.join("src");
    let mut files = vec![
        root.join("Cargo.lock"),
        root.join("Cargo.toml"),
        root.join("build.rs"),
        root.join("module/Cargo.toml"),
        root.join("pdfium-manifest.eld"),
    ];
    source_files(&root.join("module/src"), &mut files);
    source_files(&source, &mut files);
    source_files(&root.join("renderer"), &mut files);
    files.sort();

    println!("cargo:rerun-if-changed=Cargo.lock");
    println!("cargo:rerun-if-changed=Cargo.toml");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=module");
    println!("cargo:rerun-if-changed=pdfium-manifest.eld");
    println!("cargo:rerun-if-changed=renderer");
    println!("cargo:rerun-if-changed=source.sha256");
    println!("cargo:rerun-if-changed=src");

    let mut hasher = Sha256::new();
    for path in files {
        let relative = path
            .strip_prefix(&root)
            .unwrap()
            .to_string_lossy()
            .replace('\\', "/");
        hasher.update(relative.as_bytes());
        hasher.update([0]);
        hasher.update(normalized_contents(&path).as_bytes());
        hasher.update([0]);
    }
    let build_id = format!("{:x}", hasher.finalize());
    let expected = fs::read_to_string(root.join("source.sha256"))
        .expect("native/yunge-reader/source.sha256 is missing");
    assert_eq!(
        expected.trim(),
        build_id,
        "native/yunge-reader/source.sha256 is stale; expected {build_id}"
    );
    let output = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    fs::write(output.join("build-id"), format!("{build_id}\n")).unwrap();
    println!("cargo:rustc-env=YUNGE_READER_BUILD_ID={build_id}");
}
