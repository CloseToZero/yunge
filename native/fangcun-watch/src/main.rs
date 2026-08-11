// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use notify::event::{CreateKind, ModifyKind, RemoveKind};
use notify::{Event, EventKind, RecursiveMode, Watcher};
use serde::Serialize;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync::mpsc;
use std::time::UNIX_EPOCH;

type Error = Box<dyn std::error::Error>;

const BUILD_ID: &str = env!("FANGCUN_BUILD_ID");

// Emacs sends a command and YIYU ROOT pairs as process arguments.  The helper
// sends UTF-8 NDJSON on stdout and never uses stdin.  Nothing except protocol
// messages may be written to stdout.  See the protocol in ../README.org.
#[derive(Serialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
enum Message {
    /// Starts every successful response and identifies the exact source build.
    Ready {
        #[serde(rename = "build-id")]
        build_id: &'static str,
    },
    /// Describes one Org file during a scan.
    State {
        yiyu: String,
        file: String,
        mtime: f64,
        size: u64,
    },
    /// Identifies Org files affected by an unambiguous watch event.
    Event { paths: Vec<String> },
    /// Requests a full incremental sync after an ambiguous watch event.
    Rescan,
    /// Reports a watch backend error or a fatal process error.
    Error { message: String },
}

fn ready_message() -> Message {
    Message::Ready { build_id: BUILD_ID }
}

fn write_message(
    mut output: impl Write,
    message: &Message,
) -> Result<(), Error> {
    // One flushed object per line lets Emacs consume a long-lived watch
    // process without waiting for a full output buffer.
    serde_json::to_writer(&mut output, message)?;
    output.write_all(b"\n")?;
    output.flush()?;
    Ok(())
}

fn roots(
    arguments: impl Iterator<Item = String>,
) -> Result<Vec<(String, PathBuf)>, Error> {
    let arguments: Vec<_> = arguments.collect();
    if arguments.is_empty() || arguments.len() % 2 != 0 {
        return Err("expected one or more YIYU ROOT pairs".into());
    }
    Ok(arguments
        .chunks_exact(2)
        .map(|pair| (pair[0].clone(), PathBuf::from(&pair[1])))
        .collect())
}

fn sorted_entries(directory: &Path) -> Result<Vec<fs::DirEntry>, Error> {
    let mut entries =
        fs::read_dir(directory)?.collect::<Result<Vec<_>, _>>()?;
    entries.sort_by_key(|entry| entry.file_name());
    Ok(entries)
}

fn scan_directory(
    yiyu: &str,
    directory: &Path,
    states: &mut Vec<Message>,
) -> Result<(), Error> {
    for entry in sorted_entries(directory)? {
        let file_type = entry.file_type()?;
        if file_type.is_dir() {
            scan_directory(yiyu, &entry.path(), states)?;
        } else if file_type.is_file()
            && entry.path().extension() == Some(OsStr::new("org"))
        {
            let metadata = entry.metadata()?;
            let mtime = metadata.modified()?.duration_since(UNIX_EPOCH)?;
            states.push(Message::State {
                yiyu: yiyu.to_owned(),
                file: entry.path().to_string_lossy().into_owned(),
                mtime: mtime.as_secs_f64(),
                size: metadata.len(),
            });
        }
    }
    Ok(())
}

fn scan(roots: &[(String, PathBuf)]) -> Result<(), Error> {
    let stdout = io::stdout();
    let mut output = stdout.lock();
    write_message(&mut output, &ready_message())?;
    for (yiyu, root) in roots {
        let mut states = Vec::new();
        scan_directory(yiyu, root, &mut states)?;
        for state in states {
            write_message(&mut output, &state)?;
        }
    }
    Ok(())
}

fn org_path(path: &Path) -> bool {
    path.extension() == Some(OsStr::new("org"))
}

fn structural_change(event: &Event) -> bool {
    match event.kind {
        EventKind::Create(CreateKind::Folder)
        | EventKind::Remove(RemoveKind::Folder)
        | EventKind::Remove(RemoveKind::Any) => true,
        EventKind::Modify(ModifyKind::Name(_)) => {
            event.paths.iter().any(|path| !org_path(path))
        }
        EventKind::Any | EventKind::Other => true,
        _ => false,
    }
}

fn event_message(event: Event) -> Option<Message> {
    if event.need_rescan() || structural_change(&event) {
        return Some(Message::Rescan);
    }
    let paths = event
        .paths
        .into_iter()
        .filter(|path| org_path(path))
        .map(|path| path.to_string_lossy().into_owned())
        .collect::<Vec<_>>();
    (!paths.is_empty()).then_some(Message::Event { paths })
}

fn watch(roots: &[(String, PathBuf)]) -> Result<(), Error> {
    let (sender, receiver) = mpsc::channel();
    let mut watcher = notify::recommended_watcher(sender)?;
    for (_, root) in roots {
        watcher.watch(root, RecursiveMode::Recursive)?;
    }

    let stdout = io::stdout();
    let mut output = stdout.lock();
    write_message(&mut output, &ready_message())?;
    for result in receiver {
        let message = match result {
            Ok(event) => event_message(event),
            Err(error) => Some(Message::Error {
                message: error.to_string(),
            }),
        };
        if let Some(message) = message {
            write_message(&mut output, &message)?;
        }
    }
    Ok(())
}

fn run() -> Result<(), Error> {
    let mut arguments = env::args().skip(1);
    let command = arguments.next().ok_or("expected scan or watch")?;
    let roots = roots(arguments)?;
    match command.as_str() {
        "scan" => scan(&roots),
        "watch" => watch(&roots),
        _ => Err(format!("unknown command: {command}").into()),
    }
}

fn main() {
    if let Err(error) = run() {
        let _ = write_message(
            io::stdout().lock(),
            &Message::Error {
                message: error.to_string(),
            },
        );
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{Duration, Instant};

    #[test]
    fn scan_finds_org_files_below_the_root() {
        let root = env::temp_dir()
            .join(format!("fangcun-watch-test-{}", std::process::id()));
        let _ = fs::remove_dir_all(&root);
        let nested = root.join("nested");
        fs::create_dir_all(&nested).unwrap();
        fs::write(root.join("root.org"), "root").unwrap();
        fs::write(nested.join("nested.org"), "nested").unwrap();
        fs::write(nested.join("ignored.txt"), "ignored").unwrap();

        let mut states = Vec::new();
        scan_directory("test", &root, &mut states).unwrap();

        assert_eq!(states.len(), 2);
        let mut files = states
            .iter()
            .map(|state| match state {
                Message::State { yiyu, file, .. } => {
                    assert_eq!(yiyu, "test");
                    Path::new(file)
                        .file_name()
                        .unwrap()
                        .to_string_lossy()
                        .into_owned()
                }
                _ => panic!("scan returned a non-state message"),
            })
            .collect::<Vec<_>>();
        files.sort();
        assert_eq!(files, ["nested.org", "root.org"]);

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn ready_reports_the_source_build_id() {
        let value = serde_json::to_value(ready_message()).unwrap();
        assert_eq!(BUILD_ID, include_str!("../source.sha256").trim());
        assert_eq!(value["kind"], "ready");
        assert_eq!(value["build-id"], BUILD_ID);
    }

    #[test]
    fn messages_are_newline_delimited_json() {
        let cases = [
            (
                Message::State {
                    yiyu: "work".to_owned(),
                    file: "note.org".to_owned(),
                    mtime: 1.5,
                    size: 7,
                },
                concat!(
                    r#"{"kind":"state","yiyu":"work","file":"note.org","#,
                    r#""mtime":1.5,"size":7}"#,
                    "\n",
                ),
            ),
            (
                Message::Event {
                    paths: vec!["note.org".to_owned()],
                },
                "{\"kind\":\"event\",\"paths\":[\"note.org\"]}\n",
            ),
            (Message::Rescan, "{\"kind\":\"rescan\"}\n"),
            (
                Message::Error {
                    message: "failed".to_owned(),
                },
                "{\"kind\":\"error\",\"message\":\"failed\"}\n",
            ),
        ];

        for (message, expected) in cases {
            let mut output = Vec::new();
            write_message(&mut output, &message).unwrap();
            assert_eq!(String::from_utf8(output).unwrap(), expected);
        }
    }

    #[test]
    fn watcher_reports_org_files_below_the_root() {
        let root = env::temp_dir()
            .join(format!("fangcun-watch-notify-test-{}", std::process::id()));
        let _ = fs::remove_dir_all(&root);
        let nested = root.join("nested");
        fs::create_dir_all(&nested).unwrap();

        let (sender, receiver) = mpsc::channel();
        let mut watcher = notify::recommended_watcher(sender).unwrap();
        watcher.watch(&root, RecursiveMode::Recursive).unwrap();

        let note = nested.join("created.org");
        fs::write(&note, "created").unwrap();
        let deadline = Instant::now() + Duration::from_secs(5);
        let mut reported = false;
        while Instant::now() < deadline {
            let timeout = deadline.saturating_duration_since(Instant::now());
            let event = receiver.recv_timeout(timeout).unwrap().unwrap();
            if event.paths.iter().any(|path| path == &note) {
                reported = true;
                break;
            }
        }
        assert!(reported);

        drop(watcher);
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn ambiguous_removal_requests_a_rescan() {
        let event = Event::new(EventKind::Remove(RemoveKind::Any))
            .add_path(PathBuf::from("removed-directory"));
        assert!(matches!(event_message(event), Some(Message::Rescan)));
    }
}
