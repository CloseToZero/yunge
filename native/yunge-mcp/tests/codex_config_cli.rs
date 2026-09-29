// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use std::{
    io::Write,
    process::{Command, Output, Stdio},
};

use serde_json::{Value, json};
use toml_edit::DocumentMut;

fn run(args: &[&str], input: &str) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_yunge-mcp"))
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(input.as_bytes())
        .unwrap();
    child.wait_with_output().unwrap()
}

#[test]
fn codex_edit_is_one_json_round_trip_and_failures_emit_no_configuration() {
    let request = json!({
        "program": "C:/工具/yunge-mcp.exe",
        "configuration": "[mcp_servers.other]\ncommand = 'other'\n"
    });
    let output = run(&["edit-codex-config"], &request.to_string());
    assert!(output.status.success());
    let response: Value = serde_json::from_slice(&output.stdout).unwrap();
    let config: DocumentMut = response["configuration"].as_str().unwrap().parse().unwrap();
    assert_eq!(
        config["mcp_servers"]["other"]["command"].as_str(),
        Some("other")
    );
    assert_eq!(
        config["mcp_servers"]["yunge"]["command"].as_str(),
        Some("C:/工具/yunge-mcp.exe")
    );

    let invalid = json!({
        "program": "helper",
        "configuration": "secret = 'do-not-print-this'\n[broken\n"
    });
    let output = run(&["edit-codex-config"], &invalid.to_string());
    assert!(!output.status.success());
    assert!(output.stdout.is_empty());
    assert!(!String::from_utf8_lossy(&output.stderr).contains("do-not-print-this"));

    let output = run(&["unknown-command"], "");
    assert!(!output.status.success());
    assert!(output.stdout.is_empty());
}
