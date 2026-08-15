// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::io::{self, BufRead, Write};

type Error = Box<dyn std::error::Error>;

const PROTOCOL_VERSION: u32 = 1;
const BUILD_ID: &str = env!("YUNGE_READER_BUILD_ID");

#[derive(Debug, Deserialize)]
struct Request {
    id: u64,
    op: String,
    #[serde(default, rename = "params")]
    _params: Value,
}

#[derive(Debug, Serialize)]
struct Ready<'a> {
    kind: &'static str,
    protocol: u32,
    #[serde(rename = "build-id")]
    build_id: &'a str,
    capabilities: [&'static str; 1],
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

fn ready_message() -> Ready<'static> {
    Ready {
        kind: "ready",
        protocol: PROTOCOL_VERSION,
        build_id: BUILD_ID,
        capabilities: ["lifecycle"],
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

fn handle_request(request: Request) -> (Response, Control) {
    match request.op.as_str() {
        "ping" => (
            Response::success(
                request.id,
                json!({
                    "protocol": PROTOCOL_VERSION,
                    "build-id": BUILD_ID,
                    "backend": "none",
                    "capabilities": ["lifecycle"],
                }),
            ),
            Control::Continue,
        ),
        "shutdown" => (
            Response::success(request.id, json!({ "stopped": true })),
            Control::Shutdown,
        ),
        _ => (
            Response::failure(
                Some(request.id),
                "unsupported-operation",
                format!("unsupported operation: {}", request.op),
            ),
            Control::Continue,
        ),
    }
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
    write_message(&mut output, &ready_message())?;
    for line in input.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let (response, control) = match serde_json::from_str(&line) {
            Ok(request) => handle_request(request),
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
        assert_eq!(value["capabilities"], json!(["lifecycle"]));
    }

    #[test]
    fn ping_reports_the_available_backend() {
        let output = messages(r#"{"id":7,"op":"ping","params":{}}"#);
        assert_eq!(output.len(), 2);
        assert_eq!(output[1]["id"], 7);
        assert_eq!(output[1]["ok"], true);
        assert_eq!(output[1]["result"]["backend"], "none");
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
    fn end_of_input_exits_cleanly() {
        let output = messages("");
        assert_eq!(output.len(), 1);
        assert_eq!(output[0]["kind"], "ready");
    }
}
