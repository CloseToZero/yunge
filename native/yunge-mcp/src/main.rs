// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use std::{
    env,
    error::Error,
    ffi::OsString,
    fs,
    path::{Path, PathBuf},
    process::Stdio,
    sync::Arc,
};

use base64::{Engine as _, engine::general_purpose::STANDARD};
use rmcp::{
    ErrorData as McpError, ServerHandler, ServiceExt,
    model::{
        CallToolRequestParams, CallToolResponse, CallToolResult, ContentBlock,
        Implementation, ListToolsResult, PaginatedRequestParams,
        ServerCapabilities, ServerInfo, Tool,
    },
    service::{RequestContext, RoleServer},
    transport::stdio,
};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use tokio::process::Command;

const DISPATCH_FORM: &str =
    "(progn (require 'yunge-mcp) (yunge-mcp-server-dispatch))";
const BUILD_ID: &str = env!("YUNGE_MCP_BUILD_ID");

#[derive(Clone, Debug)]
struct EmacsBridge {
    program: OsString,
    connection_arguments: Vec<OsString>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RuntimeConfig {
    version: u32,
    emacsclient: PathBuf,
    connection_arguments: Vec<String>,
}

#[derive(Debug, Serialize)]
struct BridgeRequest<'a> {
    operation: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    arguments: Option<&'a Map<String, Value>>,
}

#[derive(Debug, Deserialize)]
struct BridgeResponse {
    ok: bool,
    value: Option<Value>,
    error: Option<BridgeResponseError>,
}

#[derive(Debug, Deserialize)]
struct BridgeResponseError {
    #[serde(rename = "type")]
    kind: String,
    message: String,
}

#[derive(Debug)]
struct BridgeError(String);

impl std::fmt::Display for BridgeError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl Error for BridgeError {}

impl EmacsBridge {
    fn request_argument(
        request: &BridgeRequest<'_>,
    ) -> Result<String, BridgeError> {
        let request_json = serde_json::to_vec(request)
            .map_err(|error| BridgeError(error.to_string()))?;
        Ok(STANDARD.encode(request_json))
    }

    fn runtime_file() -> Option<PathBuf> {
        if let Some(file) = env::var_os("YUNGE_MCP_RUNTIME") {
            return Some(file.into());
        }
        let executable = env::current_exe().ok()?;
        Some(executable.parent()?.parent()?.join("runtime.json"))
    }

    fn runtime_config_at(
        file: &Path,
    ) -> Result<Option<RuntimeConfig>, BridgeError> {
        if !file.exists() {
            return Ok(None);
        }
        let bytes = fs::read(file).map_err(|error| {
            BridgeError(format!(
                "could not read runtime manifest {}: {error}",
                file.display()
            ))
        })?;
        let config: RuntimeConfig =
            serde_json::from_slice(&bytes).map_err(|error| {
                BridgeError(format!(
                    "invalid runtime manifest {}: {error}",
                    file.display()
                ))
            })?;
        if config.version != 1 {
            return Err(BridgeError(format!(
                "unsupported runtime manifest version {}",
                config.version
            )));
        }
        Ok(Some(config))
    }

    fn runtime_config() -> Result<Option<RuntimeConfig>, BridgeError> {
        let Some(file) = Self::runtime_file() else {
            return Ok(None);
        };
        Self::runtime_config_at(&file)
    }

    fn from_sources(
        config: Option<RuntimeConfig>,
        program_override: Option<OsString>,
        server_file_override: Option<OsString>,
    ) -> Self {
        let program = program_override
            .or_else(|| {
                config
                    .as_ref()
                    .map(|runtime| runtime.emacsclient.clone().into_os_string())
            })
            .unwrap_or_else(|| OsString::from("emacsclient"));
        let connection_arguments = if let Some(file) = server_file_override {
            vec![OsString::from("--server-file"), file]
        } else {
            config
                .map(|runtime| {
                    runtime
                        .connection_arguments
                        .into_iter()
                        .map(OsString::from)
                        .collect()
                })
                .unwrap_or_default()
        };
        Self {
            program,
            connection_arguments,
        }
    }

    fn from_environment() -> Result<Self, BridgeError> {
        Ok(Self::from_sources(
            Self::runtime_config()?,
            env::var_os("YUNGE_EMACSCLIENT"),
            env::var_os("YUNGE_EMACS_SERVER_FILE"),
        ))
    }

    fn command_arguments(
        &self,
        request: &BridgeRequest<'_>,
    ) -> Result<Vec<OsString>, BridgeError> {
        let request_argument = Self::request_argument(request)?;
        let mut arguments = self.connection_arguments.clone();
        arguments.extend([
            OsString::from("--eval"),
            OsString::from(DISPATCH_FORM),
            OsString::from(BUILD_ID),
            OsString::from(request_argument),
        ]);
        Ok(arguments)
    }

    fn decode_response(stdout: &[u8]) -> Result<Value, BridgeError> {
        let printed = std::str::from_utf8(stdout)
            .map_err(|error| BridgeError(error.to_string()))?;
        let encoded: String =
            serde_json::from_str(printed.trim()).map_err(|error| {
                BridgeError(format!(
                    "invalid response from emacsclient: {error}"
                ))
            })?;
        let response_json = STANDARD.decode(encoded).map_err(|error| {
            BridgeError(format!("invalid response encoding: {error}"))
        })?;
        let response: BridgeResponse = serde_json::from_slice(&response_json)
            .map_err(|error| {
            BridgeError(format!("invalid response from Yunge: {error}"))
        })?;
        if response.ok {
            Ok(response.value.unwrap_or(Value::Null))
        } else {
            let error = response.error.ok_or_else(|| {
                BridgeError("Yunge returned an unspecified error".into())
            })?;
            Err(BridgeError(format!(
                "Yunge {}: {}",
                error.kind, error.message
            )))
        }
    }

    async fn request(
        &self,
        request: &BridgeRequest<'_>,
    ) -> Result<Value, BridgeError> {
        let arguments = self.command_arguments(request)?;
        let mut command = Command::new(&self.program);
        let output = command
            .args(arguments)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .kill_on_drop(true)
            .output()
            .await
            .map_err(|error| {
                BridgeError(format!("could not run emacsclient: {error}"))
            })?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let status = output.status.code().map_or_else(
                || output.status.to_string(),
                |code| code.to_string(),
            );
            return Err(BridgeError(format!(
                "emacsclient failed with status {}: {}",
                status,
                stderr.trim()
            )));
        }
        Self::decode_response(&output.stdout)
    }

    async fn list_tools(&self) -> Result<Vec<Tool>, BridgeError> {
        let value = self
            .request(&BridgeRequest {
                operation: "list-tools",
                name: None,
                arguments: None,
            })
            .await?;
        serde_json::from_value(value).map_err(|error| {
            BridgeError(format!("invalid Yunge tool descriptions: {error}"))
        })
    }

    async fn call_tool(
        &self,
        name: &str,
        arguments: Option<&Map<String, Value>>,
    ) -> Result<Value, BridgeError> {
        self.request(&BridgeRequest {
            operation: "call-tool",
            name: Some(name),
            arguments,
        })
        .await
    }
}

#[derive(Clone, Debug)]
struct YungeMcpServer {
    bridge: Arc<EmacsBridge>,
}

impl YungeMcpServer {
    fn new() -> Result<Self, BridgeError> {
        Ok(Self {
            bridge: Arc::new(EmacsBridge::from_environment()?),
        })
    }
}

impl ServerHandler for YungeMcpServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo::new(
            ServerCapabilities::builder().enable_tools().build(),
        )
        .with_server_info(
            Implementation::new("yunge-mcp", env!("CARGO_PKG_VERSION"))
                .with_title("芸阁（Yunge） MCP"),
        )
        .with_instructions(
            "芸阁（Yunge） exposes the user's running Emacs to MCP clients. \
方寸（Fangcun） indexes ordinary Org files in configured 一隅（yiyu） note \
roots and watches saved external changes automatically. Use Fangcun tools to \
discover roots, search indexed node metadata, locate nodes, inspect semantic \
backlinks, create file nodes with IDs, and assign IDs to existing headings. \
Use the client's filesystem tools for reading, literal full-text search, link \
editing, and general content changes. Write standard Org syntax directly: \
ID links are [[id:NODE-ID][DESCRIPTION]] or [[id:NODE-ID]]. To jump to a \
named target inside an ID node, use [[id:NODE-ID::target][DESCRIPTION]] and \
define <<target>> there. Standalone target links are [[target][DESCRIPTION]] \
or [[target]]. Radio targets are <<<radio target>>>; later exact plain-text \
occurrences in the same document become links automatically.",
        )
    }

    async fn list_tools(
        &self,
        _request: Option<PaginatedRequestParams>,
        _context: RequestContext<RoleServer>,
    ) -> Result<ListToolsResult, McpError> {
        self.bridge
            .list_tools()
            .await
            .map(ListToolsResult::with_all_items)
            .map_err(|error| McpError::internal_error(error.to_string(), None))
    }

    async fn call_tool(
        &self,
        request: CallToolRequestParams,
        _context: RequestContext<RoleServer>,
    ) -> Result<CallToolResponse, McpError> {
        let result = self
            .bridge
            .call_tool(&request.name, request.arguments.as_ref())
            .await;
        Ok(match result {
            Ok(value) => CallToolResult::structured(value).into(),
            Err(error) => CallToolResult::error(vec![ContentBlock::text(
                error.to_string(),
            )])
            .into(),
        })
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn Error>> {
    YungeMcpServer::new()?
        .serve(stdio())
        .await?
        .waiting()
        .await?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn sample_runtime() -> RuntimeConfig {
        RuntimeConfig {
            version: 1,
            emacsclient: PathBuf::from("/runtime/emacsclient"),
            connection_arguments: vec![
                "--socket-name".into(),
                "runtime".into(),
            ],
        }
    }

    fn encoded_stdout(response: Value) -> Vec<u8> {
        let response_json = serde_json::to_vec(&response).unwrap();
        serde_json::to_vec(&STANDARD.encode(response_json)).unwrap()
    }

    #[test]
    fn request_arguments_are_ascii_and_preserve_unicode() {
        let mut arguments = Map::new();
        arguments.insert("query".into(), Value::String("中文检索".into()));
        let request = BridgeRequest {
            operation: "call-tool",
            name: Some("fangcun_search_nodes"),
            arguments: Some(&arguments),
        };

        let encoded = EmacsBridge::request_argument(&request).unwrap();
        assert!(encoded.is_ascii());

        let decoded = STANDARD.decode(encoded).unwrap();
        let value: Value = serde_json::from_slice(&decoded).unwrap();
        assert_eq!(value["arguments"]["query"], "中文检索");
    }

    #[test]
    fn build_id_is_available_to_bridge_requests() {
        assert_eq!(BUILD_ID.len(), 64);
        assert!(BUILD_ID.bytes().all(|byte| byte.is_ascii_hexdigit()));
    }

    #[test]
    fn runtime_manifest_is_optional_and_versioned() {
        let file = env::temp_dir().join(format!(
            "yunge-mcp-runtime-test-{}.json",
            std::process::id()
        ));
        let _ = fs::remove_file(&file);

        assert!(EmacsBridge::runtime_config_at(&file).unwrap().is_none());

        fs::write(&file, b"not json").unwrap();
        let error = EmacsBridge::runtime_config_at(&file)
            .unwrap_err()
            .to_string();
        assert!(error.starts_with("invalid runtime manifest "));

        fs::write(
            &file,
            br#"{"version":2,"emacsclient":"client","connectionArguments":[]}"#,
        )
        .unwrap();
        let error = EmacsBridge::runtime_config_at(&file)
            .unwrap_err()
            .to_string();
        assert_eq!(error, "unsupported runtime manifest version 2");

        fs::write(
            &file,
            br#"{"version":1,"emacsclient":"client","connectionArguments":["--socket-name","work"]}"#,
        )
        .unwrap();
        let config = EmacsBridge::runtime_config_at(&file).unwrap().unwrap();
        assert_eq!(config.emacsclient, PathBuf::from("client"));
        assert_eq!(config.connection_arguments, ["--socket-name", "work"]);

        fs::remove_file(file).unwrap();
    }

    #[test]
    fn explicit_bridge_sources_override_runtime_defaults() {
        let bridge = EmacsBridge::from_sources(
            Some(sample_runtime()),
            Some(OsString::from("override-client")),
            Some(OsString::from("override-server")),
        );
        assert_eq!(bridge.program, OsString::from("override-client"));
        assert_eq!(
            bridge.connection_arguments,
            ["--server-file", "override-server"]
                .map(OsString::from)
                .to_vec()
        );

        let bridge =
            EmacsBridge::from_sources(Some(sample_runtime()), None, None);
        assert_eq!(
            bridge.program,
            PathBuf::from("/runtime/emacsclient").into_os_string()
        );
        assert_eq!(
            bridge.connection_arguments,
            ["--socket-name", "runtime"].map(OsString::from).to_vec()
        );

        let bridge = EmacsBridge::from_sources(None, None, None);
        assert_eq!(bridge.program, OsString::from("emacsclient"));
        assert!(bridge.connection_arguments.is_empty());
    }

    #[test]
    fn command_arguments_keep_connection_and_protocol_boundaries() {
        let bridge = EmacsBridge {
            program: OsString::from("emacsclient"),
            connection_arguments: ["--socket-name", "work"]
                .map(OsString::from)
                .to_vec(),
        };
        let request = BridgeRequest {
            operation: "list-tools",
            name: None,
            arguments: None,
        };
        let arguments = bridge.command_arguments(&request).unwrap();
        assert_eq!(
            &arguments[..5],
            ["--socket-name", "work", "--eval", DISPATCH_FORM, BUILD_ID]
                .map(OsString::from)
                .as_slice()
        );

        let decoded = STANDARD.decode(arguments[5].as_encoded_bytes()).unwrap();
        let value: Value = serde_json::from_slice(&decoded).unwrap();
        assert_eq!(value, json!({"operation": "list-tools"}));
    }

    #[test]
    fn response_decoding_preserves_success_and_domain_errors() {
        let output = encoded_stdout(json!({
            "ok": true,
            "value": {"count": 3},
            "error": null
        }));
        assert_eq!(
            EmacsBridge::decode_response(&output).unwrap(),
            json!({"count": 3})
        );

        let output = encoded_stdout(json!({
            "ok": false,
            "value": null,
            "error": {"type": "tool-error", "message": "stopped"}
        }));
        assert_eq!(
            EmacsBridge::decode_response(&output)
                .unwrap_err()
                .to_string(),
            "Yunge tool-error: stopped"
        );
    }

    #[test]
    fn response_decoding_identifies_each_protocol_layer() {
        let error = EmacsBridge::decode_response(b"not json")
            .unwrap_err()
            .to_string();
        assert!(error.starts_with("invalid response from emacsclient: "));

        let invalid_encoding = serde_json::to_vec("not base64!").unwrap();
        let error = EmacsBridge::decode_response(&invalid_encoding)
            .unwrap_err()
            .to_string();
        assert!(error.starts_with("invalid response encoding: "));

        let invalid_response =
            serde_json::to_vec(&STANDARD.encode(b"not json")).unwrap();
        let error = EmacsBridge::decode_response(&invalid_response)
            .unwrap_err()
            .to_string();
        assert!(error.starts_with("invalid response from Yunge: "));

        let unspecified = encoded_stdout(json!({
            "ok": false,
            "value": null,
            "error": null
        }));
        assert_eq!(
            EmacsBridge::decode_response(&unspecified)
                .unwrap_err()
                .to_string(),
            "Yunge returned an unspecified error"
        );
    }

    #[tokio::test]
    async fn bridge_reports_process_start_failures() {
        let bridge = EmacsBridge {
            program: env::temp_dir()
                .join("missing-yunge-emacsclient")
                .into_os_string(),
            connection_arguments: Vec::new(),
        };
        let request = BridgeRequest {
            operation: "list-tools",
            name: None,
            arguments: None,
        };
        let error = bridge.request(&request).await.unwrap_err().to_string();
        assert!(error.starts_with("could not run emacsclient: "));
    }
}
