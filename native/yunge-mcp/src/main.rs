// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use std::{
    env, error::Error, ffi::OsString, fs, path::PathBuf, process::Stdio,
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
    fn runtime_file() -> Option<PathBuf> {
        if let Some(file) = env::var_os("YUNGE_MCP_RUNTIME") {
            return Some(file.into());
        }
        let executable = env::current_exe().ok()?;
        Some(executable.parent()?.parent()?.join("runtime.json"))
    }

    fn runtime_config() -> Result<Option<RuntimeConfig>, BridgeError> {
        let Some(file) = Self::runtime_file() else {
            return Ok(None);
        };
        if !file.exists() {
            return Ok(None);
        }
        let bytes = fs::read(&file).map_err(|error| {
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

    fn from_environment() -> Result<Self, BridgeError> {
        let config = Self::runtime_config()?;
        let program = env::var_os("YUNGE_EMACSCLIENT")
            .or_else(|| {
                config
                    .as_ref()
                    .map(|runtime| runtime.emacsclient.clone().into_os_string())
            })
            .unwrap_or_else(|| OsString::from("emacsclient"));
        let connection_arguments =
            if let Some(file) = env::var_os("YUNGE_EMACS_SERVER_FILE") {
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
        Ok(Self {
            program,
            connection_arguments,
        })
    }

    async fn request(
        &self,
        request: &BridgeRequest<'_>,
    ) -> Result<Value, BridgeError> {
        let request_json = serde_json::to_string(request)
            .map_err(|error| BridgeError(error.to_string()))?;
        let mut command = Command::new(&self.program);
        command.args(&self.connection_arguments);
        let output = command
            .arg("--eval")
            .arg(DISPATCH_FORM)
            .arg(request_json)
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
            return Err(BridgeError(format!(
                "emacsclient failed: {}",
                stderr.trim()
            )));
        }
        let printed = String::from_utf8(output.stdout)
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
Use 方寸（Fangcun） tools to search or read Org notes, inspect backlinks, \
or create a note in a configured 一隅（yiyu） note root.",
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
