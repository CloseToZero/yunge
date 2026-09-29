// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use std::{env, error::Error};

use rmcp::{
    ErrorData as McpError, ServerHandler, ServiceExt,
    model::{
        CallToolRequestParams, CallToolResponse, CallToolResult, ContentBlock, Implementation,
        ListToolsResult, PaginatedRequestParams, ServerCapabilities, ServerInfo,
    },
    service::{RequestContext, RoleServer},
    transport::stdio,
};

mod bridge;
mod codex_config;

use bridge::{BridgeError, EmacsBridge};

#[derive(Clone, Debug)]
struct YungeMcpServer {
    bridge: EmacsBridge,
}

impl YungeMcpServer {
    fn new() -> Result<Self, BridgeError> {
        Ok(Self {
            bridge: EmacsBridge::from_environment()?,
        })
    }
}

impl ServerHandler for YungeMcpServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo::new(ServerCapabilities::builder().enable_tools().build())
            .with_server_info(
                Implementation::new("yunge-mcp", env!("CARGO_PKG_VERSION"))
                    .with_title("芸阁（Yunge） MCP"),
            )
            .with_instructions(
                "方寸（Fangcun） tools work with saved Org notes in the user's running Emacs. \
Search uses the index; locating a node rereads its saved file. Use the client's \
filesystem tools to read, search file contents, and edit notes. Check \
modifiedInEmacs before external edits and reconcile unsaved buffer changes first. \
Preserve existing Org IDs. External edits reach the index through an optional \
watcher; use M-x fangcun-db-sync in Emacs if search results are stale. A failed \
or cancelled creation may already have saved the note: inspect the target file \
and keep its assigned ID before trying again.",
            )
    }

    async fn list_tools(
        &self,
        _request: Option<PaginatedRequestParams>,
        context: RequestContext<RoleServer>,
    ) -> Result<ListToolsResult, McpError> {
        tokio::select! {
            biased;
            _ = context.ct.cancelled() => {
                Err(McpError::internal_error("request cancelled", None))
            }
            result = self.bridge.list_tools() => result
                .map(ListToolsResult::with_all_items)
                .map_err(|error| McpError::internal_error(error.to_string(), None)),
        }
    }

    async fn call_tool(
        &self,
        request: CallToolRequestParams,
        context: RequestContext<RoleServer>,
    ) -> Result<CallToolResponse, McpError> {
        tokio::select! {
            biased;
            _ = context.ct.cancelled() => {
                Err(McpError::internal_error("request cancelled", None))
            }
            result = self.bridge.call_tool(&request.name, request.arguments.as_ref()) => {
                match result {
                    Ok(value) => Ok(CallToolResult::structured(value).into()),
                    Err(error @ BridgeError::UnknownTool(_)) => {
                        Err(McpError::invalid_params(error.to_string(), None))
                    }
                    Err(error @ BridgeError::Tool { .. }) => Ok(CallToolResult::error(
                        vec![ContentBlock::text(error.to_string())],
                    ).into()),
                    Err(error @ BridgeError::Internal(_)) => {
                        Err(McpError::internal_error(error.to_string(), None))
                    }
                }
            }
        }
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn Error>> {
    let mut arguments = env::args_os().skip(1);
    match (arguments.next(), arguments.next()) {
        (None, None) => {
            YungeMcpServer::new()?
                .serve(stdio())
                .await?
                .waiting()
                .await?;
            Ok(())
        }
        (Some(command), None) if command == "edit-codex-config" => codex_config::run(),
        _ => Err("unknown Yunge MCP command".into()),
    }
}
