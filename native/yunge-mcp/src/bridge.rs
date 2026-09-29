// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use std::{
    env,
    error::Error,
    ffi::OsString,
    fs,
    path::{Path, PathBuf},
    process::Stdio,
};

use base64::{Engine as _, engine::general_purpose::STANDARD};
use rmcp::model::Tool;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use tokio::process::Command;

const DISPATCH_FORM: &str = "(progn (require 'yunge-mcp) (yunge-mcp-server-dispatch))";
const BUILD_ID: &str = env!("YUNGE_MCP_BUILD_ID");

#[derive(Clone, Debug)]
pub(crate) struct EmacsBridge {
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
struct BridgeResponseError {
    #[serde(rename = "type")]
    kind: String,
    message: String,
}

#[derive(Debug)]
pub(crate) enum BridgeError {
    Internal(String),
    UnknownTool(String),
    Tool { kind: String, message: String },
}

impl std::fmt::Display for BridgeError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Internal(message) => formatter.write_str(message),
            Self::UnknownTool(message) => formatter.write_str(message),
            Self::Tool { kind, message } => write!(formatter, "Yunge {kind}: {message}"),
        }
    }
}

impl Error for BridgeError {}

impl EmacsBridge {
    fn request_argument(request: &BridgeRequest<'_>) -> Result<String, BridgeError> {
        let request_json = serde_json::to_vec(request)
            .map_err(|error| BridgeError::Internal(error.to_string()))?;
        Ok(STANDARD.encode(request_json))
    }

    fn runtime_file() -> Option<PathBuf> {
        if let Some(file) = env::var_os("YUNGE_MCP_RUNTIME") {
            return Some(file.into());
        }
        let executable = env::current_exe().ok()?;
        Some(executable.parent()?.parent()?.join("runtime.json"))
    }

    fn runtime_config_at(file: &Path) -> Result<Option<RuntimeConfig>, BridgeError> {
        if !file.exists() {
            return Ok(None);
        }
        let bytes = fs::read(file).map_err(|error| {
            BridgeError::Internal(format!(
                "could not read runtime manifest {}: {error}",
                file.display()
            ))
        })?;
        let config: RuntimeConfig = serde_json::from_slice(&bytes).map_err(|error| {
            BridgeError::Internal(format!(
                "invalid runtime manifest {}: {error}",
                file.display()
            ))
        })?;
        if config.version != 1 {
            return Err(BridgeError::Internal(format!(
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

    pub(crate) fn from_environment() -> Result<Self, BridgeError> {
        Ok(Self::from_sources(
            Self::runtime_config()?,
            env::var_os("YUNGE_EMACSCLIENT"),
            env::var_os("YUNGE_EMACS_SERVER_FILE"),
        ))
    }

    fn command_arguments(&self, request: &BridgeRequest<'_>) -> Result<Vec<OsString>, BridgeError> {
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
            .map_err(|error| BridgeError::Internal(error.to_string()))?;
        let encoded: String = serde_json::from_str(printed.trim()).map_err(|error| {
            BridgeError::Internal(format!("invalid response from emacsclient: {error}"))
        })?;
        let response_json = STANDARD.decode(encoded).map_err(|error| {
            BridgeError::Internal(format!("invalid response encoding: {error}"))
        })?;
        let response: Value = serde_json::from_slice(&response_json).map_err(|error| {
            BridgeError::Internal(format!("invalid response from Yunge: {error}"))
        })?;
        let object = response.as_object().ok_or_else(|| {
            BridgeError::Internal("invalid response from Yunge: expected an object".into())
        })?;
        if object.get("ok") == Some(&Value::Bool(true)) {
            object.get("value").cloned().ok_or_else(|| {
                BridgeError::Internal("invalid response from Yunge: missing value".into())
            })
        } else if object.get("ok") == Some(&Value::Bool(false)) {
            let error: BridgeResponseError =
                serde_json::from_value(object.get("error").cloned().unwrap_or(Value::Null))
                    .map_err(|_| {
                        BridgeError::Internal("Yunge returned an unspecified error".into())
                    })?;
            if error.kind == "yunge-mcp-unknown-tool" {
                Err(BridgeError::UnknownTool(error.message))
            } else {
                Err(BridgeError::Tool {
                    kind: error.kind,
                    message: error.message,
                })
            }
        } else {
            Err(BridgeError::Internal(
                "invalid response from Yunge: missing ok boolean".into(),
            ))
        }
    }

    async fn request(&self, request: &BridgeRequest<'_>) -> Result<Value, BridgeError> {
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
                BridgeError::Internal(format!("could not run emacsclient: {error}"))
            })?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let status = output
                .status
                .code()
                .map_or_else(|| output.status.to_string(), |code| code.to_string());
            return Err(BridgeError::Internal(format!(
                "emacsclient failed with status {}: {}",
                status,
                stderr.trim()
            )));
        }
        Self::decode_response(&output.stdout)
    }

    pub(crate) async fn list_tools(&self) -> Result<Vec<Tool>, BridgeError> {
        let value = self
            .request(&BridgeRequest {
                operation: "list-tools",
                name: None,
                arguments: None,
            })
            .await?;
        serde_json::from_value(value).map_err(|error| {
            BridgeError::Internal(format!("invalid Yunge tool descriptions: {error}"))
        })
    }

    pub(crate) async fn call_tool(
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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn encoded_stdout(response: Value) -> Vec<u8> {
        let response_json = serde_json::to_vec(&response).unwrap();
        serde_json::to_vec(&STANDARD.encode(response_json)).unwrap()
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
            br#"{"version":1,"emacsclient":"client",
                 "connectionArguments":["--socket-name","work"]}"#,
        )
        .unwrap();
        let config = EmacsBridge::runtime_config_at(&file).unwrap().unwrap();
        assert_eq!(config.emacsclient, PathBuf::from("client"));
        assert_eq!(config.connection_arguments, ["--socket-name", "work"]);

        fs::remove_file(file).unwrap();
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

        let empty = encoded_stdout(json!({"ok": true, "value": null}));
        assert_eq!(EmacsBridge::decode_response(&empty).unwrap(), Value::Null);

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

        let invalid_response = serde_json::to_vec(&STANDARD.encode(b"not json")).unwrap();
        let error = EmacsBridge::decode_response(&invalid_response)
            .unwrap_err()
            .to_string();
        assert!(error.starts_with("invalid response from Yunge: "));

        let missing_value = encoded_stdout(json!({"ok": true}));
        let error = EmacsBridge::decode_response(&missing_value)
            .unwrap_err()
            .to_string();
        assert_eq!(error, "invalid response from Yunge: missing value");

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
