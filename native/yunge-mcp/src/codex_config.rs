// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use std::{
    error::Error,
    io::{self, Read, Write},
};

use serde::{Deserialize, Serialize};
use toml_edit::{Array, DocumentMut, InlineTable, Item, Table, Value, value};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct EditRequest {
    program: String,
    configuration: String,
}

#[derive(Serialize)]
struct EditResponse {
    configuration: String,
}

pub fn edit(program: &str, configuration: &str) -> Result<String, Box<dyn Error>> {
    if program.is_empty() {
        return Err("program must not be empty".into());
    }
    let mut document: DocumentMut =
        configuration
            .parse()
            .map_err(|error: toml_edit::TomlError| {
                format!("invalid Codex TOML: {}", error.message())
            })?;
    if document.get("mcp_servers").is_none() {
        document["mcp_servers"] = Item::Table(Table::new());
    }
    match document.get_mut("mcp_servers") {
        Some(Item::Table(servers)) => {
            servers.remove("yunge");
            let mut yunge = Table::new();
            yunge["command"] = value(program);
            yunge["args"] = value(Array::new());
            servers.insert("yunge", Item::Table(yunge));
        }
        Some(Item::Value(Value::InlineTable(servers))) => {
            servers.remove("yunge");
            let mut yunge = InlineTable::new();
            yunge.insert("command", Value::from(program));
            yunge.insert("args", Value::Array(Array::new()));
            servers.insert("yunge", Value::InlineTable(yunge));
        }
        _ => return Err("mcp_servers must be a TOML table or inline table".into()),
    }
    Ok(document.to_string())
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let request: EditRequest = serde_json::from_str(&input)?;
    let configuration = edit(&request.program, &request.configuration)?;
    let output = serde_json::to_vec(&EditResponse { configuration })?;
    let mut stdout = io::stdout().lock();
    stdout.write_all(&output)?;
    stdout.write_all(b"\n")?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn replaces_only_yunge_across_dotted_and_quoted_tables() {
        let before = r#"# retained comment
model = """line one
[mcp_servers.fake]
line two"""
[mcp_servers."yunge"] # old entry
command = "old"
[mcp_servers.other] # keep this whole section
command = "other"
[[profiles]]
name = "one"
[mcp_servers."yunge".env]
TOKEN = "old"
"#;
        let after = edit("C:/工具/yunge-mcp.exe", before).unwrap();
        let parsed: DocumentMut = after.parse().unwrap();
        assert!(after.contains("# retained comment"));
        assert_eq!(
            parsed["model"].as_str(),
            Some("line one\n[mcp_servers.fake]\nline two")
        );
        assert!(after.contains("[mcp_servers.other] # keep this whole section"));
        assert_eq!(
            parsed["mcp_servers"]["other"]["command"].as_str(),
            Some("other")
        );
        assert_eq!(
            parsed["mcp_servers"]["yunge"]["command"].as_str(),
            Some("C:/工具/yunge-mcp.exe")
        );
        assert!(parsed["mcp_servers"]["yunge"].get("env").is_none());
        assert_eq!(parsed["profiles"].as_array_of_tables().unwrap().len(), 1);
    }

    #[test]
    fn replaces_inline_server_without_changing_neighbors() {
        let before = "mcp_servers = { other = { command = 'other' }, yunge = { env = 'old' } }\n";
        let after = edit("/tmp/yunge-mcp", before).unwrap();
        let parsed: DocumentMut = after.parse().unwrap();
        assert_eq!(
            parsed["mcp_servers"]["other"]["command"].as_str(),
            Some("other")
        );
        assert_eq!(
            parsed["mcp_servers"]["yunge"]["command"].as_str(),
            Some("/tmp/yunge-mcp")
        );
        assert!(parsed["mcp_servers"]["yunge"].get("env").is_none());

        let dotted = "mcp_servers.other.command = 'other'\nmcp_servers.yunge.env = 'old'\n";
        let after = edit("/tmp/yunge-mcp", dotted).unwrap();
        let parsed: DocumentMut = after.parse().unwrap();
        assert_eq!(
            parsed["mcp_servers"]["other"]["command"].as_str(),
            Some("other")
        );
        assert_eq!(
            parsed["mcp_servers"]["yunge"]["command"].as_str(),
            Some("/tmp/yunge-mcp")
        );
        assert!(parsed["mcp_servers"]["yunge"].get("env").is_none());
    }

    #[test]
    fn rejects_invalid_configuration_and_server_shape() {
        assert!(edit("program", "mcp_servers = 1\n").is_err());
        assert!(edit("program", "[broken\n").is_err());
        assert!(edit("", "").is_err());
    }
}
