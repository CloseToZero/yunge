// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use std::{
    env,
    error::Error,
    fs::{self, OpenOptions},
    io::{BufRead, BufReader, Read, Write},
    path::{Path, PathBuf},
    process::{Child, ChildStdin, Command, Stdio},
    sync::mpsc::{self, Receiver},
    thread::{self, JoinHandle},
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};

use base64::{Engine as _, engine::general_purpose::STANDARD};
use serde_json::{Value, json};

const DISPATCH_FORM: &str =
    "(progn (require 'yunge-mcp) (yunge-mcp-server-dispatch))";

struct TestDirectory(PathBuf);

impl TestDirectory {
    fn new() -> Result<Self, Box<dyn Error>> {
        let nonce = SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos();
        let path = env::temp_dir()
            .join(format!("yunge-mcp-stdio-{}-{nonce}", std::process::id()));
        fs::create_dir(&path)?;
        Ok(Self(path))
    }
}

impl Drop for TestDirectory {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

struct McpChild {
    child: Child,
    input: Option<ChildStdin>,
    responses: Receiver<String>,
    output_thread: JoinHandle<()>,
    error_thread: JoinHandle<String>,
}

impl McpChild {
    fn start(
        directory: &Path,
        fail_bridge: bool,
    ) -> Result<Self, Box<dyn Error>> {
        let current_executable = env::current_exe()?;
        let mut command = Command::new(env!("CARGO_BIN_EXE_yunge-mcp"));
        command
            .env("YUNGE_EMACSCLIENT", &current_executable)
            .env("YUNGE_MCP_FAKE_CHILD", "stdio-roundtrip")
            .env("YUNGE_MCP_FAKE_LOG", directory.join("bridge-calls.jsonl"))
            .env("YUNGE_MCP_RUNTIME", directory.join("missing-runtime.json"))
            .env_remove("YUNGE_EMACS_SERVER_FILE")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        if fail_bridge {
            command.env("YUNGE_MCP_FAKE_FAILURE", "1");
        } else {
            command.env_remove("YUNGE_MCP_FAKE_FAILURE");
        }
        let mut child = command.spawn()?;
        let input = child.stdin.take().ok_or("missing helper stdin")?;
        let output = child.stdout.take().ok_or("missing helper stdout")?;
        let error = child.stderr.take().ok_or("missing helper stderr")?;
        let (sender, responses) = mpsc::channel();
        let output_thread = thread::spawn(move || {
            for line in BufReader::new(output).lines() {
                match line {
                    Ok(line) => {
                        if sender.send(line).is_err() {
                            break;
                        }
                    }
                    Err(_) => break,
                }
            }
        });
        let error_thread = thread::spawn(move || {
            let mut text = String::new();
            let _ = BufReader::new(error).read_to_string(&mut text);
            text
        });
        Ok(Self {
            child,
            input: Some(input),
            responses,
            output_thread,
            error_thread,
        })
    }

    fn send(&mut self, message: Value) -> Result<(), Box<dyn Error>> {
        let input = self.input.as_mut().ok_or("helper stdin is closed")?;
        serde_json::to_writer(&mut *input, &message)?;
        input.write_all(b"\n")?;
        input.flush()?;
        Ok(())
    }

    fn request(&mut self, message: Value) -> Result<Value, Box<dyn Error>> {
        self.send(message)?;
        let line = self
            .responses
            .recv_timeout(Duration::from_secs(5))
            .map_err(|error| {
                format!("timed out waiting for MCP response: {error}")
            })?;
        Ok(serde_json::from_str(&line)?)
    }

    fn initialize(&mut self) -> Result<Value, Box<dyn Error>> {
        let response = self.request(json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2025-11-25",
                "capabilities": {},
                "clientInfo": {"name": "stdio-test", "version": "1"}
            }
        }))?;
        self.send(json!({
            "jsonrpc": "2.0",
            "method": "notifications/initialized"
        }))?;
        Ok(response)
    }

    fn finish(mut self) -> Result<(), Box<dyn Error>> {
        drop(self.input.take());
        let deadline = Instant::now() + Duration::from_secs(5);
        let status = loop {
            if let Some(status) = self.child.try_wait()? {
                break status;
            }
            if Instant::now() >= deadline {
                self.child.kill()?;
                let _ = self.child.wait();
                return Err("MCP helper did not exit after stdin closed".into());
            }
            thread::sleep(Duration::from_millis(10));
        };
        self.output_thread
            .join()
            .map_err(|_| "MCP output reader panicked")?;
        let stderr = self
            .error_thread
            .join()
            .map_err(|_| "MCP error reader panicked")?;
        if !status.success() {
            return Err(
                format!("MCP helper exited with {status}: {stderr}").into()
            );
        }
        Ok(())
    }
}

fn fake_emacsclient() -> Result<(), Box<dyn Error>> {
    let arguments: Vec<String> = env::args().skip(1).collect();
    let log_file = PathBuf::from(
        env::var_os("YUNGE_MCP_FAKE_LOG")
            .ok_or("YUNGE_MCP_FAKE_LOG was not passed to fake emacsclient")?,
    );
    let mut log = OpenOptions::new()
        .create(true)
        .append(true)
        .open(log_file)?;
    serde_json::to_writer(&mut log, &arguments)?;
    log.write_all(b"\n")?;

    if env::var_os("YUNGE_MCP_FAKE_FAILURE").is_some() {
        eprintln!("fake emacsclient failure");
        std::process::exit(23);
    }

    let eval = arguments
        .iter()
        .position(|argument| argument == "--eval")
        .ok_or("bridge omitted --eval")?;
    if arguments.get(eval + 1).map(String::as_str) != Some(DISPATCH_FORM) {
        return Err("bridge changed its fixed dispatch form".into());
    }
    let build_id = arguments.get(eval + 2).ok_or("bridge omitted build ID")?;
    if build_id.len() != 64
        || !build_id.bytes().all(|byte| byte.is_ascii_hexdigit())
    {
        return Err("bridge supplied an invalid build ID".into());
    }
    let encoded = arguments
        .get(eval + 3)
        .ok_or("bridge omitted encoded request")?;
    let request: Value = serde_json::from_slice(&STANDARD.decode(encoded)?)?;
    let response = match request["operation"].as_str() {
        Some("list-tools") => json!({
            "ok": true,
            "value": [{
                "name": "echo",
                "description": "Echo one value",
                "inputSchema": {
                    "type": "object",
                    "properties": {"value": {"type": "string"}}
                }
            }],
            "error": null
        }),
        Some("call-tool") => match request["name"].as_str() {
            Some("echo") => json!({
                "ok": true,
                "value": {"echo": request["arguments"]["value"]},
                "error": null
            }),
            Some("fail") => json!({
                "ok": false,
                "value": null,
                "error": {"type": "tool-error", "message": "stopped"}
            }),
            _ => return Err("bridge changed the tool name".into()),
        },
        operation => {
            return Err(
                format!("unexpected bridge operation: {operation:?}").into()
            );
        }
    };
    let response = serde_json::to_vec(&response)?;
    println!("{}", serde_json::to_string(&STANDARD.encode(response))?);
    Ok(())
}

fn bridge_requests(log_file: &Path) -> Result<Vec<Value>, Box<dyn Error>> {
    let file = fs::File::open(log_file)?;
    BufReader::new(file)
        .lines()
        .map(|line| {
            let arguments: Vec<String> = serde_json::from_str(&line?)?;
            let eval = arguments
                .iter()
                .position(|argument| argument == "--eval")
                .ok_or("logged bridge call omitted --eval")?;
            if arguments.get(eval + 1).map(String::as_str)
                != Some(DISPATCH_FORM)
            {
                return Err("logged bridge call changed dispatch form".into());
            }
            let encoded = arguments
                .get(eval + 3)
                .ok_or("logged bridge call omitted request")?;
            Ok(serde_json::from_slice(&STANDARD.decode(encoded)?)?)
        })
        .collect()
}

fn stdio_roundtrip() -> Result<(), Box<dyn Error>> {
    let directory = TestDirectory::new()?;
    let mut helper = McpChild::start(&directory.0, false)?;
    let initialized = helper.initialize()?;
    assert_eq!(initialized["result"]["protocolVersion"], "2025-11-25");
    assert_eq!(initialized["result"]["serverInfo"]["name"], "yunge-mcp");
    let instructions = initialized["result"]["instructions"]
        .as_str()
        .ok_or("initialize response omitted server instructions")?;
    assert!(instructions.contains("configured 一隅（yiyu） note roots"));
    assert!(instructions.contains("inspect semantic backlinks"));
    assert!(instructions.contains("create file nodes with IDs"));
    assert!(instructions.contains("assign IDs to existing headings"));
    assert!(instructions.contains("watches saved external changes"));
    assert!(instructions.contains("filesystem tools"));
    assert!(instructions.contains("[[id:NODE-ID][DESCRIPTION]]"));
    assert!(instructions.contains("[[id:NODE-ID::target][DESCRIPTION]]"));
    assert!(instructions.contains("[[target][DESCRIPTION]]"));
    assert!(instructions.contains("<<<radio target>>>"));
    assert!(instructions.contains("plain-text occurrences"));

    let tools = helper.request(json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/list",
        "params": {}
    }))?;
    assert_eq!(tools["result"]["tools"][0]["name"], "echo");

    let called = helper.request(json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "tools/call",
        "params": {"name": "echo", "arguments": {"value": "中文"}}
    }))?;
    assert_eq!(called["result"]["structuredContent"]["echo"], "中文");

    let tool_error = helper.request(json!({
        "jsonrpc": "2.0",
        "id": 4,
        "method": "tools/call",
        "params": {"name": "fail", "arguments": {}}
    }))?;
    assert_eq!(tool_error["result"]["isError"], true);
    assert!(
        tool_error["result"]["content"][0]["text"]
            .as_str()
            .is_some_and(|text| text.contains("Yunge tool-error: stopped"))
    );
    helper.finish()?;

    let requests = bridge_requests(&directory.0.join("bridge-calls.jsonl"))?;
    assert_eq!(requests.len(), 3);
    assert_eq!(requests[0], json!({"operation": "list-tools"}));
    assert_eq!(requests[1]["operation"], "call-tool");
    assert_eq!(requests[1]["name"], "echo");
    assert_eq!(requests[1]["arguments"]["value"], "中文");
    assert_eq!(requests[2]["operation"], "call-tool");
    assert_eq!(requests[2]["name"], "fail");

    let failure_directory = TestDirectory::new()?;
    let mut helper = McpChild::start(&failure_directory.0, true)?;
    helper.initialize()?;
    let failed = helper.request(json!({
        "jsonrpc": "2.0",
        "id": 5,
        "method": "tools/list",
        "params": {}
    }))?;
    let message = failed["error"]["message"]
        .as_str()
        .ok_or("tools/list failure omitted its message")?;
    assert!(message.contains("status 23"), "{message}");
    assert!(message.contains("fake emacsclient failure"), "{message}");
    helper.finish()?;
    println!("Yunge MCP stdio and fake-emacsclient integration passed");
    Ok(())
}

fn main() {
    if env::var("YUNGE_MCP_FAKE_CHILD").as_deref() == Ok("stdio-roundtrip") {
        if let Err(error) = fake_emacsclient() {
            eprintln!("fake emacsclient failed: {error}");
            std::process::exit(64);
        }
        return;
    }
    if let Err(error) = stdio_roundtrip() {
        eprintln!("Yunge MCP stdio round trip failed: {error}");
        std::process::exit(1);
    }
}
