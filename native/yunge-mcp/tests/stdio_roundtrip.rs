// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

use std::{
    env,
    error::Error,
    fs,
    io::{BufRead, BufReader, Read, Write},
    net::{SocketAddr, TcpListener, TcpStream},
    path::{Path, PathBuf},
    process::{Child, ChildStdin, Command, Stdio},
    sync::mpsc::{self, Receiver},
    thread::{self, JoinHandle},
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};

use base64::{Engine as _, engine::general_purpose::STANDARD};
use serde_json::{Value, json};

const DISPATCH_FORM: &str = "(progn (require 'yunge-mcp) (yunge-mcp-server-dispatch))";

struct TestDirectory(PathBuf);

impl TestDirectory {
    fn new() -> Result<Self, Box<dyn Error>> {
        let nonce = SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos();
        let path = env::temp_dir().join(format!("yunge-mcp-stdio-{}-{nonce}", std::process::id()));
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
    directory: PathBuf,
    child: Child,
    input: Option<ChildStdin>,
    responses: Receiver<String>,
    output_thread: Option<JoinHandle<()>>,
    error_thread: Option<JoinHandle<String>>,
}

impl McpChild {
    fn start(directory: &Path, fail_bridge: bool) -> Result<Self, Box<dyn Error>> {
        let current_executable = env::current_exe()?;
        let runtime_file = directory.join("runtime.json");
        fs::write(
            &runtime_file,
            serde_json::to_vec(&json!({
                "version": 1,
                "emacsclient": directory.join("invalid-emacsclient"),
                "connectionArguments": ["--socket-name", "runtime"]
            }))?,
        )?;
        let mut command = Command::new(env!("CARGO_BIN_EXE_yunge-mcp"));
        command
            .env("YUNGE_EMACSCLIENT", &current_executable)
            .env("YUNGE_MCP_FAKE_CHILD", "stdio-roundtrip")
            .env("YUNGE_MCP_FAKE_DIRECTORY", directory)
            .env("YUNGE_MCP_RUNTIME", runtime_file)
            .env("YUNGE_EMACS_SERVER_FILE", "override-server")
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
            directory: directory.to_path_buf(),
            child,
            input: Some(input),
            responses,
            output_thread: Some(output_thread),
            error_thread: Some(error_thread),
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
            .map_err(|error| format!("timed out waiting for MCP response: {error}"))?;
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
        let deadline = Instant::now() + Duration::from_secs(10);
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
            .take()
            .ok_or("missing MCP output reader")?
            .join()
            .map_err(|_| "MCP output reader panicked")?;
        let stderr = self
            .error_thread
            .take()
            .ok_or("missing MCP error reader")?
            .join()
            .map_err(|_| "MCP error reader panicked")?;
        if !status.success() {
            return Err(format!("MCP helper exited with {status}: {stderr}").into());
        }
        Ok(())
    }
}

impl Drop for McpChild {
    fn drop(&mut self) {
        if self.child.try_wait().ok().flatten().is_none() {
            if let Ok(port) = fs::read_to_string(self.directory.join("bridge-port"))
                && let Ok(address) = port.parse()
            {
                let _ = TcpStream::connect_timeout(&address, Duration::from_millis(100));
            }
            let _ = self.child.kill();
            let _ = self.child.wait();
        }
    }
}

fn wait_for_bridge_port(directory: &Path) -> Result<SocketAddr, Box<dyn Error>> {
    let port_file = directory.join("bridge-port");
    let deadline = Instant::now() + Duration::from_secs(5);
    loop {
        if let Ok(port) = fs::read_to_string(&port_file) {
            return Ok(port.parse()?);
        }
        if Instant::now() >= deadline {
            return Err("fake emacsclient did not start listening".into());
        }
        thread::sleep(Duration::from_millis(10));
    }
}

fn wait_for_port_release(address: SocketAddr) -> Result<(), Box<dyn Error>> {
    let deadline = Instant::now() + Duration::from_secs(5);
    loop {
        if TcpListener::bind(address).is_ok() {
            return Ok(());
        }
        if Instant::now() >= deadline {
            return Err("cancelled emacsclient still owns its listening port".into());
        }
        thread::sleep(Duration::from_millis(10));
    }
}

fn fake_emacsclient() -> Result<(), Box<dyn Error>> {
    let arguments: Vec<String> = env::args().skip(1).collect();
    let directory = PathBuf::from(
        env::var_os("YUNGE_MCP_FAKE_DIRECTORY")
            .ok_or("YUNGE_MCP_FAKE_DIRECTORY was not passed to fake emacsclient")?,
    );

    if env::var_os("YUNGE_MCP_FAKE_FAILURE").is_some() {
        eprintln!("fake emacsclient failure");
        std::process::exit(23);
    }

    if arguments.get(0).map(String::as_str) != Some("--server-file")
        || arguments.get(1).map(String::as_str) != Some("override-server")
    {
        return Err("bridge did not apply environment overrides to runtime settings".into());
    }

    let eval = arguments
        .iter()
        .position(|argument| argument == "--eval")
        .ok_or("bridge omitted --eval")?;
    if arguments.get(eval + 1).map(String::as_str) != Some(DISPATCH_FORM) {
        return Err("bridge changed its fixed dispatch form".into());
    }
    let build_id = arguments.get(eval + 2).ok_or("bridge omitted build ID")?;
    if build_id.len() != 64 || !build_id.bytes().all(|byte| byte.is_ascii_hexdigit()) {
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
            Some("unknown") => json!({
                "ok": false,
                "error": {
                    "type": "yunge-mcp-unknown-tool",
                    "message": "Unknown Yunge MCP tool: unknown"
                }
            }),
            Some("missing-value") => json!({"ok": true}),
            Some("wait") => {
                let listener = TcpListener::bind("127.0.0.1:0")?;
                let temporary_port_file = directory.join("bridge-port.tmp");
                fs::write(&temporary_port_file, listener.local_addr()?.to_string())?;
                fs::rename(temporary_port_file, directory.join("bridge-port"))?;
                let _ = listener.accept()?;
                return Err("fake emacsclient wait was unexpectedly released".into());
            }
            _ => return Err("bridge changed the tool name".into()),
        },
        operation => {
            return Err(format!("unexpected bridge operation: {operation:?}").into());
        }
    };
    let response = serde_json::to_vec(&response)?;
    println!("{}", serde_json::to_string(&STANDARD.encode(response))?);
    Ok(())
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
    assert!(!instructions.trim().is_empty());

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
    let unknown = helper.request(json!({
        "jsonrpc": "2.0", "id": 6, "method": "tools/call",
        "params": {"name": "unknown", "arguments": {}}
    }))?;
    assert_eq!(unknown["error"]["code"], -32602);
    assert!(unknown.get("result").is_none());

    let malformed = helper.request(json!({
        "jsonrpc": "2.0", "id": 7, "method": "tools/call",
        "params": {"name": "missing-value", "arguments": {}}
    }))?;
    assert_eq!(malformed["error"]["code"], -32603);
    assert!(
        malformed["error"]["message"]
            .as_str()
            .is_some_and(|message| message.contains("missing value"))
    );
    helper.finish()?;

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
    let failed_call = helper.request(json!({
        "jsonrpc": "2.0", "id": 8, "method": "tools/call",
        "params": {"name": "echo", "arguments": {"value": "test"}}
    }))?;
    assert_eq!(failed_call["error"]["code"], -32603);
    assert!(failed_call.get("result").is_none());
    helper.finish()?;

    let cancel_directory = TestDirectory::new()?;
    let mut helper = McpChild::start(&cancel_directory.0, false)?;
    helper.initialize()?;
    helper.send(json!({
        "jsonrpc": "2.0", "id": 9, "method": "tools/call",
        "params": {"name": "wait", "arguments": {}}
    }))?;
    let address = wait_for_bridge_port(&cancel_directory.0)?;
    helper.send(json!({
        "jsonrpc": "2.0", "method": "notifications/cancelled",
        "params": {"requestId": 9, "reason": "test cancellation"}
    }))?;
    wait_for_port_release(address)?;
    let after_cancel = helper.request(json!({
        "jsonrpc": "2.0", "id": 10, "method": "tools/call",
        "params": {"name": "echo", "arguments": {"value": "after cancellation"}}
    }))?;
    assert_eq!(
        after_cancel["result"]["structuredContent"]["echo"],
        "after cancellation"
    );
    helper.finish()?;

    let eof_directory = TestDirectory::new()?;
    let mut helper = McpChild::start(&eof_directory.0, false)?;
    helper.initialize()?;
    helper.send(json!({
        "jsonrpc": "2.0", "id": 11, "method": "tools/call",
        "params": {"name": "wait", "arguments": {}}
    }))?;
    let address = wait_for_bridge_port(&eof_directory.0)?;
    helper.finish()?;
    wait_for_port_release(address)?;
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
