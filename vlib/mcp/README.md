# `mcp`

`mcp` is a Model Context Protocol module for V.

It provides:

- typed JSON-RPC request, notification, response, and initialize payload helpers
- a synchronous `Client` that performs the MCP initialize handshake automatically
- a server-side `Server` with tool, resource, resource template, and prompt registration
- streamable HTTP and stdio transports behind a common transport interface
- queues for server notifications and server-initiated requests
- buffered while waiting for a response

## HTTP example

```v
import mcp

fn main() {
	mut client := mcp.connect('http://localhost:8000/mcp')!
	init := client.initialize()!
	println(init.server_info.name)
	client.close()
}
```

## Stdio example

```v
import mcp

fn main() {
	mut client := mcp.connect_stdio('my-mcp-server', ['--stdio'], mcp.ClientConfig{})!
	client.notify('notifications/cancelled', {
		'requestId': 1
	})!
	client.close()
}
```

## Server example

```v
import mcp

fn main() {
	mut server := mcp.new_server(
		name:    'my-v-mcp-server'
		version: '1.0.0'
	)

	server.add_tool(mcp.Tool{
		name:        'say_hello'
		description: 'Greets a user by name'
	}, fn (_ mcp.Context, _ string) !mcp.ToolResult {
		return mcp.tool_text_result('Hello, user!')
	})!

	server.serve_stdio()!
}
```

## Notes

- `Client.request` auto-initializes when needed.
- `Client.take_notifications` and `Client.take_requests` drain queued server messages.
- The HTTP transport supports JSON responses and `text/event-stream` POST responses.
- `Server.serve_http` uses a single MCP endpoint, returns JSON by default, and will return SSE for POST
  requests that accept only `text/event-stream`.
