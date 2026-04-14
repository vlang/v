# `mcp`

`mcp` is a small Model Context Protocol client module for V.

It provides:

- typed JSON-RPC request, notification, response, and initialize payload helpers
- a synchronous `Client` that performs the MCP initialize handshake automatically
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

## Notes

- `Client.request` auto-initializes when needed.
- `Client.take_notifications` and `Client.take_requests` drain queued server messages.
- The HTTP transport supports JSON responses and `text/event-stream` POST responses.
