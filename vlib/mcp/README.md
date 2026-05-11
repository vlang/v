# `mcp`

Native [Model Context Protocol][spec] implementation for V — both client and
server, full coverage of the **2025-11-25** revision of the spec.

[spec]: https://modelcontextprotocol.io/specification/2025-11-25

## Capabilities

| Feature                                                | Status |
| ------------------------------------------------------ | :----: |
| JSON-RPC 2.0 base protocol                             | ✅     |
| stdio transport (newline-delimited)                    | ✅     |
| Streamable HTTP transport (POST + GET, SSE, sessions)  | ✅     |
| `Origin` header validation (DNS rebinding protection)  | ✅     |
| `MCP-Session-Id` and `MCP-Protocol-Version` headers    | ✅     |
| `Last-Event-ID` resumption                             | ✅     |
| Tools (with `annotations`)                             | ✅     |
| Resources, resource templates, `subscribe`/`updated`   | ✅     |
| Prompts                                                | ✅     |
| `completion/complete`                                  | ✅     |
| `logging/setLevel` + `notifications/message`           | ✅     |
| `notifications/progress` + cooperative cancellation    | ✅     |
| `*/list_changed` notifications (auto on `add_*`)       | ✅     |
| Server-initiated `roots/list`, `sampling/createMessage`, `elicitation/create` | ✅     |
| `Icon`, `BaseMetadata` (title), `Annotations` on tools/resources/prompts | ✅     |
| `Tool.execution.taskSupport` advertisement                | ✅     |
| Content helpers (`text`, `image`, `audio`, embedded resource, resource link) | ✅     |
| Tasks utility (`tasks/*`)                                 | ⏳ deferred (experimental) |
| OAuth Authorization                                       | ⏳ deferred (`SHOULD`)     |

A comprehensive demo server lives at
[`examples/mcp/server.v`](../../examples/mcp/server.v).

## Quick start — client

```v
import mcp

fn main() {
	mut client := mcp.connect('http://localhost:8000/mcp')!
	init := client.initialize()!
	println(init.server_info.name)
	client.close()
}
```

## Quick start — server

```v
import mcp

fn main() {
	mut server := mcp.new_server(
		name:           'my-v-mcp-server'
		version:        '1.0.0'
		enable_logging: true
	)
	server.add_tool(mcp.Tool{
		name:        'say_hello'
		description: 'Greets the caller'
		annotations: mcp.ToolAnnotations{
			read_only_hint: true
		}
	}, fn (_ mcp.Context, _ string) !mcp.ToolResult {
		return mcp.tool_text_result('Hello, user!')
	})!
	server.serve_stdio()!
}
```

## Cancellation and progress

Tool/resource/prompt handlers receive a `Context`. When the client supplies a
`_meta.progressToken`, the handler can call `ctx.notify_progress(progress, total, message)`.
For long-running work, poll `ctx.is_cancelled()` regularly — when the client
sends `notifications/cancelled`, the flag flips to `true` until the request
completes.

## Server-initiated requests

```v oksyntax
import mcp
import time

mut server := mcp.new_server(name: 'demo', version: '0')
session_id := 'session'
roots := server.list_roots(session_id, 5 * time.second)!
sampled := server.sample(session_id, mcp.CreateMessageParams{}, 30 * time.second)!
elicited := server.elicit(session_id, mcp.ElicitParams{}, 60 * time.second)!
```

These block until the client returns the matching JSON-RPC response (or until
the timeout fires).

## Content blocks

Tool, prompt and resource handlers return arrays of MCP content blocks. The
module ships ready-made helpers — pass the result through `tool_text_result`
or compose them by hand:

```v oksyntax
import mcp

text := mcp.text_content('done')
img := mcp.image_content('AAA=', 'image/png')
audio := mcp.audio_content('BBB=', 'audio/wav')
embedded_text := mcp.embedded_text_resource('res://config', 'application/json', '{}')
embedded_blob := mcp.embedded_blob_resource('res://blob', 'image/png', 'AAA=')
resource_link := mcp.resource_link_content(mcp.Resource{
	uri:  'res://docs'
	name: 'docs'
})
```

Each helper returns a JSON string conforming to the spec's `ContentBlock`
union (`type: "text" | "image" | "audio" | "resource" | "resource_link"`).

## Streamable HTTP details

- POST: returns JSON by default. Returns SSE if the client sends
  `Accept: text/event-stream` only.
- GET: opens an SSE stream of queued notifications. Resume with `Last-Event-ID`.
- DELETE: terminates the session (`MCP-Session-Id` required).
- 403 on disallowed `Origin`, 400 on unsupported `MCP-Protocol-Version`,
  406 when `Accept` lists neither `application/json` nor `text/event-stream`.

## Tests

```
v test vlib/mcp
```

`spec_compliance_test.v` cross-checks wire shapes against the
[official schema][schema]. Add a case there whenever a payload field changes.

[schema]: https://github.com/modelcontextprotocol/modelcontextprotocol/blob/main/schema/2025-11-25/schema.json