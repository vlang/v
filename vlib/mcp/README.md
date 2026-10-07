# `mcp`

Native [Model Context Protocol][spec] implementation for V — both client and
server, covering two revisions of the spec:

- **2025-11-25** — the stateful revision, and still the default: the
  `initialize` handshake plus `MCP-Session-Id` sessions.
- **2026-07-28** — the sessionless revision: no handshake, no session id, and
  every request declares its own revision, client info and capabilities.

Both are supported at once. A server speaks 2025-11-25 by default and switches
per request based on the version the client declares; a client picks the mode
explicitly. The wire shapes never mix: a request answered under 2025-11-25
carries no 2026-only fields, and vice versa.

[spec-2025]: https://modelcontextprotocol.io/specification/2025-11-25
[spec-2026]: https://modelcontextprotocol.io/specification/2026-07-28
[spec]: https://modelcontextprotocol.io/specification/2026-07-28

## Capabilities

| Feature                                                          | Status             |
| ---------------------------------------------------------------- | :----------------: |
| JSON-RPC 2.0 base protocol                                       | ✅                 |
| stdio transport (newline-delimited)                              | ✅                 |
| Streamable HTTP transport (POST + GET, SSE, sessions)            | ✅                 |
| `Origin` header validation (DNS rebinding protection)            | ✅                 |
| `MCP-Session-Id` and `MCP-Protocol-Version` headers              | ✅ 2025-11-25      |
| `Last-Event-ID` resumption                                       | ✅ 2025-11-25      |
| Tools (with `annotations`)                                       | ✅                 |
| Resources, resource templates                                    | ✅                 |
| `resources/subscribe` / `unsubscribe` / `resources/updated`      | ✅ 2025-11-25      |
| Prompts                                                          | ✅                 |
| `completion/complete`                                            | ✅                 |
| `logging/setLevel` + `notifications/message`                     | ✅ 2025-11-25      |
| `notifications/progress` + cooperative cancellation              | ✅                 |
| `*/list_changed` notifications (auto on `add_*`)                 | ✅                 |
| Server-initiated `roots/list`, `sampling/createMessage`, `elicitation/create` | ✅ 2025-11-25 |
| `Icon`, `BaseMetadata` (title), `Annotations`                    | ✅                 |
| `Tool.execution.taskSupport` advertisement                       | ✅                 |
| Content helpers (`text`, `image`, `audio`, embedded, link)       | ✅                 |
| `server/discover`                                                | ✅ 2026-07-28      |
| Stateless operation (no handshake, per-request `_meta`)          | ✅ 2026-07-28      |
| `subscriptions/listen`                                           | ✅ 2026-07-28      |
| Multi Round-Trip Requests (MRTR, `resultType: input_required`)   | ✅ 2026-07-28      |
| `resultType` on every result                                     | ✅ 2026-07-28      |
| CacheableResult `ttlMs` / `cacheScope`                           | ✅ 2026-07-28      |
| `Mcp-Method` / `Mcp-Name` request headers                        | ✅ 2026-07-28      |
| Tasks utility (`tasks/*`)                                       | ⏳ deferred (experimental) |
| OAuth Authorization                                              | ⏳ deferred (`SHOULD`)     |

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

## Mounting on an existing HTTP server

`serve_http` owns its listener, so it takes the whole port. When an existing
server already owns the port, dispatch MCP requests to the server in-process
instead of starting a second listener:

- `server.handle_http_request(req)` returns the `http.Response` for one
  request. Use it from a host route that already holds the request.
- `server.http_handler()` returns an `http.Handler` for hosts that take one,
  e.g. another `http.Server`.

Both apply the same routing rule as `serve_http`: the request URL must match
`ServerConfig.http_path` (default `/mcp`). The host server owns the listener
and its shutdown. `server.close()` only stops a listener started by
`serve_http`, so it has no effect on a mounted server.

```v
import mcp
import net.http

struct App {
mut:
	mcp http.Handler
}

fn (mut app App) handle(req http.Request) http.Response {
	if req.url.all_before('?') == '/mcp' {
		return app.mcp.handle(req)
	}
	mut response := http.Response{}
	response.set_status(.not_found)
	return response
}

fn main() {
	mut server := mcp.new_server(name: 'mounted', version: '1.0.0')
	mut app := App{
		mcp: server.http_handler()
	}
	mut host := &http.Server{
		addr:    '127.0.0.1:8080'
		handler: app
	}
	host.listen_and_serve()
}
```

A `veb` route can call `handle_http_request` and copy the response:

```v
import mcp
import veb

pub struct Context {
	veb.Context
}

pub struct App {
mut:
	mcp &mcp.Server = unsafe { nil }
}

@['/mcp'; delete; get; post]
pub fn (mut app App) mcp_endpoint(mut ctx Context) veb.Result {
	resp := app.mcp.handle_http_request(ctx.req)
	ctx.res.set_status(resp.status())
	for key in resp.header.keys() {
		for value in resp.header.custom_values(key) {
			ctx.res.header.add_custom(key, value) or {}
		}
	}
	return ctx.send_response_to_client(resp.header.get(.content_type) or { '' }, resp.body)
}

fn main() {
	mut server := mcp.new_server(name: 'veb-mounted', version: '1.0.0')
	mut app := &App{
		mcp: &server
	}
	veb.run[App, Context](mut app, 8080)
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
the timeout fires). They are a 2025-11-25 facility; under 2026-07-28 a server
cannot originate requests at all and uses MRTR instead — see below.

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

## 2026-07-28 stateless mode

Under 2026-07-28 there is no session and no handshake. Every request carries
its own protocol state in `params._meta`. The first two are **required** by
the schema — a request missing either is rejected as a malformed request
(`-32600`):

- `io.modelcontextprotocol/protocolVersion` — the revision, required
- `io.modelcontextprotocol/clientCapabilities` — what the client can do, required
- `io.modelcontextprotocol/clientInfo` — who is calling
- `io.modelcontextprotocol/logLevel` — opts the request into log notifications

Every 2026-07-28 result also identifies its server in
`_meta["io.modelcontextprotocol/serverInfo"]`. With the handshake gone, that
`_meta` is the only place a client can learn who answered — which is why
`server/discover` needs no separate identity field.

### Server

`ServerConfig.supported_versions` lists the revisions a server speaks (default:
both). The preferred `protocol_version` must be in that list. A request that
names a supported revision in `_meta` is dispatched sessionlessly, and the
revision it named decides its wire shape — so one server can serve both kinds
of client at once.

```v oksyntax
import mcp

mut server := mcp.new_server(
	name:               'dual-stack'
	version:            '1.0.0'
	supported_versions: ['2025-11-25', '2026-07-28']
	cache_ttl_ms:       300_000
	cache_scope:        'private'
)
```

`server/discover` tells a client what the server speaks, and is callable before
anything else:

```json
{"supportedVersions":["2025-11-25","2026-07-28"],"capabilities":{...}}
```

Over HTTP, a 2026-07-28 POST must also carry `MCP-Protocol-Version` (equal to
the `_meta` value) plus `Mcp-Method`, and `Mcp-Name` for the methods that
address a named tool, resource or prompt. A mismatch is a `HeaderMismatchError`
(-32020); an unknown revision is an `UnsupportedProtocolVersionError` (-32022)
whose `data` names the versions the server does speak. No `MCP-Session-Id` is
ever issued or expected.

### Client

`mcp.connect_2026` builds a client that skips the handshake entirely. Every
request it sends carries the `_meta` block and, over HTTP, the matching
headers:

```v oksyntax
import mcp

mut client := mcp.connect_2026('http://localhost:8000/mcp', mcp.ClientConfig{
	elicitation_handler: fn (_ string) string {
		return '{"action":"accept","content":{"ok":true}}'
	}
})!
tools := client.request[mcp.ListToolsResult, mcp.ListToolsResult]('tools/list', mcp.empty)
client.close()
```

If the server rejects the revision with -32022, the client reads
`data.supported`, switches to the first revision listed and retries once — the
2025-11-25 handshake runs normally from there.

### Listening for notifications

`client.listen` opens a subscription and returns the subset the server agreed
to honor; the notifications themselves arrive in `client.take_notifications()`,
each tagged with `_meta["io.modelcontextprotocol/subscriptionId"]`.
Subscription metadata preserves the originating request ID's JSON type and value:
numeric `7` and string `"7"` identify different streams. Acknowledgment matching
keeps that distinction. `subscription_id_of` returns decoded string IDs and
numeric IDs as text for display.
The call returns when its matching acknowledgment arrives, without waiting for
the live stdio subscription to close. Notifications read while awaiting later
responses remain available through `take_notifications`. HTTP listen requests
use `Accept: text/event-stream` to agree with the server's subscription transport.

```v oksyntax
import mcp

filter := client.listen(mcp.SubscriptionListenParams{
	notifications: mcp.SubscriptionFilter{
		tools_list_changed: true
		resource_uris:      ['res://config']
	}
})!
if filter.tools_list_changed {
	for notification in client.take_notifications() {
		if mcp.subscription_id_of(notification) or { '' } == '7' {
			println(notification.method)
		}
	}
}
```

**Transport caveat.** `net.http` is strictly request→response, so a
`subscriptions/listen` over HTTP is a *finite* SSE stream: it carries the
`acknowledged` notification followed by a `SubscriptionsListenResult`, and the
subscription does not outlive the response — clients re-listen when they need
live updates. On stdio the subscription is genuinely long-lived: it shares the
single stdout channel, notifications are pushed as they happen, and the server
terminates the stream with `notifications/cancelled` when the transport closes.

## Multi Round-Trip Requests (MRTR)

2026-07-28 removes server-initiated requests. Instead of calling the client
mid-flight, a handler returns an intermediate result asking for input, and the
client retries the *original* request with the answers attached. Handlers use a
take-or-require pattern: try to read the answer, and if it is not there yet,
ask for it.

```v oksyntax
import mcp

server.add_tool(mcp.Tool{ name: 'delete_project' }, fn (ctx mcp.Context, _ string) !mcp.ToolResult {
	// First pass: no answer yet, so ask.
	if answer := ctx.take_elicit_result('name') {
		return mcp.tool_text_result('deleted ${answer.content}')
	}
	// `content` is the raw JSON the client returned.
	return ctx.require_elicit('name', mcp.ElicitParams{
		message:          'Which project should I delete?'
		requested_schema: mcp.ElicitSchema{
			properties: '{"name":{"type":"string"}}'
			required:   ['name']
		}
	})
})!
```

The first call answers with `resultType: "input_required"` and an
`inputRequests` map naming what it needs. The client fills in
`params.inputResponses` and resends, and the handler is invoked again from the
top — so the `take_*` branch now wins and the tool finishes with
`resultType: "complete"`.

The typed readers are `take_elicit_result`, `take_roots_result` and
`take_sampling_result`; the asking counterparts are `require_elicit`,
`require_roots` and `require_sampling`. `ctx.request_state` carries the opaque
`requestState` token the server sent, and a `require_*` helper echoes it back
on the next round.

On the client, register `roots_handler`, `sampling_handler` and
`elicitation_handler` in `ClientConfig`; the client answers the embedded
requests and retries automatically (up to 8 rounds).
The client reads the top-level `resultType` regardless of JSON member order or
formatting; a result without that member is treated as complete.

## Streamable HTTP details

2025-11-25 transport behaviour:

- POST: returns JSON by default. Returns SSE if the client sends
  `Accept: text/event-stream` only.
- GET: opens an SSE stream of queued notifications. Resume with `Last-Event-ID`.
- DELETE: terminates the session (`MCP-Session-Id` required).
- 403 on disallowed `Origin`, 400 on unsupported `MCP-Protocol-Version`,
  406 when `Accept` lists neither `application/json` nor `text/event-stream`.

2026-07-28 stateless requests additionally require `Mcp-Method` (and
`Mcp-Name` where it applies) on every POST, and never receive an
`MCP-Session-Id`.

## Tests

```
v test vlib/mcp
```

`spec_compliance_test.v` and `spec_compliance_2026_test.v` cross-check wire
shapes against the [official 2025-11-25 schema][schema-2025] and
[2026-07-28 schema][schema-2026]. Add a case there whenever a payload field
changes.

[schema-2025]: https://github.com/modelcontextprotocol/modelcontextprotocol/blob/main/schema/2025-11-25/schema.json
[schema-2026]: https://github.com/modelcontextprotocol/modelcontextprotocol/blob/main/schema/2026-07-28/schema.json
