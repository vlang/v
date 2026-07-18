# fasthttp

`fasthttp` is a low-level, high-performance HTTP/1.1 server for V built on
platform-native I/O multiplexing. It gives you raw control over request bytes and
response bytes with a small, explicit API, and is the parallel backend used by
[`veb`](../veb).

## Features

- **Native I/O multiplexing**: `epoll` on Linux, `kqueue` on macOS/BSD, IOCP on
  Windows. On Linux each worker owns its own `SO_REUSEPORT` listener (kernel load
  balancing across cores).
- **Zero per-request allocation on the hot path** (Linux): each connection owns a
  read buffer and a write buffer that are **reused for its whole lifetime**, and
  closed connections return their state — buffers included — to a per-worker
  free-list. No per-connection hash maps, no per-request buffer churn.
- **HTTP/1.1 pipelining**: several requests arriving in one read are framed
  individually and answered into one batched write. TCP-fragmented requests are
  reassembled deterministically via exact-length framing.
- **Two handler contracts**:
  - **Append handler** (recommended): write the raw response straight into the
    connection's reused buffer — no response object, no copy.
  - **Classic handler**: build and return an `HttpResponse`.
- **Lock-free per-worker state**: an optional `make_state` hook gives each worker
  thread its own state (a DB connection, a reused scratch buffer) with no mutex.
- **Takeover**: hand a connection off to your own code (SSE / WebSocket) or write
  the response yourself and keep the connection alive.
- **Zero-copy file bodies**: return a `file_path` and the body is streamed with
  `sendfile(2)`.
- **Graceful shutdown**: drain in-flight responses, then stop.

## Installation

Part of the standard library:

```v
import fasthttp
```

## Quick start (append handler)

The append handler appends the complete raw HTTP response (status line + headers
+ body) into the reused `out` buffer and returns a `Step`. It is the zero-copy,
pipelining-friendly contract.

```v
import fasthttp

fn handle(req fasthttp.HttpRequest, mut out []u8, ws voidptr, mut ctl fasthttp.ResponseControl) fasthttp.Step {
	path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()
	if path == '/' {
		out << 'HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 13\r\n\r\nHello, World!'.bytes()
	} else {
		out << 'HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n'.bytes()
	}
	return .done
}

fn main() {
	mut server := fasthttp.new_server(fasthttp.ServerConfig{
		port:           3000
		append_handler: handle
	}) or {
		eprintln('failed to create server: ${err}')
		return
	}
	println('listening on http://localhost:3000/')
	server.run() or { eprintln('error: ${err}') }
}
```

## Quick start (classic handler)

The classic handler builds and returns an `HttpResponse`. It is simpler when you
already have the response bytes in hand.

```v
import fasthttp

fn handle(req fasthttp.HttpRequest) !fasthttp.HttpResponse {
	return fasthttp.HttpResponse{
		content: 'HTTP/1.1 200 OK\r\nContent-Length: 13\r\n\r\nHello, World!'.bytes()
	}
}

fn main() {
	mut server := fasthttp.new_server(fasthttp.ServerConfig{
		port:    3000
		handler: handle
	}) or {
		eprintln('failed to create server: ${err}')
		return
	}
	server.run() or { eprintln('error: ${err}') }
}
```

Set **exactly one** of `handler` or `append_handler`; `new_server` returns an
error otherwise.

## Reading the request

`HttpRequest` exposes zero-copy `Slice`s (`start`, `len`) into `buffer`:

```v ignore
method := req.buffer[req.method.start..req.method.start + req.method.len].bytestr()
path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()
body := req.buffer[req.body.start..req.body.start + req.body.len]
```

The parser fills `method`, `path`, `version`, `header_fields` and `body`. The
request-framing helpers (`frame_request_length`, `frame_expected_total`,
`frame_head_len`) are the pure functions the read loop uses to split pipelined
requests; they are exported for testing and for building your own reader.

## The append-handler contract

```v ignore
pub type AppendHandler = fn (req HttpRequest, mut out []u8, worker_state voidptr, mut ctl ResponseControl) Step
```

- `out` — the connection's persistent write buffer. Append the complete raw HTTP
  response. Everything appended during one readiness event is sent in a single
  write; the buffer is reused across requests — never free it or keep a reference.
- `worker_state` — the value `ServerConfig.make_state` returned on this worker
  thread (see below); `nil` if unset.
- `ctl ResponseControl` — out-of-band controls:
  - `takeover_mode` — `.manual` hands the fd off (you own it, e.g. SSE/WebSocket);
    `.reusable` means you wrote the response yourself but want keep-alive.
  - `should_close` — close the connection after this response.
  - `file_path` — stream this file after the appended bytes (`sendfile`).
- Return `Step`:
  - `.done` — response complete in `out`; keep the connection alive.
  - `.close` — send `out`, then close.
  - `.suspend` — reserved for future async handlers (no watch reactor yet, so it
    currently drops the connection).

## Lock-free per-worker state

`ServerConfig.make_state` is called once per worker thread; its return value
reaches every request on that worker as `worker_state`. Because each worker gets
its own instance, no locking is needed:

```v ignore
struct WorkerState {
mut:
	scratch []u8 // reused per-request render buffer
}

fn make_state() voidptr {
	return &WorkerState{}
}

fn handle(req fasthttp.HttpRequest, mut out []u8, ws voidptr, mut ctl fasthttp.ResponseControl) fasthttp.Step {
	mut st := unsafe { &WorkerState(ws) }
	st.scratch.clear()
	// ... build into st.scratch, then `out << st.scratch` ...
	return .done
}

// fasthttp.ServerConfig{ ..., append_handler: handle, make_state: make_state }
```

## Configuration

`ServerConfig` fields: `family` (`.ip` / `.ip6`), `port`,
`max_request_buffer_size` (bounds the request head; an oversized head gets `413`),
`timeout_in_seconds` (read/write deadlines; a stalled request gets `408`),
`user_data` (an opaque pointer surfaced as `HttpRequest.user_data`), `handler` /
`append_handler`, and `make_state`.

## Lifecycle

```v ignore
mut server := fasthttp.new_server(config)!
handle := server.handle()
spawn server.run()
handle.wait_till_running()!            // block until the listener is bound
// ... serve ...
handle.shutdown(timeout: 5 * time.second)! // drain in-flight, then stop
```

## Platform support

| Platform | Backend | Pooling + pipelining | Append handler | make_state |
|---|---|---|---|---|
| Linux   | epoll  | yes | yes | yes |
| macOS/BSD | kqueue | one request per read | yes | yes |
| Windows | IOCP   | one request per read | yes | not yet (`run` WIP) |

## Request-scoped allocation with `-prealloc`

When compiled with `-prealloc`, the **classic** handler path runs each request
inside a scoped bump arena, freed as a unit after the response is sent. The
**append** handler path does not open a reactor arena (growing the reused write
buffer inside a scope would free it out from under the connection); an append
handler that wants request-scoped arenas manages its own and leaves it before
writing into `out`. To trace arena usage:

```sh
v -prealloc -d trace_prealloc run .
```

## Example

See `examples/fasthttp/` for a small multi-route server:

```sh
./v run examples/fasthttp
```
