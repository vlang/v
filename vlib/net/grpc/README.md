# net.grpc

`net.grpc` speaks gRPC over V's standard-library HTTP stack: the 5-byte
length-prefixed message framing, the `grpc-status` / `grpc-message` terminal
status, and per-call metadata. It is transport-only — messages cross the wire as
`[]u8` — so it pairs with any protobuf codec you like, including the bundled
`encoding.protobuf` runtime and its `v pbgen` generator; see
[examples/grpc](../../../examples/grpc/README.md) for a service built that way.

Tracking issue: [vlang/v#5017](https://github.com/vlang/v/issues/5017).

**Status: experimental.** The module is listed in `vlib/.vdocignore`, so it is
not part of the published standard-library docs. The wire behaviour is
interoperability-tested against grpc-go and the official Connect conformance
suite; the V API surface may still change.

## Requirements

gRPC is HTTP/2-only, and `net.http` only negotiates HTTP/2 for `https` URLs —
it advertises `h2` over ALPN on the TLS listener and nothing else. Two
consequences:

* Point `Client.base_url` at an `https://` URL. A plain `http://` target stays
  on HTTP/1.1 and a conforming gRPC server will reject it.
* `Client` opts into `enable_http2` on every request for you. If you drive
  `http.fetch` yourself, set `enable_http2: true` or the call silently degrades
  to HTTP/1.1 and the response carries no `grpc-status`.

## Server

Implement `GrpcService` for each service and mount it:

```v ignore
module main

import net.grpc

struct EchoService {}

fn (mut s EchoService) grpc_call(path string, reqs [][]u8, mut ctx grpc.ServerContext) !([][]u8, bool) {
	match path {
		'/echo.Echo/Say' {
			// one request message in, one response message out
			return [reqs[0]], true
		}
		'/echo.Echo/Fan' {
			// one in, many out (server-streaming)
			return [reqs[0], reqs[0], reqs[0]], true
		}
		else {
			// found=false means the path belongs to another mounted service
			return [][]u8{}, false
		}
	}
}

fn main() {
	mut srv := grpc.GrpcServer{
		addr: ':9000'
		cert: 'server.pem' // leave both empty for cleartext h2c
		cert_key: 'server.key'
	}
	srv.mount(EchoService{})
	srv.listen_and_serve()!
}
```

Set both `cert` and `cert_key` to terminate TLS and advertise `h2`; leave them
empty and the server runs cleartext h2c on the plain listener, which is what
insecure gRPC clients expect. Add `in_memory_verification: true` to pass the
certificate and key as PEM strings instead of file paths.

Every gRPC response is HTTP 200 — the real outcome rides in the HTTP/2 trailers,
so even failures go out as a Trailers-Only status block rather than an HTTP
error code.

## Client

```v ignore
module main

import net.grpc
import time

fn main() {
	mut c := grpc.Client{
		base_url: 'https://api.example.com'
	}
	reply := c.unary('/echo.Echo/Say', 'hello'.bytes())!
	println(reply.payload.bytestr())
}
```

`unary` returns the single response payload plus response metadata.
`server_stream` buffers every response message of the stream, and
`client_stream` sends the whole request stream in one POST. Non-OK outcomes,
including a fired deadline, come back as a `StatusError`:

```v ignore
reply := c.unary('/echo.Echo/Say', 'hello'.bytes(), grpc.timeout(5 * time.second)) or {
	if err is grpc.StatusError {
		eprintln('${e.status.code}: ${e.status.message}')
		return
	}
	return
}
```

For certificate handling, set `verify` to the path of a `rootca.pem` holding the
trusted CA certificate(s) (leave it empty to use the platform trust store), and
`cert` / `cert_key` for mutual TLS.

A response's explicit `grpc-status` takes precedence over its HTTP status.
The client maps HTTP error statuses to gRPC codes only when `grpc-status` is absent.

## Metadata

Request metadata is multi-valued, mirroring gRPC's own model: a key may repeat,
so values are ordered lists. Incoming header names are case-insensitive; values
under different capitalizations retain their original order. Client defaults merge
with per-call options, which
compose left to right:

```v ignore
mut c := grpc.Client{
	base_url: 'https://api.example.com'
	metadata: {
		'x-api-version': ['2']
	}
}
reply := c.unary('/echo.Echo/Say', 'hi'.bytes(), grpc.header('x-request-id', 'r1'),
	grpc.metadata({'x-trace': ['a', 'b']}))!
```

On the server, `ServerContext.header` reads the first incoming value for a key
and the metadata a handler writes with `set_header` / `add_header` /
`set_trailer` / `add_trailer` goes out as response headers and HTTP/2 trailers
respectively. V's HTTP/2 layer folds trailers into the response header set, so
the client sees both in `reply.metadata`.

## Connect protocol

`ConnectServer` is the alternative server: the Connect unary protocol
([connectrpc.com](https://connectrpc.com/docs/protocol)) over plain HTTP/1.1,
with the same `Service` dispatch and the same handler code. gRPC clients cannot
talk to it — that needs HTTP/2 trailers — but connect-go and connect-es clients
can, natively, and Envoy bridges the rest.

```v ignore
mut srv := grpc.ConnectServer{
	addr: ':9000'
}
srv.mount(my_service)
srv.listen_and_serve()!
```

## Limitations

* **Streaming is buffered, not incremental.** V's `net.http` handler is one
  request in and one response out, so each call is a single POST whose body
  carries every message of the stream in order. True incremental delivery — and
  bidirectional streaming — needs a streaming-handler API in `net.http`.
* **No compression.** A frame with the compressed flag set is rejected as
  `unimplemented` rather than decompressed; no `grpc-encoding` is negotiated.
* **Deadlines are approximate.** `grpc.timeout(d)` always sends the
  `grpc-timeout` header, which is the server's half of the contract. The
  client-side half rides on `FetchConfig.read_timeout`, which `net.http`
  enforces on the HTTP/2 path as an idle watchdog polled at roughly 500 ms
  granularity (`h2_mux_conn.v`) — so a deadline fires a little after its
  nominal time, and a handler that answers first wins.
* **No server reflection or channel multiplexing API.** Connections are pooled
  by `net.http`, but there is no `Channel` type to hand out.
