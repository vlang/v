# net.grpc example: an etcd-lite key-value service

A worked example of [`net.grpc`](../../vlib/net/grpc/README.md): a small
key-value store served over **native gRPC**, exercising every RPC shape.

```
examples/grpc/
  server.v          the service, served over gRPC on HTTP/2 + TLS
  client.v          calls every RPC against a running server
  kv.proto          the schema, kept next to the code that implements it
  kv/codec.v        a hand-written proto3 encoder/decoder for that schema
  kv/codec_test.v   tests for the codec
  kv/service.v      the GrpcService implementation
  cert/             a self-signed certificate so gRPC has something to speak
```

## Run it

```sh
v run examples/grpc/server.v     # terminal 1
v run examples/grpc/client.v     # terminal 2
```

The client prints one section per RPC shape: unary, server streaming, client
streaming, a call with a deadline and request metadata, and two error cases.

## The four RPCs

| RPC | Shape | Path |
| --- | --- | --- |
| `Get` | unary | `/kv.KV/Get` |
| `Put` | unary | `/kv.KV/Put` |
| `Scan` | server streaming (one request, many responses) | `/kv.KV/Scan` |
| `PutMany` | client streaming (many requests, one response) | `/kv.KV/PutMany` |

## Two things worth copying

**A handler reports failure by returning a `StatusError`, not by encoding one.**
`GrpcServer` turns a returned `StatusError` into the `grpc-status` trailer, which
is what makes it arrive at the client as a typed code:

```v ignore
fn (mut s Service) put(req PutRequest, mut ctx grpc.ServerContext) ![]u8 {
	if req.key.len == 0 {
		return grpc.StatusError{
			status: grpc.Status{
				code:    .invalid_argument
				message: 'key must not be empty'
			}
		}
	}
	// ...
}
```

Wrapping the `StatusError` in a reply payload instead — `return [grpc.StatusError{...}]` —
looks equivalent but is not: it becomes a framed body the client tries to parse
as a message, and the call fails with a framing error instead of a status.

**`net.grpc` never looks inside a message.** Payloads are `[]u8` end to end, so
the codec is a separate concern. This example hand-writes one to stay free of
dependencies; a real project would generate it:

```sh
v install protobuf.v
v run cmd/vpbgen -m kv -o kv/codec_generated.v -grpc kv/service_generated.v kv.proto
```

and delete `kv/codec.v` and `kv/service.v`. Nothing else changes, because the
generated code plugs into the same `[][]u8` boundary.

## The certificate

gRPC is HTTP/2-only, and V's `net.http` only negotiates HTTP/2 for `https` URLs —
it advertises `h2` over ALPN on the TLS listener and nothing else. So the
example has to speak TLS even on loopback; there is no cleartext client path to
test against.

`cert/server.crt` and `cert/server.key` are a self-signed pair for `localhost`,
valid until 2050, copied from `net.http`'s own TLS tests. To make your own with
[step-ca](https://smallstep.com/docs/step-ca/):

```sh
step ca certificate "localhost" cert/server.crt cert/server.key \
	--profile localhost --no-password --insecure
```

The client trusts that certificate by naming it as its own root:

```v ignore
mut client := grpc.Client{
	base_url: 'https://localhost:50051'
	verify:   os.join_path(os.dir(@FILE), 'cert', 'server.crt')
}
```

That is the self-signed equivalent of pinning a CA. In real use, point `verify`
at your CA bundle. Leaving `cert`/`cert_key` empty on the server instead gives a
cleartext h2c listener, which an insecure gRPC client can talk to — but V's
client cannot, because it has no h2c path.

## Limitations

`net.grpc`'s limitations apply here too, and `Scan`/`PutMany` show both:

* **Streaming is buffered, not incremental.** `net.http`'s handler is one
  request in and one response out, so each call is a single POST whose body
  carries the whole stream. A `Scan` over a large keyspace materializes every
  value in memory on both sides.
* **No compression.** A frame with the compressed flag set is rejected as
  `unimplemented`.
