module grpc

import time

// Per-call configuration for a unary RPC, applied via functional options
// (the Go-gRPC idiom): `client.method(req, grpc.timeout(d), grpc.header(k, v))`.
// Options compose left-to-right over the client's defaults; later ones win.

// CallOption mutates the resolved config for one call.
pub type CallOption = fn (mut c CallConfig)

// CallConfig is the config for a single call after the client defaults and
// every CallOption have been applied. Metadata is multi-valued (a header key
// may repeat), mirroring the server's ServerContext.
pub struct CallConfig {
pub mut:
	timeout  time.Duration       // deadline for the call; 0 = transport default
	metadata map[string][]string // request headers (metadata) sent with the call
}

// timeout sets a deadline: a `grpc-timeout` header for the server plus a
// matching client-side read/write timeout, surfaced as `deadline_exceeded`.
pub fn timeout(d time.Duration) CallOption {
	return fn [d] (mut c CallConfig) {
		c.timeout = d
	}
}

// header appends one request header (metadata entry) to the call; calling it
// again with the same key adds another value rather than replacing.
pub fn header(key string, value string) CallOption {
	return fn [key, value] (mut c CallConfig) {
		c.metadata[key] << value
	}
}

// metadata appends several request headers to the call at once.
pub fn metadata(kvs map[string][]string) CallOption {
	return fn [kvs] (mut c CallConfig) {
		for k, vals in kvs {
			c.metadata[k] << vals
		}
	}
}

// Reply pairs a decoded unary response with the server's response metadata.
// V's HTTP/2 layer merges response headers and trailers into one set, so
// `metadata` holds both, minus the gRPC control headers the client consumes.
// Keys may repeat, so values are ordered lists.
pub struct Reply[T] {
pub:
	msg      T
	metadata map[string][]string
}

// RawReply is the untyped result of Client.unary: the single response message
// payload plus response metadata. Generated stubs decode it into a Reply[T].
pub struct RawReply {
pub:
	payload  []u8
	metadata map[string][]string
}

// RawStreamReply is the untyped result of Client.server_stream: every response
// message payload (buffered) plus response metadata. Generated server-streaming
// stubs decode the list into a Reply[[]T].
pub struct RawStreamReply {
pub:
	payloads [][]u8
	metadata map[string][]string
}

// encode_grpc_timeout renders a duration as a grpc-timeout value: the finest
// unit whose integer value fits the spec's 8-digit maximum, so short deadlines
// keep full precision and long ones still fit.
fn encode_grpc_timeout(d time.Duration) string {
	if d <= 0 {
		return '0n'
	}
	ns := i64(d)
	if ns <= 99_999_999 {
		return '${ns}n'
	}
	us := ns / 1_000
	if us <= 99_999_999 {
		return '${us}u'
	}
	ms := ns / 1_000_000
	if ms <= 99_999_999 {
		return '${ms}m'
	}
	secs := ns / 1_000_000_000
	if secs <= 99_999_999 {
		return '${secs}S'
	}
	mins := secs / 60
	if mins <= 99_999_999 {
		return '${mins}M'
	}
	return '${mins / 60}H'
}
