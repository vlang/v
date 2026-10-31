module grpc

import net.http

// Native gRPC server over V's HTTP/2 stack. Where ConnectServer speaks the
// Connect protocol on HTTP/1.1, GrpcServer speaks real gRPC: HTTP/2 (TLS via
// ALPN, or cleartext h2c on a plain listener), 5-byte-framed messages, and the
// terminal status in HTTP/2 trailers. Both drive the same generated Service
// dispatch, so one service is served over either transport unchanged.
//
// Unary and streaming are both covered, but streaming is *buffered*: V's
// net.http Handler is one request in / one response out, so each call is a
// single POST whose body carries every message of the stream in order.
// Incremental (and bidirectional) delivery needs an upstream streaming-handler
// API.

const grpc_content_type = 'application/grpc+proto'

// GrpcService is the native-gRPC dispatch a generated <Name>Service satisfies
// (alongside Service, the Connect one): a list of request message payloads in,
// a list of response message payloads out — so one call covers unary (one in,
// one out), server-streaming (one in, many out), and client-streaming (many in,
// one out). found=false means the path belongs to another mounted service.
pub interface GrpcService {
mut:
	grpc_call(path string, reqs [][]u8, mut ctx ServerContext) !([][]u8, bool)
}

// GrpcServer serves mounted GrpcService implementations over native gRPC.
pub struct GrpcServer {
pub mut:
	addr     string
	services []GrpcService
	// TLS termination: set both `cert` and `cert_key` to serve gRPC over HTTPS
	// (ALPN h2). Left empty, the server runs cleartext h2c on the plain
	// listener — what insecure gRPC clients use.
	cert                   string
	cert_key               string
	in_memory_verification bool // `cert`/`cert_key` are PEM strings, not paths
}

// mount registers svc to answer the paths it claims.
pub fn (mut s GrpcServer) mount(svc GrpcService) {
	s.services << svc
}

// listen_and_serve starts the HTTP/2 listener and serves gRPC until it closes.
pub fn (mut s GrpcServer) listen_and_serve() ! {
	mut srv := http.Server{
		addr:                   s.addr
		handler:                s
		enable_http2:           true
		cert:                   s.cert
		cert_key:               s.cert_key
		in_memory_verification: s.in_memory_verification
	}
	srv.listen_and_serve()
}

// handle implements http.Handler; exposed so tests can drive it without
// sockets. Every gRPC response is HTTP 200 — the real outcome rides in the
// grpc-status trailer — so even failures go out through grpc_error, never an
// HTTP error code.
pub fn (mut s GrpcServer) handle(req http.Request) http.Response {
	mut ctx := ServerContext{
		request_headers: request_metadata(req.header)
	}
	if req.method != .post {
		return grpc_error(Status{ code: .internal, message: 'gRPC requires POST' }, ctx)
	}
	path := req.url.all_before('?')
	ct := req.header.get(.content_type) or { '' }
	if !ct.starts_with('application/grpc') {
		return grpc_error(Status{
			code:    .internal
			message: 'unexpected content-type `${ct}`'
		}, ctx)
	}
	// unary + client-streaming both arrive as one or more length-prefixed
	// request messages in the body
	reqs := decode_request_frames(req.data.bytes()) or {
		if err is StatusError {
			return grpc_error(err.status, ctx)
		}
		return grpc_error(Status{ code: .internal, message: err.msg() }, ctx)
	}
	for mut svc in s.services {
		replies, found := svc.grpc_call(path, reqs, mut ctx) or {
			if err is StatusError {
				return grpc_error(err.status, ctx)
			}
			return grpc_error(Status{ code: .internal, message: err.msg() }, ctx)
		}
		if found {
			return grpc_ok(replies, ctx)
		}
	}
	return grpc_error(Status{ code: .unimplemented, message: 'no such procedure: ${path}' }, ctx)
}

// decode_request_frames splits the request body into its message payloads,
// rejecting a compressed frame (we don't negotiate compression) as unimplemented
// and a truncated one as a decode error.
fn decode_request_frames(body []u8) ![][]u8 {
	frames := decode_frames(body)!
	mut msgs := [][]u8{cap: frames.len}
	for f in frames {
		if f.compressed {
			return StatusError{
				status: Status{
					code:    .unimplemented
					message: 'compressed request not supported'
				}
			}
		}
		msgs << f.payload
	}
	return msgs
}

// grpc_ok frames every reply message into the body and closes the stream with
// grpc-status: 0 — one message for unary, many for server-streaming, none for
// an empty stream (which folds to a Trailers-Only success).
fn grpc_ok(replies [][]u8, ctx ServerContext) http.Response {
	mut out := []u8{}
	for r in replies {
		out << encode_frame(r, false)
	}
	mut resp := http.Response{
		status_code: 200
		body:        out.bytestr()
	}
	resp.header.add(.content_type, grpc_content_type)
	apply_leading(mut resp, ctx)
	mut trailers := http.new_header()
	trailers.add_custom('grpc-status', '0') or {}
	apply_trailing(mut trailers, ctx)
	resp.trailers = trailers
	return resp
}

// grpc_error sends a Trailers-Only response: no body, so V's h2 server folds
// the status into a single HEADERS block — also the shape gRPC uses when a call
// fails before producing any message.
fn grpc_error(status Status, ctx ServerContext) http.Response {
	mut resp := http.Response{
		status_code: 200
	}
	resp.header.add(.content_type, grpc_content_type)
	apply_leading(mut resp, ctx)
	mut trailers := http.new_header()
	trailers.add_custom('grpc-status', int(status.code).str()) or {}
	if status.message != '' {
		trailers.add_custom('grpc-message', percent_encode(status.message)) or {}
	}
	apply_trailing(mut trailers, ctx)
	resp.trailers = trailers
	return resp
}

// apply_leading writes leading response metadata as ordinary headers; repeated
// values repeat.
fn apply_leading(mut resp http.Response, ctx ServerContext) {
	for k, vals in ctx.resp_headers {
		for v in vals {
			resp.header.add_custom(k, v) or {}
		}
	}
}

// apply_trailing writes trailing response metadata next to grpc-status and
// grpc-message in the trailers.
fn apply_trailing(mut trailers http.Header, ctx ServerContext) {
	for k, vals in ctx.resp_trailers {
		for v in vals {
			trailers.add_custom(k, v) or {}
		}
	}
}
