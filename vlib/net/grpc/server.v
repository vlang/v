module grpc

import net.http
import json2
import encoding.base64

// Connect-protocol unary server (connectrpc.com) over V's stdlib HTTP/1.1
// server: POST /pkg.Service/Method with a bare message body in the proto or
// JSON codec. Errors leave as Connect JSON ({"code","message","details"})
// with the spec's HTTP status mapping. gRPC clients cannot talk to this — that
// needs HTTP/2 trailers — but connect-go and connect-es clients can, natively,
// and Envoy bridges the rest.

pub enum Codec {
	proto
	json
}

// ServerContext carries per-call metadata between the transport and a handler:
// request leading metadata in, response leading metadata (headers) and
// trailing metadata (trailers) out. Keys are case-insensitive and may repeat
// (HTTP metadata is multi-valued), so values are ordered lists. In the Connect
// unary protocol, trailers are sent as response headers with a `Trailer-`
// prefix; in native gRPC they are real HTTP/2 trailers.
pub struct ServerContext {
mut:
	resp_headers  map[string][]string
	resp_trailers map[string][]string
pub:
	request_headers map[string][]string
}

// header returns the first incoming value for a metadata key (case-insensitive),
// or '' — the common single-value case. For the rare repeated key, read the
// public `request_headers` map directly.
pub fn (c &ServerContext) header(key string) string {
	vals := c.request_headers[key.to_lower()] or { return '' }
	return if vals.len > 0 { vals[0] } else { '' }
}

// set_header replaces any leading response metadata for key with a single value.
pub fn (mut c ServerContext) set_header(key string, value string) {
	c.resp_headers[key.to_lower()] = [value]
}

// add_header appends a leading response metadata value, keeping any existing.
pub fn (mut c ServerContext) add_header(key string, value string) {
	c.resp_headers[key.to_lower()] << value
}

// set_trailer replaces any trailing response metadata for key with a single value.
pub fn (mut c ServerContext) set_trailer(key string, value string) {
	c.resp_trailers[key.to_lower()] = [value]
}

// add_trailer appends a trailing response metadata value, keeping any existing.
pub fn (mut c ServerContext) add_trailer(key string, value string) {
	c.resp_trailers[key.to_lower()] << value
}

// Service is implemented by generated <Name>Service dispatch structs;
// found=false means the path belongs to another service.
pub interface Service {
mut:
	call(path string, codec Codec, body []u8, mut ctx ServerContext) !([]u8, bool)
}

// ConnectServer serves mounted Service implementations over the Connect unary
// protocol on HTTP/1.1.
pub struct ConnectServer {
pub mut:
	addr                   string
	services               []Service
	cert                   string
	cert_key               string
	in_memory_verification bool
}

// mount registers svc to answer the paths it claims.
pub fn (mut s ConnectServer) mount(svc Service) {
	s.services << svc
}

// listen_and_serve starts the HTTP listener and serves until it closes.
pub fn (mut s ConnectServer) listen_and_serve() ! {
	mut srv := http.Server{
		addr:                   s.addr
		handler:                s
		cert:                   s.cert
		cert_key:               s.cert_key
		in_memory_verification: s.in_memory_verification
	}
	srv.listen_and_serve()
}

// handle implements http.Handler; exposed so tests can drive it without
// sockets.
pub fn (mut s ConnectServer) handle(req http.Request) http.Response {
	if req.method != .post {
		return plain_response(405, 'Connect requires POST')
	}
	path := req.url.all_before('?')
	ct := req.header.get(.content_type) or { '' }
	codec := if ct.starts_with('application/json') {
		Codec.json
	} else if ct.starts_with('application/proto') {
		Codec.proto
	} else {
		return plain_response(415, 'unsupported codec `${ct}`')
	}
	if enc := req.header.get(.content_encoding) {
		if enc != '' && enc != 'identity' {
			return connect_error_response(.unimplemented, 'compression is not supported',
				ServerContext{})
		}
	}
	mut ctx := ServerContext{
		request_headers: request_metadata(req.header)
	}
	body := req.data.bytes()
	for mut svc in s.services {
		res, found := svc.call(path, codec, body, mut ctx) or {
			if err is StatusError {
				return connect_error(err.status, ctx)
			}
			return connect_error_response(.internal, err.msg(), ctx)
		}
		if found {
			out_ct := if codec == .json { 'application/json' } else { 'application/proto' }
			mut resp := http.Response{
				http_version: '1.1'
				status_code:  200
				status_msg:   'OK'
				body:         res.bytestr()
			}
			resp.header.add(.content_type, out_ct)
			resp.header.add(.content_length, res.len.str())
			apply_metadata(mut resp, ctx)
			return resp
		}
	}
	// A routing miss: Connect maps an unrecognized path to HTTP 404, even
	// though the RPC error code is unimplemented (a handler that itself
	// returns unimplemented still gets the code's normal 501).
	mut nf := connect_error(Status{
		code:    .unimplemented
		message: 'no such procedure: ${path}'
	}, ctx)
	nf.status_code = 404
	return nf
}

// request_metadata lowercases every incoming header into the handler's request
// metadata, preserving repeated values in order.
fn request_metadata(h http.Header) map[string][]string {
	mut m := map[string][]string{}
	for k in h.unique_keys() {
		m[k.to_lower()] = h.custom_values(k, exact: false)
	}
	return m
}

// apply_metadata writes a handler's response metadata onto the wire: leading
// metadata as plain headers, trailing metadata as `Trailer-`-prefixed headers
// per the Connect unary protocol. Repeated values become repeated headers.
fn apply_metadata(mut resp http.Response, ctx ServerContext) {
	for k, vals in ctx.resp_headers {
		for v in vals {
			resp.header.add_custom(k, v) or {}
		}
	}
	for k, vals in ctx.resp_trailers {
		for v in vals {
			resp.header.add_custom('trailer-${k}', v) or {}
		}
	}
}

// connect_code_name is the Connect error code: the gRPC snake_case names,
// except the US spelling of canceled.
fn connect_code_name(c Code) string {
	if c == .cancelled {
		return 'canceled'
	}
	return c.str()
}

// connect_http_status is the HTTP status per the Connect protocol's code table
// (connectrpc.com/docs/protocol#error-codes). else = 500: ok, unknown,
// internal, data_loss.
fn connect_http_status(c Code) int {
	return match c {
		.cancelled { 499 }
		.invalid_argument, .failed_precondition, .out_of_range { 400 }
		.unauthenticated { 401 }
		.permission_denied { 403 }
		.not_found { 404 }
		.deadline_exceeded { 504 }
		.already_exists, .aborted { 409 }
		.resource_exhausted { 429 }
		.unimplemented { 501 }
		.unavailable { 503 }
		else { 500 }
	}
}

// connect_error renders a Status as a Connect error response: the JSON error
// body (code, message, and any typed details base64-encoded) with the spec
// HTTP status, plus the handler's response metadata.
fn connect_error(status Status, ctx ServerContext) http.Response {
	mut obj := {
		'code':    json2.Any(connect_code_name(status.code))
		'message': json2.Any(status.message)
	}
	if status.details.len > 0 {
		mut arr := []json2.Any{}
		for d in status.details {
			arr << json2.Any({
				'type':  json2.Any(d.type_name)
				// Connect error detail values are UNPADDED base64 (the spec's
				// reference uses RawStdEncoding); strip the `=` padding.
				'value': json2.Any(base64.encode(d.value).trim_right('='))
			})
		}
		obj['details'] = json2.Any(arr)
	}
	body := json2.Any(obj).json_str()
	mut resp := http.Response{
		http_version: '1.1'
		status_code:  connect_http_status(status.code)
		body:         body
	}
	resp.header.add(.content_type, 'application/json')
	resp.header.add(.content_length, body.len.str())
	apply_metadata(mut resp, ctx)
	return resp
}

// connect_error_response renders a code/message pair as a Connect error.
fn connect_error_response(code Code, message string, ctx ServerContext) http.Response {
	return connect_error(Status{ code: code, message: message }, ctx)
}

// plain_response renders a transport-level rejection as text/plain.
fn plain_response(status int, msg string) http.Response {
	mut resp := http.Response{
		http_version: '1.1'
		status_code:  status
		body:         msg
	}
	resp.header.add(.content_type, 'text/plain')
	resp.header.add(.content_length, msg.len.str())
	return resp
}
