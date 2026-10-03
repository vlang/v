module grpc

import net.http
import time

// Client speaks unary + buffered-streaming gRPC over V's stdlib HTTP client.
// Real gRPC requires HTTP/2, so every call opts into it explicitly
// (`FetchConfig.enable_http2` defaults to false since #28996) and the target
// must be `https`: `net.http` negotiates h2 over ALPN there, while plain
// `http://` targets stay on HTTP/1.1 and a conforming server refuses them.
pub struct Client {
pub mut:
	base_url string              // scheme://host[:port], no trailing slash
	metadata map[string][]string // default headers merged into every call
	// TLS verification. `verify` is a path to a rootca.pem holding the trusted
	// CA certificate(s); leave it empty to use the platform trust store.
	verify                 string
	cert                   string // client certificate, for mutual TLS
	cert_key               string // client private key, for mutual TLS
	in_memory_verification bool   // `verify`/`cert`/`cert_key` are PEM strings, not paths
}

// unary sends one framed request to path ('/pkg.Service/Method') and returns
// the single response payload plus response metadata. Client.metadata forms the
// defaults; CallOptions layer on top (deadline, per-call metadata). Non-OK
// outcomes — including a fired deadline — surface as StatusError.
pub fn (mut c Client) unary(path string, msg []u8, opts ...CallOption) !RawReply {
	status_code, h, body := c.do_call(path, encode_frame(msg, false), ...opts)!
	payload := parse_unary_response(status_code, h, body)!
	return RawReply{
		payload:  payload
		metadata: response_metadata(h)
	}
}

// server_stream sends one request and buffers the whole response stream: every
// response message the server framed into the body, in order. The transport is
// a single POST, so this materializes the stream rather than delivering it
// incrementally.
pub fn (mut c Client) server_stream(path string, msg []u8, opts ...CallOption) !RawStreamReply {
	status_code, h, body := c.do_call(path, encode_frame(msg, false), ...opts)!
	payloads := parse_response(status_code, h, body)!
	return RawStreamReply{
		payloads: payloads
		metadata: response_metadata(h)
	}
}

// client_stream sends the whole request stream — every message framed into one
// body — and returns the single response. The buffered request rides in one
// POST, so the server's receive-body cap applies to the batch.
pub fn (mut c Client) client_stream(path string, msgs [][]u8, opts ...CallOption) !RawReply {
	mut body := []u8{}
	for m in msgs {
		body << encode_frame(m, false)
	}
	status_code, h, respbody := c.do_call(path, body, ...opts)!
	payload := parse_unary_response(status_code, h, respbody)!
	return RawReply{
		payload:  payload
		metadata: response_metadata(h)
	}
}

// do_call performs the HTTP round-trip shared by every call shape: resolve the
// config, set the gRPC request headers (content-type, te, grpc-timeout, and
// metadata), POST the already-framed body, and return the raw response. A fired
// deadline or any transport failure surfaces as a StatusError.
fn (mut c Client) do_call(path string, body []u8, opts ...CallOption) !(int, http.Header, []u8) {
	mut cfg := CallConfig{
		metadata: c.metadata.clone()
	}
	for o in opts {
		o(mut cfg)
	}
	mut h := http.new_header()
	h.add(.content_type, 'application/grpc+proto')
	h.add_custom('te', 'trailers')!
	if cfg.timeout > 0 {
		h.add_custom('grpc-timeout', encode_grpc_timeout(cfg.timeout))!
	}
	for k, vals in cfg.metadata {
		for v in vals {
			h.add_custom(k, v)!
		}
	}
	// gRPC is HTTP/2-only: net.http defaults to HTTP/1.1, and without this the
	// call silently degrades and the server's reply is not a gRPC response.
	mut fc := http.FetchConfig{
		url:                    c.base_url + path
		method:                 .post
		header:                 h
		data:                   body.bytestr()
		enable_http2:           true
		verify:                 c.verify
		cert:                   c.cert
		cert_key:               c.cert_key
		in_memory_verification: c.in_memory_verification
	}
	if cfg.timeout > 0 {
		// enforce the deadline client-side too, not just as a server hint
		fc.read_timeout = cfg.timeout
		fc.write_timeout = cfg.timeout
	}
	start := time.now()
	resp := http.fetch(fc) or {
		// attribute the failure to the deadline only if the deadline actually
		// elapsed — robust regardless of how the transport words the error
		deadline_hit := cfg.timeout > 0 && time.since(start) >= cfg.timeout
		return transport_error(err, deadline_hit)
	}
	return resp.status_code, resp.header, resp.body.bytes()
}

// transport_error maps a failed fetch to a gRPC status: a fired deadline to
// deadline_exceeded, any other transport failure (refused, reset, DNS) to
// unavailable — the gRPC convention.
fn transport_error(err IError, deadline_hit bool) StatusError {
	code := if deadline_hit { Code.deadline_exceeded } else { Code.unavailable }
	return StatusError{
		status: Status{
			code:    code
			message: err.msg()
		}
	}
}

// response_metadata collects response headers/trailers, minus the gRPC control
// headers the client consumes itself, so callers see only application metadata.
// Keys are lowercased and case-insensitive, matching ServerContext.
fn response_metadata(h http.Header) map[string][]string {
	skip := ['grpc-status', 'grpc-message', 'content-type', 'te']
	mut m := map[string][]string{}
	// unique_keys() drops repeats that differ only in capitalization, and the
	// exact:false read then gathers every spelling for that one field. Without
	// it a response carrying both `X-Trace` and `x-trace` produced two entries,
	// each holding both values, and a lookup by lowercase name found nothing.
	for k in h.unique_keys() {
		if k.to_lower() in skip {
			continue
		}
		vals := h.custom_values(k, exact: false)
		if vals.len > 0 {
			m[k.to_lower()] = vals
		}
	}
	return m
}

// parse_response is the shared response contract, testable without a server:
// validate the HTTP status, content-type, and grpc-status (a non-OK status is a
// StatusError with the percent-decoded grpc-message), then return every
// response message payload the body carried. The h2 layer merges response
// trailers into the header set, so grpc-status is readable here either way.
fn parse_response(status_code int, h http.Header, body []u8) ![][]u8 {
	grpc_status := h.get_custom('grpc-status')
	if grpc_status == none && status_code != 200 {
		return StatusError{
			status: Status{
				code:    code_from_http(status_code)
				message: 'HTTP ${status_code}'
			}
		}
	}
	ct := h.get(.content_type) or { '' }
	if !ct.starts_with('application/grpc') {
		return StatusError{
			status: Status{
				code:    .unknown
				message: 'unexpected content-type `${ct}`'
			}
		}
	}
	gs := grpc_status or {
		return StatusError{
			status: Status{
				code:    .unknown
				message: 'missing grpc-status'
			}
		}
	}
	// .int() parses garbage as 0, which would read as OK — reject first
	if gs.len == 0 || !gs.bytes().all(it >= `0` && it <= `9`) {
		return StatusError{
			status: Status{
				code:    .unknown
				message: 'malformed grpc-status `${gs}`'
			}
		}
	}
	code := code_from_int(gs.int()) or {
		return StatusError{
			status: Status{
				code:    .unknown
				message: err.msg()
			}
		}
	}
	if code != .ok {
		return StatusError{
			status: Status{
				code:    code
				message: percent_decode(h.get_custom('grpc-message') or { '' })
			}
		}
	}
	frames := decode_frames(body)!
	mut payloads := [][]u8{cap: frames.len}
	for f in frames {
		if f.compressed {
			return StatusError{
				status: Status{
					code:    .unimplemented
					message: 'compressed response not supported'
				}
			}
		}
		payloads << f.payload
	}
	return payloads
}

// parse_unary_response is parse_response constrained to exactly one message.
fn parse_unary_response(status_code int, h http.Header, body []u8) ![]u8 {
	payloads := parse_response(status_code, h, body)!
	if payloads.len != 1 {
		return StatusError{
			status: Status{
				code:    .internal
				message: 'expected 1 response message, got ${payloads.len}'
			}
		}
	}
	return payloads[0]
}

// HTTP status to gRPC code, per the gRPC-over-HTTP/2 spec
fn code_from_http(status int) Code {
	return match status {
		400 { Code.internal }
		401 { Code.unauthenticated }
		403 { Code.permission_denied }
		404 { Code.unimplemented }
		429, 502, 503, 504 { Code.unavailable }
		else { Code.unknown }
	}
}
