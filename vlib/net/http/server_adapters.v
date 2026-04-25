// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.http.common
import time

// Unified handler types — same for HTTP/1.1, HTTP/2, and HTTP/3.
pub type ServerRequest = common.ServerRequest
pub type ServerResponse = common.ServerResponse

// ServerHandler is the unified handler interface for HTTP/1.1, HTTP/2, and HTTP/3.
// New code should implement this interface directly.
pub interface ServerHandler {
mut:
	handle(ServerRequest) ServerResponse
}

// Handler is the classic HTTP/1.1 handler interface preserved for backward
// compatibility. Use handler_adapter() to wrap a Handler into a ServerHandler
// for use with Server.
pub interface Handler {
mut:
	handle(Request) Response
}

// HandlerAdapter wraps a classic Handler into a ServerHandler so that
// existing HTTP/1.1 handler implementations continue to work with Server.
struct HandlerAdapter {
mut:
	classic_handler Handler
}

fn (mut a HandlerAdapter) handle(req ServerRequest) ServerResponse {
	classic_req := Request{
		method:  req.method
		url:     req.path
		host:    req.host
		header:  req.header
		data:    if req.body.len > 0 { req.body.bytestr() } else { '' }
		version: req.version
		cookies: req.cookies
	}
	resp := a.classic_handler.handle(classic_req)
	return ServerResponse{
		status_code: resp.status_code
		header:      resp.header
		body:        if resp.body.len > 0 { resp.body.bytes() } else { []u8{} }
	}
}

// handler_adapter wraps a classic Handler into a ServerHandler for backward
// compatibility with the unified server interface.
pub fn handler_adapter(handler Handler) &HandlerAdapter {
	return &HandlerAdapter{
		classic_handler: handler
	}
}

// request_to_server_request converts the parsed HTTP/1.1 Request to a
// ServerRequest for the unified handler interface.
pub fn request_to_server_request(req &Request) ServerRequest {
	return ServerRequest{
		method:  req.method
		path:    req.url
		host:    req.host
		header:  req.header
		body:    if req.data.len > 0 { req.data.bytes() } else { []u8{} }
		version: req.version
		cookies: req.cookies
	}
}

// server_response_to_response converts a ServerResponse back to a
// wire-level Response for HTTP/1.1 transmission.
pub fn server_response_to_response(sresp ServerResponse, req_version common.Version) Response {
	mut resp := Response{
		body:        if sresp.body.len > 0 { sresp.body.bytestr() } else { '' }
		header:      sresp.header
		status_code: sresp.status_code
	}
	resp.set_version(req_version)
	if sresp.status_code > 0 {
		resp.set_status(status_from_int(sresp.status_code))
	} else {
		resp.set_status(.ok)
	}
	return resp
}

// set_server_only_header replaces any existing header with the given name,
// then sets it to value. Used for server-injected headers like Remote-Addr.
fn set_server_only_header(mut header Header, name string, value string) {
	lower_name := name.to_lower()
	for key in header.keys() {
		if key.to_lower() == lower_name {
			header.delete_custom(key)
		}
	}
	header.add_custom(name, value) or {}
}

// DebugHandler implements the unified ServerHandler interface by logging
// the request and returning safe metadata. It never echoes the request body
// or headers in the response to avoid leaking sensitive data (Authorization,
// Cookie, etc.).
struct DebugHandler {}

fn (d DebugHandler) handle(req ServerRequest) ServerResponse {
	$if debug {
		eprintln('[${time.now()}] ${req.method} ${req.path}\n\r${req.header}\n\r${req.body_text()} - 200 OK')
	} $else {
		eprintln('[${time.now()}] ${req.method} ${req.path} - 200')
	}
	body_str := 'Method: ${req.method}\nPath: ${req.path}\nContent-Length: ${req.body.len}\nTimestamp: ${time.now().format_rfc3339()}'
	return ServerResponse{
		status_code: 200
		body:        body_str.bytes()
	}
}
