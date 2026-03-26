// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.urllib
import net.http.v2
import net.http.v3

// negotiate_version selects the HTTP version for a request.
//
// When req.version is explicitly set, that value is honoured directly.
// For plain HTTP the function falls back to HTTP/1.1 because HTTP/2 and
// HTTP/3 both require TLS.
// For HTTPS it defaults to HTTP/2, which has the broadest server support.
// HTTP/3 (QUIC) is still experimental and its QUIC transport layer is not
// yet fully implemented in V.
//
// TODO: True ALPN-based version negotiation is not yet possible.
// V's net.ssl module (both mbedtls and openssl backends) configures ALPN
// protocol lists during TLS handshake via SSLConnectConfig.alpn_protocols,
// but does NOT expose the server-selected protocol after handshake completion.
// Neither SSLConn nor the underlying backends provide a method like
// get_alpn_selected() or selected_protocol().
//
// Current behavior: the HTTP/2 client sets alpn_protocols: ['h2'] before
// connecting, which tells the server we prefer h2. If the server does not
// support h2, the TLS handshake may still succeed (falling back to no ALPN),
// and the subsequent HTTP/2 connection preface will fail at the protocol
// level. This is acceptable until the V SSL API adds ALPN result inspection.
//
// Correct approach (once API is available):
//   1. Perform TLS handshake with ALPN extension: ['h3', 'h2', 'http/1.1']
//   2. Call ssl_conn.get_alpn_selected() to read the negotiated protocol
//   3. Return the matching Version enum value
fn (req &Request) negotiate_version(url urllib.URL) Version {
	// If version is explicitly set, use it
	if req.version != .unknown {
		return req.version
	}

	// Only HTTPS supports HTTP/2 and HTTP/3
	if url.scheme != 'https' {
		return .v1_1
	}

	// Default to HTTP/2 for HTTPS connections.
	// HTTP/2 is more widely supported than HTTP/3, and the HTTP/3 module
	// is still experimental (QUIC transport is not complete).
	return .v2_0
}

// to_v2_method converts a net.http.Method to the v2 module's Method enum.
//
// TODO: Method is duplicated across net.http, net.http.v2, and net.http.v3.
// These three definitions should eventually be unified into a single enum
// in net.http once the cross-module import story is settled.
fn to_v2_method(m Method) v2.Method {
	return match m {
		.get { v2.Method.get }
		.post { v2.Method.post }
		.put { v2.Method.put }
		.patch { v2.Method.patch }
		.delete { v2.Method.delete }
		.head { v2.Method.head }
		.options { v2.Method.options }
		else { v2.Method.get }
	}
}

// to_v3_method converts a net.http.Method to the v3 module's Method enum.
//
// TODO: Method is duplicated across net.http, net.http.v2, and net.http.v3.
// These three definitions should eventually be unified into a single enum
// in net.http once the cross-module import story is settled.
fn to_v3_method(m Method) v3.Method {
	return match m {
		.get { v3.Method.get }
		.post { v3.Method.post }
		.put { v3.Method.put }
		.patch { v3.Method.patch }
		.delete { v3.Method.delete }
		.head { v3.Method.head }
		.options { v3.Method.options }
		else { v3.Method.get }
	}
}

// build_headers_map builds a string-keyed headers map from the request,
// injecting user-agent and content-length when absent.
// Used by both do_http2 and do_http3.
fn (req &Request) build_headers_map() map[string]string {
	mut headers_map := map[string]string{}
	for key in req.header.keys() {
		values := req.header.custom_values(key)
		if values.len > 0 {
			headers_map[key] = values.join('; ')
		}
	}
	// Add user-agent if not present
	if 'user-agent' !in headers_map {
		headers_map['user-agent'] = req.user_agent
	}
	// Add content-length if there's a body
	if req.data.len > 0 && 'content-length' !in headers_map {
		headers_map['content-length'] = req.data.len.str()
	}
	return headers_map
}

// build_request_path returns the full path (with optional query string) for
// the request URL.
fn build_request_path(url urllib.URL) string {
	p := url.escaped_path().trim_left('/')
	return if url.query().len > 0 { '/${p}?${url.query().encode()}' } else { '/${p}' }
}

// do_http2 performs an HTTP/2 request
fn (req &Request) do_http2(url urllib.URL) !Response {
	host_name := url.hostname()
	mut nport := url.port().int()
	if nport == 0 {
		nport = 443 // HTTPS default
	}

	address := '${host_name}:${nport}'

	// Create HTTP/2 client
	mut client := v2.new_client(address) or { return error('HTTP/2 connection failed: ${err}') }

	defer {
		client.close()
	}

	v2_req := v2.Request{
		method:  to_v2_method(req.method)
		url:     build_request_path(url)
		host:    host_name
		data:    req.data
		headers: req.build_headers_map()
	}

	// Send request
	v2_resp := client.request(v2_req) or { return error('HTTP/2 request failed: ${err}') }

	// Convert v2.Response to http.Response
	mut resp_header := new_header()
	for key, value in v2_resp.headers {
		resp_header.add_custom(key, value) or {}
	}

	return Response{
		body:        v2_resp.body
		status_code: v2_resp.status_code
		header:      resp_header
	}
}

// do_http3 performs an HTTP/3 request
fn (req &Request) do_http3(url urllib.URL) !Response {
	host_name := url.hostname()
	mut nport := url.port().int()
	if nport == 0 {
		nport = 443 // HTTPS default
	}

	address := '${host_name}:${nport}'

	// Create HTTP/3 client
	mut client := v3.new_client(address) or { return error('HTTP/3 connection failed: ${err}') }

	defer {
		client.close()
	}

	v3_req := v3.Request{
		method:  to_v3_method(req.method)
		url:     build_request_path(url)
		host:    host_name
		data:    req.data
		headers: req.build_headers_map()
	}

	// Send request
	v3_resp := client.request(v3_req) or { return error('HTTP/3 request failed: ${err}') }

	// Convert v3.Response to http.Response
	mut resp_header := new_header()
	for key, value in v3_resp.headers {
		resp_header.add_custom(key, value) or {}
	}

	return Response{
		body:        v3_resp.body
		status_code: v3_resp.status_code
		header:      resp_header
	}
}
