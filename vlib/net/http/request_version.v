// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.urllib
import net.http.v2
import net.http.v3

// negotiate_version negotiates the HTTP version to use based on ALPN
// Returns the negotiated version or falls back to HTTP/1.1
fn (req &Request) negotiate_version(url urllib.URL) Version {
	// If version is explicitly set, use it
	if req.version != .unknown {
		return req.version
	}

	// Only HTTPS supports HTTP/2 and HTTP/3
	if url.scheme != 'https' {
		return .v1_1
	}

	// TODO: Implement actual ALPN negotiation with TLS
	// For now, we'll try HTTP/3 first for HTTPS URLs
	// In the future, this should:
	// 1. Perform TLS handshake with ALPN protocols: ['h3', 'h2', 'http/1.1']
	// 2. Check which protocol the server selected
	// 3. Return the corresponding Version

	// Default to HTTP/3 for HTTPS, will fallback to HTTP/2 then HTTP/1.1 if it fails
	return .v3_0
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

	// Convert HTTP request to v2.SimpleRequest
	mut v2_method := v2.Method.get
	match req.method {
		.get { v2_method = .get }
		.post { v2_method = .post }
		.put { v2_method = .put }
		.patch { v2_method = .patch }
		.delete { v2_method = .delete }
		.head { v2_method = .head }
		.options { v2_method = .options }
		else { v2_method = .get }
	}

	// Build headers map
	mut headers_map := map[string]string{}
	for key in req.header.keys() {
		values := req.header.custom_values(key)
		if values.len > 0 {
			headers_map[key] = values.join('; ')
		}
	}

	// Add user agent if not present
	if 'user-agent' !in headers_map {
		headers_map['user-agent'] = req.user_agent
	}

	// Add content-length if there's data
	if req.data.len > 0 && 'content-length' !in headers_map {
		headers_map['content-length'] = req.data.len.str()
	}

	p := url.escaped_path().trim_left('/')
	path := if url.query().len > 0 { '/${p}?${url.query().encode()}' } else { '/${p}' }

	v2_req := v2.SimpleRequest{
		method:  v2_method
		url:     path
		host:    host_name
		data:    req.data
		headers: headers_map
	}

	// Send request
	v2_resp := client.request(v2_req) or { return error('HTTP/2 request failed: ${err}') }

	// Convert v2.SimpleResponse to http.Response
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

	// Convert HTTP request to v3.SimpleRequest
	mut v3_method := v3.Method.get
	match req.method {
		.get { v3_method = .get }
		.post { v3_method = .post }
		.put { v3_method = .put }
		.patch { v3_method = .patch }
		.delete { v3_method = .delete }
		.head { v3_method = .head }
		.options { v3_method = .options }
		else { v3_method = .get }
	}

	// Build headers map
	mut headers_map := map[string]string{}
	for key in req.header.keys() {
		values := req.header.custom_values(key)
		if values.len > 0 {
			headers_map[key] = values.join('; ')
		}
	}

	// Add user agent if not present
	if 'user-agent' !in headers_map {
		headers_map['user-agent'] = req.user_agent
	}

	// Add content-length if there's data
	if req.data.len > 0 && 'content-length' !in headers_map {
		headers_map['content-length'] = req.data.len.str()
	}

	p := url.escaped_path().trim_left('/')
	path := if url.query().len > 0 { '/${p}?${url.query().encode()}' } else { '/${p}' }

	v3_req := v3.SimpleRequest{
		method:  v3_method
		url:     path
		host:    host_name
		data:    req.data
		headers: headers_map
	}

	// Send request
	v3_resp := client.request(v3_req) or { return error('HTTP/3 request failed: ${err}') }

	// Convert v3.SimpleResponse to http.Response
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
