// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// This file converts between net.http's Request/Response and the HTTP/2
// client types in h2_conn.v. The actual transport wiring (ALPN negotiation on
// the TLS socket) lives in backend.c.v; these helpers are pure and backend
// agnostic, so they can be tested without a socket.

// h2_hop_by_hop are header names that must not be forwarded on HTTP/2
// (RFC 7540 Section 8.1.2.2), plus `host` (replaced by the :authority
// pseudo-header) and `cookie` (handled specially below).
const h2_hop_by_hop = ['connection', 'keep-alive', 'proxy-connection', 'transfer-encoding', 'upgrade',
	'host', 'cookie']

// h2_authority returns the :authority value for a host and port, omitting the
// port for the default HTTPS port.
fn h2_authority(host string, port int) string {
	if port == 443 || port == 0 {
		return host
	}
	return '${host}:${port}'
}

// to_h2_request builds an HTTP/2 request from this request. Header names are
// lowercased, hop-by-hop headers are dropped, the Host header becomes the
// :authority pseudo-header, and cookies are collapsed into a single field.
fn (req &Request) to_h2_request(method Method, authority string, path string, data string, header Header) H2ClientRequest {
	// An explicit Host header overrides the URL host, matching the HTTP/1.1
	// path (used for virtual-host / host-override requests).
	mut auth := authority
	if host := header.get(.host) {
		if host != '' {
			auth = host
		}
	}
	mut extra := []H2HeaderField{}
	if !header.contains(.user_agent) && req.user_agent != '' {
		extra << H2HeaderField{'user-agent', req.user_agent}
	}
	if data.len > 0 && !header.contains(.content_length) {
		extra << H2HeaderField{'content-length', data.len.str()}
	}
	for key in header.keys() {
		lkey := key.to_lower()
		if lkey in h2_hop_by_hop {
			continue
		}
		for val in header.custom_values(key) {
			// RFC 9113 §8.2.2: TE may be sent on an HTTP/2 request, but MUST NOT
			// carry any value other than 'trailers'. Drop a non-conformant TE
			// rather than generate a malformed request.
			if lkey == 'te' && val.trim_space().to_lower() != 'trailers' {
				continue
			}
			extra << H2HeaderField{lkey, val}
		}
	}
	// Cookies: the request's own cookie map plus any Cookie header values,
	// joined into one field (RFC 7540 Section 8.1.2.5 also allows splitting).
	mut cookie_parts := []string{}
	for k, v in req.cookies {
		cookie_parts << '${k}=${v}'
	}
	for cv in header.values(.cookie) {
		cookie_parts << cv
	}
	if cookie_parts.len > 0 {
		extra << H2HeaderField{'cookie', cookie_parts.join('; ')}
	}
	return H2ClientRequest{
		method:    method.str()
		scheme:    'https'
		authority: auth
		path:      path
		headers:   extra
		body:      data.bytes()
	}
}

// h2_response_to_http converts an HTTP/2 response into a net.http Response,
// decoding any Content-Encoding the same way the HTTP/1.1 path does.
fn h2_response_to_http(h2resp H2ClientResponse) Response {
	mut h := new_header()
	for f in h2resp.headers {
		h.add_custom(f.name, f.value) or {}
	}
	body := decode_response_body(h2resp.body.bytestr(), h.get(.content_encoding) or { '' })
	status := status_from_int(h2resp.status)
	return Response{
		http_version: '2.0'
		status_code:  h2resp.status
		status_msg:   status.str()
		header:       h
		body:         body
	}
}
