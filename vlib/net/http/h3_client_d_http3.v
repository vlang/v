// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.quic

// This file converts between net.http's Request/Response and the HTTP/3
// client types H3ClientRequest/H3ClientResponse (h3_mux_conn.v -- Phase 12c
// already defined them there, since H3MuxConn needed a concrete shape to
// compile against before this file existed; mirrors how H2ClientRequest
// lives in h2_conn.v, not h2_client.v, which only converts to/from it). The
// actual transport wiring (UDP dial, QUIC handshake, pooling) lives in
// h3_udp_dial.v/transport_h3.v; these helpers are pure and backend
// agnostic, so they can be tested without a socket.

// to_h3_request builds an HTTP/3 request from this request. Header names
// are lowercased, hop-by-hop headers are dropped, the Host header becomes
// the :authority pseudo-header, and cookies are collapsed into a single
// field -- reuses h2_client.v's h2_hop_by_hop list and h2_authority helper
// directly (RFC 9114 §4.1.1 intentionally mirrors RFC 9113 §8.1.2.2 here,
// so there is nothing HTTP/3-specific to re-derive).
fn (req &Request) to_h3_request(method Method, authority string, path string, data string, header Header) H3ClientRequest {
	// An explicit Host header overrides the URL host, matching the HTTP/1.1
	// and HTTP/2 paths (used for virtual-host / host-override requests).
	mut auth := authority
	if host := header.get(.host) {
		if host != '' {
			auth = host
		}
	}
	mut extra := []quic.QpackFieldLine{}
	if !header.contains(.user_agent) && req.user_agent != '' {
		extra << quic.QpackFieldLine{
			name:  'user-agent'
			value: req.user_agent
		}
	}
	if data.len > 0 && !header.contains(.content_length) {
		extra << quic.QpackFieldLine{
			name:  'content-length'
			value: data.len.str()
		}
	}
	for key in header.keys() {
		lkey := key.to_lower()
		if lkey in h2_hop_by_hop {
			continue
		}
		for val in header.custom_values(key) {
			// RFC 9114 §4.2 carries HTTP/2's identical TE restriction (RFC
			// 9113 §8.2.2): TE may be sent, but MUST NOT carry any value
			// other than 'trailers'.
			if lkey == 'te' && val.trim_space().to_lower() != 'trailers' {
				continue
			}
			extra << quic.QpackFieldLine{
				name:  lkey
				value: val
			}
		}
	}
	// Cookies: the request's own cookie map plus any Cookie header values,
	// joined into one field (RFC 9114 §4.2 also permits splitting).
	mut cookie_parts := []string{}
	for k, v in req.cookies {
		cookie_parts << '${k}=${v}'
	}
	for cv in header.values(.cookie) {
		cookie_parts << cv
	}
	if cookie_parts.len > 0 {
		extra << quic.QpackFieldLine{
			name:  'cookie'
			value: cookie_parts.join('; ')
		}
	}
	return H3ClientRequest{
		method:    method.str()
		scheme:    'https'
		authority: auth
		path:      path
		headers:   extra
		body:      data.bytes()
	}
}

// h3_response_to_http converts an HTTP/3 response into a net.http Response,
// decoding any Content-Encoding the same way the HTTP/1.1/2 paths do.
fn h3_response_to_http(h3resp H3ClientResponse) Response {
	mut h := new_header()
	for f in h3resp.headers {
		h.add_custom(f.name, f.value) or {}
	}
	body := decode_response_body(h3resp.body.bytestr(), h.get(.content_encoding) or { '' })
	status := status_from_int(h3resp.status)
	return Response{
		http_version: '3.0'
		status_code:  h3resp.status
		status_msg:   status.str()
		header:       h
		body:         body
	}
}
