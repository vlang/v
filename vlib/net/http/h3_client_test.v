// vtest build: present_openssl?
// vtest vflags: -d http3
module http

import net.quic

// Tests for the HTTP/3 <-> net.http conversion glue (h3_client.v). The
// request/response conversions are pure and need no socket -- unlike
// h2_client_test.v, there is no end-to-end fetch test here: no h3 test
// server exists in this repo (Phase 13, server support, is out of v1
// scope), so a real `fetch(enable_http3: true)` call has nothing to
// exercise it against. transport_h3_test.v covers what IS testable
// without a live peer (pool-key folding, the mTLS fast-fail, 3-way idle
// eviction with h3 entries, the singleflight dial-call struct itself).

fn test_to_h3_request_pseudo_headers_and_body() {
	req := Request{
		user_agent: 'v.http'
	}
	h3req := req.to_h3_request(.post, 'example.com', '/p?q=1', 'hello', new_header())
	assert h3req.method == 'POST'
	assert h3req.scheme == 'https'
	assert h3req.authority == 'example.com'
	assert h3req.path == '/p?q=1'
	assert h3req.body.bytestr() == 'hello'
	// user-agent (from the request) and a synthesized content-length.
	assert h3req.headers.any(it.name == 'user-agent' && it.value == 'v.http')
	assert h3req.headers.any(it.name == 'content-length' && it.value == '5')
}

fn test_to_h3_request_lowercases_and_keeps_custom_headers() {
	mut h := new_header()
	h.add_custom('Accept', 'application/json') or {}
	h.add(.content_type, 'text/plain')
	req := Request{}
	h3req := req.to_h3_request(.get, 'h.example', '/', '', h)
	assert h3req.headers.any(it.name == 'accept' && it.value == 'application/json')
	assert h3req.headers.any(it.name == 'content-type' && it.value == 'text/plain')
}

fn test_to_h3_request_strips_hop_by_hop_and_host() {
	mut h := new_header()
	h.add(.connection, 'keep-alive')
	h.add(.host, 'example.com')
	h.add_custom('Transfer-Encoding', 'chunked') or {}
	req := Request{}
	h3req := req.to_h3_request(.get, 'example.com', '/', '', h)
	assert !h3req.headers.any(it.name == 'connection')
	assert !h3req.headers.any(it.name == 'host')
	assert !h3req.headers.any(it.name == 'transfer-encoding')
}

fn test_to_h3_request_te_only_trailers() {
	// RFC 9114 §4.2 carries HTTP/2's identical TE restriction (RFC 9113
	// §8.2.2): TE may be sent, but only with the value 'trailers'.
	req := Request{}
	mut h := new_header()
	h.add_custom('TE', 'gzip') or {}
	h3req := req.to_h3_request(.get, 'h.example', '/', '', h)
	assert !h3req.headers.any(it.name == 'te'), 'a non-trailers TE must be dropped'

	mut h2 := new_header()
	h2.add_custom('TE', 'trailers') or {}
	h3req2 := req.to_h3_request(.get, 'h.example', '/', '', h2)
	te := h3req2.headers.filter(it.name == 'te')
	assert te.len == 1 && te[0].value == 'trailers', 'te: trailers must be kept'
}

fn test_to_h3_request_collapses_cookies() {
	mut h := new_header()
	h.add(.cookie, 'a=1')
	req := Request{
		cookies: {
			'sid': 'abc'
		}
	}
	h3req := req.to_h3_request(.get, 'h.example', '/', '', h)
	cookie := h3req.headers.filter(it.name == 'cookie')
	assert cookie.len == 1
	// Both the request cookie map and the Cookie header value are present.
	assert cookie[0].value.contains('sid=abc')
	assert cookie[0].value.contains('a=1')
}

fn test_to_h3_request_authority_from_host_header() {
	mut h := new_header()
	h.add(.host, 'override.example:8443')
	req := Request{}
	// The URL host is origin.example, but an explicit Host header must win.
	h3req := req.to_h3_request(.get, 'origin.example', '/', '', h)
	assert h3req.authority == 'override.example:8443'
}

fn test_h3_response_to_http() {
	h3resp := H3ClientResponse{
		status:  200
		headers: [
			quic.QpackFieldLine{
				name:  'content-type'
				value: 'text/plain'
			},
			quic.QpackFieldLine{
				name:  'x-foo'
				value: 'bar'
			},
		]
		body:    'hi'.bytes()
	}
	resp := h3_response_to_http(h3resp)
	assert resp.status_code == 200
	assert resp.http_version == '3.0'
	assert resp.version() == .v3_0
	assert resp.body == 'hi'
	assert (resp.header.get_custom('content-type') or { '' }) == 'text/plain'
	assert (resp.header.get_custom('x-foo') or { '' }) == 'bar'
}
