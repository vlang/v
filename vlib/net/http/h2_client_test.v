module http

// Tests for the HTTP/2 <-> net.http conversion glue (h2_client.v). The
// request/response conversions are pure and need no socket; the end-to-end
// fetch test is opt-in and network-dependent.

fn test_to_h2_request_pseudo_headers_and_body() {
	req := Request{
		user_agent: 'v.http'
	}
	h2req := req.to_h2_request(.post, 'example.com', '/p?q=1', 'hello', new_header())
	assert h2req.method == 'POST'
	assert h2req.scheme == 'https'
	assert h2req.authority == 'example.com'
	assert h2req.path == '/p?q=1'
	assert h2req.body.bytestr() == 'hello'
	// user-agent (from the request) and a synthesized content-length.
	assert h2req.headers.any(it.name == 'user-agent' && it.value == 'v.http')
	assert h2req.headers.any(it.name == 'content-length' && it.value == '5')
}

fn test_to_h2_request_lowercases_and_keeps_custom_headers() {
	mut h := new_header()
	h.add_custom('Accept', 'application/json') or {}
	h.add(.content_type, 'text/plain')
	req := Request{}
	h2req := req.to_h2_request(.get, 'h.example', '/', '', h)
	assert h2req.headers.any(it.name == 'accept' && it.value == 'application/json')
	assert h2req.headers.any(it.name == 'content-type' && it.value == 'text/plain')
}

fn test_to_h2_request_strips_hop_by_hop_and_host() {
	mut h := new_header()
	h.add(.connection, 'keep-alive')
	h.add(.host, 'example.com')
	h.add_custom('Transfer-Encoding', 'chunked') or {}
	req := Request{}
	h2req := req.to_h2_request(.get, 'example.com', '/', '', h)
	assert !h2req.headers.any(it.name == 'connection')
	assert !h2req.headers.any(it.name == 'host')
	assert !h2req.headers.any(it.name == 'transfer-encoding')
}

fn test_to_h2_request_te_only_trailers() {
	// RFC 9113 §8.2.2: TE may be sent on an HTTP/2 request but only with the
	// value 'trailers'; any other value must be dropped.
	req := Request{}
	mut h := new_header()
	h.add_custom('TE', 'gzip') or {}
	h2req := req.to_h2_request(.get, 'h.example', '/', '', h)
	assert !h2req.headers.any(it.name == 'te'), 'a non-trailers TE must be dropped'

	mut h2 := new_header()
	h2.add_custom('TE', 'trailers') or {}
	h2req2 := req.to_h2_request(.get, 'h.example', '/', '', h2)
	te := h2req2.headers.filter(it.name == 'te')
	assert te.len == 1 && te[0].value == 'trailers', 'te: trailers must be kept'
}

fn test_to_h2_request_collapses_cookies() {
	mut h := new_header()
	h.add(.cookie, 'a=1')
	req := Request{
		cookies: {
			'sid': 'abc'
		}
	}
	h2req := req.to_h2_request(.get, 'h.example', '/', '', h)
	cookie := h2req.headers.filter(it.name == 'cookie')
	assert cookie.len == 1
	// Both the request cookie map and the Cookie header value are present.
	assert cookie[0].value.contains('sid=abc')
	assert cookie[0].value.contains('a=1')
}

fn test_h2_response_to_http() {
	h2resp := H2ClientResponse{
		status:  200
		headers: [H2HeaderField{'content-type', 'text/plain'},
			H2HeaderField{'x-foo', 'bar'}]
		body:    'hi'.bytes()
	}
	resp := h2_response_to_http(h2resp)
	assert resp.status_code == 200
	assert resp.http_version == '2.0'
	assert resp.version() == .v2_0
	assert resp.body == 'hi'
	assert (resp.header.get_custom('content-type') or { '' }) == 'text/plain'
	assert (resp.header.get_custom('x-foo') or { '' }) == 'bar'
}

fn test_h2_authority_omits_default_port() {
	assert h2_authority('example.com', 443) == 'example.com'
	assert h2_authority('example.com', 0) == 'example.com'
	assert h2_authority('example.com', 8443) == 'example.com:8443'
}

// End-to-end test against a real HTTP/2 server. Run with `-d network`,
// e.g. `v -d network test vlib/net/http/h2_client_test.v`.
fn test_http2_fetch_real_server() {
	$if !network ? {
		return
	}
	// HTTP/2 is negotiated by default for https requests. On Windows this runs
	// over the SChannel backend's ALPN + HTTP/2 path (vlang/v#27383).
	resp := get('https://www.google.com/')!
	assert resp.version() == .v2_0
	assert resp.status_code == 200
	assert resp.body.len > 0
	// Opting out forces HTTP/1.1 against the same server.
	plain := fetch(url: 'https://www.google.com/', enable_http2: false)!
	assert plain.version() == .v1_1
}

fn test_to_h2_request_authority_from_host_header() {
	mut h := new_header()
	h.add(.host, 'override.example:8443')
	req := Request{}
	// The URL host is origin.example, but an explicit Host header must win.
	h2req := req.to_h2_request(.get, 'origin.example', '/', '', h)
	assert h2req.authority == 'override.example:8443'
	assert !h2req.headers.any(it.name == 'host')
}
