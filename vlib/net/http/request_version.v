module http

// HTTP version negotiation and multi-version request dispatch.
// Alt-Svc cache integration: do_http2 checks for Alt-Svc headers in responses
// and negotiate_version consults the cache before defaulting to HTTP/2.
//
// v3 (QUIC) support lives in request_version_d_use_ngtcp2.v and is only
// compiled when `-d use_ngtcp2` is passed. The fallback stubs are in
// request_version_notd_use_ngtcp2.v.
import net.urllib
import net.http.v2

// build_client_header prepares the request header for v2/v3 clients,
// adding user-agent and content-length if not already set.
fn (req &Request) build_client_header() Header {
	mut h := req.header
	if !h.contains_custom('user-agent') {
		h.add_custom('user-agent', req.user_agent) or {}
	}
	if req.data.len > 0 && !h.contains(.content_length) {
		h.add(.content_length, req.data.len.str()) or {}
	}
	return h
}

fn build_request_path(url urllib.URL) string {
	p := url.escaped_path().trim_left('/')
	return if url.query().len > 0 { '/${p}?${url.query().encode()}' } else { '/${p}' }
}

fn normalized_origin(url urllib.URL) string {
	mut port := url.port().int()
	if port == 0 {
		port = match url.scheme {
			'https' { 443 }
			'http' { 80 }
			else { 0 }
		}
	}
	return '${url.scheme}://${url.hostname()}:${port}'
}

// maybe_store_alt_svc checks the response for an Alt-Svc header and, if
// present, parses and stores the entries in the Alt-Svc cache.
fn (req &Request) maybe_store_alt_svc(url urllib.URL, resp_header Header) {
	if req.alt_svc_cache != unsafe { nil } {
		if alt_svc_val := resp_header.get_custom('alt-svc') {
			entries := parse_alt_svc(alt_svc_val)
			if entries.len > 0 {
				origin := normalized_origin(url)
				mut cache := unsafe { req.alt_svc_cache }
				cache.store(origin, entries)
			}
		}
	}
}

fn (req &Request) do_http2(url urllib.URL) !Response {
	host_name := url.hostname()
	mut nport := url.port().int()
	if nport == 0 {
		nport = 443
	}

	address := '${host_name}:${nport}'

	mut client := v2.new_client_with_config(address, v2.ClientConfig{
		verify:                 req.verify
		cert:                   req.cert
		cert_key:               req.cert_key
		validate:               req.validate
		in_memory_verification: req.in_memory_verification
	}) or { return error('HTTP/2 connection failed: ${err}') }

	defer {
		client.close()
	}

	v2_req := v2.Request{
		method: v2.Method(req.method)
		url:    build_request_path(url)
		host:   host_name
		data:   req.data
		header: req.build_client_header()
	}

	v2_resp := client.request(v2_req) or { return error('HTTP/2 request failed: ${err}') }

	actual_resp := if v2.is_misdirected(v2_resp) {
		client.close()
		v2.handle_misdirected(address, v2_req) or {
			return error('HTTP/2 misdirected retry failed: ${err}')
		}
	} else {
		v2_resp
	}

	req.maybe_store_alt_svc(url, actual_resp.header)

	return Response{
		body:        actual_resp.body
		status_code: actual_resp.status_code
		header:      actual_resp.header
	}
}
