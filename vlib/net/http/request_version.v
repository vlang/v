module http

// HTTP version negotiation and multi-version request dispatch.
// Alt-Svc cache integration: do_http2 checks for Alt-Svc headers in responses
// and negotiate_version consults the cache before defaulting to HTTP/2.
import net.urllib
import net.http.v2
import net.http.v3

// negotiate_version selects the HTTP version for a request.
// Checks Alt-Svc cache for h3 entries before defaulting to v2 for HTTPS.
fn (req &Request) negotiate_version(url urllib.URL) Version {
	if req.version != .unknown {
		return req.version
	}

	if url.scheme != 'https' {
		return .v1_1
	}

	if req.alt_svc_cache != unsafe { nil } {
		origin := normalized_origin(url)
		mut cache := unsafe { req.alt_svc_cache }
		if _ := cache.get_h3_endpoint(origin) {
			return .v3_0
		}
	}

	return .v2_0
}

// Method is now unified via common.Method — direct cast replaces manual mapping.

// build_client_header prepares the request header for v2/v3 clients,
// adding user-agent and content-length if not already set.
fn (req &Request) build_client_header() Header {
	mut h := req.header
	if !h.contains_custom('user-agent') {
		h.add_custom('user-agent', req.user_agent) or {}
	}
	if req.data.len > 0 && !h.contains(.content_length) {
		h.add(.content_length, req.data.len.str())
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

fn (req &Request) do_http2(url urllib.URL) !Response {
	host_name := url.hostname()
	mut nport := url.port().int()
	if nport == 0 {
		nport = 443
	}

	address := '${host_name}:${nport}'

	mut client := v2.new_client(address) or { return error('HTTP/2 connection failed: ${err}') }

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

	if req.alt_svc_cache != unsafe { nil } {
		if alt_svc_val := actual_resp.header.get_custom('alt-svc') {
			entries := parse_alt_svc(alt_svc_val)
			if entries.len > 0 {
				origin := normalized_origin(url)
				mut cache := unsafe { req.alt_svc_cache }
				cache.store(origin, entries)
			}
		}
	}

	return Response{
		body:        actual_resp.body
		status_code: actual_resp.status_code
		header:      actual_resp.header
	}
}

fn (req &Request) do_http3(url urllib.URL) !Response {
	host_name := url.hostname()
	mut nport := url.port().int()
	if nport == 0 {
		nport = 443
	}

	address := '${host_name}:${nport}'

	mut client := v3.new_client(address) or { return error('HTTP/3 connection failed: ${err}') }

	defer {
		client.close()
	}

	v3_req := v3.Request{
		method: v3.Method(req.method)
		url:    build_request_path(url)
		host:   host_name
		data:   req.data
		header: req.build_client_header()
	}

	v3_resp := client.request(v3_req) or { return error('HTTP/3 request failed: ${err}') }

	actual_resp := if v3.is_misdirected(v3_resp) {
		client.close()
		v3.handle_misdirected(address, v3_req) or {
			return error('HTTP/3 misdirected retry failed: ${err}')
		}
	} else {
		v3_resp
	}

	if req.alt_svc_cache != unsafe { nil } {
		if alt_svc_val := actual_resp.header.get_custom('alt-svc') {
			entries := parse_alt_svc(alt_svc_val)
			if entries.len > 0 {
				origin := normalized_origin(url)
				mut cache := unsafe { req.alt_svc_cache }
				cache.store(origin, entries)
			}
		}
	}

	return Response{
		body:        actual_resp.body
		status_code: actual_resp.status_code
		header:      actual_resp.header
	}
}
