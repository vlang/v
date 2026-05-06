module http

// HTTP/3 (QUIC) request support — compiled only with `-d use_ngtcp2`.
import net.urllib
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

	req.maybe_store_alt_svc(url, actual_resp.header)

	return Response{
		body:        actual_resp.body
		status_code: actual_resp.status_code
		header:      actual_resp.header
	}
}
