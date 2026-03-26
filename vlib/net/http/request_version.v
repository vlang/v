module http

// HTTP version negotiation and multi-version request dispatch.
import net.urllib
import net.http.v2
import net.http.v3

fn (req &Request) negotiate_version(url urllib.URL) Version {
	if req.version != .unknown {
		return req.version
	}

	if url.scheme != 'https' {
		return .v1_1
	}

	return .v2_0
}

// Method enums are duplicated across net.http, v2, and v3 due to circular import constraints.
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

// Method enums are duplicated across net.http, v2, and v3 due to circular import constraints.
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

fn (req &Request) build_headers_map() map[string]string {
	mut headers_map := map[string]string{}
	for key in req.header.keys() {
		values := req.header.custom_values(key)
		if values.len > 0 {
			headers_map[key] = values.join('; ')
		}
	}
	if 'user-agent' !in headers_map {
		headers_map['user-agent'] = req.user_agent
	}
	if req.data.len > 0 && 'content-length' !in headers_map {
		headers_map['content-length'] = req.data.len.str()
	}
	return headers_map
}

fn build_request_path(url urllib.URL) string {
	p := url.escaped_path().trim_left('/')
	return if url.query().len > 0 { '/${p}?${url.query().encode()}' } else { '/${p}' }
}

// do_http2 and do_http3 follow parallel structure because v2 and v3 modules
// define separate Request/Response types to avoid circular imports.

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
		method:  to_v2_method(req.method)
		url:     build_request_path(url)
		host:    host_name
		data:    req.data
		headers: req.build_headers_map()
	}

	v2_resp := client.request(v2_req) or { return error('HTTP/2 request failed: ${err}') }

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
		method:  to_v3_method(req.method)
		url:     build_request_path(url)
		host:    host_name
		data:    req.data
		headers: req.build_headers_map()
	}

	v3_resp := client.request(v3_req) or { return error('HTTP/3 request failed: ${err}') }

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
