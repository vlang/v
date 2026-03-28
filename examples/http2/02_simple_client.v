// HTTP/2 Client Example — uses the unified net.http API to make HTTPS requests.
// HTTP/2 is negotiated automatically over TLS via ALPN.
//
// Usage: v run examples/http2/02_simple_client.v
module main

import net.http

fn main() {
	println('=== HTTP/2 Client Example ===\n')

	// Use http.fetch() — HTTP/2 is negotiated automatically over TLS
	println('Fetching https://nghttp2.org/ via unified API...')

	response := http.fetch(
		url:    'https://nghttp2.org/'
		method: .get
		header: http.new_header_from_map({
			.user_agent: 'V-HTTP2-Client/1.0'
			.accept:     '*/*'
		})
	) or {
		eprintln('Request failed: ${err}')
		return
	}

	println('Status: ${response.status_code}')
	println('Headers:')
	for key in response.header.keys() {
		value := response.header.get_custom(key) or { '' }
		println('  ${key}: ${value}')
	}
	body_preview := if response.body.len > 200 {
		response.body[..200] + '...'
	} else {
		response.body
	}
	println('Body (${response.body.len} bytes):\n${body_preview}')
	println('\n=== Done ===')
}
