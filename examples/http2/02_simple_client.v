// HTTP/2 Client Example — connects to a real HTTPS server using TLS with ALPN 'h2'
// and sends a GET request over HTTP/2.
//
// Usage: v run examples/http2/02_simple_client.v
import net.http.v2

fn main() {
	println('=== HTTP/2 Client Example ===\n')

	// Connect to nghttp2.org over TLS with ALPN 'h2' negotiation
	mut client := v2.new_client('nghttp2.org:443') or {
		eprintln('Failed to connect: ${err}')
		return
	}
	defer {
		client.close()
	}
	println('Connected to nghttp2.org via HTTP/2 over TLS')

	// Send a GET request
	response := client.request(v2.Request{
		method:  .get
		url:     '/'
		host:    'nghttp2.org'
		headers: {
			'user-agent': 'V-HTTP2-Client/1.0'
			'accept':     '*/*'
		}
	}) or {
		eprintln('Request failed: ${err}')
		return
	}

	println('Status: ${response.status_code}')
	println('Headers:')
	for key, value in response.headers {
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
