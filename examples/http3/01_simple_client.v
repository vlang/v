// HTTP/3 Client Example
// Demonstrates HTTP/3 client usage via the unified net.http API.
// HTTP/3 is discovered automatically through Alt-Svc headers.
//
// Note: For direct QUIC connection management, use net.quic.
// The unified API handles protocol negotiation transparently.
//
// Usage: v run examples/http3/01_simple_client.v
module main

import net.http

fn main() {
	println('=== HTTP/3 Client Example ===\n')

	// Example 1: Simple fetch with automatic protocol negotiation
	println('--- Example 1: Fetch with Protocol Discovery ---')
	fetch_example()

	// Example 2: Using Client with Alt-Svc cache for HTTP/3 upgrade
	println('\n--- Example 2: Alt-Svc Cache for HTTP/3 Discovery ---')
	alt_svc_example()

	// Example 3: HTTP methods via unified API
	println('\n--- Example 3: HTTP Methods ---')
	methods_example()

	println('\n=== HTTP/3 Client Example Complete ===')
}

fn fetch_example() {
	// Use http.fetch() — protocol is negotiated automatically.
	// If the server advertises HTTP/3 via Alt-Svc header, subsequent
	// requests can upgrade when using an Alt-Svc cache.
	println('Fetching https://cloudflare-quic.com/ ...')

	response := http.fetch(
		url:    'https://cloudflare-quic.com/'
		method: .get
		header: http.new_header_from_map({
			.user_agent: 'V-HTTP3-Client/1.0'
			.accept:     '*/*'
		})
	) or {
		println('Request result: ${err}')
		println('  (This may fail if DNS resolution or network is unavailable)')
		return
	}

	println('Status: ${response.status_code}')
	body_preview := if response.body.len > 200 {
		response.body[..200] + '...'
	} else {
		response.body
	}
	println('Body (${response.body.len} bytes):\n${body_preview}')
}

fn alt_svc_example() {
	// Create a reusable client with shared Alt-Svc cache.
	// The cache stores Alt-Svc headers from server responses
	// to automatically upgrade subsequent requests to HTTP/3.
	mut client := http.new_client()
	println('Created HTTP client with Alt-Svc cache for HTTP/3 discovery')

	// First request discovers HTTP/3 support via Alt-Svc header
	println('First request (discovers HTTP/3 via Alt-Svc)...')
	response1 := client.get('https://cloudflare-quic.com/') or {
		println('  Request result: ${err}')
		return
	}
	println('  Status: ${response1.status_code}')

	// Second request may use HTTP/3 if Alt-Svc was cached
	println('Second request (may upgrade to HTTP/3)...')
	response2 := client.get('https://cloudflare-quic.com/') or {
		println('  Request result: ${err}')
		return
	}
	println('  Status: ${response2.status_code}')
}

fn methods_example() {
	// All HTTP methods are available through http.fetch()
	methods := ['get', 'post', 'put', 'delete', 'patch', 'head', 'options']

	for method in methods {
		config := http.FetchConfig{
			url:    'https://example.com/${method}'
			method: match method {
				'get' { .get }
				'post' { .post }
				'put' { .put }
				'delete' { .delete }
				'patch' { .patch }
				'head' { .head }
				'options' { .options }
				else { .get }
			}
		}
		println('  - ${method.to_upper()}: ${config.method}')
	}
	println('All HTTP methods supported via unified http.fetch()')
}
