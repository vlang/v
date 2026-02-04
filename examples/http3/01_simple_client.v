// Simple HTTP/3 Client Example
// Demonstrates basic HTTP/3 client usage with QUIC
//
// Note: This example requires QUIC protocol support:
// 1. QUIC library (ngtcp2, quiche, or msquic)
// 2. TLS 1.3 support
// 3. UDP socket handling
//
// Install dependencies (macOS): brew install openssl ngtcp2
// Install dependencies (Linux): apt-get install libssl-dev libngtcp2-dev
//
// Current status: QUIC implementation is in progress.
// The client will gracefully handle connection failures.
import net.http.v3

fn main() {
	println('=== HTTP/3 Client Example ===\n')

	// Create HTTP/3 client
	// The address format is "host:port"
	mut client := v3.new_client('www.cloudflare.com:443') or {
		eprintln('Failed to create HTTP/3 client: ${err}')
		eprintln('\nThis is expected - HTTP/3 requires full QUIC implementation.')
		eprintln('The error message provides details about what is needed.')
		return
	}

	println('HTTP/3 client created successfully')

	// Example 1: Simple GET request
	println('\n--- Example 1: GET Request ---')
	get_example(mut client)

	// Example 2: POST request with JSON
	println('\n--- Example 2: POST Request ---')
	post_example(mut client)

	// Example 3: Multiple requests
	println('\n--- Example 3: Multiple Requests ---')
	multiple_requests_example(mut client)

	// Close client connection
	client.close()

	println('\n=== HTTP/3 Client Example Complete ===')
}

fn get_example(mut client v3.Client) {
	// Build a GET request using the SimpleRequest structure
	request := v3.SimpleRequest{
		method:  .get
		url:     '/'
		host:    'www.cloudflare.com'
		data:    ''
		headers: {
			'user-agent': 'V-HTTP3-Client/1.0'
			'accept':     'text/html'
		}
	}

	// Send the request
	response := client.request(request) or {
		eprintln('GET request failed: ${err}')
		return
	}

	println('Status: ${response.status_code}')
	println('Body length: ${response.body.len} bytes')
	println('Protocol: HTTP/3 (QUIC)')

	// Print first 100 characters of body
	if response.body.len > 0 {
		body_preview := if response.body.len > 100 {
			response.body[..100] + '...'
		} else {
			response.body
		}
		println('Body preview: ${body_preview}')
	}
}

fn post_example(mut client v3.Client) {
	// Prepare JSON data
	json_data := '{"message":"Hello from HTTP/3","protocol":"h3"}'

	// Build POST request
	request := v3.SimpleRequest{
		method:  .post
		url:     '/post'
		host:    'httpbin.org'
		data:    json_data
		headers: {
			'content-type':   'application/json'
			'content-length': json_data.len.str()
			'user-agent':     'V-HTTP3-Client/1.0'
		}
	}

	// Send the request
	response := client.request(request) or {
		eprintln('POST request failed: ${err}')
		return
	}

	println('Status: ${response.status_code}')

	// Print response body (truncated)
	body_preview := if response.body.len > 100 {
		response.body[..100] + '...'
	} else {
		response.body
	}
	println('Response: ${body_preview}')
}

fn multiple_requests_example(mut client v3.Client) {
	// HTTP/3 supports multiplexing - multiple requests on same connection
	// This demonstrates sending multiple requests sequentially
	urls := [
		'/',
		'/cdn-cgi/trace',
		'/robots.txt',
	]

	println('Sending ${urls.len} requests...')

	for i, url in urls {
		request := v3.SimpleRequest{
			method:  .get
			url:     url
			host:    'www.cloudflare.com'
			data:    ''
			headers: {
				'user-agent': 'V-HTTP3-Client/1.0'
			}
		}

		response := client.request(request) or {
			eprintln('Request ${i + 1} failed: ${err}')
			continue
		}

		println('Request ${i + 1} (${url}): ${response.status_code} (${response.body.len} bytes)')
	}

	println('All requests completed')
}
