// Simple HTTP/3 Client Example
// Demonstrates basic HTTP/3 client usage with QUIC
//
// Install dependencies (macOS): brew install openssl@3 libngtcp2
// Install dependencies (Linux): apt-get install libssl-dev libngtcp2-dev
//
// Build and run: ./v run examples/http3/01_simple_client.v
import net.http.v3
import net.quic

fn main() {
	println('=== HTTP/3 Client Example ===\n')

	// Example 1: Demonstrate QUIC connection
	println('--- Example 1: QUIC Connection ---')
	quic_connection_example()

	// Example 2: Building requests
	println('\n--- Example 2: Request Building ---')
	build_request_example()

	// Example 3: HTTP methods
	println('\n--- Example 3: HTTP Methods ---')
	methods_example()

	println('\n=== HTTP/3 Client Example Complete ===')
}

fn quic_connection_example() {
	// Create a QUIC connection to a public HTTP/3 server
	println('Creating QUIC connection to cloudflare-quic.com:443...')

	mut conn := quic.new_connection(
		remote_addr: 'cloudflare-quic.com:443'
		alpn:        ['h3']
	) or {
		println('Connection creation: ${err}')
		println('  (This may fail if DNS resolution or UDP is blocked)')
		return
	}
	println('QUIC connection created successfully')

	// Attempt the TLS 1.3 / QUIC handshake
	println('Performing QUIC/TLS handshake...')
	conn.perform_handshake() or {
		println('Handshake result: ${err}')
		println('  (Handshake requires network connectivity to the server)')
		conn.close()
		return
	}

	println('QUIC connection established!')
	conn.close()
	println('Connection closed')
}

fn build_request_example() {
	// Build a GET request
	get_request := v3.Request{
		method:  .get
		url:     '/'
		host:    'example.com'
		data:    ''
		headers: {
			'user-agent': 'V-HTTP3-Client/1.0'
			'accept':     'text/html'
		}
	}
	println('GET request built: ${get_request.method} ${get_request.url}')

	// Build a POST request with JSON body
	json_data := '{"name":"test","value":123}'
	post_request := v3.Request{
		method:  .post
		url:     '/api/data'
		host:    'example.com'
		data:    json_data
		headers: {
			'content-type':   'application/json'
			'content-length': json_data.len.str()
			'user-agent':     'V-HTTP3-Client/1.0'
		}
	}
	println('POST request built: ${post_request.method} ${post_request.url}')
}

fn methods_example() {
	// HTTP methods available
	methods := ['get', 'post', 'put', 'delete', 'patch', 'head', 'options']

	for method in methods {
		request := v3.Request{
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
			url:    '/${method}'
			host:   'example.com'
		}
		println('  - ${method.to_upper()}: ${request.method}')
	}
	println('All HTTP methods supported')
}
