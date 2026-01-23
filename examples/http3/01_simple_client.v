// Simple HTTP/3 Client Example
// Demonstrates basic HTTP/3 client usage with QUIC
import net.http.v3
import time

fn main() {
	println('=== HTTP/3 Client Example ===\n')

	// Note: This example requires OpenSSL and ngtcp2 libraries
	// Install with: brew install openssl ngtcp2 (macOS)
	//            or: apt-get install libssl-dev libngtcp2-dev (Linux)

	// Create HTTP/3 client configuration
	config := v3.ClientConfig{
		max_idle_timeout:         30 * time.second
		max_udp_payload_size:     1350
		initial_max_data:         1048576
		initial_max_stream_data:  524288
		initial_max_streams_bidi: 100
	}

	// Create HTTP/3 client
	mut client := v3.new_client(config) or {
		eprintln('Failed to create HTTP/3 client: ${err}')
		eprintln('\nNote: HTTP/3 requires OpenSSL and ngtcp2 libraries')
		eprintln('Install with:')
		eprintln('  macOS:  brew install openssl ngtcp2')
		eprintln('  Linux:  apt-get install libssl-dev libngtcp2-dev')
		return
	}

	println('HTTP/3 client created successfully')

	// Example 1: Simple GET request
	println('\n--- Example 1: GET Request ---')
	get_example(mut client)

	// Example 2: POST request with JSON
	println('\n--- Example 2: POST Request ---')
	post_example(mut client)

	// Example 3: Multiple concurrent requests
	println('\n--- Example 3: Concurrent Requests ---')
	concurrent_example(mut client)

	println('\n=== HTTP/3 Client Example Complete ===')
}

fn get_example(mut client v3.Client) {
	// Make a simple GET request
	response := client.get('https://www.cloudflare.com') or {
		eprintln('GET request failed: ${err}')
		return
	}

	println('Status: ${response.status_code}')
	println('Body length: ${response.body.len} bytes')
	println('Protocol: HTTP/3 (QUIC)')
}

fn post_example(mut client v3.Client) {
	// Prepare JSON data
	json_data := '{"message":"Hello from HTTP/3","protocol":"h3"}'

	// Make POST request
	response := client.post('https://httpbin.org/post', 'application/json', json_data.bytes()) or {
		eprintln('POST request failed: ${err}')
		return
	}

	println('Status: ${response.status_code}')
	println('Response: ${response.body.bytestr()[..100]}...')
}

fn concurrent_example(mut client v3.Client) {
	// HTTP/3 supports multiplexing - multiple requests on same connection
	urls := [
		'https://www.cloudflare.com',
		'https://www.cloudflare.com/cdn-cgi/trace',
		'https://www.cloudflare.com/robots.txt',
	]

	sw := time.new_stopwatch()

	for i, url in urls {
		response := client.get(url) or {
			eprintln('Request ${i + 1} failed: ${err}')
			continue
		}

		println('Request ${i + 1}: ${response.status_code} (${response.body.len} bytes)')
	}

	elapsed := sw.elapsed()
	println('Total time: ${elapsed.milliseconds()}ms')
	println('Average: ${elapsed.milliseconds() / urls.len}ms per request')
}
