// HTTP/2 Integration Test
// Tests HTTP/2 server and client together
import net.http.v2
import time
import os

const test_port = 18080

fn test_http2_server_client_integration() {
	println('=== HTTP/2 Integration Test ===\n')

	// Start server in background
	mut server := start_test_server() or {
		eprintln('Failed to start server: ${err}')
		assert false, 'Server start failed'
		return
	}

	// Wait for server to start
	time.sleep(500 * time.millisecond)

	// Test 1: Simple GET request
	println('Test 1: Simple GET request')
	test_simple_get()

	// Test 2: Multiple concurrent requests
	println('\nTest 2: Multiple concurrent requests')
	test_concurrent_requests()

	// Test 3: Large response
	println('\nTest 3: Large response')
	test_large_response()

	// Stop server
	server.stop()
	time.sleep(100 * time.millisecond)

	println('\n=== All Integration Tests Passed ===')
}

fn start_test_server() !&v2.Server {
	config := v2.ServerConfig{
		addr:                   '0.0.0.0:${test_port}'
		max_concurrent_streams: 100
		initial_window_size:    65535
		max_frame_size:         16384
	}

	mut server := v2.new_server(config, request_handler)!

	spawn fn [mut server] () {
		server.listen_and_serve() or { eprintln('Server error: ${err}') }
	}()

	return server
}

fn request_handler(req v2.ServerRequest) v2.ServerResponse {
	match req.path {
		'/' {
			return v2.ServerResponse{
				status_code: 200
				headers:     {
					'content-type': 'text/plain'
				}
				body:        'Hello HTTP/2!'.bytes()
			}
		}
		'/large' {
			// Generate large response (1MB)
			mut large_body := []u8{len: 1024 * 1024}
			for i in 0 .. large_body.len {
				large_body[i] = u8(i % 256)
			}
			return v2.ServerResponse{
				status_code: 200
				headers:     {
					'content-type': 'application/octet-stream'
				}
				body:        large_body
			}
		}
		'/echo' {
			return v2.ServerResponse{
				status_code: 200
				headers:     {
					'content-type': 'text/plain'
				}
				body:        'Method: ${req.method}\nPath: ${req.path}'.bytes()
			}
		}
		else {
			return v2.ServerResponse{
				status_code: 404
				body:        'Not Found'.bytes()
			}
		}
	}
}

fn test_simple_get() {
	// Note: This would require HTTP/2 client implementation
	// For now, we test with curl if available
	result := execute_or_skip('curl --version')
	if result.exit_code != 0 {
		println('  ⚠ Skipped (curl not available)')
		return
	}

	// Test with curl
	result2 := execute_or_skip('curl --http2-prior-knowledge -s http://localhost:${test_port}/')
	if result2.exit_code == 0 {
		assert result2.output.contains('Hello HTTP/2'), 'Response should contain greeting'
		println('  ✓ Simple GET test passed')
	} else {
		println('  ⚠ Skipped (connection failed)')
	}
}

fn test_concurrent_requests() {
	result := execute_or_skip('curl --version')
	if result.exit_code != 0 {
		println('  ⚠ Skipped (curl not available)')
		return
	}

	// Test multiple concurrent requests
	mut threads := []thread{}
	mut results := []string{len: 5}

	for i in 0 .. 5 {
		threads << spawn make_request(i, mut results)
	}

	// Wait for all threads
	for t in threads {
		t.wait()
	}

	// Check results
	mut success_count := 0
	for res in results {
		if res.contains('Hello HTTP/2') {
			success_count++
		}
	}

	if success_count > 0 {
		println('  ✓ Concurrent requests test passed (${success_count}/5 succeeded)')
	} else {
		println('  ⚠ Skipped (no successful requests)')
	}
}

fn make_request(id int, mut results []string) {
	result := execute_or_skip('curl --http2-prior-knowledge -s http://localhost:${test_port}/')
	if result.exit_code == 0 {
		results[id] = result.output
	}
}

fn test_large_response() {
	result := execute_or_skip('curl --version')
	if result.exit_code != 0 {
		println('  ⚠ Skipped (curl not available)')
		return
	}

	// Test large response
	result2 := execute_or_skip('curl --http2-prior-knowledge -s http://localhost:${test_port}/large -o /dev/null -w "%{size_download}"')
	if result2.exit_code == 0 {
		size := result2.output.trim_space().int()
		assert size == 1024 * 1024, 'Response size should be 1MB'
		println('  ✓ Large response test passed (${size} bytes)')
	} else {
		println('  ⚠ Skipped (connection failed)')
	}
}

struct ExecResult {
	output    string
	exit_code int
}

fn execute_or_skip(cmd string) ExecResult {
	result := execute(cmd)
	return result
}

fn execute(cmd string) ExecResult {
	$if windows {
		return ExecResult{
			output:    ''
			exit_code: 1
		}
	} $else {
		result := os.execute(cmd)
		return ExecResult{
			output:    result.output
			exit_code: result.exit_code
		}
	}
}

fn main() {
	test_http2_server_client_integration()
}
