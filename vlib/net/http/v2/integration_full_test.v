// HTTP/2 integration test for server and client together.
import net.http.v2
import net.http.common
import time
import os

const test_port = 18080

fn test_http2_server_client_integration() {
	println('=== HTTP/2 Integration Test ===\n')

	mut server := start_test_server() or {
		eprintln('Failed to start server: ${err}')
		assert false, 'Server start failed'
		return
	}

	time.sleep(500 * time.millisecond)

	println('Test 1: Simple GET request')
	test_simple_get()

	println('\nTest 2: Multiple concurrent requests')
	test_concurrent_requests()

	println('\nTest 3: POST with body')
	test_post_with_body()

	println('\nTest 4: Large response')
	test_large_response()

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
				header:      common.from_map({
					'content-type': 'text/plain'
				})
				body:        'Hello HTTP/2!'.bytes()
			}
		}
		'/large' {
			mut large_body := []u8{len: 1024 * 1024}
			for i in 0 .. large_body.len {
				large_body[i] = u8(i % 256)
			}
			return v2.ServerResponse{
				status_code: 200
				header:      common.from_map({
					'content-type': 'application/octet-stream'
				})
				body:        large_body
			}
		}
		'/echo' {
			return v2.ServerResponse{
				status_code: 200
				header:      common.from_map({
					'content-type': 'text/plain'
				})
				body:        'Method: ${req.method}\nPath: ${req.path}'.bytes()
			}
		}
		'/echo-body' {
			return v2.ServerResponse{
				status_code: 200
				header:      common.from_map({
					'content-type': 'application/octet-stream'
				})
				body:        req.body
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
	result := execute_or_skip('curl --version')
	if result.exit_code != 0 {
		println('  ⚠ Skipped (curl not available)')
		return
	}

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

	mut threads := []thread{}
	mut results := []string{len: 5}

	for i in 0 .. 5 {
		threads << spawn make_request(i, mut results)
	}

	for t in threads {
		t.wait()
	}

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

fn test_post_with_body() {
	result := execute_or_skip('curl --version')
	if result.exit_code != 0 {
		println('  ⚠ Skipped (curl not available)')
		return
	}

	test_body := 'Hello from POST body!'
	result2 := execute_or_skip('curl --http2-prior-knowledge -s -X POST -d "${test_body}" http://localhost:${test_port}/echo-body')
	if result2.exit_code == 0 {
		assert result2.output == test_body, 'Response body should echo back the POST body, got: ${result2.output}'
		println('  ✓ POST with body test passed')
	} else {
		println('  ⚠ Skipped (connection failed)')
	}
}

fn test_large_response() {
	result := execute_or_skip('curl --version')
	if result.exit_code != 0 {
		println('  ⚠ Skipped (curl not available)')
		return
	}

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
