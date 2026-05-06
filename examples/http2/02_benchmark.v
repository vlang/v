// HTTP Performance Benchmark
// Benchmarks unified HTTP API operations: response building, header handling,
// and request processing throughput.
//
// Note: For low-level HTTP/2 frame encoding and HPACK compression benchmarks,
// use net.http.v2 directly for advanced protocol-level access.
module main

import net.http
import time

const iterations = 10000
const large_payload_size = 1024 * 64 // 64KB

fn main() {
	println('=== HTTP Performance Benchmark ===\n')

	println('Benchmark 1: ServerResponse Building')
	benchmark_response_building()

	println('\nBenchmark 2: Header Creation and Lookup')
	benchmark_header_operations()

	println('\nBenchmark 3: Large Body Handling')
	benchmark_large_bodies()

	println('\nBenchmark 4: Multiple Request/Response Simulation')
	benchmark_multiple_requests()

	println('\n=== Benchmark Complete ===')
}

fn benchmark_response_building() {
	mut total_time := i64(0)
	mut total_bytes := i64(0)

	for i in 0 .. iterations {
		body := 'Hello HTTP World! Request ${i}'.bytes()

		start := time.now()
		response := http.ServerResponse{
			status_code: 200
			header:      http.new_header_from_map({
				.content_type: 'text/plain'
			})
			body:        body
		}
		elapsed := time.now() - start

		total_time += elapsed.microseconds()
		total_bytes += response.body.len
	}

	avg_time := f64(total_time) / f64(iterations)
	throughput := f64(total_bytes) / (f64(total_time) / 1_000_000.0) / 1024.0 / 1024.0

	println('  Iterations: ${iterations}')
	println('  Average build time: ${avg_time:.2f} \u03bcs')
	println('  Total bytes: ${total_bytes} bytes')
	println('  Throughput: ${throughput:.2f} MB/s')
}

fn benchmark_header_operations() {
	mut total_create_time := i64(0)
	mut total_lookup_time := i64(0)
	num_headers := 7

	for _ in 0 .. iterations {
		// Header creation
		start := time.now()
		header := http.new_header_from_map({
			.content_type:    'application/json'
			.accept:          'application/json'
			.user_agent:      'V-HTTP-Client/1.0'
			.accept_encoding: 'gzip, deflate'
			.accept_language: 'en-US,en;q=0.9'
			.host:            'example.com'
			.connection:      'keep-alive'
		})
		create_time := time.now() - start

		// Header lookup
		start2 := time.now()
		_ = header.get(.content_type) or { '' }
		_ = header.get(.accept) or { '' }
		_ = header.get(.user_agent) or { '' }
		_ = header.get(.accept_encoding) or { '' }
		_ = header.get(.accept_language) or { '' }
		_ = header.get(.host) or { '' }
		_ = header.get(.connection) or { '' }
		lookup_time := time.now() - start2

		total_create_time += create_time.microseconds()
		total_lookup_time += lookup_time.microseconds()
	}

	avg_create_time := f64(total_create_time) / f64(iterations)
	avg_lookup_time := f64(total_lookup_time) / f64(iterations)

	println('  Iterations: ${iterations}')
	println('  Average creation time: ${avg_create_time:.2f} \u03bcs')
	println('  Average lookup time (${num_headers} headers): ${avg_lookup_time:.2f} \u03bcs')
}

fn benchmark_large_bodies() {
	mut large_payload := []u8{len: large_payload_size}
	for i in 0 .. large_payload.len {
		large_payload[i] = u8(i % 256)
	}

	mut total_time := i64(0)
	test_iterations := 1000

	for _ in 0 .. test_iterations {
		start := time.now()
		response := http.ServerResponse{
			status_code: 200
			header:      http.new_header_from_map({
				.content_type: 'application/octet-stream'
			})
			body:        large_payload.clone()
		}
		elapsed := time.now() - start

		total_time += elapsed.microseconds()
		assert response.body.len == large_payload_size
	}

	avg_time := f64(total_time) / f64(test_iterations)
	throughput := f64(large_payload_size * test_iterations) / (f64(total_time) / 1_000_000.0) / 1024.0 / 1024.0

	println('  Iterations: ${test_iterations}')
	println('  Payload size: ${large_payload_size} bytes (${large_payload_size / 1024} KB)')
	println('  Average build time: ${avg_time:.2f} \u03bcs')
	println('  Throughput: ${throughput:.2f} MB/s')
}

fn benchmark_multiple_requests() {
	num_streams := 100
	requests_per_stream := 100

	mut total_pairs := 0

	start := time.now()

	for stream_id in 1 .. num_streams + 1 {
		for req_id in 0 .. requests_per_stream {
			request := http.ServerRequest{
				method: .get
				path:   '/stream/${stream_id}/request/${req_id}'
				host:   'benchmark.local'
				body:   'Stream ${stream_id} Request ${req_id}'.bytes()
			}
			_ := http.ServerResponse{
				status_code: 200
				body:        request.body
			}
			total_pairs++
		}
	}

	elapsed := time.now() - start
	total_time := elapsed.microseconds()

	avg_time_per_pair := f64(total_time) / f64(total_pairs)
	pairs_per_second := f64(total_pairs) / (f64(total_time) / 1_000_000.0)

	println('  Number of streams: ${num_streams}')
	println('  Requests per stream: ${requests_per_stream}')
	println('  Total request/response pairs: ${total_pairs}')
	println('  Total time: ${total_time / 1000} ms')
	println('  Average time per pair: ${avg_time_per_pair:.2f} \u03bcs')
	println('  Pairs per second: ${pairs_per_second:.0f}')
}
