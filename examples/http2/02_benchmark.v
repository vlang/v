// HTTP/2 Performance Benchmark
// Benchmarks HTTP/2 frame encoding/decoding and HPACK compression
module main

import net.http.v2
import time

const iterations = 10000
const large_payload_size = 1024 * 64 // 64KB

fn main() {
	println('=== HTTP/2 Performance Benchmark ===\n')

	// Benchmark 1: Frame encoding/decoding
	println('Benchmark 1: Frame Encoding/Decoding')
	benchmark_frame_operations()

	// Benchmark 2: HPACK compression
	println('\nBenchmark 2: HPACK Header Compression')
	benchmark_hpack_compression()

	// Benchmark 3: Large payload handling
	println('\nBenchmark 3: Large Payload Handling')
	benchmark_large_payloads()

	// Benchmark 4: Multiple streams simulation
	println('\nBenchmark 4: Multiple Streams Simulation')
	benchmark_multiple_streams()

	println('\n=== Benchmark Complete ===')
}

fn benchmark_frame_operations() {
	// Test DATA frame encoding
	mut total_time := i64(0)
	mut total_bytes := i64(0)

	for i in 0 .. iterations {
		payload := 'Hello HTTP/2 World! Request ${i}'.bytes()

		frame := v2.Frame{
			header:  v2.FrameHeader{
				length:     u32(payload.len)
				frame_type: .data
				flags:      0x01 // END_STREAM
				stream_id:  u32(i + 1)
			}
			payload: payload
		}

		start := time.now()
		encoded := v2.encode_frame(frame)
		elapsed := time.now() - start

		total_time += elapsed.microseconds()
		total_bytes += encoded.len
	}

	avg_time := f64(total_time) / f64(iterations)
	throughput := f64(total_bytes) / (f64(total_time) / 1_000_000.0) / 1024.0 / 1024.0

	println('  Iterations: ${iterations}')
	println('  Average encoding time: ${avg_time:.2f} μs')
	println('  Total bytes encoded: ${total_bytes} bytes')
	println('  Throughput: ${throughput:.2f} MB/s')
}

fn benchmark_hpack_compression() {
	mut encoder := v2.new_encoder()
	mut decoder := v2.new_decoder()

	headers := [
		v2.HeaderField{
			name:  ':method'
			value: 'GET'
		},
		v2.HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		v2.HeaderField{
			name:  ':path'
			value: '/api/v1/users'
		},
		v2.HeaderField{
			name:  ':authority'
			value: 'example.com'
		},
		v2.HeaderField{
			name:  'user-agent'
			value: 'V-HTTP2-Client/1.0'
		},
		v2.HeaderField{
			name:  'accept'
			value: 'application/json'
		},
		v2.HeaderField{
			name:  'accept-encoding'
			value: 'gzip, deflate'
		},
		v2.HeaderField{
			name:  'accept-language'
			value: 'en-US,en;q=0.9'
		},
	]

	mut total_encode_time := i64(0)
	mut total_decode_time := i64(0)
	mut total_original_size := i64(0)
	mut total_compressed_size := i64(0)

	for _ in 0 .. iterations {
		// Encoding
		start := time.now()
		encoded := encoder.encode(headers)
		encode_time := time.now() - start

		// Decoding
		start2 := time.now()
		decoded := decoder.decode(encoded) or {
			eprintln('Decoding error: ${err}')
			continue
		}
		decode_time := time.now() - start2

		// Calculate original size (rough estimate)
		mut original_size := 0
		for header in headers {
			original_size += header.name.len + header.value.len + 2 // +2 for ": "
		}

		// Verify decoding
		assert decoded.len == headers.len

		total_encode_time += encode_time.microseconds()
		total_decode_time += decode_time.microseconds()
		total_original_size += original_size
		total_compressed_size += encoded.len
	}

	avg_encode_time := f64(total_encode_time) / f64(iterations)
	avg_decode_time := f64(total_decode_time) / f64(iterations)
	compression_ratio := f64(total_original_size) / f64(total_compressed_size)

	println('  Iterations: ${iterations}')
	println('  Average encoding time: ${avg_encode_time:.2f} μs')
	println('  Average decoding time: ${avg_decode_time:.2f} μs')
	println('  Original size: ${total_original_size} bytes')
	println('  Compressed size: ${total_compressed_size} bytes')
	println('  Compression ratio: ${compression_ratio:.2f}x')
}

fn benchmark_large_payloads() {
	// Create large payload
	mut large_payload := []u8{len: large_payload_size}
	for i in 0 .. large_payload.len {
		large_payload[i] = u8(i % 256)
	}

	mut total_time := i64(0)
	test_iterations := 1000 // Fewer iterations for large payloads

	for i in 0 .. test_iterations {
		frame := v2.Frame{
			header:  v2.FrameHeader{
				length:     u32(large_payload.len)
				frame_type: .data
				flags:      0x01
				stream_id:  u32(i + 1)
			}
			payload: large_payload.clone()
		}

		start := time.now()
		encoded := v2.encode_frame(frame)
		elapsed := time.now() - start

		total_time += elapsed.microseconds()

		// Verify encoded size
		assert encoded.len == large_payload.len + 9 // 9 bytes for frame header
	}

	avg_time := f64(total_time) / f64(test_iterations)
	throughput := f64(large_payload_size * test_iterations) / (f64(total_time) / 1_000_000.0) / 1024.0 / 1024.0

	println('  Iterations: ${test_iterations}')
	println('  Payload size: ${large_payload_size} bytes (${large_payload_size / 1024} KB)')
	println('  Average encoding time: ${avg_time:.2f} μs')
	println('  Throughput: ${throughput:.2f} MB/s')
}

fn benchmark_multiple_streams() {
	num_streams := 100
	requests_per_stream := 100

	mut total_time := i64(0)
	mut total_frames := 0

	start := time.now()

	for stream_id in 1 .. num_streams + 1 {
		for req_id in 0 .. requests_per_stream {
			payload := 'Stream ${stream_id} Request ${req_id}'.bytes()

			frame := v2.Frame{
				header:  v2.FrameHeader{
					length:     u32(payload.len)
					frame_type: .data
					flags:      if req_id == requests_per_stream - 1 { u8(0x01) } else { u8(0) }
					stream_id:  u32(stream_id)
				}
				payload: payload
			}

			_ := v2.encode_frame(frame)
			total_frames++
		}
	}

	elapsed := time.now() - start
	total_time = elapsed.microseconds()

	avg_time_per_frame := f64(total_time) / f64(total_frames)
	frames_per_second := f64(total_frames) / (f64(total_time) / 1_000_000.0)

	println('  Number of streams: ${num_streams}')
	println('  Requests per stream: ${requests_per_stream}')
	println('  Total frames: ${total_frames}')
	println('  Total time: ${total_time / 1000} ms')
	println('  Average time per frame: ${avg_time_per_frame:.2f} μs')
	println('  Frames per second: ${frames_per_second:.0f}')
}
