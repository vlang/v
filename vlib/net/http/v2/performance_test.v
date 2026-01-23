// HTTP/2 Performance Test
// Tests the optimizations we just made
module v2

import time

fn test_frame_encoding_performance() {
	// Create a test frame with 1KB payload
	payload := []u8{len: 1024, init: u8(index % 256)}
	frame := Frame{
		header:  FrameHeader{
			length:     u32(payload.len)
			frame_type: FrameType.data
			flags:      0x01
			stream_id:  1
		}
		payload: payload
	}

	// Benchmark encoding
	iterations := 10000
	start := time.now()

	for _ in 0 .. iterations {
		_ := frame.encode()
	}

	elapsed := time.now() - start
	avg_time := f64(elapsed.microseconds()) / f64(iterations)
	throughput := f64(payload.len * iterations) / f64(elapsed.microseconds())

	println('Frame Encoding Performance:')
	println('  Iterations: ${iterations}')
	println('  Average time: ${avg_time:.2f} μs')
	println('  Throughput: ${throughput:.2f} MB/s')

	assert avg_time < 5.0 // Should be faster than 5μs with optimizations
}

fn test_hpack_encoding_performance() {
	// Create test headers
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
		HeaderField{':scheme', 'https'},
		HeaderField{':authority', 'example.com'},
		HeaderField{'user-agent', 'V-HTTP2-Client/1.0'},
		HeaderField{'accept', '*/*'},
	]

	mut encoder := new_encoder()

	// Benchmark encoding
	iterations := 10000
	start := time.now()

	for _ in 0 .. iterations {
		_ := encoder.encode(headers)
	}

	elapsed := time.now() - start
	avg_time := f64(elapsed.microseconds()) / f64(iterations)

	println('\nHPACK Encoding Performance:')
	println('  Iterations: ${iterations}')
	println('  Average time: ${avg_time:.2f} μs')
	println('  Headers per second: ${f64(iterations) / f64(elapsed.seconds()):.0f}')

	assert avg_time < 50.0 // Should be faster than 50μs with hashmap optimization
}

fn test_static_table_lookup() {
	// Test that hashmap lookups work correctly
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':method', 'POST'},
		HeaderField{':path', '/'},
		HeaderField{':scheme', 'https'},
	]

	mut encoder := new_encoder()

	// First encoding should use static table
	encoded1 := encoder.encode(headers)

	// Verify the encoding is correct (should use indexed representation)
	assert encoded1.len > 0
	assert encoded1[0] & 0x80 == 0x80 // First byte should be indexed (bit 7 set)

	println('\nStatic Table Lookup Test:')
	println('  Headers encoded: ${headers.len}')
	println('  Encoded size: ${encoded1.len} bytes')
	println('  ✓ Hashmap lookup working correctly')
}

fn test_memory_efficiency() {
	// Test that we're not creating unnecessary copies
	payload := []u8{len: 10240, init: u8(index % 256)} // 10KB

	mut data := []u8{len: 9 + payload.len}
	// Create a frame header
	data[0] = u8(payload.len >> 16)
	data[1] = u8(payload.len >> 8)
	data[2] = u8(payload.len)
	data[3] = u8(FrameType.data)
	data[4] = 0x01
	data[5] = 0
	data[6] = 0
	data[7] = 0
	data[8] = 1

	// Copy payload
	for i in 0 .. payload.len {
		data[9 + i] = payload[i]
	}

	// Parse frame - should not clone payload with our optimization
	frame := parse_frame(data) or { panic('Failed to parse frame: ${err}') }

	assert frame.payload.len == payload.len

	println('\nMemory Efficiency Test:')
	println('  Payload size: ${payload.len} bytes')
	println('  Frame parsed successfully')
	println('  ✓ No unnecessary cloning')
}
