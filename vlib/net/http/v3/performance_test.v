// HTTP/3 Performance Test
// Tests the optimizations we just made to QPACK and QUIC components
module v3

import time

fn test_qpack_encoding_performance() {
	// Create test headers
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
		HeaderField{':scheme', 'https'},
		HeaderField{':authority', 'example.com'},
		HeaderField{'user-agent', 'V-HTTP3-Client/1.0'},
		HeaderField{'accept', '*/*'},
	]

	mut encoder := new_qpack_encoder(4096, 100)

	// Benchmark encoding
	iterations := 10000
	start := time.now()

	for _ in 0 .. iterations {
		_ := encoder.encode(headers)
	}

	elapsed := time.now() - start
	avg_time := f64(elapsed.microseconds()) / f64(iterations)

	println('QPACK Encoding Performance:')
	println('  Iterations: ${iterations}')
	println('  Average time: ${avg_time:.2f} μs')
	println('  Headers per second: ${f64(iterations) / f64(elapsed.seconds()):.0f}')

	assert avg_time < 50.0 // Should be faster than 50μs with hashmap optimization
}

fn test_qpack_static_table_lookup() {
	// Test that hashmap lookups work correctly
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':method', 'POST'},
		HeaderField{':path', '/'},
		HeaderField{':scheme', 'https'},
	]

	mut encoder := new_qpack_encoder(4096, 100)

	// First encoding should use static table
	encoded1 := encoder.encode(headers)

	// Verify the encoding is correct
	assert encoded1.len > 0
	// First two bytes are Required Insert Count and Delta Base
	assert encoded1[0] == 0x00
	assert encoded1[1] == 0x00
	// Third byte should be indexed (11XXXXXX pattern for static indexed)
	assert encoded1[2] & 0xc0 == 0xc0 // Check bits 7-6 are both 1

	println('\nQPACK Static Table Lookup Test:')
	println('  Headers encoded: ${headers.len}')
	println('  Encoded size: ${encoded1.len} bytes')
	println('  ✓ Hashmap lookup working correctly')
}

fn test_qpack_integer_encoding() {
	// Test integer encoding performance
	iterations := 100000
	start := time.now()

	for i in 0 .. iterations {
		_ := encode_integer(i % 1000, 7)
	}

	elapsed := time.now() - start
	avg_time := f64(elapsed.microseconds()) / f64(iterations)

	println('\nQPACK Integer Encoding Performance:')
	println('  Iterations: ${iterations}')
	println('  Average time: ${avg_time:.3f} μs')
	println('  Integers per second: ${f64(iterations) / f64(elapsed.seconds()):.0f}')

	assert avg_time < 1.0 // Should be sub-microsecond with pre-allocation
}

fn test_qpack_string_encoding() {
	// Test string encoding performance
	test_string := 'user-agent'
	iterations := 50000
	start := time.now()

	for _ in 0 .. iterations {
		_ := encode_qpack_string(test_string)
	}

	elapsed := time.now() - start
	avg_time := f64(elapsed.microseconds()) / f64(iterations)

	println('\nQPACK String Encoding Performance:')
	println('  Iterations: ${iterations}')
	println('  String length: ${test_string.len} bytes')
	println('  Average time: ${avg_time:.3f} μs')
	println('  Strings per second: ${f64(iterations) / f64(elapsed.seconds()):.0f}')

	assert avg_time < 2.0 // Should be fast with pre-allocation
}

fn test_qpack_compression_ratio() {
	// Test compression effectiveness
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/api/v1/users'},
		HeaderField{':scheme', 'https'},
		HeaderField{':authority', 'api.example.com'},
		HeaderField{'user-agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'},
		HeaderField{'accept', 'application/json, text/plain, */*'},
		HeaderField{'accept-encoding', 'gzip, deflate, br'},
		HeaderField{'accept-language', 'en-US,en;q=0.9'},
		HeaderField{'cache-control', 'no-cache'},
		HeaderField{'pragma', 'no-cache'},
	]

	// Calculate original size
	mut original_size := 0
	for header in headers {
		original_size += header.name.len + header.value.len + 2 // +2 for separators
	}

	mut encoder := new_qpack_encoder(4096, 100)
	encoded := encoder.encode(headers)

	compression_ratio := f64(original_size) / f64(encoded.len)

	println('\nQPACK Compression Ratio Test:')
	println('  Headers: ${headers.len}')
	println('  Original size: ${original_size} bytes')
	println('  Compressed size: ${encoded.len} bytes')
	println('  Compression ratio: ${compression_ratio:.2f}x')
	println('  Bandwidth savings: ${((1.0 - f64(encoded.len) / f64(original_size)) * 100.0):.1f}%')

	assert compression_ratio > 1.5 // Should achieve at least 1.5x compression
}

fn test_qpack_repeated_headers() {
	// Test performance with repeated headers (common in HTTP/3)
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
		HeaderField{':scheme', 'https'},
		HeaderField{':authority', 'example.com'},
	]

	mut encoder := new_qpack_encoder(4096, 100)

	// Encode same headers multiple times
	iterations := 1000
	start := time.now()

	for _ in 0 .. iterations {
		_ := encoder.encode(headers)
	}

	elapsed := time.now() - start
	avg_time := f64(elapsed.microseconds()) / f64(iterations)

	println('\nQPACK Repeated Headers Performance:')
	println('  Iterations: ${iterations}')
	println('  Average time: ${avg_time:.2f} μs')
	println('  Requests per second: ${f64(iterations) / f64(elapsed.seconds()):.0f}')

	// With hashmap lookup, repeated headers should be very fast
	assert avg_time < 10.0
}
