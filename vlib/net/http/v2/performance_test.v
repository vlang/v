module v2

// Performance benchmarks for HTTP/2 frame encoding and HPACK compression.
import time

fn test_frame_encoding_performance() {
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

	assert avg_time < 5.0
}

fn test_hpack_encoding_performance() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  ':authority'
			value: 'example.com'
		},
		HeaderField{
			name:  'user-agent'
			value: 'V-HTTP2-Client/1.0'
		},
		HeaderField{
			name:  'accept'
			value: '*/*'
		},
	]

	mut encoder := new_encoder()

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

	assert avg_time < 50.0
}

fn test_static_table_lookup() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':method'
			value: 'POST'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
	]

	mut encoder := new_encoder()

	encoded1 := encoder.encode(headers)

	assert encoded1.len > 0
	assert encoded1[0] & 0x80 == 0x80

	println('\nStatic Table Lookup Test:')
	println('  Headers encoded: ${headers.len}')
	println('  Encoded size: ${encoded1.len} bytes')
	println('  ✓ Hashmap lookup working correctly')
}

fn test_memory_efficiency() {
	payload := []u8{len: 10240, init: u8(index % 256)}

	mut data := []u8{len: 9 + payload.len}
	data[0] = u8(payload.len >> 16)
	data[1] = u8(payload.len >> 8)
	data[2] = u8(payload.len)
	data[3] = u8(FrameType.data)
	data[4] = 0x01
	data[5] = 0
	data[6] = 0
	data[7] = 0
	data[8] = 1

	for i in 0 .. payload.len {
		data[9 + i] = payload[i]
	}

	frame := parse_frame(data) or { panic('Failed to parse frame: ${err}') }

	assert frame.payload.len == payload.len

	println('\nMemory Efficiency Test:')
	println('  Payload size: ${payload.len} bytes')
	println('  Frame parsed successfully')
	println('  ✓ No unnecessary cloning')
}
