// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

// Test for HTTP/3 varint encoding/decoding

fn test_encode_decode_varint_1byte() {
	// Test 1-byte encoding (0-63)
	value := u64(42)
	encoded := encode_varint(value)
	assert encoded.len == 1
	assert encoded[0] == 42

	decoded, bytes_read := decode_varint(encoded) or {
		assert false, 'Failed to decode varint'
		return
	}
	assert decoded == value
	assert bytes_read == 1
}

fn test_encode_decode_varint_2byte() {
	// Test 2-byte encoding (64-16383)
	value := u64(1000)
	encoded := encode_varint(value)
	assert encoded.len == 2

	decoded, bytes_read := decode_varint(encoded) or {
		assert false, 'Failed to decode varint'
		return
	}
	assert decoded == value
	assert bytes_read == 2
}

fn test_encode_decode_varint_4byte() {
	// Test 4-byte encoding (16384-1073741823)
	value := u64(1000000)
	encoded := encode_varint(value)
	assert encoded.len == 4

	decoded, bytes_read := decode_varint(encoded) or {
		assert false, 'Failed to decode varint'
		return
	}
	assert decoded == value
	assert bytes_read == 4
}

fn test_encode_decode_varint_8byte() {
	// Test 8-byte encoding (1073741824+)
	value := u64(10000000000)
	encoded := encode_varint(value)
	assert encoded.len == 8

	decoded, bytes_read := decode_varint(encoded) or {
		assert false, 'Failed to decode varint'
		return
	}
	assert decoded == value
	assert bytes_read == 8
}

fn test_encode_decode_string() {
	test_str := 'www.example.com'
	encoded := encode_string(test_str)

	decoded, bytes_read := decode_string(encoded) or {
		assert false, 'Failed to decode string'
		return
	}
	assert decoded == test_str
	assert bytes_read == encoded.len
}

fn test_encode_decode_headers() {
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
		HeaderField{':scheme', 'https'},
		HeaderField{'custom-header', 'custom-value'},
	]

	encoded := encode_headers(headers)
	decoded := decode_headers(encoded) or {
		assert false, 'Failed to decode headers'
		return
	}

	assert decoded.len == headers.len
	for i, header in headers {
		assert decoded[i].name == header.name
		assert decoded[i].value == header.value
	}
}

fn test_frame_type() {
	// Test frame type enum values
	assert u64(FrameType.data) == 0x0
	assert u64(FrameType.headers) == 0x1
	assert u64(FrameType.settings) == 0x4
	assert u64(FrameType.goaway) == 0x7
}
