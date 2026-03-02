// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

// Test for HTTP/3 varint encoding/decoding

fn test_encode_decode_varint_1byte() {
	// Test 1-byte encoding (0-63)
	value := u64(42)
	encoded := encode_varint(value) or {
		assert false, 'Failed to encode varint'
		return
	}
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
	encoded := encode_varint(value) or {
		assert false, 'Failed to encode varint'
		return
	}
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
	encoded := encode_varint(value) or {
		assert false, 'Failed to encode varint'
		return
	}
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
	encoded := encode_varint(value) or {
		assert false, 'Failed to encode varint'
		return
	}
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
	encoded := encode_string(test_str) or {
		assert false, 'Failed to encode string'
		return
	}

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

	encoded := encode_headers(headers) or {
		assert false, 'Failed to encode headers: ${err}'
		return
	}
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

fn test_max_varint_constant() {
	// max_varint must equal 2^62 - 1 (RFC 9000 Section 16)
	assert max_varint == u64(0x3FFF_FFFF_FFFF_FFFF)
}

fn test_encode_varint_zero() {
	// Value 0 is valid and encodes as a single byte
	encoded := encode_varint(u64(0)) or {
		assert false, 'Value 0 should be valid'
		return
	}
	assert encoded.len == 1
	assert encoded[0] == 0
}

fn test_encode_varint_max_valid() {
	// Value 2^62 - 1 is the maximum valid value, encodes as 8 bytes
	encoded := encode_varint(max_varint) or {
		assert false, 'max_varint should be valid'
		return
	}
	assert encoded.len == 8
	decoded, bytes_read := decode_varint(encoded) or {
		assert false, 'Failed to decode max_varint'
		return
	}
	assert decoded == max_varint
	assert bytes_read == 8
}

fn test_encode_varint_too_large() {
	// Value 2^62 exceeds the 62-bit limit and must return an error
	too_large := u64(0x4000_0000_0000_0000) // 2^62
	_ := encode_varint(too_large) or {
		// expected: error returned
		return
	}
	assert false, 'encode_varint should have returned an error for value > max_varint'
}

fn test_encode_varint_boundary_1byte_max() {
	// 63 is the maximum value for 1-byte encoding
	encoded := encode_varint(u64(63)) or {
		assert false, 'Value 63 should be valid'
		return
	}
	assert encoded.len == 1
}

fn test_encode_varint_boundary_2byte_min() {
	// 64 is the minimum value for 2-byte encoding
	encoded := encode_varint(u64(64)) or {
		assert false, 'Value 64 should be valid'
		return
	}
	assert encoded.len == 2
}

fn test_encode_varint_boundary_2byte_max() {
	// 16383 is the maximum value for 2-byte encoding
	encoded := encode_varint(u64(16383)) or {
		assert false, 'Value 16383 should be valid'
		return
	}
	assert encoded.len == 2
}

fn test_encode_varint_boundary_4byte_min() {
	// 16384 is the minimum value for 4-byte encoding
	encoded := encode_varint(u64(16384)) or {
		assert false, 'Value 16384 should be valid'
		return
	}
	assert encoded.len == 4
}

fn test_encode_varint_boundary_4byte_max() {
	// 1073741823 is the maximum value for 4-byte encoding
	encoded := encode_varint(u64(1073741823)) or {
		assert false, 'Value 1073741823 should be valid'
		return
	}
	assert encoded.len == 4
}

fn test_encode_varint_boundary_8byte_min() {
	// 1073741824 is the minimum value for 8-byte encoding
	encoded := encode_varint(u64(1073741824)) or {
		assert false, 'Value 1073741824 should be valid'
		return
	}
	assert encoded.len == 8
}
