module v3

// Tests for HTTP/3 varint encoding, string codec, and frame types.

fn test_encode_decode_varint_1byte() {
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
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
		HeaderField{':scheme', 'https'},
		HeaderField{'custom-header', 'custom-value'},
	]

	encoded := encoder.encode(headers)
	assert encoded.len > 0

	decoded := decoder.decode(encoded) or {
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
	assert u64(FrameType.data) == 0x0
	assert u64(FrameType.headers) == 0x1
	assert u64(FrameType.settings) == 0x4
	assert u64(FrameType.goaway) == 0x7
}

fn test_max_varint_constant() {
	assert max_varint == u64(0x3FFF_FFFF_FFFF_FFFF)
}

fn test_encode_varint_zero() {
	encoded := encode_varint(u64(0)) or {
		assert false, 'Value 0 should be valid'
		return
	}
	assert encoded.len == 1
	assert encoded[0] == 0
}

fn test_encode_varint_max_valid() {
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
	too_large := u64(0x4000_0000_0000_0000)
	_ := encode_varint(too_large) or { return }
	assert false, 'encode_varint should have returned an error for value > max_varint'
}

fn test_encode_varint_boundary_1byte_max() {
	encoded := encode_varint(u64(63)) or {
		assert false, 'Value 63 should be valid'
		return
	}
	assert encoded.len == 1
}

fn test_encode_varint_boundary_2byte_min() {
	encoded := encode_varint(u64(64)) or {
		assert false, 'Value 64 should be valid'
		return
	}
	assert encoded.len == 2
}

fn test_encode_varint_boundary_2byte_max() {
	encoded := encode_varint(u64(16383)) or {
		assert false, 'Value 16383 should be valid'
		return
	}
	assert encoded.len == 2
}

fn test_encode_varint_boundary_4byte_min() {
	encoded := encode_varint(u64(16384)) or {
		assert false, 'Value 16384 should be valid'
		return
	}
	assert encoded.len == 4
}

fn test_encode_varint_boundary_4byte_max() {
	encoded := encode_varint(u64(1073741823)) or {
		assert false, 'Value 1073741823 should be valid'
		return
	}
	assert encoded.len == 4
}

fn test_encode_varint_boundary_8byte_min() {
	encoded := encode_varint(u64(1073741824)) or {
		assert false, 'Value 1073741824 should be valid'
		return
	}
	assert encoded.len == 8
}
