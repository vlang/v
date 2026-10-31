module pem

// https://github.com/vlang/v/issues/30690
// A block with an empty body could not be decoded, not even the encoder's own output.

fn test_decode_encoded_empty_block() {
	original := Block.new('TEST')
	encoded := original.encode() or {
		assert false, 'encode failed'
		return
	}
	block, rest := decode(encoded) or {
		assert false, 'decode of `${encoded}` failed'
		return
	}
	assert block == original
	assert block.data.len == 0
	assert rest == ''
	assert decode_only(encoded) or { Block{} } == original
}

fn test_decode_encoded_empty_block_with_crlf() {
	original := Block.new('TEST')
	encoded := original.encode(line_ending: '\r\n') or {
		assert false, 'encode failed'
		return
	}
	block := decode_only(encoded) or {
		assert false, 'decode_only failed'
		return
	}
	assert block == original
}

fn test_decode_end_line_right_after_begin_line() {
	block, rest := decode('-----BEGIN TEST-----\n-----END TEST-----\nrest') or {
		assert false, 'decode failed'
		return
	}
	assert block.block_type == 'TEST'
	assert block.headers.len == 0
	assert block.data.len == 0
	assert rest == '\nrest'
}

fn test_decode_end_line_right_after_crlf_begin_line() {
	block := decode_only('-----BEGIN TEST-----\r\n-----END TEST-----\r\n') or {
		assert false, 'decode_only failed'
		return
	}
	assert block.block_type == 'TEST'
	assert block.data.len == 0
}

fn test_decode_non_empty_body_is_unchanged() {
	block, rest := decode('-----BEGIN TEST-----\nQUJD\n-----END TEST-----\nrest') or {
		assert false, 'decode failed'
		return
	}
	assert block.data == 'ABC'.bytes()
	assert rest == '\nrest'
}
