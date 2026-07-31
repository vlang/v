module quic

fn test_varint_boundary_values_round_trip() {
	// (value, expected encoded length)
	cases := [
		[u64(0), 1],
		[u64(63), 1], // 0x3F, largest 1-byte
		[u64(64), 2], // 0x40, smallest 2-byte
		[u64(16383), 2], // 0x3FFF, largest 2-byte
		[u64(16384), 4], // smallest 4-byte
		[u64(1073741823), 4], // 0x3FFFFFFF, largest 4-byte
		[u64(1073741824), 8], // smallest 8-byte
		[max_varint, 8], // 0x3FFFFFFFFFFFFFFF, largest representable value
	]
	for c in cases {
		value := c[0]
		expected_len := int(c[1])
		encoded := encode_varint(value)!
		assert encoded.len == expected_len, 'value ${value}: expected ${expected_len} bytes, got ${encoded.len}'
		decoded_value, consumed := decode_varint(encoded)!
		assert decoded_value == value, 'value ${value}: round trip mismatch, got ${decoded_value}'
		assert consumed == expected_len
	}
}

fn test_varint_rejects_value_above_max() {
	encode_varint(max_varint + 1) or {
		assert err.msg().contains('exceeds')
		return
	}
	assert false, 'expected an error for a value above 2^62-1'
}

fn test_varint_rejects_truncated_buffer() {
	// 0x40 signals a 2-byte encoding, but only 1 byte is supplied.
	decode_varint([u8(0x40)]) or {
		assert err.msg().contains('truncated')
		return
	}
	assert false, 'expected an error for a truncated buffer'
}

fn test_varint_rejects_non_minimal_encoding() {
	// Bytes [0x40, 0x05] use the 2-byte length class (top bits `01`) to
	// encode the value 5 — but 5 fits in the 1-byte class (max 0x3F), so its
	// minimal encoding is a single byte (0x05). This 2-byte form must be
	// rejected as non-minimal.
	non_minimal := [u8(0x40), 0x05]
	decode_varint(non_minimal) or {
		assert err.msg().contains('non-minimal')
		return
	}
	assert false, 'expected an error for a non-minimally-encoded varint'
}

fn test_varint_empty_buffer_errors() {
	decode_varint([]u8{}) or {
		assert err.msg().contains('empty')
		return
	}
	assert false, 'expected an error for an empty buffer'
}

fn test_varint_random_round_trip() {
	mut seed := u64(0x1234_5678_9abc_def0)
	for _ in 0 .. 200 {
		// xorshift64* for a deterministic, dependency-free pseudo-random spread
		seed ^= seed >> 12
		seed ^= seed << 25
		seed ^= seed >> 27
		value := (seed * u64(0x2545F4914F6CDD1D)) & max_varint
		encoded := encode_varint(value)!
		decoded_value, consumed := decode_varint(encoded)!
		assert decoded_value == value
		assert consumed == encoded.len
	}
}
