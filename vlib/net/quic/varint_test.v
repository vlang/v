// vtest build: present_openssl?
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

// test_varint_accepts_non_minimal_encoding is a regression test for a
// Codex finding (vlang/v#27680 pullrequestreview-4781706846):
// decode_varint used to reject a value encoded in a longer-than-minimal
// length class, even though RFC 9000 §16 permits any of the 4 classes a
// value fits in -- only the ENCODER is expected to prefer the minimal
// form (encode_varint always does; see the round-trip test below), not
// the decoder to reject anything else. Exercises all 3 non-minimal
// classes for the same value (5, whose minimal encoding is 1 byte).
fn test_varint_accepts_non_minimal_encoding() {
	// 2-byte class (top bits `01`): 0x40, 0x05.
	value2, consumed2 := decode_varint([u8(0x40), 0x05])!
	assert value2 == 5
	assert consumed2 == 2

	// 4-byte class (top bits `10`): 0x80, 0x00, 0x00, 0x05.
	value4, consumed4 := decode_varint([u8(0x80), 0x00, 0x00, 0x05])!
	assert value4 == 5
	assert consumed4 == 4

	// 8-byte class (top bits `11`): 0xC0 followed by 7 zero bytes then 0x05.
	value8, consumed8 := decode_varint([u8(0xC0), 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05])!
	assert value8 == 5
	assert consumed8 == 8
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
