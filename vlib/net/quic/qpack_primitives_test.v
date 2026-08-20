// vtest build: present_openssl?
module quic

fn test_decode_prefixed_int_single_byte() {
	value, consumed := decode_prefixed_int([u8(0x05)], 5) or { panic('${err}') }
	assert value == 5
	assert consumed == 1
}

fn test_decode_prefixed_int_max_prefix_no_continuation_needed() {
	// prefix_bits=5, max_prefix=31 -- a value of exactly 30 fits without continuation.
	value, consumed := decode_prefixed_int([u8(30)], 5) or { panic('${err}') }
	assert value == 30
	assert consumed == 1
}

fn test_decode_prefixed_int_with_continuation() {
	// RFC 9204 Appendix B.2's own Set Dynamic Table Capacity=220 example.
	value, consumed := decode_prefixed_int([u8(0x1f), 0xbd, 0x01], 5) or { panic('${err}') }
	assert value == 220
	assert consumed == 3
}

fn test_encode_decode_prefixed_int_roundtrip_various_widths_and_values() {
	values := [u64(0), 1, 30, 31, 32, 100, 1000, 1_000_000, qpack_max_prefixed_int]
	widths := [2, 3, 4, 5, 6, 7, 8]
	for w in widths {
		for v in values {
			mut out := []u8{}
			encode_prefixed_int(mut out, v, w, 0)
			decoded, consumed := decode_prefixed_int(out, w) or {
				panic('prefix_bits=${w} value=${v}: ${err}')
			}
			assert decoded == v
			assert consumed == out.len
		}
	}
}

fn test_encode_prefixed_int_preserves_high_bits() {
	mut out := []u8{}
	encode_prefixed_int(mut out, 5, 6, 0x80) // simulate T=1 pattern bit above a 6-bit prefix
	assert out == [u8(0x85)]
}

fn test_decode_prefixed_int_rejects_value_beyond_implementation_limit() {
	// A pathologically long continuation sequence that would decode past
	// qpack_max_prefixed_int (2^62-1) must be rejected, not wrapped.
	mut buf := [u8(0x1f)] // 5-bit prefix, max_prefix=31, forces continuation
	for _ in 0 .. 10 {
		buf << 0xff // continuation bit set, near-max payload byte
	}
	buf << 0x7f // last byte, no continuation bit, large payload
	if _, _ := decode_prefixed_int(buf, 5) {
		assert false, 'expected an error for an over-limit integer'
	}
}

fn test_decode_prefixed_int_truncated_continuation_is_error() {
	if _, _ := decode_prefixed_int([u8(0x1f), 0xff], 5) {
		assert false, 'expected truncated-continuation error'
	}
}

fn test_decode_prefixed_int_empty_buffer_is_error() {
	if _, _ := decode_prefixed_int([]u8{}, 5) {
		assert false, 'expected empty-buffer error'
	}
}

fn test_encode_decode_prefixed_string_roundtrip_raw_and_huffman() {
	// A short, punctuation-heavy string that Huffman-codes LONGER than raw
	// (forcing the raw path) and a plain-lowercase string Huffman codes well.
	strs := ['', 'a', 'www.example.com', '!!!===+++', 'the quick brown fox']
	for s in strs {
		mut out := []u8{}
		encode_prefixed_string(mut out, s, 7, 0)
		decoded, _, consumed := decode_prefixed_string(out, 7) or { panic('"${s}": ${err}') }
		assert decoded == s
		assert consumed == out.len
	}
}

fn test_decode_prefixed_string_huffman_flag_set_correctly() {
	mut out := []u8{}
	// Lowercase ASCII Huffman-codes shorter than raw for any string of
	// reasonable length -- assert the H bit reflects that choice.
	encode_prefixed_string(mut out, 'aaaaaaaaaa', 7, 0)
	_, huffman, _ := decode_prefixed_string(out, 7) or { panic('${err}') }
	assert huffman
	assert out.len < 11 // shorter than the 1 (length byte) + 10 (raw) it would take unencoded
}

fn test_decode_prefixed_string_rejects_oversized_declared_length() {
	mut out := []u8{}
	encode_prefixed_int(mut out, u64(qpack_max_string_literal_len) + 1, 7, 0)
	if _, _, _ := decode_prefixed_string(out, 7) {
		assert false, 'expected an implementation-limit error'
	}
}

fn test_decode_prefixed_string_rejects_length_exceeding_buffer() {
	mut out := []u8{}
	encode_prefixed_int(mut out, 100, 7, 0) // claims 100 bytes follow
	if _, _, _ := decode_prefixed_string(out, 7) {
		assert false, 'expected a truncated-buffer error'
	}
}

fn test_decode_prefixed_string_empty_buffer_is_error() {
	if _, _, _ := decode_prefixed_string([]u8{}, 7) {
		assert false, 'expected empty-buffer error'
	}
}
