// Coverage for the 32-bit half of leb128. The module's existing suite drives
// only encode_u64/decode_u64 and encode_i64/decode_i64; the four 32-bit
// functions were untested. Every expected byte sequence below was measured,
// not recalled.
import encoding.hex
import encoding.leb128

// Signed values that pin the 7-bit group boundaries and the sign-extension
// path: 0, the 6-bit singles, the values that need a second byte, the
// three-byte boundary, and the i32 extremes.
const i32_cases = [
	IntCase{ value: 0, encoded: '00' },
	IntCase{ value: 1, encoded: '01' },
	IntCase{ value: -1, encoded: '7f' },
	IntCase{ value: -64, encoded: '40' },
	IntCase{ value: 63, encoded: '3f' },
	IntCase{ value: 64, encoded: 'c000' },
	IntCase{ value: -65, encoded: 'bf7f' },
	IntCase{ value: 127, encoded: 'ff00' },
	IntCase{ value: 128, encoded: '8001' },
	IntCase{ value: 8191, encoded: 'ff3f' },
	IntCase{ value: 8192, encoded: '80c000' },
	IntCase{ value: -1048576, encoded: '808040' },
	IntCase{ value: 1048576, encoded: '8080c000' },
	IntCase{ value: 2147483647, encoded: 'ffffffff07' },
	IntCase{ value: -2147483648, encoded: '8080808078' },
]

// The unsigned equivalents. u32 needs five bytes for the top half of the
// range, and 4294967295 is the longest possible encoding.
const u32_cases = [
	UIntCase{ value: 0, encoded: '00' },
	UIntCase{ value: 1, encoded: '01' },
	UIntCase{ value: 127, encoded: '7f' },
	UIntCase{ value: 128, encoded: '8001' },
	UIntCase{ value: 16383, encoded: 'ff7f' },
	UIntCase{ value: 16384, encoded: '808001' },
	UIntCase{ value: 2147483647, encoded: 'ffffffff07' },
	UIntCase{ value: 2147483648, encoded: '8080808008' },
	UIntCase{ value: 4294967295, encoded: 'ffffffff0f' },
]

struct IntCase {
	value   i32
	encoded string
}

struct UIntCase {
	value   u32
	encoded string
}

fn test_encode_i32_known_vectors() {
	for c in i32_cases {
		got := leb128.encode_i32(c.value)
		assert got == hex.decode(c.encoded) or { panic('bad hex: ${c.encoded}') }, c.encoded
	}
}

fn test_encode_u32_known_vectors() {
	for c in u32_cases {
		got := leb128.encode_u32(c.value)
		assert got == hex.decode(c.encoded) or { panic('bad hex: ${c.encoded}') }, c.encoded
	}
}

fn test_i32_round_trip_and_byte_count() {
	for c in i32_cases {
		enc := hex.decode(c.encoded)!
		value, used := leb128.decode_i32(enc)
		assert value == c.value, c.encoded
		assert used == enc.len, '${c.encoded}: byte count ${used} != ${enc.len}'
		// Re-encoding the decoded value reproduces the same bytes.
		assert leb128.encode_i32(value) == enc, c.encoded
	}
}

fn test_u32_round_trip_and_byte_count() {
	for c in u32_cases {
		enc := hex.decode(c.encoded)!
		value, used := leb128.decode_u32(enc)
		assert value == c.value, c.encoded
		assert used == enc.len, '${c.encoded}: byte count ${used} != ${enc.len}'
		assert leb128.encode_u32(value) == enc, c.encoded
	}
}

// A 32-bit value must encode identically to the same value widened to 64
// bits: LEB128 is width agnostic for in-range values.
fn test_i32_encoding_matches_the_i64_encoding() {
	for c in i32_cases {
		assert leb128.encode_i32(c.value) == leb128.encode_i64(i64(c.value)), c.encoded
	}
}

fn test_u32_encoding_matches_the_u64_encoding() {
	for c in u32_cases {
		assert leb128.encode_u32(c.value) == leb128.encode_u64(u64(c.value)), c.encoded
	}
}

// The returned byte count is what makes a mixed-width buffer walkable.
fn test_decode_i32_walks_a_multi_value_buffer() {
	mut buf := []u8{}
	values := [i32(1), -2, 3, -4, 2147483647, -2147483648, 0]
	for v in values {
		buf << leb128.encode_i32(v)
	}
	mut offset := 0
	for i in 0 .. values.len {
		value, used := leb128.decode_i32(buf[offset..])
		assert value == values[i], 'index ${i}'
		assert used > 0, 'index ${i} consumed no bytes'
		offset += used
	}
	assert offset == buf.len, 'the walk overran the buffer'
}

fn test_decode_u32_walks_a_multi_value_buffer() {
	mut buf := []u8{}
	values := [u32(300), 70000, 4294967295, 0, 1, 127, 128]
	for v in values {
		buf << leb128.encode_u32(v)
	}
	mut offset := 0
	for i in 0 .. values.len {
		value, used := leb128.decode_u32(buf[offset..])
		assert value == values[i], 'index ${i}'
		assert used > 0, 'index ${i} consumed no bytes'
		offset += used
	}
	assert offset == buf.len, 'the walk overran the buffer'
}

// NOTE: the decoders do not reject over-long (non-canonical) encodings, so
// 0x80 0x00 reads as 0 rather than an error. That is lenient, not wrong for
// LEB128, and it is what the 64-bit functions do too, but it is pinned here
// so a change is visible.
fn test_decode_accepts_non_canonical_encodings() {
	v, used := leb128.decode_i32([u8(0x80), 0x00])
	assert v == 0
	assert used == 2
	uv, uused := leb128.decode_u32([u8(0x80), 0x00])
	assert uv == 0
	assert uused == 2
}

// NOTE: on an unterminated run of continuation bytes the unsigned decoders
// report one more byte than they consumed, because they compute
// `shift / 7 + 1` without adjusting for the missing terminator. Three bytes
// of 0x80 report 4. The signed decoders report the correct 3.
fn test_decode_reports_an_extra_byte_on_an_unterminated_input() {
	truncated := [u8(0x80), 0x80, 0x80]
	sv, sused := leb128.decode_i32(truncated)
	assert sv == 0
	assert sused == 3
	uv, uused := leb128.decode_u32(truncated)
	assert uv == 0
	assert uused == 4, 'the unsigned decoders over-count by one here'
}

// NOTE: the two width families disagree about how many bytes an empty input
// uses: the signed decoders report 0 and the unsigned ones report 1, from
// the same `+ 1` the note above describes.
fn test_decode_of_an_empty_input_reports_different_byte_counts() {
	iv, iused := leb128.decode_i32([]u8{})
	assert iv == 0
	assert iused == 0
	uv, uused := leb128.decode_u32([]u8{})
	assert uv == 0
	assert uused == 1
}
