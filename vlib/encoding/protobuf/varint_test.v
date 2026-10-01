module protobuf

// u64_max and the int64 bounds are spelled out rather than taken from u64_max
// and friends, which this compiler does not provide.
const u64_max = u64(0xffff_ffff_ffff_ffff)
const i64_max = i64(0x7fff_ffff_ffff_ffff)
const i64_min = i64(-0x8000_0000_0000_0000)

fn test_varint_size() {
	assert varint_size(0) == 1
	assert varint_size(1) == 1
	assert varint_size(127) == 1
	assert varint_size(128) == 2
	assert varint_size(300) == 2
	assert varint_size(16383) == 2
	assert varint_size(16384) == 3
	assert varint_size(u64(0xffff_ffff)) == 5
	assert varint_size(u64(0xffff_ffff_ffff_ffff)) == 10
}

fn test_put_varint_matches_size() {
	values := [
		u64(0),
		u64(1),
		u64(127),
		u64(128),
		u64(300),
		u64(16383),
		u64(16384),
		u64(0xffff_ffff),
		u64(0xffff_ffff_ffff_ffff),
	]
	for value in values {
		mut buf := []u8{}
		put_varint(mut buf, value)
		assert buf.len == varint_size(value), 'varint_size(${value})'
	}
}

fn test_put_varint_known_encodings() {
	// The examples from the protobuf encoding spec, plus the 10-byte maximum.
	assert encode_varint(0) == [0x00]
	assert encode_varint(1) == [0x01]
	assert encode_varint(127) == [0x7f]
	assert encode_varint(128) == [0x80, 0x01]
	assert encode_varint(300) == [0xac, 0x02]
	assert encode_varint(16383) == [0xff, 0x7f]
	assert encode_varint(16384) == [0x80, 0x80, 0x01]
	assert encode_varint(0xffff_ffff) == [0xff, 0xff, 0xff, 0xff, 0x0f]
	assert encode_varint(u64(0xffff_ffff_ffff_ffff)) == [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
		0xff, 0xff, 0x01]
}

// encode_varint is a test-local helper that puts a varint into a fresh array,
// keeping the expectations above readable.
fn encode_varint(value u64) []u8 {
	mut buf := []u8{}
	put_varint(mut buf, value)
	return buf
}

fn test_read_varint_round_trip() {
	values := [u64(0), 1, 127, 128, 300, 16383, 16384, 0xffff_ffff, u64(0xffff_ffff_ffff_ffff)]
	for value in values {
		data := encode_varint(value)
		got, pos := read_varint(data, 0)!
		assert got == value, 'round trip of ${value} gave ${got}'
		assert pos == data.len
	}
}

fn test_read_varint_at_offset() {
	data := [0xaa, 0xac, 0x02, 0xbb]
	got, pos := read_varint(data, 1)!
	assert got == 300
	assert pos == 3
	// the trailing byte is untouched
	assert data[pos] == 0xbb
}

fn test_read_varint_accepts_nonminimal_encoding() {
	// The spec requires parsers to accept a varint padded with a redundant zero
	// group. 300 is `ac 02` in its shortest form, and `ac 82 00` is the same
	// value written in three bytes.
	data := [u8(0xac), u8(0x82), u8(0x00)]
	got, pos := read_varint(data, 0)!
	assert got == 300
	assert pos == 3
}

fn test_read_varint_truncated_fails() {
	if got, _ := read_varint([u8(0x80)], 0) {
		assert false, 'expected a truncated varint to fail, got ${got}'
	} else {
		assert err is UnexpectedEofError
	}
}

fn test_read_varint_too_long_fails() {
	// Eleven continuation bytes exceeds what 64 bits can need.
	data := []u8{len: 11, init: 0x80}
	if got, _ := read_varint(data, 0) {
		assert false, 'expected an overlong varint to fail, got ${got}'
	} else {
		assert err is MalformedError
	}
}

fn test_read_varint_tenth_byte_overflow_is_truncated() {
	// The tenth byte may only carry one significant bit. The reference
	// implementations accept the rest and drop the excess, so a producer's
	// overflow must not break a consumer.
	data := [u8(0xff), u8(0xff), u8(0xff), u8(0xff), u8(0xff), u8(0xff), u8(0xff), u8(0xff), u8(0xff),
		u8(0x7f)]
	got, pos := read_varint(data, 0)!
	assert got == u64_max
	assert pos == 10
}

fn test_zigzag_encode_i32() {
	assert zigzag_encode_i32(0) == 0
	assert zigzag_encode_i32(-1) == 1
	assert zigzag_encode_i32(1) == 2
	assert zigzag_encode_i32(-2) == 3
	assert zigzag_encode_i32(2) == 4
	assert zigzag_encode_i32(2147483647) == 4294967294
	assert zigzag_encode_i32(-2147483648) == 4294967295
}

fn test_zigzag_encode_i64() {
	assert zigzag_encode_i64(0) == 0
	assert zigzag_encode_i64(-1) == 1
	assert zigzag_encode_i64(1) == 2
	assert zigzag_encode_i64(-2) == 3
	assert zigzag_encode_i64(i64_max) == u64_max - 1
	assert zigzag_encode_i64(i64_min) == u64_max
}

fn test_zigzag_round_trip() {
	i32s := [i32(0), 1, -1, 2, -2, 2147483647, -2147483648, 12345, -12345]
	for value in i32s {
		assert zigzag_decode_i32(zigzag_encode_i32(value)) == value, 'i32 ${value}'
	}
	i64s := [i64(0), 1, -1, 2, -2, i64_max, i64_min, 1234567890123, -1234567890123]
	for value in i64s {
		assert zigzag_decode_i64(zigzag_encode_i64(value)) == value, 'i64 ${value}'
	}
}

fn test_zigzag_keeps_small_negatives_small() {
	// The point of zigzag: -1 costs one byte, where an int32 would cost ten.
	assert varint_size(u64(zigzag_encode_i32(-1))) == 1
	assert varint_size(u64(zigzag_encode_i32(-64))) == 1
	assert varint_size(u64(zigzag_encode_i32(-65))) == 2
	// by contrast the plain int32 encoding
	assert varint_size(int32_varint(-1)) == 10
}

fn test_int32_varint_sign_extends_negative() {
	// int32 has no compact negative form: the spec requires sign extension to
	// 64 bits, so -1 takes ten bytes rather than being truncated to 1.
	assert int32_varint(0) == 0
	assert int32_varint(1) == 1
	assert int32_varint(-1) == u64_max
	assert int32_varint(2147483647) == 2147483647
	assert int32_varint(-2147483648) == 0xffff_ffff_8000_0000
}

fn test_int64_varint_sign_extends_negative() {
	assert int64_varint(0) == 0
	assert int64_varint(-1) == u64_max
	assert int64_varint(i64_max) == i64_max
	assert int64_varint(i64_min) == 0x8000_0000_0000_0000
}

fn test_int32_varint_round_trips_through_varint() {
	for value in [i32(0), 1, -1, 2147483647, -2147483648] {
		data := encode_varint(int32_varint(value))
		got, _ := read_varint(data, 0)!
		assert i32(got) == value, 'int32 ${value} came back as ${got}'
	}
}
