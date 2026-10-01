module protobuf

// max_varint_bytes bounds a base-128 varint. Sixty-four bits need at most ten
// 7-bit groups, so an eleventh byte means the input is malformed rather than
// merely large.
pub const max_varint_bytes = 10

// varint_size returns the number of bytes `value` occupies as a varint.
pub fn varint_size(value u64) int {
	mut size := 1
	mut rest := value
	for rest >= 0x80 {
		rest >>= 7
		size++
	}
	return size
}

// put_varint appends `value` to `buf` in base 128: the low seven bits per
// byte, with the high bit set on every byte but the last.
pub fn put_varint(mut buf []u8, value u64) {
	mut rest := value
	for rest >= 0x80 {
		buf << u8(rest & 0x7f) | 0x80
		rest >>= 7
	}
	buf << u8(rest)
}

// read_varint reads one base-128 varint at `pos` and returns it with the
// position just past it.
//
// A varint that runs off the end of the buffer, or that needs more than
// max_varint_bytes bytes, is an error rather than a silent misread. A varint
// written with more bytes than strictly needed is accepted, because the spec
// requires parsers to tolerate it: the extra bytes are zero and contribute
// nothing. For the same reason the tenth byte's upper six bits are discarded
// instead of rejected, which matches the reference implementations and keeps
// a producer's overflow from breaking a consumer.
pub fn read_varint(data []u8, pos int) !(u64, int) {
	mut value := u64(0)
	mut shift := u32(0)
	mut i := pos
	for _ in 0 .. max_varint_bytes {
		if i >= data.len {
			// `need` is the one-more-than-available form: a truncated varint
			// does not know its own final width, only that it needs more.
			return unexpected_eof_at(pos, data.len - pos + 1, data.len - pos)
		}
		b := data[i]
		value |= u64(b & 0x7f) << shift
		i++
		if b & 0x80 == 0 {
			return value, i
		}
		shift += 7
	}
	return malformed_at(pos, 'varint is longer than ${max_varint_bytes} bytes')
}

// zigzag_encode_i32 maps a signed 32-bit value onto an unsigned one so that
// small magnitudes stay small: 0, -1, 1, -2, 2 become 0, 1, 2, 3, 4. Without
// it every negative `sint32` would cost the full ten bytes an `int32` does.
pub fn zigzag_encode_i32(value i32) u32 {
	return u32(u32(value) << 1) ^ u32(value >> 31)
}

// zigzag_encode_i64 is zigzag_encode_i32 for 64-bit values.
pub fn zigzag_encode_i64(value i64) u64 {
	return u64(value) << 1 ^ u64(value >> 63)
}

// zigzag_decode_i32 inverts zigzag_encode_i32.
pub fn zigzag_decode_i32(value u32) i32 {
	return i32(value >> 1) ^ -i32(value & 1)
}

// zigzag_decode_i64 inverts zigzag_encode_i64.
pub fn zigzag_decode_i64(value u64) i64 {
	return i64(value >> 1) ^ -i64(value & 1)
}

// int32_varint returns the varint encoding of `value` as an `int32` reaches the
// wire. A negative value is sign-extended to sixty-four bits, so it costs ten
// bytes: that is what the spec mandates, and truncating to 32 bits would turn
// -1 into a value a reader cannot recover.
pub fn int32_varint(value i32) u64 {
	return u64(i64(value))
}

// int64_varint returns the varint encoding of `value` as an `int64` reaches the
// wire, sign-extending a negative value to the full 64 bits.
pub fn int64_varint(value i64) u64 {
	return u64(value)
}
