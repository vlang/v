module leb128

// encode_i32 encodes the `value` as leb128 encoded byte array
pub fn encode_i32(value i32) []u8 {
	mut result := []u8{cap: 4}
	mut val := value
	mut i := 0
	for {
		mut b := u8(val & 0x7f)
		val >>= 7
		if (val == 0 && b & 0x40 == 0) || (val == -1 && b & 0x40 != 0) {
			result << b
			break
		}
		result << b | 0x80
		i++
	}
	return result
}

// encode_i64 encodes the `value` as leb128 encoded byte array
pub fn encode_i64(value i64) []u8 {
	mut result := []u8{cap: 8}
	mut val := value
	for {
		mut b := u8(val & 0x7f)
		val >>= 7
		if (val == 0 && b & 0x40 == 0) || (val == -1 && b & 0x40 != 0) {
			result << b
			break
		}
		result << b | 0x80
	}
	return result
}

// encode_u64 encodes the `value` as leb128 encoded byte array
pub fn encode_u64(value u64) []u8 {
	mut result := []u8{cap: 8}
	mut val := value
	for {
		mut b := u8(val & 0x7f)
		val >>= 7
		if val == 0 {
			result << b
			break
		}
		result << b | 0x80
	}
	return result
}

// encode_u32 encodes the `value` as leb128 encoded byte array
pub fn encode_u32(value u32) []u8 {
	mut result := []u8{cap: 4}
	mut val := value
	for {
		mut b := u8(val & 0x7f)
		val >>= 7
		if val == 0 {
			result << b
			break
		}
		result << b | 0x80
	}
	return result
}

// decode_i32 decodes an i32 and returns the number of bytes used from the given leb128 encoded array `value`.
// A non-positive length means the input did not hold a complete value:
// 0 for a truncated or empty buffer, and a negative length for an
// overflowing one, like Go's encoding/binary varint functions.
pub fn decode_i32(value []u8) (i32, int) {
	mut result := u32(0)
	mut shift := 0
	for i, b in value {
		if i == 4 {
			// Unused payload bits must repeat the signed value's sign bit.
			if b & 0x80 != 0 || (b > 0x07 && b < 0x78) {
				return 0, -(i + 1)
			}
		}
		result |= u32(b & 0x7f) << shift
		shift += 7
		if b & 0x80 == 0 {
			if shift < 32 && b & 0x40 != 0 {
				result |= u32(~0) << shift
			}
			return i32(result), i + 1
		}
	}
	return 0, 0
}

// decode_i64 decodes an i64 and returns the number of bytes used from the given leb128 encoded array `value`.
// A non-positive length means the input did not hold a complete value:
// 0 for a truncated or empty buffer, and a negative length for an
// overflowing one, like Go's encoding/binary varint functions.
pub fn decode_i64(value []u8) (i64, int) {
	mut result := u64(0)
	mut shift := 0
	for i, b in value {
		if i == 9 {
			// Only bit 63 remains; the other payload bits are sign extension.
			if b != 0x00 && b != 0x7f {
				return 0, -(i + 1)
			}
		}
		result |= u64(b & 0x7f) << shift
		shift += 7
		if b & 0x80 == 0 {
			if shift < 64 && b & 0x40 != 0 {
				result |= ~u64(0) << shift
			}
			return i64(result), i + 1
		}
	}
	return 0, 0
}

// decode_u64 decodes an u64 and returns the number of bytes used from the given leb128 encoded array `value`.
// A non-positive length means the input did not hold a complete value:
// 0 for a truncated or empty buffer, and a negative length for an
// overflowing one, like Go's encoding/binary.Uvarint.
pub fn decode_u64(value []u8) (u64, int) {
	mut result := u64(0)
	mut shift := 0
	for i, b in value {
		if i == 9 {
			// the tenth group can only fill bit 63
			if b & 0x7f > 1 || b & 0x80 != 0 {
				return 0, -(i + 1)
			}
		}
		result |= u64(b & 0x7f) << shift
		shift += 7
		if b & 0x80 == 0 {
			return result, i + 1
		}
	}
	return 0, 0
}

// decode_u32 decodes an u32 and returns the number of bytes used from the given leb128 encoded array `value`.
// A non-positive length means the input did not hold a complete value:
// 0 for a truncated or empty buffer, and a negative length for an
// overflowing one, like Go's encoding/binary.Uvarint.
pub fn decode_u32(value []u8) (u32, int) {
	mut result := u32(0)
	mut shift := 0
	for i, b in value {
		if i == 4 {
			// the fifth group can only fill bits 28..31
			if b & 0x70 != 0 || b & 0x80 != 0 {
				return 0, -(i + 1)
			}
		}
		result |= u32(b & 0x7f) << shift
		shift += 7
		if b & 0x80 == 0 {
			return result, i + 1
		}
	}
	return 0, 0
}
