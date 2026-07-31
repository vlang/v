module quic

// QUIC variable-length integer encoding (RFC 9000 §16).
//
// This is NOT the same encoding as vlib/encoding/leb128 — the two are
// bit-incompatible and must never be interchanged. LEB128 uses a
// continuation bit in every byte; QUIC's varint instead packs a 2-bit
// length class into the top bits of the FIRST byte:
//
//   0b00xxxxxx                                            -> 1 byte,  6-bit value
//   0b01xxxxxx xxxxxxxx                                   -> 2 bytes, 14-bit value
//   0b10xxxxxx xxxxxxxx xxxxxxxx xxxxxxxx                  -> 4 bytes, 30-bit value
//   0b11xxxxxx (+ 7 more bytes)                            -> 8 bytes, 62-bit value
//
// The maximum representable value is 2^62 - 1.
pub const max_varint = u64(0x3FFF_FFFF_FFFF_FFFF) // 2^62 - 1

// varint_len returns the number of bytes `encode_varint` would use to encode
// `value`, without actually encoding it.
pub fn varint_len(value u64) !int {
	if value > max_varint {
		return error('value ${value} exceeds the maximum QUIC varint (2^62-1)')
	}
	return match true {
		value <= 0x3F { 1 }
		value <= 0x3FFF { 2 }
		value <= 0x3FFF_FFFF { 4 }
		else { 8 }
	}
}

// encode_varint encodes `value` as a QUIC variable-length integer, using the
// smallest length class that fits (RFC 9000 §16 requires minimal encoding).
pub fn encode_varint(value u64) ![]u8 {
	n := varint_len(value)!
	mut out := []u8{len: n}
	match n {
		1 {
			out[0] = u8(value)
		}
		2 {
			out[0] = u8(0x40 | (value >> 8))
			out[1] = u8(value)
		}
		4 {
			out[0] = u8(0x80 | (value >> 24))
			out[1] = u8(value >> 16)
			out[2] = u8(value >> 8)
			out[3] = u8(value)
		}
		8 {
			out[0] = u8(0xC0 | (value >> 56))
			out[1] = u8(value >> 48)
			out[2] = u8(value >> 40)
			out[3] = u8(value >> 32)
			out[4] = u8(value >> 24)
			out[5] = u8(value >> 16)
			out[6] = u8(value >> 8)
			out[7] = u8(value)
		}
		else {
			return error('unreachable varint length ${n}')
		}
	}

	return out
}

// decode_varint decodes a QUIC variable-length integer from the start of
// `buf`, returning the decoded value and the number of bytes consumed.
// Rejects non-minimal encodings (RFC 9000 §16 requires the smallest possible
// encoding; accepting longer-than-necessary forms is a canonicalization
// hazard) and truncated input.
pub fn decode_varint(buf []u8) !(u64, int) {
	if buf.len == 0 {
		return error('cannot decode varint from empty buffer')
	}
	length_class := (buf[0] & 0xC0) >> 6
	n := match length_class {
		0 { 1 }
		1 { 2 }
		2 { 4 }
		else { 8 }
	}

	if buf.len < n {
		return error('truncated varint: need ${n} bytes, have ${buf.len}')
	}
	mut value := u64(buf[0] & 0x3F)
	for i in 1 .. n {
		value = (value << 8) | u64(buf[i])
	}
	// Reject non-minimal encodings: the decoded value must require exactly
	// the length class it was encoded with, not a shorter one.
	minimal_len := varint_len(value)!
	if minimal_len != n {
		return error('non-minimal varint encoding: value ${value} was encoded in ${n} bytes, minimal is ${minimal_len}')
	}
	return value, n
}
