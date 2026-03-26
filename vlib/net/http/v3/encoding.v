module v3

// Variable-length integer and string encoding utilities (RFC 9000).

// max_varint is the maximum value encodable as a QUIC variable-length integer.
pub const max_varint = u64(0x3FFF_FFFF_FFFF_FFFF)

// encode_varint encodes a value using QUIC variable-length integer encoding.
pub fn encode_varint(value u64) ![]u8 {
	if value > max_varint {
		return error('varint value ${value} exceeds maximum 62-bit value (max_varint)')
	}
	if value < 64 {
		return [u8(value)]
	} else if value < 16384 {
		return [u8((value >> 8) | 0x40), u8(value)]
	} else if value < 1073741824 {
		return [u8((value >> 24) | 0x80), u8(value >> 16), u8(value >> 8), u8(value)]
	} else {
		return [u8((value >> 56) | 0xc0), u8(value >> 48), u8(value >> 40), u8(value >> 32),
			u8(value >> 24), u8(value >> 16), u8(value >> 8), u8(value)]
	}
}

// decode_varint decodes a QUIC variable-length integer, returning the value and bytes read.
pub fn decode_varint(data []u8) !(u64, int) {
	if data.len == 0 {
		return error('empty data for varint decoding')
	}

	first := data[0]
	prefix := first >> 6

	match prefix {
		0 {
			return u64(first & 0x3f), 1
		}
		1 {
			if data.len < 2 {
				return error('incomplete 2-byte varint')
			}
			value := (u64(first & 0x3f) << 8) | u64(data[1])
			return value, 2
		}
		2 {
			if data.len < 4 {
				return error('incomplete 4-byte varint')
			}
			value := (u64(first & 0x3f) << 24) | (u64(data[1]) << 16) | (u64(data[2]) << 8) | u64(data[3])
			return value, 4
		}
		3 {
			if data.len < 8 {
				return error('incomplete 8-byte varint')
			}
			value := (u64(first & 0x3f) << 56) | (u64(data[1]) << 48) | (u64(data[2]) << 40) | (u64(data[3]) << 32) | (u64(data[4]) << 24) | (u64(data[5]) << 16) | (u64(data[6]) << 8) | u64(data[7])
			return value, 8
		}
		else {
			return error('invalid varint prefix: ${prefix}')
		}
	}
}

// encode_string encodes a string with a varint length prefix.
pub fn encode_string(s string) ![]u8 {
	bytes := s.bytes()
	mut result := []u8{cap: 8 + bytes.len}
	result << encode_varint(u64(bytes.len))!
	result << bytes
	return result
}

// decode_string decodes a varint length-prefixed string, returning the string and bytes read.
pub fn decode_string(data []u8) !(string, int) {
	length, bytes_read := decode_varint(data)!

	total_bytes := bytes_read + int(length)
	if data.len < total_bytes {
		return error('incomplete string: expected ${total_bytes} bytes, got ${data.len}')
	}

	str_data := data[bytes_read..total_bytes]
	return str_data.bytestr(), total_bytes
}
