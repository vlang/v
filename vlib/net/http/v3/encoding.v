// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

// Common encoding/decoding utilities for HTTP/3 (RFC 9000)

// encode_varint encodes a variable-length integer using QUIC varint encoding
// Supports values up to 2^62-1 with 1, 2, 4, or 8 byte encodings
pub fn encode_varint(value u64) []u8 {
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

// decode_varint decodes a QUIC variable-length integer
// Returns the decoded value and the number of bytes read
pub fn decode_varint(data []u8) !(u64, int) {
	if data.len == 0 {
		return error('empty data for varint decoding')
	}

	first := data[0]
	prefix := first >> 6

	match prefix {
		0 {
			// 1-byte encoding (6-bit value)
			return u64(first & 0x3f), 1
		}
		1 {
			// 2-byte encoding (14-bit value)
			if data.len < 2 {
				return error('incomplete 2-byte varint')
			}
			value := (u64(first & 0x3f) << 8) | u64(data[1])
			return value, 2
		}
		2 {
			// 4-byte encoding (30-bit value)
			if data.len < 4 {
				return error('incomplete 4-byte varint')
			}
			value := (u64(first & 0x3f) << 24) | (u64(data[1]) << 16) | (u64(data[2]) << 8) | u64(data[3])
			return value, 4
		}
		3 {
			// 8-byte encoding (62-bit value)
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

// encode_string encodes a string with varint length prefix
pub fn encode_string(s string) []u8 {
	bytes := s.bytes()
	mut result := []u8{cap: 8 + bytes.len}
	result << encode_varint(u64(bytes.len))
	result << bytes
	return result
}

// decode_string decodes a varint length-prefixed string
// Returns the decoded string and the number of bytes read
pub fn decode_string(data []u8) !(string, int) {
	length, bytes_read := decode_varint(data)!

	total_bytes := bytes_read + int(length)
	if data.len < total_bytes {
		return error('incomplete string: expected ${total_bytes} bytes, got ${data.len}')
	}

	str_data := data[bytes_read..total_bytes]
	return str_data.bytestr(), total_bytes
}
