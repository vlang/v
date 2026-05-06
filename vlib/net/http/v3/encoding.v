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

// build_settings_payload encodes HTTP/3 settings into a SETTINGS frame payload
// containing QPACK_MAX_TABLE_CAPACITY (0x01), MAX_FIELD_SECTION_SIZE (0x06),
// and QPACK_BLOCKED_STREAMS (0x07) per RFC 9114 §7.2.4.
pub fn build_settings_payload(s Settings) ![]u8 {
	mut payload := []u8{cap: 30}
	payload << encode_varint(u64(0x01))!
	payload << encode_varint(s.qpack_max_table_capacity)!
	payload << encode_varint(u64(0x06))!
	payload << encode_varint(s.max_field_section_size)!
	payload << encode_varint(u64(0x07))!
	payload << encode_varint(s.qpack_blocked_streams)!
	return payload
}

// parse_settings_payload decodes a SETTINGS frame payload into parallel arrays
// of setting IDs and values. Used for testing and validation.
pub fn parse_settings_payload(payload []u8) !([]u64, []u64) {
	mut ids := []u64{cap: 8}
	mut values := []u64{cap: 8}
	mut idx := 0
	for idx < payload.len {
		id, br1 := decode_varint(payload[idx..])!
		idx += br1
		val, br2 := decode_varint(payload[idx..])!
		idx += br2
		ids << id
		values << val
	}
	return ids, values
}

// build_goaway_frame builds an encoded GOAWAY frame with the given stream ID.
pub fn build_goaway_frame(stream_id u64) ![]u8 {
	payload := encode_varint(stream_id)!
	mut data := []u8{cap: 20}
	data << encode_varint(u64(FrameType.goaway))!
	data << encode_varint(u64(payload.len))!
	data << payload
	return data
}

// extract_goaway_stream_id extracts the stream ID from an encoded GOAWAY frame.
pub fn extract_goaway_stream_id(data []u8) !u64 {
	_, br1 := decode_varint(data)! // frame type
	_, br2 := decode_varint(data[br1..])! // length
	stream_id, _ := decode_varint(data[br1 + br2..])!
	return stream_id
}
