module v2

// HPACK header compression for HTTP/2 (RFC 7541).

// HeaderField represents a name-value pair.
pub struct HeaderField {
pub mut:
	name  string
	value string
}

// size returns the size of the header field in bytes (RFC 7541 Section 4.1).
pub fn (h HeaderField) size() int {
	return 32 + h.name.len + h.value.len
}

// DynamicTable represents the HPACK dynamic table.
// Uses LIFO ordering per RFC 7541 §2.3.3: newest entry at index 0.
pub struct DynamicTable {
mut:
	entries  []HeaderField
	size     int
	max_size int = 4096
}

// add adds an entry to the dynamic table, evicting oldest entries as needed (RFC 7541 §4.4).
pub fn (mut dt DynamicTable) add(field HeaderField) {
	entry_size := field.size()

	if entry_size > dt.max_size {
		dt.entries = []HeaderField{}
		dt.size = 0
		return
	}

	for dt.size + entry_size > dt.max_size && dt.entries.len > 0 {
		removed := dt.entries.pop()
		dt.size -= removed.size()
	}

	// insert(0) is O(n), but max_size keeps the array small (~128 entries max)
	dt.entries.insert(0, field)
	dt.size += entry_size
}

// get retrieves an entry from the dynamic table (1-indexed).
pub fn (dt DynamicTable) get(index int) ?HeaderField {
	if index < 1 || index > dt.entries.len {
		return none
	}
	return dt.entries[index - 1]
}

// set_max_size updates the maximum size of the dynamic table.
pub fn (mut dt DynamicTable) set_max_size(size int) {
	dt.max_size = size

	for dt.size > dt.max_size && dt.entries.len > 0 {
		removed := dt.entries.pop()
		dt.size -= removed.size()
	}
}

// Encoder encodes headers using HPACK.
pub struct Encoder {
mut:
	dynamic_table DynamicTable
}

// new_encoder creates a new HPACK encoder.
pub fn new_encoder() Encoder {
	return Encoder{
		dynamic_table: DynamicTable{}
	}
}

// Decoder decodes headers using HPACK.
pub struct Decoder {
mut:
	dynamic_table DynamicTable
}

// new_decoder creates a new HPACK decoder.
pub fn new_decoder() Decoder {
	return Decoder{
		dynamic_table: DynamicTable{}
	}
}

fn get_indexed(dynamic_table &DynamicTable, index int) ?HeaderField {
	if index == 0 {
		return none
	}

	if index < static_table.len {
		return static_table[index]
	}

	dynamic_index := index - static_table.len + 1
	return dynamic_table.get(dynamic_index)
}

fn encode_hpack_integer(value int, prefix_bits int) []u8 {
	mut result := []u8{cap: 5}
	max_prefix := (1 << prefix_bits) - 1

	if value < max_prefix {
		result << u8(value)
	} else {
		result << u8(max_prefix)
		mut remaining := value - max_prefix

		for remaining >= 128 {
			result << u8((remaining % 128) + 128)
			remaining = remaining / 128
		}
		result << u8(remaining)
	}

	return result
}

fn decode_integer(data []u8, prefix_bits int) !(int, int) {
	if data.len == 0 {
		return error('empty data')
	}

	max_prefix := (1 << prefix_bits) - 1
	mask := u8(max_prefix)

	value := int(data[0] & mask)

	if value < max_prefix {
		return value, 1
	}

	mut result := value
	mut m := 0
	mut idx := 1

	for idx < data.len {
		b := data[idx]
		result += int(b & 0x7f) << m
		m += 7
		idx++

		if (b & 0x80) == 0 {
			return result, idx
		}

		if m >= 32 {
			return error('integer overflow')
		}
	}

	return error('incomplete integer')
}

fn encode_string(s string, huffman bool) []u8 {
	if huffman {
		huffman_encoded := encode_huffman(s.bytes())
		encoded_len := encode_hpack_integer(huffman_encoded.len, 7)
		mut result := []u8{cap: encoded_len.len + huffman_encoded.len}
		result << (encoded_len[0] | 0x80)
		if encoded_len.len > 1 {
			result << encoded_len[1..]
		}
		result << huffman_encoded
		return result
	} else {
		encoded := encode_hpack_integer(s.len, 7)
		mut result := []u8{cap: encoded.len + s.len}
		result << encoded
		result << s.bytes()
		return result
	}
}

fn decode_string(data []u8) !(string, int) {
	if data.len == 0 {
		return error('empty data')
	}

	huffman := (data[0] & 0x80) != 0
	length, bytes_read := decode_integer(data, 7)!

	if data.len < bytes_read + length {
		return error('incomplete string')
	}

	str_data := data[bytes_read..bytes_read + length]

	if huffman {
		decoded := decode_huffman(str_data)!
		return decoded.bytestr(), bytes_read + length
	}

	return str_data.bytestr(), bytes_read + length
}

// encode encodes a list of header fields.
pub fn (mut e Encoder) encode(headers []HeaderField) []u8 {
	mut estimated_size := 0
	for header in headers {
		estimated_size += header.name.len + header.value.len + 10
	}
	mut result := []u8{cap: estimated_size}

	for header in headers {
		mut found_index := 0
		mut found_name_index := 0
		exact_key := '${header.name}:${header.value}'
		if exact_key in static_table_exact_map {
			found_index = static_table_exact_map[exact_key]
		}
		if found_index == 0 && header.name in static_table_name_map {
			indices := static_table_name_map[header.name]
			if indices.len > 0 {
				found_name_index = indices[0]
			}
		}
		if found_index == 0 {
			for i := 0; i < e.dynamic_table.entries.len; i++ {
				entry := e.dynamic_table.entries[i]
				if entry.name == header.name {
					if entry.value == header.value {
						found_index = static_table.len + i
						break
					} else if found_name_index == 0 {
						found_name_index = static_table.len + i
					}
				}
			}
		}
		if found_index > 0 {
			encode_indexed_field(found_index, mut result)
		} else if found_name_index > 0 {
			encode_literal_indexed_name(found_name_index, header, mut result)
			e.dynamic_table.add(header)
		} else {
			encode_literal_new_name(header, mut result)
			e.dynamic_table.add(header)
		}
	}

	return result
}

fn encode_indexed_field(idx int, mut result []u8) {
	encoded := encode_hpack_integer(idx, 7)
	result << (encoded[0] | 0x80)
	if encoded.len > 1 {
		result << encoded[1..]
	}
}

fn encode_literal_indexed_name(name_idx int, field HeaderField, mut result []u8) {
	encoded := encode_hpack_integer(name_idx, 6)
	result << (encoded[0] | 0x40)
	if encoded.len > 1 {
		result << encoded[1..]
	}
	result << encode_string(field.value, true)
}

fn encode_literal_new_name(field HeaderField, mut result []u8) {
	result << u8(0x40)
	result << encode_string(field.name, true)
	result << encode_string(field.value, true)
}

fn decode_literal_field(dynamic_table &DynamicTable, data []u8, prefix_bits int) !(HeaderField, int) {
	mut idx := 0
	index, bytes_read := decode_integer(data, prefix_bits)!
	idx += bytes_read

	mut name := ''
	if index == 0 {
		mut name_bytes_read := 0
		name, name_bytes_read = decode_string(data[idx..])!
		idx += name_bytes_read
	} else {
		field := get_indexed(dynamic_table, index) or { return error('invalid index: ${index}') }
		name = field.name
	}

	value, bytes_read2 := decode_string(data[idx..])!
	idx += bytes_read2

	return HeaderField{name, value}, idx
}

// decode decodes a header block.
pub fn (mut d Decoder) decode(data []u8) ![]HeaderField {
	mut headers := []HeaderField{}
	mut idx := 0

	for idx < data.len {
		first_byte := data[idx]

		if (first_byte & 0x80) != 0 {
			index, bytes_read := decode_integer(data[idx..], 7)!
			idx += bytes_read

			field := get_indexed(&d.dynamic_table, index) or {
				return error('invalid index: ${index}')
			}
			headers << field
		} else if (first_byte & 0x40) != 0 {
			field, consumed := decode_literal_field(&d.dynamic_table, data[idx..], 6)!
			idx += consumed
			headers << field
			d.dynamic_table.add(field)
		} else if (first_byte & 0x20) != 0 {
			size, bytes_read := decode_integer(data[idx..], 5)!
			idx += bytes_read
			d.dynamic_table.set_max_size(size)
		} else if (first_byte & 0xf0) == 0x10 {
			field, consumed := decode_literal_field(&d.dynamic_table, data[idx..], 4)!
			idx += consumed
			headers << field
		} else {
			field, consumed := decode_literal_field(&d.dynamic_table, data[idx..], 4)!
			idx += consumed
			headers << field
		}
	}

	return headers
}
