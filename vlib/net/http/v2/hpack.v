module v2

// HPACK encoder and decoder for HTTP/2 (RFC 7541).

// Encoder encodes headers using HPACK.
pub struct Encoder {
mut:
	dynamic_table             DynamicTable
	pending_table_size_update int = -1 // -1 means no pending update (RFC 7541 §4.2)
}

// new_encoder creates a new HPACK encoder.
pub fn new_encoder() Encoder {
	return Encoder{
		dynamic_table: DynamicTable{}
	}
}

// set_max_table_size signals that the encoder should emit a dynamic table size update.
pub fn (mut e Encoder) set_max_table_size(size int) {
	e.pending_table_size_update = size
	e.dynamic_table.set_max_size(size)
}

// Decoder decodes headers using HPACK.
pub struct Decoder {
mut:
	dynamic_table        DynamicTable
	max_header_list_size u32 // 0 means unlimited (RFC 7540 §6.5.2)
}

// new_decoder creates a new HPACK decoder.
pub fn new_decoder() Decoder {
	return Decoder{
		dynamic_table: DynamicTable{}
	}
}

// new_decoder_with_limit creates a new HPACK decoder with a maximum header list size.
pub fn new_decoder_with_limit(max_header_list_size u32) Decoder {
	return Decoder{
		dynamic_table:        DynamicTable{}
		max_header_list_size: max_header_list_size
	}
}

// encode encodes a list of header fields.
pub fn (mut e Encoder) encode(headers []HeaderField) []u8 {
	mut estimated_size := 0
	for header in headers {
		estimated_size += header.name.len + header.value.len + 10
	}
	mut result := []u8{cap: estimated_size}

	if e.pending_table_size_update >= 0 {
		emit_table_size_update(e.pending_table_size_update, mut result)
		e.pending_table_size_update = -1
	}

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

fn emit_table_size_update(size int, mut result []u8) {
	encoded := encode_hpack_integer(size, 5)
	result << (encoded[0] | 0x20)
	if encoded.len > 1 {
		result << encoded[1..]
	}
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
	mut total_size := u32(0)

	for idx < data.len {
		first_byte := data[idx]

		if (first_byte & 0x80) != 0 {
			// §6.1: Indexed header field
			index, bytes_read := decode_integer(data[idx..], 7)!
			idx += bytes_read
			field := get_indexed(&d.dynamic_table, index) or {
				return error('invalid index: ${index}')
			}
			total_size = check_header_list_size(total_size, field, d.max_header_list_size)!
			headers << field
		} else if (first_byte & 0x40) != 0 {
			// §6.2.1: Literal with incremental indexing
			field, consumed := decode_literal_field(&d.dynamic_table, data[idx..], 6)!
			idx += consumed
			total_size = check_header_list_size(total_size, field, d.max_header_list_size)!
			headers << field
			d.dynamic_table.add(field)
		} else if (first_byte & 0x20) != 0 {
			// §6.3: Dynamic table size update
			size, bytes_read := decode_integer(data[idx..], 5)!
			idx += bytes_read
			d.dynamic_table.set_max_size(size)
		} else {
			// §6.2.2/§6.2.3: Literal without indexing or never-indexed
			field, consumed := decode_literal_field(&d.dynamic_table, data[idx..], 4)!
			idx += consumed
			total_size = check_header_list_size(total_size, field, d.max_header_list_size)!
			headers << field
		}
	}

	return headers
}

fn check_header_list_size(current u32, field HeaderField, max_size u32) !u32 {
	total := current + u32(field.size())
	if max_size > 0 && total > max_size {
		return error('header list size exceeds max_header_list_size limit')
	}
	return total
}
