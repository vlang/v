// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

// Decoder handles QPACK decoding
pub struct Decoder {
mut:
	dynamic_table DynamicTable
	max_blocked   u64
}

// new_qpack_decoder creates a new QPACK decoder with the specified table capacity and blocked streams limit.
pub fn new_qpack_decoder(max_table_capacity int, max_blocked u64) Decoder {
	return Decoder{
		dynamic_table: new_dynamic_table(max_table_capacity)
		max_blocked:   max_blocked
	}
}

// decode decodes QPACK-encoded headers into header fields.
// Handles static table indexed, dynamic table indexed (post-base), and literal
// forms both with and without name references.
// TODO: parse Required Insert Count and Delta Base for blocked-stream support
// (RFC 9204 Section 3.2.5 / 4.4).
pub fn (mut d Decoder) decode(data []u8) ![]HeaderField {
	if data.len < 2 {
		return error('QPACK data too short')
	}

	mut headers := []HeaderField{}
	mut idx := 0

	// Skip Required Insert Count and Delta Base (simplified; always 0 for now)
	idx += 2

	// Decode header fields
	for idx < data.len {
		first_byte := data[idx]

		if (first_byte & 0xc0) == 0xc0 {
			// Indexed Field Line — static table (11XXXXXX)
			index, bytes_read := d.decode_indexed_field_line(data[idx..])!
			headers << static_table[index]
			idx += bytes_read
		} else if (first_byte & 0xf0) == 0x10 {
			// Post-base Indexed Field Line — dynamic table (0001XXXX)
			field, bytes_read := d.decode_indexed_dynamic(data[idx..])!
			headers << field
			idx += bytes_read
		} else if (first_byte & 0xf0) == 0x50 {
			// Literal with Name Reference — static table (0101XXXX)
			header, bytes_read := d.decode_literal_name_ref(data[idx..])!
			headers << header
			idx += bytes_read
		} else if (first_byte & 0xe0) == 0x20 {
			// Literal without Name Reference (001XXXXX)
			header, bytes_read := d.decode_literal_no_ref(data[idx..])!
			d.dynamic_table.insert(header)
			headers << header
			idx += bytes_read
		} else if (first_byte & 0xf8) == 0x00 {
			// Post-base Literal with Name Reference — dynamic table (00000XXX)
			header, bytes_read := d.decode_literal_name_ref_dynamic(data[idx..])!
			d.dynamic_table.insert(header)
			headers << header
			idx += bytes_read
		} else {
			return error('Unknown QPACK instruction: 0x${first_byte:02x}')
		}
	}

	return headers
}

fn (d Decoder) decode_indexed_field_line(data []u8) !(int, int) {
	first_byte := data[0]
	index_prefix := int(first_byte & 0x3f)

	if index_prefix < 63 {
		if index_prefix >= static_table.len {
			return error('Static table index out of range: ${index_prefix}')
		}
		return index_prefix, 1
	}

	// Multi-byte index
	index_val, len := decode_prefixed_integer(data, 6)!

	// static table check
	if index_val >= static_table.len {
		return error('Static table index out of range: ${index_val}')
	}

	return index_val, len
}

// decode_indexed_dynamic decodes a post-base indexed field line (0001XXXX).
fn (d Decoder) decode_indexed_dynamic(data []u8) !(HeaderField, int) {
	index, bytes_read := decode_prefixed_integer(data, 4)!
	field := d.dynamic_table.get(index) or {
		return error('Dynamic table post-base index out of range: ${index}')
	}
	return field, bytes_read
}

fn (d Decoder) decode_literal_name_ref(data []u8) !(HeaderField, int) {
	// Use decode_prefixed_integer to parse the 4-bit name index
	index, idx := decode_prefixed_integer(data, 4)!

	if index >= static_table.len {
		return error('Static table index out of range: ${index}')
	}

	value, bytes_read := decode_qpack_string(data[idx..])!

	return HeaderField{
		name:  static_table[index].name
		value: value
	}, idx + bytes_read
}

// decode_literal_name_ref_dynamic decodes a post-base literal with dynamic name reference (00000XXX).
fn (d Decoder) decode_literal_name_ref_dynamic(data []u8) !(HeaderField, int) {
	index, idx := decode_prefixed_integer(data, 3)!
	field_name := d.dynamic_table.get(index) or {
		return error('Dynamic table post-base name index out of range: ${index}')
	}
	value, bytes_read := decode_qpack_string(data[idx..])!
	return HeaderField{
		name:  field_name.name
		value: value
	}, idx + bytes_read
}

fn (d Decoder) decode_literal_no_ref(data []u8) !(HeaderField, int) {
	mut idx := 1

	name, name_bytes := decode_qpack_string(data[idx..])!
	idx += name_bytes

	value, value_bytes := decode_qpack_string(data[idx..])!
	idx += value_bytes

	return HeaderField{
		name:  name
		value: value
	}, idx
}
