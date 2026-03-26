// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

// Decoder handles QPACK decoding
pub struct Decoder {
mut:
	dynamic_table      DynamicTable
	max_blocked        u64
	known_insert_count int
}

// new_qpack_decoder creates a new QPACK decoder with the specified table capacity and blocked streams limit.
pub fn new_qpack_decoder(max_table_capacity int, max_blocked u64) Decoder {
	return Decoder{
		dynamic_table:      new_dynamic_table(max_table_capacity)
		max_blocked:        max_blocked
		known_insert_count: 0
	}
}

// acknowledge_insert updates the decoder's known insert count.
// Called when the decoder has processed encoder stream instructions.
pub fn (mut d Decoder) acknowledge_insert(count int) {
	d.known_insert_count += count
}

// decode_section_prefix decodes the QPACK section prefix.
// Returns (required_insert_count, base, bytes_consumed).
// Per RFC 9204 §4.5.1.
fn decode_section_prefix(data []u8, max_table_capacity int, total_inserts int) !(int, int, int) {
	if data.len < 2 {
		return error('QPACK data too short for prefix')
	}

	encoded_ric, ric_bytes := decode_prefixed_integer(data, 8)!

	mut ric := 0
	if encoded_ric > 0 {
		me := max_entries(max_table_capacity)
		if me == 0 {
			return error('invalid max entries for non-zero RIC')
		}
		full_range := 2 * me
		if encoded_ric > full_range {
			return error('encoded RIC ${encoded_ric} exceeds full range ${full_range}')
		}
		max_value := total_inserts + me
		max_wrapped := (max_value / full_range) * full_range
		ric = max_wrapped + encoded_ric - 1
		if ric > max_value {
			if ric <= full_range {
				return error('invalid RIC after unwrapping')
			}
			ric -= full_range
		}
		if ric == 0 {
			return error('decoded RIC must not be zero when encoded is non-zero')
		}
	}

	if ric_bytes >= data.len {
		return error('QPACK data too short for delta base')
	}

	sign := (data[ric_bytes] & 0x80) != 0
	delta_base, db_bytes := decode_prefixed_integer(data[ric_bytes..], 7)!

	mut base := 0
	if ric > 0 {
		base = if sign { ric - delta_base - 1 } else { ric + delta_base }
	}

	return ric, base, ric_bytes + db_bytes
}

// decode decodes QPACK-encoded headers into header fields.
// Handles static/dynamic indexed (relative and post-base) and literal forms.
pub fn (mut d Decoder) decode(data []u8) ![]HeaderField {
	if data.len < 2 {
		return error('QPACK data too short')
	}

	ric, base, prefix_bytes := decode_section_prefix(data, d.dynamic_table.max_size, int(d.dynamic_table.insert_count))!

	// Blocked stream check: decoder needs at least RIC inserts
	if ric > 0 && ric > d.known_insert_count && ric > int(d.dynamic_table.insert_count) {
		return error('blocked: need insert count ${ric}, have ${d.known_insert_count}')
	}

	mut headers := []HeaderField{}
	mut idx := prefix_bytes

	for idx < data.len {
		header, bytes_read := d.decode_field_line(data, idx, base)!
		headers << header
		idx += bytes_read
	}

	return headers
}

// decode_field_line decodes a single QPACK field line at position i in data.
// Returns the decoded header field and the number of bytes consumed.
fn (mut d Decoder) decode_field_line(data []u8, i int, base int) !(HeaderField, int) {
	first_byte := data[i]

	if (first_byte & 0x80) != 0 {
		if (first_byte & 0x40) != 0 {
			// 11XXXXXX: Indexed Field Line — static table
			index, bytes_read := d.decode_indexed_field_line(data[i..])!
			return static_table[index], bytes_read
		}
		// 10XXXXXX: Indexed Field Line — dynamic table (relative)
		return d.decode_indexed_dynamic_relative(data[i..], base)
	} else if (first_byte & 0x40) != 0 {
		if (first_byte & 0x10) != 0 {
			// 01X1XXXX: Literal with Name Reference — static table
			return d.decode_literal_name_ref(data[i..])
		}
		// 01X0XXXX: Literal with Name Reference — dynamic table (relative)
		header, bytes_read := d.decode_literal_name_ref_dynamic_relative(data[i..], base)!
		d.dynamic_table.insert(header)
		return header, bytes_read
	} else if (first_byte & 0x20) != 0 {
		// 001XXXXX: Literal without Name Reference
		header, bytes_read := d.decode_literal_no_ref(data[i..])!
		d.dynamic_table.insert(header)
		return header, bytes_read
	} else if (first_byte & 0x10) != 0 {
		// 0001XXXX: Post-Base Indexed Field Line — dynamic table
		return d.decode_indexed_dynamic(data[i..], base)
	} else {
		// 0000XXXX: Post-Base Literal with Name Reference — dynamic table
		header, bytes_read := d.decode_literal_name_ref_dynamic(data[i..], base)!
		d.dynamic_table.insert(header)
		return header, bytes_read
	}
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

// decode_indexed_dynamic_relative decodes a dynamic indexed field line with relative
// index: 10XXXXXX (RFC 9204 §4.5.2, T=0). Computes absolute index as base - index - 1.
fn (d Decoder) decode_indexed_dynamic_relative(data []u8, base int) !(HeaderField, int) {
	index, bytes_read := decode_prefixed_integer(data, 6)!
	abs_idx := base - index - 1
	field := d.dynamic_table.get_by_absolute(abs_idx) or {
		return error('Dynamic table relative index out of range: ${index} (abs: ${abs_idx})')
	}
	return field, bytes_read
}

// decode_indexed_dynamic decodes a post-base indexed field line (0001XXXX).
fn (d Decoder) decode_indexed_dynamic(data []u8, base int) !(HeaderField, int) {
	index, bytes_read := decode_prefixed_integer(data, 4)!
	abs_idx := base + index
	field := d.dynamic_table.get_by_absolute(abs_idx) or {
		return error('Dynamic table post-base index out of range: ${index} (abs: ${abs_idx})')
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

// decode_literal_name_ref_dynamic_relative decodes a literal with dynamic name
// reference using relative index: 01X0XXXX (RFC 9204 §4.5.5, T=0).
fn (d Decoder) decode_literal_name_ref_dynamic_relative(data []u8, base int) !(HeaderField, int) {
	index, idx := decode_prefixed_integer(data, 4)!
	abs_idx := base - index - 1
	field_name := d.dynamic_table.get_by_absolute(abs_idx) or {
		return error('Dynamic table relative name index out of range: ${index} (abs: ${abs_idx})')
	}
	value, bytes_read := decode_qpack_string(data[idx..])!
	return HeaderField{
		name:  field_name.name
		value: value
	}, idx + bytes_read
}

// decode_literal_name_ref_dynamic decodes a post-base literal with dynamic name reference (0000XXXX).
fn (d Decoder) decode_literal_name_ref_dynamic(data []u8, base int) !(HeaderField, int) {
	index, idx := decode_prefixed_integer(data, 3)!
	abs_idx := base + index
	field_name := d.dynamic_table.get_by_absolute(abs_idx) or {
		return error('Dynamic table post-base name index out of range: ${index} (abs: ${abs_idx})')
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
