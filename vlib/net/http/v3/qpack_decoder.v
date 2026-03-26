module v3

// QPACK header compression decoder (RFC 9204).

// BlockedEntry represents a header block waiting for dynamic table updates.
struct BlockedEntry {
	stream_id u64
	data      []u8
	ric       u64
}

// DecodedBlock represents a successfully decoded blocked header block.
pub struct DecodedBlock {
pub:
	stream_id u64
	headers   []HeaderField
}

// Decoder handles QPACK decoding.
pub struct Decoder {
mut:
	dynamic_table      DynamicTable
	max_blocked        u64
	known_insert_count int
	ack_buf            []u8
	blocked_entries    []BlockedEntry
}

// new_qpack_decoder creates a new QPACK decoder with the specified capacity and blocked streams limit.
pub fn new_qpack_decoder(max_table_capacity int, max_blocked u64) Decoder {
	return Decoder{
		dynamic_table:      new_dynamic_table(max_table_capacity)
		max_blocked:        max_blocked
		known_insert_count: 0
	}
}

// acknowledge_insert updates the decoder's known insert count.
pub fn (mut d Decoder) acknowledge_insert(count int) {
	d.known_insert_count += count
}

// pending_acknowledgments returns any decoder stream instructions (section acknowledgments)
// generated during decoding and clears the internal buffer.
pub fn (mut d Decoder) pending_acknowledgments() []u8 {
	result := d.ack_buf.clone()
	d.ack_buf.clear()
	return result
}

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
// When the required insert count exceeds the known insert count,
// the block is queued and a "BLOCKED:" prefixed error is returned.
pub fn (mut d Decoder) decode(data []u8) ![]HeaderField {
	if data.len < 2 {
		return error('QPACK data too short')
	}

	ric, _, _ := decode_section_prefix(data, d.dynamic_table.max_size, int(d.dynamic_table.insert_count))!

	if ric > 0 && ric > d.known_insert_count && ric > int(d.dynamic_table.insert_count) {
		d.blocked_entries << BlockedEntry{
			stream_id: 0
			data:      data.clone()
			ric:       u64(ric)
		}
		return error('BLOCKED: stream blocked, need insert count ${ric}, have ${d.known_insert_count}')
	}

	return d.decode_field_section(data)
}

// decode_field_section decodes header field lines from a QPACK-encoded block.
fn (mut d Decoder) decode_field_section(data []u8) ![]HeaderField {
	if data.len < 2 {
		return error('QPACK data too short')
	}

	ric, base, prefix_bytes := decode_section_prefix(data, d.dynamic_table.max_size, int(d.dynamic_table.insert_count))!

	mut headers := []HeaderField{}
	mut idx := prefix_bytes

	for idx < data.len {
		header, bytes_read := d.decode_field_line(data, idx, base)!
		headers << header
		idx += bytes_read
	}

	if ric > 0 {
		ack := SectionAcknowledgment{
			stream_id: 0
		}
		d.ack_buf << ack.encode()
	}

	return headers
}

fn (mut d Decoder) decode_field_line(data []u8, i int, base int) !(HeaderField, int) {
	first_byte := data[i]

	if (first_byte & 0x80) != 0 {
		if (first_byte & 0x40) != 0 {
			index, bytes_read := d.decode_indexed_field_line(data[i..])!
			return static_table[index], bytes_read
		}
		return d.decode_indexed_dynamic_relative(data[i..], base)
	} else if (first_byte & 0x40) != 0 {
		if (first_byte & 0x10) != 0 {
			return d.decode_literal_name_ref(data[i..])
		}
		header, bytes_read := d.decode_literal_name_ref_dynamic_relative(data[i..], base)!
		d.dynamic_table.insert(header)
		return header, bytes_read
	} else if (first_byte & 0x20) != 0 {
		header, bytes_read := d.decode_literal_no_ref(data[i..])!
		d.dynamic_table.insert(header)
		return header, bytes_read
	} else if (first_byte & 0x10) != 0 {
		return d.decode_indexed_dynamic(data[i..], base)
	} else {
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

	index_val, len := decode_prefixed_integer(data, 6)!

	if index_val >= static_table.len {
		return error('Static table index out of range: ${index_val}')
	}

	return index_val, len
}

fn (d Decoder) decode_indexed_dynamic_relative(data []u8, base int) !(HeaderField, int) {
	index, bytes_read := decode_prefixed_integer(data, 6)!
	abs_idx := base - index - 1
	field := d.dynamic_table.get_by_absolute(abs_idx) or {
		return error('Dynamic table relative index out of range: ${index} (abs: ${abs_idx})')
	}
	return field, bytes_read
}

fn (d Decoder) decode_indexed_dynamic(data []u8, base int) !(HeaderField, int) {
	index, bytes_read := decode_prefixed_integer(data, 4)!
	abs_idx := base + index
	field := d.dynamic_table.get_by_absolute(abs_idx) or {
		return error('Dynamic table post-base index out of range: ${index} (abs: ${abs_idx})')
	}
	return field, bytes_read
}

fn (d Decoder) decode_literal_name_ref(data []u8) !(HeaderField, int) {
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

// blocked_count returns the number of blocked header blocks waiting for dynamic table updates.
pub fn (d Decoder) blocked_count() int {
	return d.blocked_entries.len
}

// process_blocked updates the known insert count and decodes any blocked entries
// whose required insert count is now satisfied.
pub fn (mut d Decoder) process_blocked(known_count u64) []DecodedBlock {
	d.known_insert_count = int(known_count)
	mut resolved := []DecodedBlock{}
	mut remaining := []BlockedEntry{}

	for entry in d.blocked_entries {
		if entry.ric <= known_count {
			headers := d.decode_field_section(entry.data) or {
				remaining << entry
				continue
			}
			resolved << DecodedBlock{
				stream_id: entry.stream_id
				headers:   headers
			}
		} else {
			remaining << entry
		}
	}

	d.blocked_entries = remaining
	return resolved
}
