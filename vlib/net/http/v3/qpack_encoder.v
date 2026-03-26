module v3

// QPACK header compression encoder (RFC 9204).

// Encoder handles QPACK encoding.
pub struct Encoder {
mut:
	dynamic_table   DynamicTable
	max_blocked     u64
	blocked_streams int
}

// new_qpack_encoder creates a new QPACK encoder for HTTP/3 header compression.
pub fn new_qpack_encoder(max_table_capacity int, max_blocked u64) Encoder {
	return Encoder{
		dynamic_table:   new_dynamic_table(max_table_capacity)
		max_blocked:     max_blocked
		blocked_streams: 0
	}
}

fn encode_section_prefix(ric int, base int, max_table_capacity int) []u8 {
	mut result := []u8{cap: 10}

	if ric == 0 {
		result << u8(0x00)
		result << u8(0x00)
		return result
	}

	me := max_entries(max_table_capacity)
	encoded_ric := if me > 0 { (ric % (2 * me)) + 1 } else { ric + 1 }
	result << encode_integer(encoded_ric, 8)

	if base >= ric {
		delta := base - ric
		result << encode_integer(delta, 7)
	} else {
		delta := ric - base - 1
		mut delta_bytes := encode_integer(delta, 7)
		delta_bytes[0] |= 0x80
		result << delta_bytes
	}

	return result
}

// acknowledge_stream signals that a previously blocked stream has been unblocked.
pub fn (mut e Encoder) acknowledge_stream() {
	if e.blocked_streams > 0 {
		e.blocked_streams--
	}
}

// encode encodes headers using QPACK compression for HTTP/3.
pub fn (mut e Encoder) encode(headers []HeaderField) []u8 {
	mut estimated_size := 10
	for header in headers {
		estimated_size += header.name.len + header.value.len + 10
	}
	mut body := []u8{cap: estimated_size}

	base := int(e.dynamic_table.insert_count)
	mut max_abs_ref := -1
	force_literal := e.blocked_streams >= int(e.max_blocked)

	for header in headers {
		exact_key := '${header.name}:${header.value}'
		if exact_key in qpack_static_exact_map {
			body << encode_indexed_static(qpack_static_exact_map[exact_key])
			continue
		}

		mut dyn_exact_abs := -1
		mut dyn_name_abs := -1
		if !force_literal {
			dyn_exact_abs, dyn_name_abs = e.find_dynamic_match(header)
		}

		if dyn_exact_abs >= 0 {
			if dyn_exact_abs > max_abs_ref {
				max_abs_ref = dyn_exact_abs
			}
			body << encode_dynamic_indexed(dyn_exact_abs, base)
			continue
		}

		if header.name in qpack_static_name_map {
			indices := qpack_static_name_map[header.name]
			if indices.len > 0 {
				body << e.encode_with_name_ref(header, indices[0], base, true)
				continue
			}
		}

		if dyn_name_abs >= 0 {
			if dyn_name_abs > max_abs_ref {
				max_abs_ref = dyn_name_abs
			}
			body << e.encode_with_name_ref(header, dyn_name_abs, base, false)
			continue
		}

		body << encode_literal_without_name_ref(header.name, header.value)
		e.dynamic_table.insert(header)
	}

	if max_abs_ref >= 0 {
		e.blocked_streams++
	}

	ric := if max_abs_ref >= 0 { max_abs_ref + 1 } else { 0 }
	prefix := encode_section_prefix(ric, base, e.dynamic_table.max_size)

	mut result := []u8{cap: prefix.len + body.len}
	result << prefix
	result << body
	return result
}

fn (e Encoder) find_dynamic_match(header HeaderField) (int, int) {
	mut dyn_exact_abs := -1
	mut dyn_name_abs := -1
	for i := 0; i < e.dynamic_table.count; i++ {
		entry := e.dynamic_table.get(i) or { break }
		abs_idx := int(e.dynamic_table.insert_count) - 1 - i
		if entry.name == header.name {
			if dyn_name_abs == -1 {
				dyn_name_abs = abs_idx
			}
			if entry.value == header.value {
				dyn_exact_abs = abs_idx
				break
			}
		}
	}
	return dyn_exact_abs, dyn_name_abs
}

fn encode_dynamic_indexed(abs_idx int, base int) []u8 {
	if abs_idx < base {
		return encode_indexed_dynamic_relative(base - abs_idx - 1)
	}
	return encode_indexed_dynamic(abs_idx - base)
}

fn (mut e Encoder) encode_with_name_ref(header HeaderField, name_idx int, base int, is_static bool) []u8 {
	if is_static {
		result := encode_literal_with_name_ref_static(name_idx, header.value)
		e.dynamic_table.insert(header)
		return result
	}
	mut result := []u8{}
	if name_idx < base {
		result = encode_literal_with_name_ref_dynamic_relative(base - name_idx - 1, header.value)
	} else {
		result = encode_literal_with_name_ref_dynamic(name_idx - base, header.value)
	}
	e.dynamic_table.insert(header)
	return result
}

@[inline]
fn encode_indexed_static(index int) []u8 {
	mut result := []u8{cap: 6}
	if index < 64 {
		result << u8(0xc0 | index)
	} else {
		mut suffix := encode_integer(index, 6)
		suffix[0] |= 0xc0
		result << suffix
	}
	return result
}

@[inline]
fn encode_indexed_dynamic(index int) []u8 {
	mut result := []u8{cap: 6}
	if index < 16 {
		result << u8(0x10 | index)
	} else {
		mut suffix := encode_integer(index, 4)
		suffix[0] |= 0x10
		result << suffix
	}
	return result
}

@[inline]
fn encode_indexed_dynamic_relative(relative_index int) []u8 {
	mut result := []u8{cap: 6}
	if relative_index < 64 {
		result << u8(0x80 | relative_index)
	} else {
		mut suffix := encode_integer(relative_index, 6)
		suffix[0] |= 0x80
		result << suffix
	}
	return result
}

fn encode_literal_with_name_ref_static(index int, value string) []u8 {
	mut result := []u8{cap: 10 + value.len}
	if index < 16 {
		result << u8(0x50 | index)
	} else {
		mut suffix := encode_integer(index, 4)
		suffix[0] |= 0x50
		result << suffix
	}
	result << encode_qpack_string(value)
	return result
}

fn encode_literal_with_name_ref_dynamic(index int, value string) []u8 {
	mut result := []u8{cap: 10 + value.len}
	if index < 8 {
		result << u8(index)
	} else {
		mut suffix := encode_integer(index, 3)
		result << suffix
	}
	result << encode_qpack_string(value)
	return result
}

fn encode_literal_with_name_ref_dynamic_relative(relative_index int, value string) []u8 {
	mut result := []u8{cap: 10 + value.len}
	if relative_index < 16 {
		result << u8(0x40 | relative_index)
	} else {
		mut suffix := encode_integer(relative_index, 4)
		suffix[0] |= 0x40
		result << suffix
	}
	result << encode_qpack_string(value)
	return result
}

fn encode_literal_without_name_ref(name string, value string) []u8 {
	mut result := []u8{cap: 15 + name.len + value.len}
	result << 0x20
	result << encode_qpack_string(name)
	result << encode_qpack_string(value)
	return result
}
