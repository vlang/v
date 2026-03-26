// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

// Encoder handles QPACK encoding
pub struct Encoder {
mut:
	dynamic_table   DynamicTable
	max_blocked     u64
	blocked_streams int
}

// new_qpack_encoder creates a new QPACK encoder for HTTP/3 header compression
pub fn new_qpack_encoder(max_table_capacity int, max_blocked u64) Encoder {
	return Encoder{
		dynamic_table:   new_dynamic_table(max_table_capacity)
		max_blocked:     max_blocked
		blocked_streams: 0
	}
}

// encode_section_prefix encodes the QPACK section prefix (RIC + Delta Base).
// Per RFC 9204 §4.5.1.
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
// Called when the encoder receives a Section Acknowledgment from the decoder.
pub fn (mut e Encoder) acknowledge_stream() {
	if e.blocked_streams > 0 {
		e.blocked_streams--
	}
}

// encode encodes headers using QPACK compression for HTTP/3.
// Checks the static table first (O(1) via hashmap), then the dynamic table.
// Entries not found in either table are encoded as literals and added to the
// dynamic table for future reuse. Falls back to literal encoding when the
// blocked streams limit (QPACK_MAX_BLOCKED_STREAMS) would be exceeded.
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
		// 1. Try exact match in static table (O(1))
		exact_key := '${header.name}:${header.value}'
		if exact_key in qpack_static_exact_map {
			body << encode_indexed_static(qpack_static_exact_map[exact_key])
			continue
		}

		// 2. Try dynamic table match (skipped when blocked limit reached)
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

		// 3. Name-only match in static table
		if header.name in qpack_static_name_map {
			indices := qpack_static_name_map[header.name]
			if indices.len > 0 {
				body << e.encode_with_name_ref(header, indices[0], base, true)
				continue
			}
		}

		// 4. Name-only match in dynamic table
		if dyn_name_abs >= 0 {
			if dyn_name_abs > max_abs_ref {
				max_abs_ref = dyn_name_abs
			}
			body << e.encode_with_name_ref(header, dyn_name_abs, base, false)
			continue
		}

		// 5. Fully literal — no match anywhere
		body << encode_literal_without_name_ref(header.name, header.value)
		e.dynamic_table.insert(header)
	}

	// Track potentially blocked stream
	if max_abs_ref >= 0 {
		e.blocked_streams++
	}

	// Compute RIC and build prefix
	ric := if max_abs_ref >= 0 { max_abs_ref + 1 } else { 0 }
	prefix := encode_section_prefix(ric, base, e.dynamic_table.max_size)

	mut result := []u8{cap: prefix.len + body.len}
	result << prefix
	result << body
	return result
}

// find_dynamic_match searches the dynamic table for exact and name-only matches.
// Returns (exact_absolute_index, name_absolute_index), each -1 when not found.
fn (e Encoder) find_dynamic_match(header HeaderField) (int, int) {
	mut dyn_exact_abs := -1
	mut dyn_name_abs := -1
	for i := 0; i < e.dynamic_table.entries.len; i++ {
		entry := e.dynamic_table.entries[e.dynamic_table.entries.len - 1 - i]
		abs_idx := int(e.dynamic_table.insert_count) - 1 - i
		if entry.field.name == header.name {
			if dyn_name_abs == -1 {
				dyn_name_abs = abs_idx
			}
			if entry.field.value == header.value {
				dyn_exact_abs = abs_idx
				break
			}
		}
	}
	return dyn_exact_abs, dyn_name_abs
}

// encode_dynamic_indexed encodes a dynamic table entry using relative or
// post-base index format depending on its position relative to the base.
fn encode_dynamic_indexed(abs_idx int, base int) []u8 {
	if abs_idx < base {
		return encode_indexed_dynamic_relative(base - abs_idx - 1)
	}
	return encode_indexed_dynamic(abs_idx - base)
}

// encode_with_name_ref encodes a literal field line with a name reference from
// either the static or dynamic table, and inserts the entry into the dynamic table.
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

// encode_indexed_static encodes an indexed field line from static table
@[inline]
fn encode_indexed_static(index int) []u8 {
	mut result := []u8{cap: 6} // Pre-allocate for worst case
	// Static Indexed: 11XXXXXX pattern
	if index < 64 {
		result << u8(0xc0 | index)
	} else {
		mut suffix := encode_integer(index, 6)
		suffix[0] |= 0xc0
		result << suffix
	}
	return result
}

// encode_indexed_dynamic encodes an indexed field line referencing the dynamic table.
// Uses the post-base index format: 0001XXXX (RFC 9204 Section 4.5.4).
@[inline]
fn encode_indexed_dynamic(index int) []u8 {
	mut result := []u8{cap: 6}
	// Post-base indexed: 0001XXXX
	if index < 16 {
		result << u8(0x10 | index)
	} else {
		mut suffix := encode_integer(index, 4)
		suffix[0] |= 0x10
		result << suffix
	}
	return result
}

// encode_indexed_dynamic_relative encodes an indexed field line referencing
// the dynamic table using relative index format: 10XXXXXX (RFC 9204 §4.5.2, T=0).
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

// encode_literal_with_name_ref_static encodes a literal with name reference from static table
fn encode_literal_with_name_ref_static(index int, value string) []u8 {
	// Pre-allocate with estimated size
	mut result := []u8{cap: 10 + value.len}
	// Literal with Name Reference: 01NTXXXX pattern (N=name ref type, T=0 for static)
	if index < 16 {
		result << u8(0x50 | index) // 0101XXXX
	} else {
		mut suffix := encode_integer(index, 4)
		suffix[0] |= 0x50
		result << suffix
	}
	// Encode value length and value
	result << encode_qpack_string(value)
	return result
}

// encode_literal_with_name_ref_dynamic encodes a literal with a dynamic table name reference.
// Uses post-base name reference format per RFC 9204 Section 4.5.7: 0000XXXX.
// The high 4 bits are all zero — this is intentional per the RFC wire format,
// not a bug. No prefix bits are set; the index occupies the low 3 bits directly.
fn encode_literal_with_name_ref_dynamic(index int, value string) []u8 {
	mut result := []u8{cap: 10 + value.len}
	// Post-base literal name ref: 0000XXXX (RFC 9204 §4.5.7)
	// High nibble is 0x00 per spec — 0x00 | index equals index, which is correct.
	if index < 8 {
		result << u8(index) // 0000 0XXX — high 4 bits intentionally zero per RFC 9204 §4.5.7
	} else {
		mut suffix := encode_integer(index, 3)
		// suffix[0] high bits remain 0x00 per RFC 9204 §4.5.7 wire format
		result << suffix
	}
	result << encode_qpack_string(value)
	return result
}

// encode_literal_with_name_ref_dynamic_relative encodes a literal with dynamic
// table name reference using relative index: 0100XXXX (RFC 9204 §4.5.5, N=0, T=0).
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

// encode_literal_without_name_ref encodes a literal without name reference
fn encode_literal_without_name_ref(name string, value string) []u8 {
	// Pre-allocate with estimated size
	mut result := []u8{cap: 15 + name.len + value.len}
	// Literal without Name Reference: 001NXXXX pattern
	result << 0x20 // 00100000
	// Encode name
	result << encode_qpack_string(name)
	// Encode value
	result << encode_qpack_string(value)
	return result
}
