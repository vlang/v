// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

// Encoder handles QPACK encoding
pub struct Encoder {
mut:
	dynamic_table DynamicTable
	max_blocked   u64
}

// new_qpack_encoder creates a new QPACK encoder for HTTP/3 header compression
pub fn new_qpack_encoder(max_table_capacity int, max_blocked u64) Encoder {
	return Encoder{
		dynamic_table: new_dynamic_table(max_table_capacity)
		max_blocked:   max_blocked
	}
}

// encode encodes headers using QPACK compression for HTTP/3.
// Checks the static table first (O(1) via hashmap), then the dynamic table.
// Entries not found in either table are encoded as literals and added to the
// dynamic table for future reuse.
// TODO: Emit encoder stream instructions (RFC 9204 Section 4.3) to synchronise
// the dynamic table with the peer decoder.
pub fn (mut e Encoder) encode(headers []HeaderField) []u8 {
	// Pre-allocate result with estimated size
	mut estimated_size := 2 // Required Insert Count + Delta Base
	for header in headers {
		estimated_size += header.name.len + header.value.len + 10
	}
	mut result := []u8{cap: estimated_size}

	// Placeholder bytes for Required Insert Count and Delta Base; filled in below.
	result << u8(0)
	result << u8(0)

	for header in headers {
		// 1. Try exact match in static table (O(1))
		exact_key := '${header.name}:${header.value}'
		if exact_key in qpack_static_exact_map {
			result << encode_indexed_static(qpack_static_exact_map[exact_key])
			continue
		}

		// 2. Try exact match in dynamic table
		mut dyn_exact_idx := -1
		mut dyn_name_idx := -1
		for i := 0; i < e.dynamic_table.entries.len; i++ {
			entry := e.dynamic_table.entries[e.dynamic_table.entries.len - 1 - i]
			if entry.field.name == header.name {
				if dyn_name_idx == -1 {
					dyn_name_idx = i
				}
				if entry.field.value == header.value {
					dyn_exact_idx = i
					break
				}
			}
		}

		if dyn_exact_idx >= 0 {
			// Indexed Field Line referencing dynamic table (post-base index)
			result << encode_indexed_dynamic(dyn_exact_idx)
			continue
		}

		// 3. Name-only match in static table
		if header.name in qpack_static_name_map {
			indices := qpack_static_name_map[header.name]
			if indices.len > 0 {
				result << encode_literal_with_name_ref_static(indices[0], header.value)
				e.dynamic_table.insert(header)
				continue
			}
		}

		// 4. Name-only match in dynamic table
		if dyn_name_idx >= 0 {
			result << encode_literal_with_name_ref_dynamic(dyn_name_idx, header.value)
			e.dynamic_table.insert(header)
			continue
		}

		// 5. Fully literal — no match anywhere
		result << encode_literal_without_name_ref(header.name, header.value)
		e.dynamic_table.insert(header)
	}

	// Required Insert Count and Delta Base stay 0 for now.
	// TODO: full Required Insert Count encoding per RFC 9204 Section 3.2.6
	// when encoder/decoder stream handling is implemented.
	result[0] = 0x00
	result[1] = 0x00

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
// Uses the post-base index format: 0001XXXX (RFC 9204 Section 3.2.3).
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
