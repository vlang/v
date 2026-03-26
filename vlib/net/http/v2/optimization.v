// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

// Performance optimizations for HTTP/2 implementation

// encode_optimized performs HPACK encoding with buffer reuse for better performance.
// It searches the static and dynamic tables for exact or name-only matches and
// uses RFC 7541-compliant multi-byte integer encoding for indices >= 127.
// Updates the dynamic table when emitting literal representations.
// Returns the number of bytes written to the buffer.
pub fn (mut e Encoder) encode_optimized(headers []HeaderField, mut buf []u8) int {
	mut offset := 0

	for header in headers {
		mut found_exact_idx := 0
		mut found_name_idx := 0

		// Search static table for exact or name-only match
		for i, entry in static_table {
			if entry.name == header.name {
				if entry.value == header.value {
					found_exact_idx = i
					break
				} else if found_name_idx == 0 {
					found_name_idx = i
				}
			}
		}

		// Search dynamic table if no static exact match found
		if found_exact_idx == 0 {
			for i := 0; i < e.dynamic_table.entries.len; i++ {
				entry := e.dynamic_table.entries[i]
				if entry.name == header.name {
					dyn_idx := static_table.len + i
					if entry.value == header.value {
						found_exact_idx = dyn_idx
						break
					} else if found_name_idx == 0 {
						found_name_idx = dyn_idx
					}
				}
			}
		}

		if found_exact_idx > 0 {
			offset += encode_optimized_indexed(found_exact_idx, mut buf, offset)
		} else {
			offset = encode_optimized_literal(header, found_name_idx, mut buf, offset)
			e.dynamic_table.add(header)
		}
	}

	return offset
}

// encode_optimized_indexed writes an indexed header field to the buffer (RFC 7541 §6.1).
// Returns the number of bytes written.
fn encode_optimized_indexed(idx int, mut buf []u8, offset int) int {
	encoded_len := encode_integer(u64(idx), 7, mut buf, offset)
	buf[offset] |= 0x80
	return encoded_len
}

// encode_optimized_literal writes a literal header field to the buffer (RFC 7541 §6.2.1).
// When name_idx > 0, uses indexed name; otherwise encodes the name fresh.
// Returns the new offset after writing.
fn encode_optimized_literal(field HeaderField, name_idx int, mut buf []u8, offset int) int {
	mut pos := offset
	if name_idx > 0 {
		encoded_len := encode_integer(u64(name_idx), 6, mut buf, pos)
		buf[pos] |= 0x40
		pos += encoded_len
	} else {
		if pos + 1 > buf.len {
			return pos
		}
		buf[pos] = 0x40
		pos++
		pos = encode_optimized_string(field.name, mut buf, pos)
	}
	pos = encode_optimized_string(field.value, mut buf, pos)
	return pos
}

// encode_integer encodes a variable-length integer using fast encoding.
// Returns the number of bytes written to the buffer, or 0 if the buffer
// is too small to hold the encoded integer at the given offset.
pub fn encode_integer(value u64, prefix_bits u8, mut buf []u8, offset int) int {
	max_prefix := (u64(1) << prefix_bits) - 1

	if value < max_prefix {
		if offset >= buf.len {
			return 0
		}
		buf[offset] = u8(value)
		return 1
	}

	if offset >= buf.len {
		return 0
	}
	buf[offset] = u8(max_prefix)
	mut remaining := value - max_prefix
	mut pos := offset + 1

	for remaining >= 128 {
		if pos >= buf.len {
			return pos - offset
		}
		buf[pos] = u8((remaining % 128) + 128)
		remaining /= 128
		pos++
	}

	if pos >= buf.len {
		return pos - offset
	}
	buf[pos] = u8(remaining)
	return pos - offset + 1
}

// encode_optimized_string encodes a string into the buffer using Huffman coding when
// it produces a shorter representation (RFC 7541 §5.2), otherwise uses literal encoding.
// Returns the new offset after writing.
fn encode_optimized_string(s string, mut buf []u8, start_offset int) int {
	mut offset := start_offset
	raw_bytes := s.bytes()
	huffman_bits := huffman_encoded_length(raw_bytes)
	huffman_len := (huffman_bits + 7) / 8

	if huffman_len < s.len {
		// Huffman is shorter — use it
		encoded := encode_huffman(raw_bytes)
		len_bytes := encode_integer(u64(huffman_len), 7, mut buf, offset)
		buf[offset] |= 0x80 // set Huffman bit
		offset += len_bytes
		for b in encoded {
			if offset >= buf.len {
				return offset
			}
			buf[offset] = b
			offset++
		}
	} else {
		// Literal is same size or shorter
		len_bytes := encode_integer(u64(s.len), 7, mut buf, offset)
		offset += len_bytes
		for b in raw_bytes {
			if offset >= buf.len {
				return offset
			}
			buf[offset] = b
			offset++
		}
	}
	return offset
}
