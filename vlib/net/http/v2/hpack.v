// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

// HPACK - Header Compression for HTTP/2 (RFC 7541)

// Static table entries (RFC 7541 Appendix A)
const static_table = [
	HeaderField{'', ''},
	HeaderField{':authority', ''},
	HeaderField{':method', 'GET'},
	HeaderField{':method', 'POST'},
	HeaderField{':path', '/'},
	HeaderField{':path', '/index.html'},
	HeaderField{':scheme', 'http'},
	HeaderField{':scheme', 'https'},
	HeaderField{':status', '200'},
	HeaderField{':status', '204'},
	HeaderField{':status', '206'},
	HeaderField{':status', '304'},
	HeaderField{':status', '400'},
	HeaderField{':status', '404'},
	HeaderField{':status', '500'},
	HeaderField{'accept-charset', ''},
	HeaderField{'accept-encoding', 'gzip, deflate'},
	HeaderField{'accept-language', ''},
	HeaderField{'accept-ranges', ''},
	HeaderField{'accept', ''},
	HeaderField{'access-control-allow-origin', ''},
	HeaderField{'age', ''},
	HeaderField{'allow', ''},
	HeaderField{'authorization', ''},
	HeaderField{'cache-control', ''},
	HeaderField{'content-disposition', ''},
	HeaderField{'content-encoding', ''},
	HeaderField{'content-language', ''},
	HeaderField{'content-length', ''},
	HeaderField{'content-location', ''},
	HeaderField{'content-range', ''},
	HeaderField{'content-type', ''},
	HeaderField{'cookie', ''},
	HeaderField{'date', ''},
	HeaderField{'etag', ''},
	HeaderField{'expect', ''},
	HeaderField{'expires', ''},
	HeaderField{'from', ''},
	HeaderField{'host', ''},
	HeaderField{'if-match', ''},
	HeaderField{'if-modified-since', ''},
	HeaderField{'if-none-match', ''},
	HeaderField{'if-range', ''},
	HeaderField{'if-unmodified-since', ''},
	HeaderField{'last-modified', ''},
	HeaderField{'link', ''},
	HeaderField{'location', ''},
	HeaderField{'max-forwards', ''},
	HeaderField{'proxy-authenticate', ''},
	HeaderField{'proxy-authorization', ''},
	HeaderField{'range', ''},
	HeaderField{'referer', ''},
	HeaderField{'refresh', ''},
	HeaderField{'retry-after', ''},
	HeaderField{'server', ''},
	HeaderField{'set-cookie', ''},
	HeaderField{'strict-transport-security', ''},
	HeaderField{'transfer-encoding', ''},
	HeaderField{'user-agent', ''},
	HeaderField{'vary', ''},
	HeaderField{'via', ''},
	HeaderField{'www-authenticate', ''},
]

// Static table lookup maps for O(1) access
// Map from "name:value" to index (for exact matches)
const static_table_exact_map = build_exact_map()

// Map from "name" to list of indices (for name-only matches)
const static_table_name_map = build_name_map()

// build_exact_map builds a map for exact header matches
fn build_exact_map() map[string]int {
	mut m := map[string]int{}
	for i, entry in static_table {
		if entry.name != '' {
			key := '${entry.name}:${entry.value}'
			if key !in m {
				m[key] = i + 1 // HPACK indices are 1-based
			}
		}
	}
	return m
}

// build_name_map builds a map for name-only matches
fn build_name_map() map[string][]int {
	mut m := map[string][]int{}
	for i, entry in static_table {
		if entry.name != '' {
			if entry.name !in m {
				m[entry.name] = []int{}
			}
			m[entry.name] << (i + 1) // HPACK indices are 1-based
		}
	}
	return m
}

// HeaderField represents a name-value pair
pub struct HeaderField {
pub mut:
	name  string
	value string
}

// size returns the size of the header field in bytes (RFC 7541 Section 4.1)
pub fn (h HeaderField) size() int {
	return 32 + h.name.len + h.value.len
}

// DynamicTable represents the HPACK dynamic table
pub struct DynamicTable {
mut:
	entries  []HeaderField
	size     int
	max_size int = 4096 // Default from RFC 7541
}

// add adds an entry to the dynamic table
pub fn (mut dt DynamicTable) add(field HeaderField) {
	// Evict entries if necessary
	for dt.size + field.size() > dt.max_size && dt.entries.len > 0 {
		removed := dt.entries.pop()
		dt.size -= removed.size()
	}

	// Add new entry at the beginning
	dt.entries.insert(0, field)
	dt.size += field.size()
}

// get retrieves an entry from the dynamic table (1-indexed)
pub fn (dt DynamicTable) get(index int) ?HeaderField {
	if index < 1 || index > dt.entries.len {
		return none
	}
	return dt.entries[index - 1]
}

// set_max_size updates the maximum size of the dynamic table
pub fn (mut dt DynamicTable) set_max_size(size int) {
	dt.max_size = size

	// Evict entries if necessary
	for dt.size > dt.max_size && dt.entries.len > 0 {
		removed := dt.entries.pop()
		dt.size -= removed.size()
	}
}

// Encoder encodes headers using HPACK
pub struct Encoder {
mut:
	dynamic_table DynamicTable
}

// new_encoder creates a new HPACK encoder
pub fn new_encoder() Encoder {
	return Encoder{
		dynamic_table: DynamicTable{}
	}
}

// Decoder decodes headers using HPACK
pub struct Decoder {
mut:
	dynamic_table DynamicTable
}

// new_decoder creates a new HPACK decoder
pub fn new_decoder() Decoder {
	return Decoder{
		dynamic_table: DynamicTable{}
	}
}

// get_indexed retrieves a header field from static or dynamic table
fn get_indexed(dynamic_table &DynamicTable, index int) ?HeaderField {
	if index == 0 {
		return none
	}

	// Static table
	if index <= static_table.len {
		return static_table[index - 1]
	}

	// Dynamic table
	dynamic_index := index - static_table.len
	return dynamic_table.get(dynamic_index)
}

// encode_integer encodes an integer using HPACK integer representation
fn encode_integer(value int, prefix_bits int) []u8 {
	// Pre-allocate with capacity for worst case (5 bytes for 32-bit int)
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

// decode_integer decodes an integer using HPACK integer representation
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

// encode_string encodes a string (with optional Huffman coding)
fn encode_string(s string, huffman bool) []u8 {
	// Pre-allocate with estimated size: length encoding (1-5 bytes) + string bytes
	mut result := []u8{cap: 5 + s.len}

	if huffman {
		// TODO: Implement Huffman encoding
		// For now, use literal encoding
		encoded := encode_integer(s.len, 7)
		result << (encoded[0] | 0x80) // Set H bit
		if encoded.len > 1 {
			result << encoded[1..]
		}
		result << s.bytes()
	} else {
		// Literal encoding
		encoded := encode_integer(s.len, 7)
		result << encoded
		result << s.bytes()
	}

	return result
}

// decode_string decodes a string (with optional Huffman coding)
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
		// TODO: Implement Huffman decoding
		return error('Huffman decoding not yet implemented')
	}

	return str_data.bytestr(), bytes_read + length
}

// encode encodes a list of header fields
pub fn (mut e Encoder) encode(headers []HeaderField) []u8 {
	// Pre-allocate result with estimated size
	mut estimated_size := 0
	for header in headers {
		estimated_size += header.name.len + header.value.len + 10 // +10 for encoding overhead
	}
	mut result := []u8{cap: estimated_size}

	for header in headers {
		mut found_index := 0
		mut found_name_index := 0

		// Try exact match in static table using hashmap (O(1))
		exact_key := '${header.name}:${header.value}'
		if exact_key in static_table_exact_map {
			found_index = static_table_exact_map[exact_key]
		}

		// If no exact match, try name-only match in static table
		if found_index == 0 && header.name in static_table_name_map {
			indices := static_table_name_map[header.name]
			if indices.len > 0 {
				found_name_index = indices[0] // Use first match
			}
		}

		// Search dynamic table (still linear, but typically much smaller)
		if found_index == 0 {
			for i := 0; i < e.dynamic_table.entries.len; i++ {
				entry := e.dynamic_table.entries[i]
				if entry.name == header.name {
					if entry.value == header.value {
						found_index = static_table.len + i + 1
						break
					} else if found_name_index == 0 {
						found_name_index = static_table.len + i + 1
					}
				}
			}
		}

		if found_index > 0 {
			// Indexed header field (RFC 7541 Section 6.1)
			encoded := encode_integer(found_index, 7)
			result << (encoded[0] | 0x80)
			if encoded.len > 1 {
				result << encoded[1..]
			}
		} else {
			// Literal header field with incremental indexing (RFC 7541 Section 6.2.1)
			if found_name_index > 0 {
				encoded := encode_integer(found_name_index, 6)
				result << (encoded[0] | 0x40)
				if encoded.len > 1 {
					result << encoded[1..]
				}
			} else {
				result << u8(0x40)
				result << encode_string(header.name, false)
			}
			result << encode_string(header.value, false)

			// Add to dynamic table
			e.dynamic_table.add(header)
		}
	}

	return result
}

// decode decodes a header block
pub fn (mut d Decoder) decode(data []u8) ![]HeaderField {
	mut headers := []HeaderField{}
	mut idx := 0

	for idx < data.len {
		first_byte := data[idx]

		if (first_byte & 0x80) != 0 {
			// Indexed header field (RFC 7541 Section 6.1)
			index, bytes_read := decode_integer(data[idx..], 7)!
			idx += bytes_read

			field := get_indexed(&d.dynamic_table, index) or {
				return error('invalid index: ${index}')
			}
			headers << field
		} else if (first_byte & 0x40) != 0 {
			// Literal header field with incremental indexing (RFC 7541 Section 6.2.1)
			index, bytes_read := decode_integer(data[idx..], 6)!
			idx += bytes_read

			mut name := ''
			if index == 0 {
				mut name_bytes_read := 0
				name, name_bytes_read = decode_string(data[idx..])!
				idx += name_bytes_read
			} else {
				field := get_indexed(&d.dynamic_table, index) or {
					return error('invalid index: ${index}')
				}
				name = field.name
			}

			value, bytes_read2 := decode_string(data[idx..])!
			idx += bytes_read2

			field := HeaderField{name, value}
			headers << field
			d.dynamic_table.add(field)
		} else if (first_byte & 0x20) != 0 {
			// Dynamic table size update (RFC 7541 Section 6.3)
			size, bytes_read := decode_integer(data[idx..], 5)!
			idx += bytes_read
			d.dynamic_table.set_max_size(size)
		} else {
			// Literal header field without indexing (RFC 7541 Section 6.2.2)
			index, bytes_read := decode_integer(data[idx..], 4)!
			idx += bytes_read

			mut name := ''
			if index == 0 {
				mut name_bytes_read := 0
				name, name_bytes_read = decode_string(data[idx..])!
				idx += name_bytes_read
			} else {
				field := get_indexed(&d.dynamic_table, index) or {
					return error('invalid index: ${index}')
				}
				name = field.name
			}

			value, bytes_read2 := decode_string(data[idx..])!
			idx += bytes_read2

			headers << HeaderField{name, value}
		}
	}

	return headers
}
