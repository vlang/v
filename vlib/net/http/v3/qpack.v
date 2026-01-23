// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

// QPACK: Header Compression for HTTP/3 (RFC 9204)
// QPACK is similar to HPACK but designed for QUIC's out-of-order delivery

// QPACK Static Table (RFC 9204 Appendix A)
const static_table = [
	HeaderField{
		name:  ':authority'
		value: ''
	},
	HeaderField{
		name:  ':path'
		value: '/'
	},
	HeaderField{
		name:  'age'
		value: '0'
	},
	HeaderField{
		name:  'content-disposition'
		value: ''
	},
	HeaderField{
		name:  'content-length'
		value: '0'
	},
	HeaderField{
		name:  'cookie'
		value: ''
	},
	HeaderField{
		name:  'date'
		value: ''
	},
	HeaderField{
		name:  'etag'
		value: ''
	},
	HeaderField{
		name:  'if-modified-since'
		value: ''
	},
	HeaderField{
		name:  'if-none-match'
		value: ''
	},
	HeaderField{
		name:  'last-modified'
		value: ''
	},
	HeaderField{
		name:  'link'
		value: ''
	},
	HeaderField{
		name:  'location'
		value: ''
	},
	HeaderField{
		name:  'referer'
		value: ''
	},
	HeaderField{
		name:  'set-cookie'
		value: ''
	},
	HeaderField{
		name:  ':method'
		value: 'CONNECT'
	},
	HeaderField{
		name:  ':method'
		value: 'DELETE'
	},
	HeaderField{
		name:  ':method'
		value: 'GET'
	},
	HeaderField{
		name:  ':method'
		value: 'HEAD'
	},
	HeaderField{
		name:  ':method'
		value: 'OPTIONS'
	},
	HeaderField{
		name:  ':method'
		value: 'POST'
	},
	HeaderField{
		name:  ':method'
		value: 'PUT'
	},
	HeaderField{
		name:  ':scheme'
		value: 'http'
	},
	HeaderField{
		name:  ':scheme'
		value: 'https'
	},
	HeaderField{
		name:  ':status'
		value: '103'
	},
	HeaderField{
		name:  ':status'
		value: '200'
	},
	HeaderField{
		name:  ':status'
		value: '304'
	},
	HeaderField{
		name:  ':status'
		value: '404'
	},
	HeaderField{
		name:  ':status'
		value: '503'
	},
	HeaderField{
		name:  'accept'
		value: '*/*'
	},
	HeaderField{
		name:  'accept'
		value: 'application/dns-message'
	},
	HeaderField{
		name:  'accept-encoding'
		value: 'gzip, deflate, br'
	},
	HeaderField{
		name:  'accept-ranges'
		value: 'bytes'
	},
	HeaderField{
		name:  'access-control-allow-headers'
		value: 'cache-control'
	},
	HeaderField{
		name:  'access-control-allow-headers'
		value: 'content-type'
	},
	HeaderField{
		name:  'access-control-allow-origin'
		value: '*'
	},
	HeaderField{
		name:  'cache-control'
		value: 'max-age=0'
	},
	HeaderField{
		name:  'cache-control'
		value: 'max-age=2592000'
	},
	HeaderField{
		name:  'cache-control'
		value: 'max-age=604800'
	},
	HeaderField{
		name:  'cache-control'
		value: 'no-cache'
	},
	HeaderField{
		name:  'cache-control'
		value: 'no-store'
	},
	HeaderField{
		name:  'cache-control'
		value: 'public, max-age=31536000'
	},
	HeaderField{
		name:  'content-encoding'
		value: 'br'
	},
	HeaderField{
		name:  'content-encoding'
		value: 'gzip'
	},
	HeaderField{
		name:  'content-type'
		value: 'application/dns-message'
	},
	HeaderField{
		name:  'content-type'
		value: 'application/javascript'
	},
	HeaderField{
		name:  'content-type'
		value: 'application/json'
	},
	HeaderField{
		name:  'content-type'
		value: 'application/x-www-form-urlencoded'
	},
	HeaderField{
		name:  'content-type'
		value: 'image/gif'
	},
	HeaderField{
		name:  'content-type'
		value: 'image/jpeg'
	},
	HeaderField{
		name:  'content-type'
		value: 'image/png'
	},
	HeaderField{
		name:  'content-type'
		value: 'text/css'
	},
	HeaderField{
		name:  'content-type'
		value: 'text/html; charset=utf-8'
	},
	HeaderField{
		name:  'content-type'
		value: 'text/plain'
	},
	HeaderField{
		name:  'content-type'
		value: 'text/plain;charset=utf-8'
	},
	HeaderField{
		name:  'range'
		value: 'bytes=0-'
	},
	HeaderField{
		name:  'strict-transport-security'
		value: 'max-age=31536000'
	},
	HeaderField{
		name:  'strict-transport-security'
		value: 'max-age=31536000; includesubdomains'
	},
	HeaderField{
		name:  'strict-transport-security'
		value: 'max-age=31536000; includesubdomains; preload'
	},
	HeaderField{
		name:  'vary'
		value: 'accept-encoding'
	},
	HeaderField{
		name:  'vary'
		value: 'origin'
	},
	HeaderField{
		name:  'x-content-type-options'
		value: 'nosniff'
	},
	HeaderField{
		name:  'x-xss-protection'
		value: '1; mode=block'
	},
	HeaderField{
		name:  ':status'
		value: '100'
	},
	HeaderField{
		name:  ':status'
		value: '204'
	},
	HeaderField{
		name:  ':status'
		value: '206'
	},
	HeaderField{
		name:  ':status'
		value: '302'
	},
	HeaderField{
		name:  ':status'
		value: '400'
	},
	HeaderField{
		name:  ':status'
		value: '403'
	},
	HeaderField{
		name:  ':status'
		value: '421'
	},
	HeaderField{
		name:  ':status'
		value: '425'
	},
	HeaderField{
		name:  ':status'
		value: '500'
	},
	HeaderField{
		name:  'accept-language'
		value: ''
	},
	HeaderField{
		name:  'access-control-allow-credentials'
		value: 'FALSE'
	},
	HeaderField{
		name:  'access-control-allow-credentials'
		value: 'TRUE'
	},
	HeaderField{
		name:  'access-control-allow-headers'
		value: '*'
	},
	HeaderField{
		name:  'access-control-allow-methods'
		value: 'get'
	},
	HeaderField{
		name:  'access-control-allow-methods'
		value: 'get, post, options'
	},
	HeaderField{
		name:  'access-control-allow-methods'
		value: 'options'
	},
	HeaderField{
		name:  'access-control-expose-headers'
		value: 'content-length'
	},
	HeaderField{
		name:  'access-control-request-headers'
		value: 'content-type'
	},
	HeaderField{
		name:  'access-control-request-method'
		value: 'get'
	},
	HeaderField{
		name:  'access-control-request-method'
		value: 'post'
	},
	HeaderField{
		name:  'alt-svc'
		value: 'clear'
	},
	HeaderField{
		name:  'authorization'
		value: ''
	},
	HeaderField{
		name:  'content-security-policy'
		value: "script-src 'none'; object-src 'none'; base-uri 'none'"
	},
	HeaderField{
		name:  'early-data'
		value: '1'
	},
	HeaderField{
		name:  'expect-ct'
		value: ''
	},
	HeaderField{
		name:  'forwarded'
		value: ''
	},
	HeaderField{
		name:  'if-range'
		value: ''
	},
	HeaderField{
		name:  'origin'
		value: ''
	},
	HeaderField{
		name:  'purpose'
		value: 'prefetch'
	},
	HeaderField{
		name:  'server'
		value: ''
	},
	HeaderField{
		name:  'timing-allow-origin'
		value: '*'
	},
	HeaderField{
		name:  'upgrade-insecure-requests'
		value: '1'
	},
	HeaderField{
		name:  'user-agent'
		value: ''
	},
	HeaderField{
		name:  'x-forwarded-for'
		value: ''
	},
	HeaderField{
		name:  'x-frame-options'
		value: 'deny'
	},
	HeaderField{
		name:  'x-frame-options'
		value: 'sameorigin'
	},
]

// QPACK static table lookup maps for O(1) access
// Map from "name:value" to index (for exact matches)
const qpack_static_exact_map = build_qpack_exact_map()

// Map from "name" to list of indices (for name-only matches)
const qpack_static_name_map = build_qpack_name_map()

// build_qpack_exact_map builds a map for exact header matches
fn build_qpack_exact_map() map[string]int {
	mut m := map[string]int{}
	for i, entry in static_table {
		key := '${entry.name}:${entry.value}'
		if key !in m {
			m[key] = i
		}
	}
	return m
}

// build_qpack_name_map builds a map for name-only matches
fn build_qpack_name_map() map[string][]int {
	mut m := map[string][]int{}
	for i, entry in static_table {
		if entry.name !in m {
			m[entry.name] = []int{}
		}
		m[entry.name] << i
	}
	return m
}

// DynamicTableEntry represents an entry in the dynamic table
struct DynamicTableEntry {
	field HeaderField
	size  int // Size in bytes (name.len + value.len + 32)
}

// DynamicTable manages the dynamic table for QPACK
struct DynamicTable {
mut:
	entries      []DynamicTableEntry
	size         int
	max_size     int
	insert_count u64
}

fn new_dynamic_table(max_size int) DynamicTable {
	return DynamicTable{
		entries:      []DynamicTableEntry{}
		size:         0
		max_size:     max_size
		insert_count: 0
	}
}

fn (mut dt DynamicTable) insert(field HeaderField) {
	entry_size := field.name.len + field.value.len + 32

	// Evict entries if necessary
	for dt.size + entry_size > dt.max_size && dt.entries.len > 0 {
		removed := dt.entries[0]
		dt.entries.delete(0)
		dt.size -= removed.size
	}

	// Add new entry
	if entry_size <= dt.max_size {
		dt.entries << DynamicTableEntry{
			field: field
			size:  entry_size
		}
		dt.size += entry_size
		dt.insert_count++
	}
}

fn (dt &DynamicTable) get(index int) ?HeaderField {
	if index < 0 || index >= dt.entries.len {
		return none
	}
	return dt.entries[dt.entries.len - 1 - index].field
}

// Encoder handles QPACK encoding
pub struct Encoder {
mut:
	dynamic_table DynamicTable
	max_blocked   u64
}

pub fn new_qpack_encoder(max_table_capacity int, max_blocked u64) Encoder {
	return Encoder{
		dynamic_table: new_dynamic_table(max_table_capacity)
		max_blocked:   max_blocked
	}
}

// encode encodes headers using QPACK
pub fn (mut e Encoder) encode(headers []HeaderField) []u8 {
	// Pre-allocate result with estimated size
	mut estimated_size := 2 // Required Insert Count + Delta Base
	for header in headers {
		estimated_size += header.name.len + header.value.len + 10
	}
	mut result := []u8{cap: estimated_size}

	// Encode Required Insert Count and Delta Base (simplified - always 0 for now)
	result << 0x00 // Required Insert Count = 0
	result << 0x00 // Delta Base = 0

	for header in headers {
		// Try exact match in static table using hashmap (O(1))
		exact_key := '${header.name}:${header.value}'
		mut found_index := -1
		mut exact_match := false

		if exact_key in qpack_static_exact_map {
			found_index = qpack_static_exact_map[exact_key]
			exact_match = true
		}

		// If no exact match, try name-only match in static table
		if !exact_match && header.name in qpack_static_name_map {
			indices := qpack_static_name_map[header.name]
			if indices.len > 0 {
				found_index = indices[0] // Use first match
			}
		}

		if exact_match {
			// Indexed Field Line (static table)
			result << encode_indexed_static(found_index)
		} else if found_index >= 0 {
			// Literal Field Line with Name Reference (static table)
			result << encode_literal_with_name_ref_static(found_index, header.value)
		} else {
			// Literal Field Line without Name Reference
			result << encode_literal_without_name_ref(header.name, header.value)
		}
	}

	return result
}

// encode_indexed_static encodes an indexed field line from static table
fn encode_indexed_static(index int) []u8 {
	mut result := []u8{cap: 6} // Pre-allocate for worst case
	// Static Indexed: 11XXXXXX pattern
	if index < 64 {
		result << u8(0xc0 | index)
	} else {
		result << 0xc0
		result << encode_integer(index - 64, 6)
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
		result << 0x50
		result << encode_integer(index - 16, 4)
	}
	// Encode value length and value
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

// encode_qpack_string encodes a string with length prefix
fn encode_qpack_string(s string) []u8 {
	// Pre-allocate with estimated size: length encoding (1-5 bytes) + string bytes
	bytes := s.bytes()
	mut result := []u8{cap: 5 + bytes.len}
	// Length prefix (no Huffman encoding for now)
	result << encode_integer(bytes.len, 7)
	result << bytes
	return result
}

// encode_integer encodes an integer with N-bit prefix
fn encode_integer(value int, n int) []u8 {
	// Pre-allocate with capacity for worst case (5 bytes for 32-bit int)
	mut result := []u8{cap: 5}
	max_prefix := (1 << n) - 1

	if value < max_prefix {
		result << u8(value)
	} else {
		result << u8(max_prefix)
		mut remaining := value - max_prefix
		for remaining >= 128 {
			result << u8((remaining % 128) + 128)
			remaining /= 128
		}
		result << u8(remaining)
	}

	return result
}

// Decoder handles QPACK decoding
pub struct Decoder {
mut:
	dynamic_table DynamicTable
	max_blocked   u64
}

pub fn new_qpack_decoder(max_table_capacity int, max_blocked u64) Decoder {
	return Decoder{
		dynamic_table: new_dynamic_table(max_table_capacity)
		max_blocked:   max_blocked
	}
}

// decode decodes QPACK-encoded headers
pub fn (mut d Decoder) decode(data []u8) ![]HeaderField {
	mut headers := []HeaderField{}
	mut idx := 0

	if data.len < 2 {
		return error('QPACK data too short')
	}

	// Decode Required Insert Count
	required_insert_count := data[idx]
	idx++

	// Decode Delta Base
	_ := data[idx]
	idx++

	// Decode header fields
	for idx < data.len {
		first_byte := data[idx]

		if (first_byte & 0xc0) == 0xc0 {
			// Indexed Field Line (static table)
			index_val := int(first_byte & 0x3f)
			mut index := index_val

			if index_val == 63 {
				// Multi-byte index
				decoded_int, bytes_read := decode_integer(data[idx + 1..], 6)!
				index = index_val + decoded_int
				idx += bytes_read
			}

			if index >= static_table.len {
				return error('Static table index out of range: ${index}')
			}

			headers << static_table[index]
			idx++
		} else if (first_byte & 0xf0) == 0x50 {
			// Literal with Name Reference (static table)
			index_val := int(first_byte & 0x0f)
			mut index := index_val
			idx++

			if index_val == 15 {
				decoded_int, bytes_read := decode_integer(data[idx..], 4)!
				index = index_val + decoded_int
				idx += bytes_read
			}

			if index >= static_table.len {
				return error('Static table index out of range: ${index}')
			}

			// Decode value
			value, bytes_read := decode_qpack_string(data[idx..])!
			idx += bytes_read

			headers << HeaderField{
				name:  static_table[index].name
				value: value
			}
		} else if (first_byte & 0xe0) == 0x20 {
			// Literal without Name Reference
			idx++

			// Decode name
			name, name_bytes := decode_qpack_string(data[idx..])!
			idx += name_bytes

			// Decode value
			value, value_bytes := decode_qpack_string(data[idx..])!
			idx += value_bytes

			headers << HeaderField{
				name:  name
				value: value
			}
		} else {
			return error('Unknown QPACK instruction: 0x${first_byte:02x}')
		}
	}

	return headers
}

// decode_integer decodes an integer with N-bit prefix
fn decode_integer(data []u8, n int) !(int, int) {
	if data.len == 0 {
		return error('No data to decode integer')
	}

	max_prefix := (1 << n) - 1
	mut value := int(data[0] & u8(max_prefix))
	mut idx := 1

	if value < max_prefix {
		return value, idx
	}

	mut m := 0
	for idx < data.len {
		b := data[idx]
		value += int(b & 0x7f) << m
		m += 7
		idx++

		if (b & 0x80) == 0 {
			break
		}
	}

	return value, idx
}

// decode_qpack_string decodes a string with length prefix
fn decode_qpack_string(data []u8) !(string, int) {
	if data.len == 0 {
		return error('No data to decode string')
	}

	huffman := (data[0] & 0x80) != 0
	length_data := []u8{len: 1, init: data[0] & 0x7f}

	mut length := int(length_data[0])
	mut idx := 1

	if length == 127 {
		decoded_len, bytes_read := decode_integer(data[idx..], 7)!
		length = 127 + decoded_len
		idx += bytes_read
	}

	if idx + length > data.len {
		return error('String length exceeds data length')
	}

	if huffman {
		// Huffman decoding not implemented yet
		return error('Huffman decoding not yet implemented')
	}

	str_bytes := data[idx..idx + length]
	return str_bytes.bytestr(), idx + length
}
