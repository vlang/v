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
// Uses post-base name reference format: 00000XXX (RFC 9204 Section 3.2.4).
fn encode_literal_with_name_ref_dynamic(index int, value string) []u8 {
	mut result := []u8{cap: 10 + value.len}
	// Post-base literal name ref: 00000XXX
	if index < 8 {
		result << u8(0x00 | index)
	} else {
		mut suffix := encode_integer(index, 3)
		suffix[0] |= 0x00
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

// encode_qpack_string encodes a string with length prefix (optimized)
@[inline]
fn encode_qpack_string(s string) []u8 {
	// Pre-allocate with estimated size: length encoding (1-5 bytes) + string bytes
	bytes := s.bytes()
	bytes_len := bytes.len
	mut result := []u8{cap: 5 + bytes_len}
	// Length prefix (no Huffman encoding for now)
	result << encode_integer(bytes_len, 7)
	if bytes_len > 0 {
		result << bytes
	}
	return result
}

// encode_integer encodes an integer with N-bit prefix (optimized)
@[inline]
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

// Helper methods for decoding specific field types

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

// decode_prefixed_integer decodes an integer with N-bit prefix.
// data should start at the byte containing the prefix.
fn decode_prefixed_integer(data []u8, prefix_bits int) !(int, int) {
	if data.len == 0 {
		return error('empty data')
	}

	mask := u8((1 << prefix_bits) - 1)
	prefix_val := int(data[0] & mask)

	if prefix_val < int(mask) {
		return prefix_val, 1
	}

	mut m := 0
	mut decoded_int := i64(0)
	mut idx := 1

	for idx < data.len {
		b := data[idx]
		decoded_int += i64(u64(b & 0x7f) << m)
		m += 7
		idx++
		if (b & 0x80) == 0 {
			break
		}
	}

	return prefix_val + int(decoded_int), idx
}

// qpack_huffman_table holds the HPACK/QPACK Huffman codes (RFC 7541 Appendix B).
// Each entry is [code, bit_length]. QPACK uses the identical table as HPACK.
const qpack_huffman_table = [
	[u32(0x1ff8), u32(13)], // 0
	[u32(0x7fffd8), u32(23)], // 1
	[u32(0xfffffe2), u32(28)], // 2
	[u32(0xfffffe3), u32(28)], // 3
	[u32(0xfffffe4), u32(28)], // 4
	[u32(0xfffffe5), u32(28)], // 5
	[u32(0xfffffe6), u32(28)], // 6
	[u32(0xfffffe7), u32(28)], // 7
	[u32(0xfffffe8), u32(28)], // 8
	[u32(0xffffea), u32(24)], // 9
	[u32(0x3ffffffc), u32(30)], // 10
	[u32(0xfffffe9), u32(28)], // 11
	[u32(0xfffffea), u32(28)], // 12
	[u32(0x3ffffffd), u32(30)], // 13
	[u32(0xfffffeb), u32(28)], // 14
	[u32(0xfffffec), u32(28)], // 15
	[u32(0xfffffed), u32(28)], // 16
	[u32(0xfffffee), u32(28)], // 17
	[u32(0xfffffef), u32(28)], // 18
	[u32(0xffffff0), u32(28)], // 19
	[u32(0xffffff1), u32(28)], // 20
	[u32(0xffffff2), u32(28)], // 21
	[u32(0x3ffffffe), u32(30)], // 22
	[u32(0xffffff3), u32(28)], // 23
	[u32(0xffffff4), u32(28)], // 24
	[u32(0xffffff5), u32(28)], // 25
	[u32(0xffffff6), u32(28)], // 26
	[u32(0xffffff7), u32(28)], // 27
	[u32(0xffffff8), u32(28)], // 28
	[u32(0xffffff9), u32(28)], // 29
	[u32(0xffffffa), u32(28)], // 30
	[u32(0xffffffb), u32(28)], // 31
	[u32(0x14), u32(6)], // 32 ' '
	[u32(0x3f8), u32(10)], // 33 '!'
	[u32(0x3f9), u32(10)], // 34 '"'
	[u32(0xffa), u32(12)], // 35 '#'
	[u32(0x1ff9), u32(13)], // 36 '$'
	[u32(0x15), u32(6)], // 37 '%'
	[u32(0xf8), u32(8)], // 38 '&'
	[u32(0x7fa), u32(11)], // 39 '\''
	[u32(0x3fa), u32(10)], // 40 '('
	[u32(0x3fb), u32(10)], // 41 ')'
	[u32(0xf9), u32(8)], // 42 '*'
	[u32(0x7fb), u32(11)], // 43 '+'
	[u32(0xfa), u32(8)], // 44 ','
	[u32(0x16), u32(6)], // 45 '-'
	[u32(0x17), u32(6)], // 46 '.'
	[u32(0x18), u32(6)], // 47 '/'
	[u32(0x0), u32(5)], // 48 '0'
	[u32(0x1), u32(5)], // 49 '1'
	[u32(0x2), u32(5)], // 50 '2'
	[u32(0x19), u32(6)], // 51 '3'
	[u32(0x1a), u32(6)], // 52 '4'
	[u32(0x1b), u32(6)], // 53 '5'
	[u32(0x1c), u32(6)], // 54 '6'
	[u32(0x1d), u32(6)], // 55 '7'
	[u32(0x1e), u32(6)], // 56 '8'
	[u32(0x1f), u32(6)], // 57 '9'
	[u32(0x5c), u32(7)], // 58 ':'
	[u32(0xfb), u32(8)], // 59 ';'
	[u32(0x7ffc), u32(15)], // 60 '<'
	[u32(0x20), u32(6)], // 61 '='
	[u32(0xffb), u32(12)], // 62 '>'
	[u32(0x3fc), u32(10)], // 63 '?'
	[u32(0x1ffa), u32(13)], // 64 '@'
	[u32(0x21), u32(6)], // 65 'A'
	[u32(0x5d), u32(7)], // 66 'B'
	[u32(0x5e), u32(7)], // 67 'C'
	[u32(0x5f), u32(7)], // 68 'D'
	[u32(0x60), u32(7)], // 69 'E'
	[u32(0x61), u32(7)], // 70 'F'
	[u32(0x62), u32(7)], // 71 'G'
	[u32(0x63), u32(7)], // 72 'H'
	[u32(0x64), u32(7)], // 73 'I'
	[u32(0x65), u32(7)], // 74 'J'
	[u32(0x66), u32(7)], // 75 'K'
	[u32(0x67), u32(7)], // 76 'L'
	[u32(0x68), u32(7)], // 77 'M'
	[u32(0x69), u32(7)], // 78 'N'
	[u32(0x6a), u32(7)], // 79 'O'
	[u32(0x6b), u32(7)], // 80 'P'
	[u32(0x6c), u32(7)], // 81 'Q'
	[u32(0x6d), u32(7)], // 82 'R'
	[u32(0x6e), u32(7)], // 83 'S'
	[u32(0x6f), u32(7)], // 84 'T'
	[u32(0x70), u32(7)], // 85 'U'
	[u32(0x71), u32(7)], // 86 'V'
	[u32(0x72), u32(7)], // 87 'W'
	[u32(0xfc), u32(8)], // 88 'X'
	[u32(0x73), u32(7)], // 89 'Y'
	[u32(0xfd), u32(8)], // 90 'Z'
	[u32(0x1ffb), u32(13)], // 91 '['
	[u32(0x7fff0), u32(19)], // 92 '\'
	[u32(0x1ffc), u32(13)], // 93 ']'
	[u32(0x3ffc), u32(14)], // 94 '^'
	[u32(0x22), u32(6)], // 95 '_'
	[u32(0x7ffd), u32(15)], // 96 '`'
	[u32(0x3), u32(5)], // 97 'a'
	[u32(0x23), u32(6)], // 98 'b'
	[u32(0x4), u32(5)], // 99 'c'
	[u32(0x24), u32(6)], // 100 'd'
	[u32(0x5), u32(5)], // 101 'e'
	[u32(0x25), u32(6)], // 102 'f'
	[u32(0x26), u32(6)], // 103 'g'
	[u32(0x27), u32(6)], // 104 'h'
	[u32(0x6), u32(5)], // 105 'i'
	[u32(0x74), u32(7)], // 106 'j'
	[u32(0x75), u32(7)], // 107 'k'
	[u32(0x28), u32(6)], // 108 'l'
	[u32(0x29), u32(6)], // 109 'm'
	[u32(0x2a), u32(6)], // 110 'n'
	[u32(0x7), u32(5)], // 111 'o'
	[u32(0x2b), u32(6)], // 112 'p'
	[u32(0x76), u32(7)], // 113 'q'
	[u32(0x2c), u32(6)], // 114 'r'
	[u32(0x8), u32(5)], // 115 's'
	[u32(0x9), u32(5)], // 116 't'
	[u32(0x2d), u32(6)], // 117 'u'
	[u32(0x77), u32(7)], // 118 'v'
	[u32(0x78), u32(7)], // 119 'w'
	[u32(0x79), u32(7)], // 120 'x'
	[u32(0x7a), u32(7)], // 121 'y'
	[u32(0x7b), u32(7)], // 122 'z'
	[u32(0x7ffe), u32(15)], // 123 '{'
	[u32(0x7fc), u32(11)], // 124 '|'
	[u32(0x3ffd), u32(14)], // 125 '}'
	[u32(0x1ffd), u32(13)], // 126 '~'
	[u32(0xffffffc), u32(28)], // 127
	[u32(0xfffe6), u32(20)], // 128
	[u32(0x3fffd2), u32(22)], // 129
	[u32(0xfffe7), u32(20)], // 130
	[u32(0xfffe8), u32(20)], // 131
	[u32(0x3fffd3), u32(22)], // 132
	[u32(0x3fffd4), u32(22)], // 133
	[u32(0x3fffd5), u32(22)], // 134
	[u32(0x7fffd9), u32(23)], // 135
	[u32(0x3fffd6), u32(22)], // 136
	[u32(0x7fffda), u32(23)], // 137
	[u32(0x7fffdb), u32(23)], // 138
	[u32(0x7fffdc), u32(23)], // 139
	[u32(0x7fffdd), u32(23)], // 140
	[u32(0x7fffde), u32(23)], // 141
	[u32(0xffffeb), u32(24)], // 142
	[u32(0x7fffdf), u32(23)], // 143
	[u32(0xffffec), u32(24)], // 144
	[u32(0xffffed), u32(24)], // 145
	[u32(0x3fffd7), u32(22)], // 146
	[u32(0x7fffe0), u32(23)], // 147
	[u32(0xffffee), u32(24)], // 148
	[u32(0x7fffe1), u32(23)], // 149
	[u32(0x7fffe2), u32(23)], // 150
	[u32(0x7fffe3), u32(23)], // 151
	[u32(0x7fffe4), u32(23)], // 152
	[u32(0x1fffdc), u32(21)], // 153
	[u32(0x3fffd8), u32(22)], // 154
	[u32(0x7fffe5), u32(23)], // 155
	[u32(0x3fffd9), u32(22)], // 156
	[u32(0x7fffe6), u32(23)], // 157
	[u32(0x7fffe7), u32(23)], // 158
	[u32(0xffffef), u32(24)], // 159
	[u32(0x3fffda), u32(22)], // 160
	[u32(0x1fffdd), u32(21)], // 161
	[u32(0xfffe9), u32(20)], // 162
	[u32(0x3fffdb), u32(22)], // 163
	[u32(0x3fffdc), u32(22)], // 164
	[u32(0x7fffe8), u32(23)], // 165
	[u32(0x7fffe9), u32(23)], // 166
	[u32(0x1fffde), u32(21)], // 167
	[u32(0x7fffea), u32(23)], // 168
	[u32(0x3fffdd), u32(22)], // 169
	[u32(0x3fffde), u32(22)], // 170
	[u32(0xfffff0), u32(24)], // 171
	[u32(0x1fffdf), u32(21)], // 172
	[u32(0x3fffdf), u32(22)], // 173
	[u32(0x7fffeb), u32(23)], // 174
	[u32(0x7fffec), u32(23)], // 175
	[u32(0x1fffe0), u32(21)], // 176
	[u32(0x1fffe1), u32(21)], // 177
	[u32(0x3fffe0), u32(22)], // 178
	[u32(0x1fffe2), u32(21)], // 179
	[u32(0x7fffed), u32(23)], // 180
	[u32(0x3fffe1), u32(22)], // 181
	[u32(0x7fffee), u32(23)], // 182
	[u32(0x7fffef), u32(23)], // 183
	[u32(0xfffea), u32(20)], // 184
	[u32(0x3fffe2), u32(22)], // 185
	[u32(0x3fffe3), u32(22)], // 186
	[u32(0x3fffe4), u32(22)], // 187
	[u32(0x7ffff0), u32(23)], // 188
	[u32(0x3fffe5), u32(22)], // 189
	[u32(0x3fffe6), u32(22)], // 190
	[u32(0x7ffff1), u32(23)], // 191
	[u32(0x3ffffe0), u32(26)], // 192
	[u32(0x3ffffe1), u32(26)], // 193
	[u32(0xfffeb), u32(20)], // 194
	[u32(0x7fff1), u32(19)], // 195
	[u32(0x3fffe7), u32(22)], // 196
	[u32(0x7ffff2), u32(23)], // 197
	[u32(0x3fffe8), u32(22)], // 198
	[u32(0x1ffffec), u32(25)], // 199
	[u32(0x3ffffe2), u32(26)], // 200
	[u32(0x3ffffe3), u32(26)], // 201
	[u32(0x3ffffe4), u32(26)], // 202
	[u32(0x7ffffde), u32(27)], // 203
	[u32(0x7ffffdf), u32(27)], // 204
	[u32(0x3ffffe5), u32(26)], // 205
	[u32(0xfffff1), u32(24)], // 206
	[u32(0x1ffffed), u32(25)], // 207
	[u32(0x7fff2), u32(19)], // 208
	[u32(0x1fffe3), u32(21)], // 209
	[u32(0x3ffffe6), u32(26)], // 210
	[u32(0x7ffffe0), u32(27)], // 211
	[u32(0x7ffffe1), u32(27)], // 212
	[u32(0x3ffffe7), u32(26)], // 213
	[u32(0x7ffffe2), u32(27)], // 214
	[u32(0xfffff2), u32(24)], // 215
	[u32(0x1fffe4), u32(21)], // 216
	[u32(0x1fffe5), u32(21)], // 217
	[u32(0x3ffffe8), u32(26)], // 218
	[u32(0x3ffffe9), u32(26)], // 219
	[u32(0xffffffd), u32(28)], // 220
	[u32(0x7ffffe3), u32(27)], // 221
	[u32(0x7ffffe4), u32(27)], // 222
	[u32(0x7ffffe5), u32(27)], // 223
	[u32(0xfffec), u32(20)], // 224
	[u32(0xfffff3), u32(24)], // 225
	[u32(0xfffed), u32(20)], // 226
	[u32(0x1fffe6), u32(21)], // 227
	[u32(0x3fffe9), u32(22)], // 228
	[u32(0x1fffe7), u32(21)], // 229
	[u32(0x1fffe8), u32(21)], // 230
	[u32(0x7ffff3), u32(23)], // 231
	[u32(0x3fffea), u32(22)], // 232
	[u32(0x3fffeb), u32(22)], // 233
	[u32(0x1ffffee), u32(25)], // 234
	[u32(0x1ffffef), u32(25)], // 235
	[u32(0xfffff4), u32(24)], // 236
	[u32(0xfffff5), u32(24)], // 237
	[u32(0x3ffffea), u32(26)], // 238
	[u32(0x7ffff4), u32(23)], // 239
	[u32(0x3ffffeb), u32(26)], // 240
	[u32(0x7ffffe6), u32(27)], // 241
	[u32(0x3ffffec), u32(26)], // 242
	[u32(0x3ffffed), u32(26)], // 243
	[u32(0x7ffffe7), u32(27)], // 244
	[u32(0x7ffffe8), u32(27)], // 245
	[u32(0x7ffffe9), u32(27)], // 246
	[u32(0x7ffffea), u32(27)], // 247
	[u32(0x7ffffeb), u32(27)], // 248
	[u32(0xffffffe), u32(28)], // 249
	[u32(0x7ffffec), u32(27)], // 250
	[u32(0x7ffffed), u32(27)], // 251
	[u32(0x7ffffee), u32(27)], // 252
	[u32(0x7ffffef), u32(27)], // 253
	[u32(0x7fffff0), u32(27)], // 254
	[u32(0x3ffffee), u32(26)], // 255
]

// huffman_decode decodes a Huffman-encoded byte slice using the RFC 7541 table.
// Remaining padding bits (up to 7) must be all 1s per RFC 7541 Section 5.2.
fn huffman_decode(data []u8) ![]u8 {
	if data.len == 0 {
		return []u8{}
	}

	mut result := []u8{cap: data.len * 2}
	mut code := u32(0)
	mut code_len := u32(0)

	for b in data {
		for bit_pos := 7; bit_pos >= 0; bit_pos-- {
			code = (code << 1) | u32((b >> bit_pos) & 1)
			code_len++

			// Minimum code length in RFC 7541 table is 5 bits
			if code_len < 5 {
				continue
			}

			mut found := false
			for sym := 0; sym < 256; sym++ {
				if qpack_huffman_table[sym][1] == code_len && qpack_huffman_table[sym][0] == code {
					result << u8(sym)
					code = 0
					code_len = 0
					found = true
					break
				}
			}
			if found {
				continue
			}

			// Maximum code length in RFC 7541 table is 30 bits
			if code_len > 30 {
				return error('invalid Huffman code: no match at ${code_len} bits')
			}
		}
	}

	// Remaining bits must be EOS padding (all 1s), at most 7 bits
	if code_len > 7 {
		return error('invalid Huffman padding: ${code_len} bits remaining')
	}
	if code_len > 0 {
		padding_mask := (u32(1) << code_len) - 1
		if (code & padding_mask) != padding_mask {
			return error('invalid Huffman padding bits')
		}
	}

	return result
}

// decode_qpack_string decodes a QPACK string literal with 7-bit length prefix.
// If the Huffman flag (MSB of the first byte) is set, the payload is Huffman
// decoded using the RFC 7541/RFC 9204 static table.
fn decode_qpack_string(data []u8) !(string, int) {
	if data.len == 0 {
		return error('No data to decode string')
	}

	is_huffman := (data[0] & 0x80) != 0
	length, hdr_bytes := decode_prefixed_integer(data, 7)!

	end := hdr_bytes + length
	if end > data.len {
		return error('String length exceeds data length')
	}

	payload := data[hdr_bytes..end]

	if is_huffman {
		decoded := huffman_decode(payload)!
		return decoded.bytestr(), end
	}

	return payload.bytestr(), end
}
