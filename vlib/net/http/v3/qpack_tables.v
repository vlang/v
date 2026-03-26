// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

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

// DynamicTable manages the dynamic table for QPACK.
// QPACK uses absolute indexing per RFC 9204 §3.2: entries are appended to the
// back of the array (newest at the highest absolute index). References use
// absolute or relative indices, unlike HPACK (HTTP/2) which inserts newest
// entries at index 0 (LIFO ordering per RFC 7541 §2.3.3).
//
// Internally uses a ring buffer for O(1) eviction instead of O(n) array shifts.
struct DynamicTable {
mut:
	entries      []DynamicTableEntry
	head         int // index of the oldest entry in the ring buffer
	count        int // number of valid entries currently stored
	size         int
	max_size     int
	insert_count u64
}

fn new_dynamic_table(max_size int) DynamicTable {
	cap := max_entries(max_size)
	return DynamicTable{
		entries:      []DynamicTableEntry{len: cap}
		head:         0
		count:        0
		size:         0
		max_size:     max_size
		insert_count: 0
	}
}

// insert adds a header field to the dynamic table, evicting oldest entries as
// needed. Uses ring buffer indexing for O(1) eviction.
fn (mut dt DynamicTable) insert(field HeaderField) {
	entry_size := field.name.len + field.value.len + 32
	cap := dt.entries.len

	if cap == 0 || entry_size > dt.max_size {
		return
	}

	// Evict oldest entries until there is room
	for dt.size + entry_size > dt.max_size && dt.count > 0 {
		dt.size -= dt.entries[dt.head].size
		dt.head = (dt.head + 1) % cap
		dt.count--
	}

	// Add new entry at the tail of the ring buffer
	tail := (dt.head + dt.count) % cap
	dt.entries[tail] = DynamicTableEntry{
		field: field
		size:  entry_size
	}
	dt.count++
	dt.size += entry_size
	dt.insert_count++
}

// get returns the entry at the given relative index (newest = 0).
fn (dt &DynamicTable) get(index int) ?HeaderField {
	if index < 0 || index >= dt.count {
		return none
	}
	cap := dt.entries.len
	actual_idx := (dt.head + dt.count - 1 - index) % cap
	return dt.entries[actual_idx].field
}

// get_by_absolute returns the entry at the given absolute index.
// The absolute index is the insertion-order index assigned when an entry was added.
fn (dt &DynamicTable) get_by_absolute(abs_index int) ?HeaderField {
	first_abs := int(dt.insert_count) - dt.count
	j := abs_index - first_abs
	if j < 0 || j >= dt.count {
		return none
	}
	cap := dt.entries.len
	actual_idx := (dt.head + j) % cap
	return dt.entries[actual_idx].field
}
