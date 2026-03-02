// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

// Test for HPACK header compression

fn test_encode_decode_integer() {
	// Test small integer (< max_prefix)
	encoded := encode_hpack_integer(10, 5)
	decoded, bytes_read := decode_integer(encoded, 5) or {
		assert false, 'Failed to decode integer'
		return
	}
	assert decoded == 10
	assert bytes_read == 1

	// Test large integer (>= max_prefix)
	encoded2 := encode_hpack_integer(1337, 5)
	decoded2, bytes_read2 := decode_integer(encoded2, 5) or {
		assert false, 'Failed to decode large integer'
		return
	}
	assert decoded2 == 1337
	assert bytes_read2 > 1
}

fn test_encode_decode_string() {
	test_str := 'www.example.com'
	encoded := encode_string(test_str, false)
	decoded, bytes_read := decode_string(encoded) or {
		assert false, 'Failed to decode string'
		return
	}
	assert decoded == test_str
	assert bytes_read == encoded.len
}

fn test_static_table() {
	// Test that static table is properly initialized
	assert static_table.len > 0
	assert static_table[1].name == ':authority'
	assert static_table[2].name == ':method'
	assert static_table[2].value == 'GET'
}

fn test_dynamic_table() {
	mut dt := DynamicTable{}

	field := HeaderField{'custom-header', 'custom-value'}
	dt.add(field)

	retrieved := dt.get(1) or {
		assert false, 'Failed to get from dynamic table'
		return
	}

	assert retrieved.name == field.name
	assert retrieved.value == field.value
}

fn test_dynamic_table_eviction() {
	mut dt := DynamicTable{
		max_size: 100
	}

	// Add entries until eviction occurs
	for i in 0 .. 10 {
		field := HeaderField{'header-${i}', 'value-${i}'}
		dt.add(field)
	}

	// Table size should not exceed max_size
	assert dt.size <= dt.max_size
}

fn test_encoder_decoder() {
	mut encoder := new_encoder()
	mut decoder := new_decoder()

	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
		HeaderField{':scheme', 'https'},
		HeaderField{'custom-header', 'custom-value'},
	]

	encoded := encoder.encode(headers)
	decoded := decoder.decode(encoded) or {
		assert false, 'Failed to decode headers'
		return
	}

	assert decoded.len == headers.len
	for i, header in headers {
		assert decoded[i].name == header.name
		assert decoded[i].value == header.value
	}
}

fn test_indexed_header() {
	mut encoder := new_encoder()
	mut decoder := new_decoder()

	// Use static table entry
	headers := [
		HeaderField{':method', 'GET'}, // Index 2 in static table
	]

	encoded := encoder.encode(headers)

	// Indexed header should be compact (1-2 bytes)
	assert encoded.len <= 2

	decoded := decoder.decode(encoded) or {
		assert false, 'Failed to decode indexed header'
		return
	}

	assert decoded.len == 1
	assert decoded[0].name == ':method'
	assert decoded[0].value == 'GET'
}

// Issue #12: RFC 7541 §6.2.3 — Literal Header Field Never Indexed
fn test_decode_never_indexed_literal_new_name() {
	mut decoder := new_decoder()

	// Manually craft a "never indexed" header with new name (index=0, prefix=4-bit):
	//   0x10 = 0001 0000  → never indexed, index=0 (new name)
	// Then two literal strings: name and value (non-Huffman, 1-byte length prefix)
	name := 'x-secret'
	value := 'top-secret'
	mut data := []u8{}
	data << u8(0x10) // never indexed, new name
	data << u8(name.len)
	data << name.bytes()
	data << u8(value.len)
	data << value.bytes()

	headers := decoder.decode(data) or {
		assert false, 'Failed to decode never-indexed header: ${err}'
		return
	}

	assert headers.len == 1
	assert headers[0].name == name
	assert headers[0].value == value
	// Field must NOT be added to the dynamic table
	assert decoder.dynamic_table.entries.len == 0
}

// Issue #12: Never indexed with indexed name reference (static table)
fn test_decode_never_indexed_indexed_name() {
	mut decoder := new_decoder()

	// 0x10 | 2 = 0x12 → never indexed, name from static table index 2 (:method)
	value := 'DELETE'
	mut data := []u8{}
	data << u8(0x12) // never indexed, name index=2 (:method)
	data << u8(value.len)
	data << value.bytes()

	headers := decoder.decode(data) or {
		assert false, 'Failed to decode never-indexed header with indexed name: ${err}'
		return
	}

	assert headers.len == 1
	assert headers[0].name == ':method'
	assert headers[0].value == value
	// Must NOT be added to dynamic table
	assert decoder.dynamic_table.entries.len == 0
}

// Issue #13: DynamicTable.add must evict entries when new entry alone exceeds max_size
fn test_dynamic_table_add_entry_larger_than_max_size() {
	mut dt := DynamicTable{
		max_size: 50
	}

	// A header whose size (32 + name.len + value.len) > 50
	big_field := HeaderField{'big-name', 'big-value-that-overflows-max'}
	// big_field.size() = 32 + 8 + 28 = 68 > 50
	dt.add(big_field)

	// Table must be empty — the entry is too large to fit
	assert dt.entries.len == 0
	assert dt.size == 0
}

// Issue #13: Eviction order — oldest (end) entries evicted first
fn test_dynamic_table_eviction_order() {
	mut dt := DynamicTable{
		max_size: 200
	}

	// Each entry: 32 + 6 + 1 = 39 bytes
	dt.add(HeaderField{'first!', '1'})
	dt.add(HeaderField{'secnd!', '2'})
	dt.add(HeaderField{'third!', '3'})
	dt.add(HeaderField{'fourt!', '4'})
	dt.add(HeaderField{'fifth!', '5'})
	// 5 * 39 = 195 bytes — fits within 200

	// Now add a 6th — total would be 234, need to evict 1 oldest (first!)
	dt.add(HeaderField{'sixth!', '6'})

	// 6 entries would be 234 > 200, so oldest must be gone
	assert dt.size <= dt.max_size
	// Newest (sixth!) must be at index 1
	newest := dt.get(1) or {
		assert false, 'Could not get newest entry'
		return
	}
	assert newest.name == 'sixth!'
	// Oldest (first!) must have been evicted — index 5 should be second-oldest
	oldest := dt.get(dt.entries.len) or {
		assert false, 'Could not get oldest remaining entry'
		return
	}
	assert oldest.name != 'first!', 'first! should have been evicted'
}
