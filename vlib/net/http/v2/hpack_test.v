// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

// Test for HPACK header compression

fn test_encode_decode_integer() {
	// Test small integer (< max_prefix)
	encoded := encode_integer(10, 5)
	decoded, bytes_read := decode_integer(encoded, 5) or {
		assert false, 'Failed to decode integer'
		return
	}
	assert decoded == 10
	assert bytes_read == 1

	// Test large integer (>= max_prefix)
	encoded2 := encode_integer(1337, 5)
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
