module v2

// Tests for HPACK header compression encoding, decoding, and dynamic table management.

fn test_encode_decode_integer() {
	encoded := encode_hpack_integer(10, 5)
	decoded, bytes_read := decode_integer(encoded, 5) or {
		assert false, 'Failed to decode integer'
		return
	}
	assert decoded == 10
	assert bytes_read == 1

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

	for i in 0 .. 10 {
		field := HeaderField{'header-${i}', 'value-${i}'}
		dt.add(field)
	}

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

	headers := [
		HeaderField{':method', 'GET'},
	]

	encoded := encoder.encode(headers)
	assert encoded.len <= 2

	decoded := decoder.decode(encoded) or {
		assert false, 'Failed to decode indexed header'
		return
	}

	assert decoded.len == 1
	assert decoded[0].name == ':method'
	assert decoded[0].value == 'GET'
}

fn test_decode_never_indexed_literal_new_name() {
	mut decoder := new_decoder()

	name := 'x-secret'
	value := 'top-secret'
	mut data := []u8{}
	data << u8(0x10)
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
	assert decoder.dynamic_table.entries.len == 0
}

fn test_decode_never_indexed_indexed_name() {
	mut decoder := new_decoder()

	value := 'DELETE'
	mut data := []u8{}
	data << u8(0x12)
	data << u8(value.len)
	data << value.bytes()

	headers := decoder.decode(data) or {
		assert false, 'Failed to decode never-indexed header with indexed name: ${err}'
		return
	}

	assert headers.len == 1
	assert headers[0].name == ':method'
	assert headers[0].value == value
	assert decoder.dynamic_table.entries.len == 0
}

fn test_dynamic_table_add_entry_larger_than_max_size() {
	mut dt := DynamicTable{
		max_size: 50
	}

	big_field := HeaderField{'big-name', 'big-value-that-overflows-max'}
	dt.add(big_field)

	assert dt.entries.len == 0
	assert dt.size == 0
}

fn test_dynamic_table_eviction_order() {
	mut dt := DynamicTable{
		max_size: 200
	}

	dt.add(HeaderField{'first!', '1'})
	dt.add(HeaderField{'secnd!', '2'})
	dt.add(HeaderField{'third!', '3'})
	dt.add(HeaderField{'fourt!', '4'})
	dt.add(HeaderField{'fifth!', '5'})

	dt.add(HeaderField{'sixth!', '6'})

	assert dt.size <= dt.max_size
	newest := dt.get(1) or {
		assert false, 'Could not get newest entry'
		return
	}
	assert newest.name == 'sixth!'
	oldest := dt.get(dt.entries.len) or {
		assert false, 'Could not get oldest remaining entry'
		return
	}
	assert oldest.name != 'first!', 'first! should have been evicted'
}
