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

	field := HeaderField{
		name:  'custom-header'
		value: 'custom-value'
	}
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
		field := HeaderField{
			name:  'header-${i}'
			value: 'value-${i}'
		}
		dt.add(field)
	}

	assert dt.size <= dt.max_size
}

fn test_encoder_decoder() {
	mut encoder := new_encoder()
	mut decoder := new_decoder()

	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  'custom-header'
			value: 'custom-value'
		},
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
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
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

	big_field := HeaderField{
		name:  'big-name'
		value: 'big-value-that-overflows-max'
	}
	dt.add(big_field)

	assert dt.entries.len == 0
	assert dt.size == 0
}

fn test_dynamic_table_eviction_order() {
	mut dt := DynamicTable{
		max_size: 200
	}

	dt.add(HeaderField{ name: 'first!', value: '1' })
	dt.add(HeaderField{ name: 'secnd!', value: '2' })
	dt.add(HeaderField{ name: 'third!', value: '3' })
	dt.add(HeaderField{ name: 'fourt!', value: '4' })
	dt.add(HeaderField{ name: 'fifth!', value: '5' })

	dt.add(HeaderField{ name: 'sixth!', value: '6' })

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

fn test_decoder_max_header_list_size_enforced() {
	mut encoder := new_encoder()
	headers := [
		HeaderField{
			name:  'x-large-header'
			value: 'a'.repeat(100)
		},
		HeaderField{
			name:  'x-another-header'
			value: 'b'.repeat(100)
		},
	]
	encoded := encoder.encode(headers)

	// Per RFC 7541 §4.1: size = name.len + value.len + 32 per field
	// field 1: 14 + 100 + 32 = 146, field 2: 16 + 100 + 32 = 148, total: 294
	mut decoder := new_decoder_with_limit(200)
	decoder.decode(encoded) or {
		assert err.msg().contains('header list size')
		return
	}
	assert false, 'expected error for exceeding max_header_list_size'
}

fn test_decoder_max_header_list_size_unlimited() {
	mut encoder := new_encoder()
	headers := [
		HeaderField{
			name:  'x-large-header'
			value: 'a'.repeat(100)
		},
		HeaderField{
			name:  'x-another-header'
			value: 'b'.repeat(100)
		},
	]
	encoded := encoder.encode(headers)

	mut decoder := new_decoder()
	decoded := decoder.decode(encoded) or {
		assert false, 'default decoder should not limit header size: ${err}'
		return
	}
	assert decoded.len == headers.len
}

fn test_decoder_max_header_list_size_exact_boundary() {
	mut encoder := new_encoder()
	headers := [HeaderField{
		name:  'name'
		value: 'value'
	}]
	encoded := encoder.encode(headers)

	// Per RFC 7541 §4.1: size = 4 + 5 + 32 = 41
	mut decoder := new_decoder_with_limit(41)
	decoded := decoder.decode(encoded) or {
		assert false, 'headers at exact limit should pass: ${err}'
		return
	}
	assert decoded.len == 1
	assert decoded[0].name == 'name'
	assert decoded[0].value == 'value'
}

fn test_encoder_table_size_update_emitted() {
	mut encoder := new_encoder()
	encoder.set_max_table_size(2048)

	headers := [HeaderField{
		name:  ':method'
		value: 'GET'
	}]
	encoded := encoder.encode(headers)

	// First byte(s) must be dynamic table size update: 001xxxxx prefix
	assert encoded.len > 0
	assert (encoded[0] & 0xe0) == 0x20, 'first byte should have 001xxxxx pattern for table size update, got 0x${encoded[0]:02x}'

	// After encoding, pending update should be consumed
	encoded2 := encoder.encode(headers)
	assert (encoded2[0] & 0xe0) != 0x20, 'second encode should not emit table size update'
}

fn test_encoder_table_size_update_decoded() {
	mut encoder := new_encoder()
	encoder.set_max_table_size(2048)

	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
	]
	encoded := encoder.encode(headers)

	mut decoder := new_decoder()
	decoded := decoder.decode(encoded) or {
		assert false, 'failed to decode headers with table size update: ${err}'
		return
	}
	assert decoded.len == headers.len
	assert decoded[0].name == ':method'
	assert decoded[0].value == 'GET'
	assert decoded[1].name == ':path'
	assert decoded[1].value == '/'
	assert decoder.dynamic_table.max_size == 2048
}
