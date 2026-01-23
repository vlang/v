// QPACK Header Compression Tests
module v3

fn test_qpack_static_table_indexed() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	// Test encoding :method GET (static table index 17)
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
	]

	encoded := encoder.encode(headers)
	assert encoded.len > 0

	decoded := decoder.decode(encoded) or {
		assert false, 'Decoding failed: ${err}'
		return
	}

	assert decoded.len == 1
	assert decoded[0].name == ':method'
	assert decoded[0].value == 'GET'

	println('✓ QPACK static table indexed test passed')
}

fn test_qpack_literal_with_name_ref() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	// Test encoding :path with custom value
	headers := [
		HeaderField{
			name:  ':path'
			value: '/api/v1/users'
		},
	]

	encoded := encoder.encode(headers)
	assert encoded.len > 0

	decoded := decoder.decode(encoded) or {
		assert false, 'Decoding failed: ${err}'
		return
	}

	assert decoded.len == 1
	assert decoded[0].name == ':path'
	assert decoded[0].value == '/api/v1/users'

	println('✓ QPACK literal with name reference test passed')
}

fn test_qpack_literal_without_name_ref() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	// Test encoding custom header
	headers := [
		HeaderField{
			name:  'x-custom-header'
			value: 'custom-value'
		},
	]

	encoded := encoder.encode(headers)
	assert encoded.len > 0

	decoded := decoder.decode(encoded) or {
		assert false, 'Decoding failed: ${err}'
		return
	}

	assert decoded.len == 1
	assert decoded[0].name == 'x-custom-header'
	assert decoded[0].value == 'custom-value'

	println('✓ QPACK literal without name reference test passed')
}

fn test_qpack_multiple_headers() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	// Test encoding multiple headers
	headers := [
		HeaderField{
			name:  ':method'
			value: 'POST'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  ':path'
			value: '/api/data'
		},
		HeaderField{
			name:  ':authority'
			value: 'example.com'
		},
		HeaderField{
			name:  'content-type'
			value: 'application/json'
		},
		HeaderField{
			name:  'user-agent'
			value: 'V-HTTP3-Client/1.0'
		},
	]

	encoded := encoder.encode(headers)
	assert encoded.len > 0

	decoded := decoder.decode(encoded) or {
		assert false, 'Decoding failed: ${err}'
		return
	}

	assert decoded.len == 6
	assert decoded[0].name == ':method'
	assert decoded[0].value == 'POST'
	assert decoded[3].name == ':authority'
	assert decoded[3].value == 'example.com'

	println('✓ QPACK multiple headers test passed')
}

fn test_qpack_compression_ratio() {
	mut encoder := new_qpack_encoder(4096, 100)

	// Test compression efficiency
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  ':authority'
			value: 'example.com'
		},
		HeaderField{
			name:  'accept'
			value: '*/*'
		},
		HeaderField{
			name:  'accept-encoding'
			value: 'gzip, deflate, br'
		},
		HeaderField{
			name:  'user-agent'
			value: 'V-HTTP3-Client/1.0'
		},
	]

	// Calculate original size
	mut original_size := 0
	for header in headers {
		original_size += header.name.len + header.value.len + 2 // +2 for ": "
	}

	encoded := encoder.encode(headers)
	compressed_size := encoded.len

	compression_ratio := f64(original_size) / f64(compressed_size)

	println('Original size: ${original_size} bytes')
	println('Compressed size: ${compressed_size} bytes')
	println('Compression ratio: ${compression_ratio:.2f}x')

	// Should achieve at least 2x compression
	assert compression_ratio > 2.0

	println('✓ QPACK compression ratio test passed')
}

fn test_qpack_empty_headers() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	headers := []HeaderField{}

	encoded := encoder.encode(headers)
	assert encoded.len == 2 // Just the prefix bytes

	decoded := decoder.decode(encoded) or {
		assert false, 'Decoding failed: ${err}'
		return
	}

	assert decoded.len == 0

	println('✓ QPACK empty headers test passed')
}

fn test_qpack_large_header_value() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	// Test with large header value
	large_value := 'x'.repeat(1000)
	headers := [
		HeaderField{
			name:  'x-large-header'
			value: large_value
		},
	]

	encoded := encoder.encode(headers)
	assert encoded.len > 1000

	decoded := decoder.decode(encoded) or {
		assert false, 'Decoding failed: ${err}'
		return
	}

	assert decoded.len == 1
	assert decoded[0].name == 'x-large-header'
	assert decoded[0].value == large_value

	println('✓ QPACK large header value test passed')
}

fn test_qpack_special_characters() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	// Test with special characters
	headers := [
		HeaderField{
			name:  'x-test'
			value: 'value with spaces'
		},
		HeaderField{
			name:  'x-unicode'
			value: 'Hello 世界 🌍'
		},
	]

	encoded := encoder.encode(headers)
	assert encoded.len > 0

	decoded := decoder.decode(encoded) or {
		assert false, 'Decoding failed: ${err}'
		return
	}

	assert decoded.len == 2
	assert decoded[0].value == 'value with spaces'
	assert decoded[1].value == 'Hello 世界 🌍'

	println('✓ QPACK special characters test passed')
}
