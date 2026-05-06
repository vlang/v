module v2

// Tests for HPACK never-indexed encoding (RFC 7541 §6.2.3).

fn test_encode_never_indexed_authorization() {
	mut encoder := new_encoder()
	headers := [HeaderField{
		name:  'authorization'
		value: 'Bearer token123'
	}]
	encoded := encoder.encode(headers)
	// Authorization is in never_index_names set.
	// First byte must have never-indexed prefix 0001xxxx (§6.2.3).
	assert encoded.len > 0
	assert (encoded[0] & 0xf0) == 0x10, 'expected never-indexed prefix 0001xxxx, got 0x${encoded[0]:02x}'
}

fn test_encode_never_indexed_cookie() {
	mut encoder := new_encoder()
	headers := [HeaderField{
		name:  'cookie'
		value: 'session=abc123'
	}]
	encoded := encoder.encode(headers)
	assert encoded.len > 0
	assert (encoded[0] & 0xf0) == 0x10, 'expected never-indexed prefix for cookie, got 0x${encoded[0]:02x}'
}

fn test_encode_never_indexed_explicit_sensitive() {
	mut encoder := new_encoder()
	headers := [
		HeaderField{
			name:      'x-custom-secret'
			value:     'secret-value'
			sensitive: true
		},
	]
	encoded := encoder.encode(headers)
	// sensitive=true forces never-indexed encoding even for non-default names.
	assert encoded.len > 0
	assert (encoded[0] & 0xf0) == 0x10, 'expected never-indexed prefix for sensitive header, got 0x${encoded[0]:02x}'
}

fn test_encode_never_indexed_not_in_dynamic_table() {
	mut encoder := new_encoder()
	headers := [HeaderField{
		name:  'authorization'
		value: 'Bearer token123'
	}]
	encoder.encode(headers)
	// Never-indexed headers must NOT be added to the dynamic table (§6.2.3).
	assert encoder.dynamic_table.entries.len == 0, 'never-indexed header should not be in dynamic table'
}

fn test_encode_normal_header_uses_incremental() {
	mut encoder := new_encoder()
	headers := [HeaderField{
		name:  'x-custom'
		value: 'value123'
	}]
	encoded := encoder.encode(headers)
	// Non-sensitive, non-default headers use literal with incremental indexing (01xxxxxx).
	assert encoded.len > 0
	assert (encoded[0] & 0xc0) == 0x40, 'expected incremental indexing prefix 01xxxxxx, got 0x${encoded[0]:02x}'
}

fn test_decode_roundtrip_never_indexed() {
	mut encoder := new_encoder()
	mut decoder := new_decoder()
	headers := [
		HeaderField{
			name:  'authorization'
			value: 'Bearer token123'
		},
		HeaderField{
			name:  'cookie'
			value: 'session=abc'
		},
		HeaderField{
			name:  'x-normal'
			value: 'normal-value'
		},
	]
	encoded := encoder.encode(headers)
	decoded := decoder.decode(encoded) or {
		assert false, 'failed to decode never-indexed roundtrip: ${err}'
		return
	}
	assert decoded.len == headers.len
	for i, h in headers {
		assert decoded[i].name == h.name, 'name mismatch at ${i}'
		assert decoded[i].value == h.value, 'value mismatch at ${i}'
	}
}
