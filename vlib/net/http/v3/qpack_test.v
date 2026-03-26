module v3

// Tests for QPACK header compression, stream instructions, Huffman encoding, and dynamic table.

fn test_qpack_static_table_indexed() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

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

	mut original_size := 0
	for header in headers {
		original_size += header.name.len + header.value.len + 2
	}

	encoded := encoder.encode(headers)
	compressed_size := encoded.len

	compression_ratio := f64(original_size) / f64(compressed_size)

	println('Original size: ${original_size} bytes')
	println('Compressed size: ${compressed_size} bytes')
	println('Compression ratio: ${compression_ratio:.2f}x')

	assert compression_ratio > 2.0

	println('✓ QPACK compression ratio test passed')
}

fn test_qpack_empty_headers() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	headers := []HeaderField{}

	encoded := encoder.encode(headers)
	assert encoded.len == 2

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

	large_value := 'x'.repeat(1000)
	headers := [
		HeaderField{
			name:  'x-large-header'
			value: large_value
		},
	]

	encoded := encoder.encode(headers)
	assert encoded.len > 800

	decoded := decoder.decode(encoded) or {
		assert false, 'Decoding failed: ${err}'
		return
	}

	assert decoded.len == 1
	assert decoded[0].name == 'x-large-header'
	assert decoded[0].value == large_value

	println('✓ QPACK large header value test passed')
}

fn test_ric_zero_for_static_only() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	headers := [HeaderField{
		name:  ':method'
		value: 'GET'
	}]
	encoded := encoder.encode(headers)

	assert encoded[0] == 0x00
	assert encoded[1] == 0x00

	decoded := decoder.decode(encoded) or {
		assert false, 'Decoding failed: ${err}'
		return
	}
	assert decoded.len == 1
	assert decoded[0].name == ':method'
	assert decoded[0].value == 'GET'
}

fn test_ric_nonzero_with_dynamic_reference() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	h := [HeaderField{
		name:  'x-test'
		value: 'abc'
	}]
	enc1 := encoder.encode(h)
	_ := decoder.decode(enc1) or {
		assert false, 'First decode failed: ${err}'
		return
	}

	enc2 := encoder.encode(h)

	assert enc2[0] == 0x02
	assert enc2[1] == 0x00

	decoded := decoder.decode(enc2) or {
		assert false, 'Second decode failed: ${err}'
		return
	}
	assert decoded.len == 1
	assert decoded[0].name == 'x-test'
	assert decoded[0].value == 'abc'
}

fn test_ric_delta_base_positive() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	h1 := [HeaderField{
		name:  'x-a'
		value: 'a1'
	}]
	h2 := [HeaderField{
		name:  'x-b'
		value: 'b1'
	}]
	h3 := [HeaderField{
		name:  'x-c'
		value: 'c1'
	}]

	for h in [h1, h2, h3] {
		enc := encoder.encode(h)
		_ := decoder.decode(enc) or {
			assert false, 'Initial decode failed: ${err}'
			return
		}
	}

	enc := encoder.encode(h1)

	assert enc[0] == 0x02
	assert enc[1] == 0x02

	decoded := decoder.decode(enc) or {
		assert false, 'Reference decode failed: ${err}'
		return
	}
	assert decoded.len == 1
	assert decoded[0].name == 'x-a'
	assert decoded[0].value == 'a1'
}

fn test_section_prefix_roundtrip() {
	cases := [
		[0, 0, 4096],
		[1, 1, 4096],
		[1, 5, 4096],
		[10, 10, 4096],
		[5, 20, 4096],
		[1, 1, 256],
	]

	for tc in cases {
		ric := tc[0]
		base := tc[1]
		max_cap := tc[2]

		encoded := encode_section_prefix(ric, base, max_cap)
		dec_ric, dec_base, _ := decode_section_prefix(encoded, max_cap, base) or {
			assert false, 'Prefix decode failed for ric=${ric} base=${base}: ${err}'
			return
		}

		assert dec_ric == ric, 'RIC mismatch: expected ${ric}, got ${dec_ric}'
		assert dec_base == base, 'Base mismatch: expected ${base}, got ${dec_base}'
	}
}

fn test_qpack_special_characters() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

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

fn test_encoder_stream_insert_with_static_name_ref() {
	instr := InsertWithNameRef{
		is_static:  true
		name_index: 1
		value:      '/test'
	}
	encoded := instr.encode()
	decoded, bytes_read := decode_insert_with_name_ref(encoded) or {
		assert false, 'Decode failed: ${err}'
		return
	}
	assert bytes_read == encoded.len
	assert decoded.is_static == true
	assert decoded.name_index == 1
	assert decoded.value == '/test'
}

fn test_encoder_stream_insert_with_dynamic_name_ref() {
	instr := InsertWithNameRef{
		is_static:  false
		name_index: 3
		value:      'dynamic-val'
	}
	encoded := instr.encode()
	decoded, _ := decode_insert_with_name_ref(encoded) or {
		assert false, 'Decode failed: ${err}'
		return
	}
	assert decoded.is_static == false
	assert decoded.name_index == 3
	assert decoded.value == 'dynamic-val'
}

fn test_encoder_stream_insert_without_name_ref() {
	instr := InsertWithoutNameRef{
		name:  'x-custom'
		value: 'test-value'
	}
	encoded := instr.encode()
	decoded, bytes_read := decode_insert_without_name_ref(encoded) or {
		assert false, 'Decode failed: ${err}'
		return
	}
	assert bytes_read == encoded.len
	assert decoded.name == 'x-custom'
	assert decoded.value == 'test-value'
}

fn test_encoder_stream_duplicate() {
	instr := Duplicate{
		index: 5
	}
	encoded := instr.encode()
	decoded, bytes_read := decode_duplicate(encoded) or {
		assert false, 'Decode failed: ${err}'
		return
	}
	assert bytes_read == encoded.len
	assert decoded.index == 5
}

fn test_encoder_stream_set_capacity() {
	instr := SetDynamicTableCapacity{
		capacity: 4096
	}
	encoded := instr.encode()
	decoded, bytes_read := decode_set_dynamic_table_capacity(encoded) or {
		assert false, 'Decode failed: ${err}'
		return
	}
	assert bytes_read == encoded.len
	assert decoded.capacity == 4096
}

fn test_decoder_stream_section_ack() {
	instr := SectionAcknowledgment{
		stream_id: 42
	}
	encoded := instr.encode()
	decoded, bytes_read := decode_section_acknowledgment(encoded) or {
		assert false, 'Decode failed: ${err}'
		return
	}
	assert bytes_read == encoded.len
	assert decoded.stream_id == 42
}

fn test_decoder_stream_cancellation() {
	instr := StreamCancellation{
		stream_id: 7
	}
	encoded := instr.encode()
	decoded, bytes_read := decode_stream_cancellation(encoded) or {
		assert false, 'Decode failed: ${err}'
		return
	}
	assert bytes_read == encoded.len
	assert decoded.stream_id == 7
}

fn test_decoder_stream_insert_count_increment() {
	instr := InsertCountIncrement{
		increment: 3
	}
	encoded := instr.encode()
	decoded, bytes_read := decode_insert_count_increment(encoded) or {
		assert false, 'Decode failed: ${err}'
		return
	}
	assert bytes_read == encoded.len
	assert decoded.increment == 3
}

fn test_stream_instructions_large_values() {
	ack := SectionAcknowledgment{
		stream_id: 200
	}
	enc_ack := ack.encode()
	dec_ack, _ := decode_section_acknowledgment(enc_ack) or {
		assert false, 'Large stream_id decode failed: ${err}'
		return
	}
	assert dec_ack.stream_id == 200

	cap_instr := SetDynamicTableCapacity{
		capacity: 8192
	}
	enc_cap := cap_instr.encode()
	dec_cap, _ := decode_set_dynamic_table_capacity(enc_cap) or {
		assert false, 'Large capacity decode failed: ${err}'
		return
	}
	assert dec_cap.capacity == 8192
}

fn test_huffman_encoding_typical_header() {
	encoded := encode_qpack_string('www.example.com')
	assert (encoded[0] & 0x80) != 0

	decoded, _ := decode_qpack_string(encoded) or {
		assert false, 'Huffman decode failed: ${err}'
		return
	}
	assert decoded == 'www.example.com'
}

fn test_huffman_encoding_roundtrip() {
	test_strings := [
		'application/json',
		'text/html; charset=utf-8',
		'gzip, deflate, br',
		'/api/v1/users',
		'Mozilla/5.0',
	]

	for s in test_strings {
		encoded := encode_qpack_string(s)
		decoded, bytes_read := decode_qpack_string(encoded) or {
			assert false, 'Huffman roundtrip failed for "${s}": ${err}'
			return
		}
		assert decoded == s, 'Mismatch for "${s}": got "${decoded}"'
		assert bytes_read == encoded.len
	}
}

fn test_huffman_shorter_than_literal() {
	s := 'application/json'
	literal_len := s.len
	encoded := encode_qpack_string(s)
	is_huffman := (encoded[0] & 0x80) != 0
	assert is_huffman, 'Expected Huffman encoding for "${s}"'
	assert encoded.len < 1 + literal_len
}

fn test_huffman_full_encode_decode_roundtrip() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/api/v1/users'
		},
		HeaderField{
			name:  'content-type'
			value: 'application/json'
		},
	]

	encoded := encoder.encode(headers)
	decoded := decoder.decode(encoded) or {
		assert false, 'Full roundtrip decode failed: ${err}'
		return
	}

	assert decoded.len == headers.len
	for i, h in headers {
		assert decoded[i].name == h.name
		assert decoded[i].value == h.value
	}
}

fn test_blocked_stream_ric_exceeds_known() {
	mut decoder := new_qpack_decoder(4096, 100)
	mut data := encode_section_prefix(5, 5, 4096)
	data << u8(0xc0 | 17)

	result := decoder.decode(data) or {
		assert err.msg().contains('blocked')
		return
	}
	assert false, 'Expected blocked error, got ${result.len} headers'
}

fn test_blocked_stream_after_acknowledge() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	h := [HeaderField{
		name:  'x-test'
		value: 'val1'
	}]
	enc1 := encoder.encode(h)
	_ := decoder.decode(enc1) or {
		assert false, 'First decode failed: ${err}'
		return
	}

	enc2 := encoder.encode(h)

	decoder.acknowledge_insert(1)
	assert decoder.known_insert_count == 1

	decoded := decoder.decode(enc2) or {
		assert false, 'Post-acknowledge decode failed: ${err}'
		return
	}
	assert decoded.len == 1
	assert decoded[0].name == 'x-test'
	assert decoded[0].value == 'val1'
}

fn test_encoder_blocked_streams_limit() {
	mut encoder := new_qpack_encoder(4096, 2)
	mut decoder := new_qpack_decoder(4096, 2)

	h := [HeaderField{
		name:  'x-custom'
		value: 'v1'
	}]

	enc1 := encoder.encode(h)
	_ := decoder.decode(enc1) or {
		assert false, 'Decode 1 failed: ${err}'
		return
	}

	enc2 := encoder.encode(h)
	assert enc2[0] != 0x00

	enc3 := encoder.encode(h)
	assert enc3[0] != 0x00

	enc4 := encoder.encode(h)
	assert enc4[0] == 0x00

	encoder.acknowledge_stream()
	enc5 := encoder.encode(h)
	assert enc5[0] != 0x00
}

fn test_encoder_zero_max_blocked_forces_literal() {
	mut encoder := new_qpack_encoder(4096, 0)
	mut decoder := new_qpack_decoder(4096, 0)

	h := [HeaderField{
		name:  'x-custom'
		value: 'v1'
	}]

	enc1 := encoder.encode(h)
	_ := decoder.decode(enc1) or {
		assert false, 'Decode failed: ${err}'
		return
	}

	enc2 := encoder.encode(h)
	assert enc2[0] == 0x00

	decoded := decoder.decode(enc2) or {
		assert false, 'Literal decode failed: ${err}'
		return
	}
	assert decoded.len == 1
	assert decoded[0].name == 'x-custom'
	assert decoded[0].value == 'v1'
}

fn test_dynamic_table_eviction_correctness() {
	mut dt := new_dynamic_table(102)

	dt.insert(HeaderField{ name: 'a', value: '1' })
	dt.insert(HeaderField{ name: 'b', value: '2' })
	dt.insert(HeaderField{ name: 'c', value: '3' })
	dt.insert(HeaderField{ name: 'd', value: '4' })
	dt.insert(HeaderField{ name: 'e', value: '5' })

	assert dt.insert_count == 5

	e0 := dt.get(0) or {
		assert false, 'get(0) failed'
		return
	}
	assert e0.name == 'e'
	assert e0.value == '5'

	e1 := dt.get(1) or {
		assert false, 'get(1) failed'
		return
	}
	assert e1.name == 'd'
	assert e1.value == '4'

	e2 := dt.get(2) or {
		assert false, 'get(2) failed'
		return
	}
	assert e2.name == 'c'
	assert e2.value == '3'

	if _ := dt.get(3) {
		assert false, 'get(3) should fail for evicted entry'
	}

	if _ := dt.get_by_absolute(0) {
		assert false, 'abs 0 should be evicted'
	}
	if _ := dt.get_by_absolute(1) {
		assert false, 'abs 1 should be evicted'
	}

	a2 := dt.get_by_absolute(2) or {
		assert false, 'abs 2 failed'
		return
	}
	assert a2.name == 'c'

	a3 := dt.get_by_absolute(3) or {
		assert false, 'abs 3 failed'
		return
	}
	assert a3.name == 'd'

	a4 := dt.get_by_absolute(4) or {
		assert false, 'abs 4 failed'
		return
	}
	assert a4.name == 'e'
}

fn test_encoder_generates_instructions() {
	mut encoder := new_qpack_encoder(4096, 100)

	// Encoding headers with a non-static name triggers a dynamic table insert
	// which should also buffer the corresponding encoder stream instruction.
	headers := [
		HeaderField{
			name:  'x-custom-instr'
			value: 'some-value'
		},
	]
	_ = encoder.encode(headers)

	instructions := encoder.pending_instructions()
	assert instructions.len > 0, 'pending_instructions should be non-empty after dynamic table insert'
}

fn test_encoder_instructions_cleared_after_read() {
	mut encoder := new_qpack_encoder(4096, 100)

	headers := [
		HeaderField{
			name:  'x-clear-test'
			value: 'value1'
		},
	]
	_ = encoder.encode(headers)

	first := encoder.pending_instructions()
	assert first.len > 0, 'first call should return instructions'

	second := encoder.pending_instructions()
	assert second.len == 0, 'second call should return empty after first read'
}

fn test_decoder_pending_acknowledgments_after_decode() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
	]
	encoded := encoder.encode(headers)

	_ = decoder.decode(encoded) or {
		assert false, 'decode failed: ${err}'
		return
	}

	ack := decoder.pending_acknowledgments()
	// After decoding a header block, the decoder should have a section acknowledgment ready
	// (only if the block referenced the dynamic table, which this static-only case does not).
	// So for static-only, ack should be empty.
	assert ack.len == 0, 'static-only decode should produce no ack instructions'
}

fn test_decoder_pending_acknowledgments_with_dynamic() {
	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	// First encode inserts into dynamic table
	h := [HeaderField{
		name:  'x-ack-test'
		value: 'val1'
	}]
	enc1 := encoder.encode(h)
	_ = decoder.decode(enc1) or {
		assert false, 'First decode failed: ${err}'
		return
	}

	// Second encode references dynamic table (RIC > 0)
	enc2 := encoder.encode(h)
	_ = decoder.decode(enc2) or {
		assert false, 'Second decode failed: ${err}'
		return
	}

	ack := decoder.pending_acknowledgments()
	// With dynamic table reference, decoder should have buffered a section acknowledgment
	assert ack.len > 0, 'dynamic-table decode should produce ack instructions'

	ack2 := decoder.pending_acknowledgments()
	assert ack2.len == 0, 'ack should be cleared after first read'
}

fn test_dynamic_table_wraparound() {
	mut dt := new_dynamic_table(68)

	for i in 0 .. 10 {
		dt.insert(HeaderField{ name: '${i}', value: 'v' })
	}

	assert dt.insert_count == 10

	e0 := dt.get(0) or {
		assert false, 'get(0) failed'
		return
	}
	assert e0.name == '9'

	e1 := dt.get(1) or {
		assert false, 'get(1) failed'
		return
	}
	assert e1.name == '8'

	if _ := dt.get(2) {
		assert false, 'get(2) should fail — only 2 entries fit'
	}

	if _ := dt.get_by_absolute(7) {
		assert false, 'abs 7 should be evicted'
	}

	a8 := dt.get_by_absolute(8) or {
		assert false, 'abs 8 failed'
		return
	}
	assert a8.name == '8'

	a9 := dt.get_by_absolute(9) or {
		assert false, 'abs 9 failed'
		return
	}
	assert a9.name == '9'
}
