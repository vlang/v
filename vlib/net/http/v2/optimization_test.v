module v2

// Tests for encode_optimized: indexed lookups, dynamic table updates, and Huffman encoding.

fn test_encode_optimized_small_index_single_byte() {
	mut encoder := new_encoder()
	mut buf := []u8{len: 4096}

	headers := [HeaderField{':method', 'GET'}]
	n := encoder.encode_optimized(headers, mut buf)
	assert n == 1, ':method GET (index 2) should encode to exactly 1 byte, got ${n}'
	assert buf[0] == u8(0x82), 'expected 0x82, got 0x${buf[0].hex()}'
}

fn test_encode_optimized_updates_dynamic_table() {
	mut encoder := new_encoder()
	mut buf := []u8{len: 4096}

	headers := [HeaderField{'x-custom', 'my-value'}]
	n1 := encoder.encode_optimized(headers, mut buf)
	assert n1 > 1, 'first encoding of new header should be a literal (>1 byte)'

	n2 := encoder.encode_optimized(headers, mut buf)
	assert n2 < n1, 'second encoding should be shorter (indexed), got n1=${n1} n2=${n2}'
}

fn test_encode_optimized_high_index_multi_byte() {
	mut encoder := new_encoder()
	mut buf := []u8{len: 16384}

	for i in 0 .. 70 {
		filler := [HeaderField{'x-fill-${i}', 'v-${i}'}]
		encoder.encode_optimized(filler, mut buf)
	}

	target := [HeaderField{'x-fill-0', 'v-0'}]
	n := encoder.encode_optimized(target, mut buf)
	assert n > 0, 'encode_optimized must write at least one byte'
	assert n > 1, 'index 131 must be encoded with multi-byte HPACK integer (got ${n} byte(s))'
	assert buf[0] == 0xff, 'first byte for index >= 128 must be 0xFF (prefix saturated), got 0x${buf[0].hex()}'
}

fn test_encode_optimized_huffman_shorter() {
	mut encoder := new_encoder()
	mut buf_opt := []u8{len: 4096}
	mut buf_std := []u8{len: 4096}

	headers := [HeaderField{'content-type', 'application/json'}]

	n_opt := encoder.encode_optimized(headers, mut buf_opt)
	assert n_opt > 0, 'encode_optimized must produce output'

	if n_opt > 1 {
		value_len_byte := buf_opt[1]
		huffman_bit := (value_len_byte & 0x80) != 0
		assert huffman_bit, 'expected Huffman bit set on value string length byte, got 0x${value_len_byte.hex()}'
	}

	mut decoder := new_decoder()
	encoded := buf_opt[..n_opt].clone()
	decoded := decoder.decode(encoded) or {
		assert false, 'HPACK decode failed on Huffman-encoded output: ${err}'
		return
	}
	assert decoded.len == 1
	assert decoded[0].name == 'content-type'
	assert decoded[0].value == 'application/json'
}

fn test_encode_optimized_huffman_newname() {
	mut encoder := new_encoder()
	mut decoder := new_decoder()
	mut buf := []u8{len: 4096}

	headers := [HeaderField{'x-trace-id', 'abc123def456'}]
	n := encoder.encode_optimized(headers, mut buf)
	assert n > 0, 'encode_optimized must produce output'

	encoded := buf[..n].clone()
	mut dec := new_decoder()
	decoded := dec.decode(encoded) or {
		assert false, 'HPACK decode failed on new-name Huffman output: ${err}'
		return
	}
	assert decoded.len == 1
	assert decoded[0].name == 'x-trace-id'
	assert decoded[0].value == 'abc123def456'
}

fn test_encode_optimized_static_name_only_match() {
	mut encoder := new_encoder()
	mut decoder := new_decoder()
	mut buf := []u8{len: 4096}

	headers := [HeaderField{':method', 'PATCH'}]
	n := encoder.encode_optimized(headers, mut buf)
	assert n > 0, 'must produce output for :method PATCH'
	assert buf[0] == 0x42, 'expected literal+indexed-name byte 0x42, got 0x${buf[0].hex()}'

	encoded := buf[..n].clone()
	decoded := decoder.decode(encoded) or {
		assert false, 'HPACK decode failed: ${err}'
		return
	}
	assert decoded.len == 1
	assert decoded[0].name == ':method'
	assert decoded[0].value == 'PATCH'
}

fn test_encode_optimized_mixed_match_types() {
	mut encoder := new_encoder()
	mut decoder := new_decoder()
	mut buf := []u8{len: 4096}

	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':status', '201'},
		HeaderField{'x-request-id', 'abc'},
		HeaderField{':path', '/api/v1'},
		HeaderField{':scheme', 'https'},
	]

	n := encoder.encode_optimized(headers, mut buf)
	assert n > 0

	encoded := buf[..n].clone()
	decoded := decoder.decode(encoded) or {
		assert false, 'HPACK decode failed: ${err}'
		return
	}
	assert decoded.len == headers.len, 'header count mismatch: want ${headers.len}, got ${decoded.len}'
	for i, h in headers {
		assert decoded[i].name == h.name, 'name mismatch at ${i}: want ${h.name}, got ${decoded[i].name}'
		assert decoded[i].value == h.value, 'value mismatch at ${i}: want ${h.value}, got ${decoded[i].value}'
	}
}

fn test_encode_optimized_result_decodable() {
	mut encoder := new_encoder()
	mut decoder := new_decoder()
	mut buf := []u8{len: 4096}

	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
		HeaderField{':scheme', 'https'},
		HeaderField{'x-custom', 'hello'},
	]

	n := encoder.encode_optimized(headers, mut buf)
	assert n > 0, 'encode_optimized must write bytes'

	encoded := buf[..n].clone()
	decoded := decoder.decode(encoded) or {
		assert false, 'HPACK decode failed: ${err}'
		return
	}

	assert decoded.len == headers.len, 'decoded header count mismatch: want ${headers.len}, got ${decoded.len}'
	for i, h in headers {
		assert decoded[i].name == h.name, 'name mismatch at ${i}: want ${h.name}, got ${decoded[i].name}'
		assert decoded[i].value == h.value, 'value mismatch at ${i}: want ${h.value}, got ${decoded[i].value}'
	}
}
