module v3

// Tests for QPACK encoding/decoding and varint codec.

fn test_qpack_encoding_decoding() {
	mut encoder := new_qpack_encoder(4096, 0)
	mut decoder := new_qpack_decoder(4096, 0)

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
			value: '/index.html'
		},
		HeaderField{
			name:  'content-type'
			value: 'text/html'
		},
	]

	println('Testing QPACK encoding...')
	encoded := encoder.encode(headers)
	assert encoded.len > 0

	println('Testing QPACK decoding...')
	decoded := decoder.decode(encoded) or {
		println('Decode Error: ${err}')
		assert false
		return
	}

	assert decoded.len == headers.len
	for i in 0 .. headers.len {
		assert decoded[i].name == headers[i].name
		assert decoded[i].value == headers[i].value
	}
	println('QPACK test passed')
}

fn test_varint_encoding_decoding() {
	println('Testing VarInt encoding/decoding...')

	cases := {
		u64(25):                 1
		u64(15293):              2
		u64(494878333):          4
		u64(151288809941952652): 8
	}

	for val, expected_len in cases {
		encoded := encode_varint(val) or {
			assert false, 'Failed to encode varint'
			return
		}
		assert encoded.len == expected_len

		decoded, bytes_read := decode_varint(encoded) or {
			assert false, 'Failed to decode varint'
			return
		}
		assert decoded == val
		assert bytes_read == expected_len
	}
	println('VarInt test passed')
}

fn test_header_helpers() {
	println('Testing header helpers...')

	mut encoder := new_qpack_encoder(4096, 100)
	mut decoder := new_qpack_decoder(4096, 100)

	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  'content-type'
			value: 'application/json'
		},
	]

	encoded := encoder.encode(headers)
	assert encoded.len > 0

	decoded := decoder.decode(encoded) or {
		assert false, 'Failed to decode headers'
		return
	}

	assert decoded.len == headers.len
	assert decoded[0].name == ':method'
	assert decoded[1].value == 'application/json'
	println('Header helpers test passed')
}
