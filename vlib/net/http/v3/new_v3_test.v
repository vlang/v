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

fn test_goaway_stream_id_extraction() {
	// Build a GOAWAY frame: frame_type=0x07, payload=varint(stream_id=8)
	goaway_stream_id := u64(8)
	payload := encode_varint(goaway_stream_id) or {
		assert false, 'Failed to encode varint for GOAWAY'
		return
	}

	mut frame_data := []u8{}
	frame_data << encode_varint(u64(FrameType.goaway)) or {
		assert false, 'Failed to encode frame type'
		return
	}
	frame_data << encode_varint(u64(payload.len)) or {
		assert false, 'Failed to encode frame length'
		return
	}
	frame_data << payload

	mut encoder := new_qpack_encoder(4096, 0)
	mut decoder := new_qpack_decoder(4096, 0)

	// Build a HEADERS frame first so parse_response_frames has something before GOAWAY
	headers := [
		HeaderField{
			name:  ':status'
			value: '200'
		},
	]
	encoded_headers := encoder.encode(headers)
	mut all_data := []u8{}
	all_data << encode_varint(u64(FrameType.headers)) or {
		assert false, 'header frame type encode failed'
		return
	}
	all_data << encode_varint(u64(encoded_headers.len)) or {
		assert false, 'header frame len encode failed'
		return
	}
	all_data << encoded_headers
	all_data << frame_data

	// Create a minimal client for parse_response_frames
	mut client := Client{
		qpack_decoder: decoder
	}

	parsed_headers, _ := client.parse_response_frames(all_data) or {
		assert false, 'parse_response_frames failed: ${err}'
		return
	}

	assert parsed_headers.len == 1
	assert parsed_headers[0].name == ':status'

	// Verify the GOAWAY stream ID was extracted and stored
	assert client.last_peer_goaway_stream_id == goaway_stream_id
}

fn test_goaway_blocks_new_streams() {
	mut client := Client{
		last_peer_goaway_stream_id: 4
		next_stream_id:             8
	}

	// Attempting request with stream_id (8) > goaway limit (4) should fail
	client.request(Request{
		method: .get
		url:    '/'
		host:   'example.com'
	}) or {
		assert err.msg().contains('going away')
		return
	}
	assert false, 'expected goaway error'
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

fn test_large_body_chunking() {
	println('Testing large body chunking...')

	// Body that spans 2 full chunks + 1 partial chunk
	body_size := max_data_frame_size * 2 + 100
	body := []u8{len: body_size, init: u8(0x41)}.bytestr()

	frames := create_data_frames(body)

	// Expect 3 frames: 2 full (16384 bytes each) + 1 partial (100 bytes)
	assert frames.len == 3, 'expected 3 frames, got ${frames.len}'
	assert frames[0].payload.len == max_data_frame_size
	assert frames[0].length == u64(max_data_frame_size)
	assert frames[1].payload.len == max_data_frame_size
	assert frames[1].length == u64(max_data_frame_size)
	assert frames[2].payload.len == 100
	assert frames[2].length == u64(100)

	// All frames must be DATA type
	for f in frames {
		assert f.frame_type == .data
	}

	// Total payload must equal original body
	mut total := 0
	for f in frames {
		total += f.payload.len
	}
	assert total == body_size

	println('Large body chunking test passed')
}

fn test_small_body_no_chunking() {
	println('Testing small body produces single frame...')

	small_body := 'hello world'
	frames := create_data_frames(small_body)

	assert frames.len == 1
	assert frames[0].payload.len == small_body.len
	assert frames[0].frame_type == .data

	println('Small body no chunking test passed')
}

fn test_empty_body_data_frame() {
	println('Testing empty body produces empty DATA frame...')

	frames := create_data_frames('')

	assert frames.len == 1
	assert frames[0].payload.len == 0
	assert frames[0].length == u64(0)
	assert frames[0].frame_type == .data

	println('Empty body data frame test passed')
}

fn test_exact_chunk_boundary() {
	println('Testing body at exact chunk boundary...')

	body := []u8{len: max_data_frame_size * 2, init: u8(0x42)}.bytestr()
	frames := create_data_frames(body)

	// Exactly 2 full frames, no partial
	assert frames.len == 2
	assert frames[0].payload.len == max_data_frame_size
	assert frames[1].payload.len == max_data_frame_size

	println('Exact chunk boundary test passed')
}
