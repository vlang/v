module grpc

fn test_roundtrip() {
	payload := 'hello proto'.bytes()
	framed := encode_frame(payload, false)
	assert framed.len == frame_header_len + payload.len
	assert framed[0] == 0
	frame, n := decode_frame(framed)!
	assert n == framed.len
	assert !frame.compressed
	assert frame.payload == payload
}

fn test_compressed_flag() {
	framed := encode_frame([u8(1), 2, 3], true)
	assert framed[0] == 1
	frame, _ := decode_frame(framed)!
	assert frame.compressed
}

fn test_empty_payload() {
	framed := encode_frame([]u8{}, false)
	assert framed.len == frame_header_len
	frame, n := decode_frame(framed)!
	assert n == frame_header_len
	assert frame.payload.len == 0
}

fn test_incomplete_data() {
	framed := encode_frame('abcdef'.bytes(), false)
	// short header, then short payload: both signal incomplete via n == 0
	for cut in [0, 3, frame_header_len, framed.len - 1] {
		_, n := decode_frame(framed[..cut])!
		assert n == 0, 'cut at ${cut} should be incomplete'
	}
}

fn test_invalid_flag() {
	mut framed := encode_frame([u8(1)], false)
	framed[0] = 2
	if _, _ := decode_frame(framed) {
		assert false, 'flag 2 must error'
	}
}

fn test_length_limit() {
	mut data := []u8{len: frame_header_len}
	data[1] = 0xff // 0xff000000 > max_frame_len
	if _, _ := decode_frame(data) {
		assert false, 'oversized length must error'
	}
}

fn test_stream_of_frames() {
	mut stream := []u8{}
	stream << encode_frame('one'.bytes(), false)
	stream << encode_frame('two'.bytes(), true)
	stream << encode_frame([]u8{}, false)
	frames := decode_frames(stream)!
	assert frames.len == 3
	assert frames[0].payload == 'one'.bytes()
	assert frames[1].compressed
	assert frames[2].payload.len == 0
}

fn test_truncated_stream() {
	mut stream := encode_frame('one'.bytes(), false)
	stream << encode_frame('two'.bytes(), false)[..4]
	if _ := decode_frames(stream) {
		assert false, 'truncated tail must error'
	}
}
