import encoding.base64

fn test_long_encoding() {
	repeats := 1000
	input_size := 3000

	s_original := []byte{len: input_size, init: `a`}
	s_encoded := base64.encode(s_original)
	s_decoded := base64.decode(s_encoded)

	assert s_encoded.len > s_original.len
	assert s_original == s_decoded

	mut s := 0

	ebuffer := unsafe { malloc(s_encoded.len) }
	for _ in 0 .. repeats {
		resultsize := base64.encode_in_buffer(s_original, ebuffer)
		s += resultsize
		assert resultsize == s_encoded.len
	}

	dbuffer := unsafe { malloc(s_decoded.len) }
	for _ in 0 .. repeats {
		resultsize := base64.decode_in_buffer(s_encoded, dbuffer)
		s += resultsize
		assert resultsize == s_decoded.len
	}

	println('Final s: $s')
	//	assert s == 39147008
}
