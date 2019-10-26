import encoding.base64

fn test_long_encoding(){
	repeats := 256
	input_size := 131072
	
	s_original := 'a'.repeat(input_size)
	mut s_encoded := base64.encode(s_original)
	assert s_encoded.len > s_original.len
	mut s_decoded := base64.decode(s_encoded)
	assert s_original == s_decoded

	mut s := 0
	for i := 0; i < repeats; i++ {
		s_encoded = base64.encode(s_original)
		s+= s_encoded.len
	}

	for i := 0; i < repeats; i++ {
		s_decoded = base64.decode(s_encoded)
		s+= s_decoded.len
	}
	
	assert s_encoded.len > s_original.len
	assert s_original == s_decoded	
}
