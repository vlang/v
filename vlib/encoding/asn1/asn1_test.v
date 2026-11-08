module asn1

fn test_int() {
	b := encode_int(42)
	assert decode_int(b)! == 42
}

fn test_bool() {
	b := encode_bool(true)
	assert decode_bool(b)! == true
}

fn test_bytes() {
	b := encode_bytes('hi'.bytes())
	assert decode_bytes(b)! == 'hi'.bytes()
}

fn test_seq() {
	parts := [encode_int(1), encode_int(2)]
	b := encode_sequence(parts)
	out := decode_sequence(b)!
	assert out.len == 2
}
