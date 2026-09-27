fn raw_string_identity[T](value T) T {
	return value
}

fn test_generic_raw_string_literal_infers_string() {
	assert raw_string_identity(r'\n\r\b') == r'\n\r\b'
}
