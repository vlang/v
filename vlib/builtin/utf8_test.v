fn test_utf8_char_len() {
	assert utf8_char_len(`a`) == 1
	println(utf8_char_len(`a`))
	s := 'п'
	assert utf8_char_len(s[0]) == 2
}

fn test_utf8_wide_char() {
	r := `✔`
	val := r.str().str
	unsafe {
		assert '${val[0]:x}${val[1]:x}${val[2]:x}${val[3]:x}' == 'e29c940'
	}
}
