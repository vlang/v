fn test_utf8_char_len() {
	assert utf8_char_len(`a`) == 1
	println(utf8_char_len(`a`))
	s := 'п'
	assert utf8_char_len(s[0]) == 2
}

fn test_utf8_wide_char() {
	r := `✔`
	s := '✔'
	println('r: $r')
	println('s: $s')
	assert utf8_char_len(r) == 1
	assert utf8_char_len(s[0]) == 3
	rstr := r.str()
	println('rstr: $rstr')
	assert s == rstr
	//
	val := rstr.str
	unsafe {
		assert val[0].hex() == 'e2'
		assert val[1].hex() == '9c'
		assert val[2].hex() == '94'
	}
}
