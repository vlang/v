fn test_utf8_char_len() {
	assert utf8_char_len(`a`) == 1
	println(utf8_char_len(`a`))
	s := 'п'
	assert utf8_char_len(s[0]) == 2
}

fn test_utf8_wide_char() {
	$if msvc {
		// TODO: make this test pass msvc too
		return
	}
	r := `✔`
	s := '✔'
	println('r: ${r}')
	println('s: ${s}')
	rstr := r.str()
	println('rstr: ${rstr}')
	assert utf8_char_len(r) == 1
	assert utf8_char_len(s[0]) == 3
	assert s == rstr
	val := rstr.str
	unsafe {
		assert val[0].hex() == 'e2'
		assert val[1].hex() == '9c'
		assert val[2].hex() == '94'
	}
}

fn test_to_wide_latin() {
	s := 'abc 123'
	w := s.to_wide()
	unsafe {
		assert w[0] == 97
		assert w[1] == 98
		assert w[2] == 99
		assert w[3] == 32
		assert w[4] == 49
		assert w[5] == 50
		assert w[6] == 51
		assert w[7] == 0
	}
}

fn test_to_wide_cyrillic() {
	s := 'Проба'
	w := s.to_wide()
	unsafe {
		assert w[0] == 1055
		assert w[1] == 1088
		assert w[2] == 1086
		assert w[3] == 1073
		assert w[4] == 1072
		assert w[5] == 0
	}
}
