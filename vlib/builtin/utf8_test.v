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

const little_serial_number = [u8(67), 0, 76, 0, 52, 0, 54, 0, 73, 0, 49, 0, 65, 0, 48, 0, 48, 0,
	54, 0, 52, 0, 57, 0, 0, 0, 0]
const big_serial_number = [u8(0), 67, 0, 76, 0, 52, 0, 54, 0, 73, 0, 49, 0, 65, 0, 48, 0, 48, 0,
	54, 0, 52, 0, 57, 0, 0, 0, 0]

const swide_serial_number = 'CL46I1A00649'

fn test_string_from_wide() {
	$if little_endian {
		z := unsafe { string_from_wide(little_serial_number.data) }
		assert z == swide_serial_number
	} $else {
		z := unsafe { string_from_wide(big_serial_number.data) }
		assert z == swide_serial_number
	}
}

fn test_string_from_wide2() {
	$if little_endian {
		z := unsafe { string_from_wide2(little_serial_number.data, 12) }
		assert z == swide_serial_number
	} $else {
		z := unsafe { string_from_wide2(big_serial_number.data, 12) }
		assert z == swide_serial_number
	}
}

fn test_reverse_cyrillic_with_string_from_wide() {
	s := 'Проба'
	ws := s.to_wide()
	z := unsafe { string_from_wide(ws) }
	assert z == s
}

fn test_wide_to_ansi() {
	ws := 'abc'.to_wide()
	assert wide_to_ansi(ws) == [u8(97), 98, 99, 0]
}

fn test_string_to_ansi_not_null_terminated() {
	assert string_to_ansi_not_null_terminated('abc') == [u8(97), 98, 99]
}
