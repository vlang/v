module base58

fn test_encode_int() {
	a := 0x24 // should be 'd' in base58
	assert encode_int(a)! == 'd'

	test_encode_int_walpha()
}

fn test_encode_int_walpha() {
	// random alphabet
	abc := new_alphabet('abcdefghij\$lmnopqrstuvwxyz0123456789_ABCDEFGHIJLMNOPQRSTUV') or {
		panic(@MOD + '.' + @FN + ': this should never happen')
	}
	a := 0x24 // should be '_' in base58 with our custom alphabet
	assert encode_int_walpha(a, abc)! == '_'
}

fn test_decode_int() {
	a := 'd'
	assert decode_int(a)! == 0x24

	test_decode_int_walpha()
}

fn test_decode_int_walpha() {
	abc := new_alphabet('abcdefghij\$lmnopqrstuvwxyz0123456789_ABCDEFGHIJLMNOPQRSTUV') or {
		panic(@MOD + '.' + @FN + ': this should never happen')
	}
	a := '_'
	assert decode_int_walpha(a, abc)! == 0x24
}

fn test_encode_string() {
	// should be 'TtaR6twpTGu8VpY' in base58 and '0P7yfPSL0pQh2L5' with our custom alphabet
	a := 'lorem ipsum'
	assert encode(a) == 'TtaR6twpTGu8VpY'

	abc := new_alphabet('abcdefghij\$lmnopqrstuvwxyz0123456789_ABCDEFGHIJLMNOPQRSTUV') or {
		panic(@MOD + '.' + @FN + ': this should never happen')
	}
	assert encode_walpha(a, abc) == '0P7yfPSL0pQh2L5'
}

fn test_decode_string() {
	a := 'TtaR6twpTGu8VpY'
	assert decode(a)! == 'lorem ipsum'

	abc := new_alphabet('abcdefghij\$lmnopqrstuvwxyz0123456789_ABCDEFGHIJLMNOPQRSTUV') or {
		panic(@MOD + '.' + @FN + ': this should never happen')
	}
	b := '0P7yfPSL0pQh2L5'
	assert decode_walpha(b, abc)! == 'lorem ipsum'
}

fn test_fails() ! {
	a := -238
	b := 0
	if z := encode_int(a) {
		return error(@MOD + '.' + @FN + ': expected encode_int to fail, got $z')
	}
	if z := encode_int(b) {
		return error(@MOD + '.' + @FN + ': expected encode_int to fail, got $z')
	}

	c := '!'
	if z := decode_int(c) {
		return error(@MOD + '.' + @FN + ': expected decode_int to fail, got $z')
	}
	if z := decode(c) {
		return error(@MOD + '.' + @FN + ': expected decode to fail, got $z')
	}

	// repeating character
	if abc := new_alphabet('aaaaafghij\$lmnopqrstuvwxyz0123456789_ABCDEFGHIJLMNOPQRSTUV') {
		return error(@MOD + '.' + @FN + ': expected new_alphabet to fail, got $abc')
	}
	// more than 58 characters long
	if abc := new_alphabet('abcdefghij\$lmnopqrstuvwxyz0123456789_ABCDEFGHIJLMNOPQRSTUVWXYZ') {
		return error(@MOD + '.' + @FN + ': expected new_alphabet to fail, got $abc')
	}
}
