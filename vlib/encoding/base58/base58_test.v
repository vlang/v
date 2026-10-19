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

	aa := '0JHRktAz1yEtdGl6RnO'
	assert encode(aa) == '9qkYDxBXPdc6nW825HkNx7oFMY'

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
		return error(@MOD + '.' + @FN + ': expected encode_int to fail, got ${z}')
	}
	if z := encode_int(b) {
		return error(@MOD + '.' + @FN + ': expected encode_int to fail, got ${z}')
	}

	c := '!'
	if z := decode_int(c) {
		return error(@MOD + '.' + @FN + ': expected decode_int to fail, got ${z}')
	}
	if z := decode(c) {
		return error(@MOD + '.' + @FN + ': expected decode to fail, got ${z}')
	}

	// repeating character
	if abc := new_alphabet('aaaaafghij\$lmnopqrstuvwxyz0123456789_ABCDEFGHIJLMNOPQRSTUV') {
		return error(@MOD + '.' + @FN + ': expected new_alphabet to fail, got ${abc}')
	}
	// more than 58 characters long
	if abc := new_alphabet('abcdefghij\$lmnopqrstuvwxyz0123456789_ABCDEFGHIJLMNOPQRSTUVWXYZ') {
		return error(@MOD + '.' + @FN + ': expected new_alphabet to fail, got ${abc}')
	}
}

fn test_decode_rejects_non_ascii_input() {
	// a byte above 127 is just a malformed digit, so it errors like one
	mut input := 'ab'.bytes()
	input << 0xff
	if z := decode_bytes(input) {
		assert false, 'decode_bytes accepted a non-ASCII digit: ${z}'
	} else {
		assert err.msg().contains('invalid base58 digit (255)')
	}
	if z := decode_walpha_bytes(input, btc_alphabet) {
		assert false, 'decode_walpha_bytes accepted a non-ASCII digit: ${z}'
	} else {
		assert err.msg().contains('invalid base58 digit (255)')
	}
	if z := decode_walpha(input.bytestr(), btc_alphabet) {
		assert false, 'decode_walpha accepted a non-ASCII digit: ${z}'
	} else {
		assert err.msg().contains('invalid base58 digit (255)')
	}
}

fn test_alphabet_rejects_non_ascii_bytes() {
	mut input := btc_alphabet.str().bytes()
	input[0] = 0x80
	if alphabet := new_alphabet(input.bytestr()) {
		assert false, 'accepted non-ASCII alphabet: ${alphabet}'
	} else {
		assert err.msg() == 'base58.new_alphabet: alphabet must contain only ASCII characters'
	}
}

fn test_decode_int_exact_and_overflow() ! {
	assert decode_int('4GmR58')! == 2147483647
	max_encoded := if sizeof(int) == 8 { 'NQm6nKp8qFC' } else { '4GmR58' }
	overflow_encoded := if sizeof(int) == 8 { 'NQm6nKp8qFD' } else { '4GmR59' }
	assert decode_int(max_encoded)! == max_int
	assert decode_int('111' + max_encoded)! == max_int
	if v := decode_int(overflow_encoded) {
		assert false, 'overflowing input decoded as ${v}'
	} else {
		assert err.msg().contains('does not fit in an int')
	}
	if sizeof(int) == 8 {
		// Integer accumulation remains exact above the precision limit of f64.
		assert i64(decode_int('zzzzzzzzzz')!) == i64(430804206899405823)
	}
	assert decode_int('z')! == 57
	assert decode_int('zz')! == 57 * 58 + 57
	assert decode_int('2')! == 1
	for alphabet in [btc_alphabet, flickr_alphabet, ripple_alphabet] {
		encoded := encode_int_walpha(max_int, alphabet)!
		assert decode_int_walpha(encoded, alphabet)! == max_int
	}
}
