module ascii85

fn test_round_trip_empty() {
	assert encode([]u8{}) == ''
	assert decode('')! == []u8{}
}

fn test_round_trip_short() {
	for n in 0 .. 20 {
		mut data := []u8{len: n}
		for i in 0 .. n {
			data[i] = u8((i * 37 + 11) % 256)
		}
		enc := encode(data)
		dec := decode(enc)!
		assert dec == data
	}
}

fn test_zero_group() {
	data := [u8(0), 0, 0, 0, 0, 0, 0, 0]
	enc := encode(data)
	assert enc == 'zz'
	assert decode(enc)! == data
}

fn test_known() {
	// "Man" -> "9jqo^"
	assert encode('Man'.bytes()) == '9jqo'
	assert decode('9jqo')! == 'Man'.bytes()
}

fn test_whitespace_ignored() {
	assert decode('9jqo\n')! == 'Man'.bytes()
}

fn test_invalid_char() {
	if _ := decode('~') {
		assert false
	} else {
		assert err.msg().contains('invalid character')
	}
}
