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
	assert encode('Man'.bytes()) == '9jqo'
	assert decode('9jqo')! == 'Man'.bytes()
	assert encode('Man '.bytes()) == '9jqo^'
	assert decode('9jqo^')! == 'Man '.bytes()
	assert encode('Hello, World!'.bytes()) == '87cURD_*#4DfTZ)+T'
	assert decode('87cURD_*#4DfTZ)+T')! == 'Hello, World!'.bytes()
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

fn test_maximum_value_and_partial_groups() {
	assert encode([u8(0xFF)]) == 'rr'
	assert encode([u8(0xFF), 0xFF]) == 's8N'
	assert encode([u8(0xFF), 0xFF, 0xFF]) == 's8W*'
	assert encode([u8(0xFF), 0xFF, 0xFF, 0xFF]) == 's8W-!'
	assert decode('rr')! == [u8(0xFF)]
	assert decode('s8N')! == [u8(0xFF), 0xFF]
	assert decode('s8W*')! == [u8(0xFF), 0xFF, 0xFF]
	assert decode('s8W-!')! == [u8(0xFF), 0xFF, 0xFF, 0xFF]
}

fn test_decode_rejects_full_group_overflow() {
	for text in ['s8W-"', 'uuuuu', 'z s8W-"'] {
		if _ := decode(text) {
			assert false, text
		} else {
			assert err.msg().contains('overflow')
		}
	}
}

fn test_decode_rejects_partial_group_overflow() {
	for text in ['uu', 'uuu', 'uuuu', 's8W-', 's8W-!uu'] {
		if _ := decode(text) {
			assert false, text
		} else {
			assert err.msg().contains('overflow')
		}
	}
}

fn test_zero_remainders_do_not_use_shortcut() {
	assert encode([u8(0)]) == '!!'
	assert encode([u8(0), 0]) == '!!!'
	assert encode([u8(0), 0, 0]) == '!!!!'
	assert encode([u8(0), 0, 0, 0, 0]) == 'z!!'
	assert decode('!!')! == [u8(0)]
	assert decode('!!!')! == [u8(0), 0]
	assert decode('!!!!')! == [u8(0), 0, 0]
	assert decode('z!!')! == [u8(0), 0, 0, 0, 0]
}

fn test_whitespace_inside_groups_and_around_shortcuts() {
	assert decode(' \t\n\r\f\v9 j\tq\no\r^\fz\v')! == [u8(0x4D), 0x61, 0x6E, 0x20, 0, 0, 0, 0]
	assert decode('9 j\tq\no\r')! == 'Man'.bytes()
	assert decode(' \t\n\r\f\v')! == []u8{}
}

fn test_decode_rejects_misplaced_shortcuts_and_short_tails() {
	for text in ['!z', '!!z', '!!!z', '!!!!z', '! \tz'] {
		if _ := decode(text) {
			assert false, text
		} else {
			assert err.msg().contains('z inside group')
		}
	}
	for text in ['!', 'z!', '!!!!! !'] {
		if _ := decode(text) {
			assert false, text
		} else {
			assert err.msg().contains('invalid trailing group')
		}
	}
	for text in ['v', 'y', '\x80', '<~z~>'] {
		if _ := decode(text) {
			assert false, text
		} else {
			assert err.msg().contains('invalid character')
		}
	}
}

fn test_round_trip_every_single_byte() {
	for n in 0 .. 256 {
		data := [u8(n)]
		assert decode(encode(data))! == data
	}
}
