module big

fn test_decimal_parse_boundaries() {
	for size in [1, 2, 8, 9, 10, 17, 18, 19, 20, 99, 100, 143, 144, 145, 287, 288, 289, 575, 576,
		577, 1000, 1151, 1152, 1153, 2000, 10_000, 16_383, 16_384, 16_385, 18_432, 18_433] {
		// The old parser is quadratic, so long texts are checked with one sign only.
		prefixes := if size < 10_000 { ['', '+', '-'] } else { ['-'] }
		for text in ['0'.repeat(size), '9'.repeat(size), '1' + '0'.repeat(size - 1),
			'1' + '0'.repeat(size - 1) + '1'] {
			for prefix in prefixes {
				input := prefix + text
				old := integer_from_regular_string(input, 10)
				value := integer_from_string(input)!
				assert value == old
				assert value.is_const == old.is_const
				assert integer_from_radix(input, 10)! == old
			}
		}
	}
}

fn test_decimal_parse_limb_boundaries() {
	for limbs in [1, 2, 3, 15, 16, 17, 64, 65, 907, 908, 1024] {
		power := one_int.left_shift(u32(limbs * digit_bits))
		for value in [power - one_int, power, power + one_int] {
			for signed in [value, value.neg()] {
				assert integer_from_string(signed.str())! == signed
			}
		}
	}
}

fn test_decimal_parse_random() {
	mut seed := u64(0xbb67ae8584caa73b)
	for size in [20, 100, 1000, 10_000, 40_000] {
		mut text := []u8{len: size}
		for i in 0 .. size {
			seed ^= seed << 13
			seed ^= seed >> 7
			seed ^= seed << 17
			text[i] = u8(seed % 10) + `0`
		}
		input := text.bytestr()
		assert integer_from_string(input)! == integer_from_regular_string(input, 10)
		assert integer_from_string(input)!.str() == input.trim_left('0')
	}
}

fn test_decimal_parse_validation() {
	for input, expected in {
		'':     'math.big: No digits in the given string'
		'+':    'math.big: No digits in the given string'
		'-':    'math.big: No digits in the given string'
		' 1':   'math.big: Invalid character 32'
		'1 ':   'math.big: Invalid character 32'
		'1_0':  'math.big: Invalid character 95'
		'1.0':  'math.big: Invalid character 46'
		'1e2':  'math.big: Invalid character 101 for base 10'
		'1a':   'math.big: Invalid character 97 for base 10'
		'--1':  'math.big: Invalid character 45'
		'+-1':  'math.big: Invalid character 45'
		'+0x1': 'math.big: Invalid character 120 for base 10'
		'1/2':  'math.big: Invalid character 47'
		'1:2':  'math.big: Invalid character 58'
	} {
		if _ := integer_from_string(input) {
			assert false
		} else {
			assert err.msg() == expected
		}
	}
}

fn test_decimal_parse_validation_after_long_prefix() {
	for sign in ['', '+', '-'] {
		prefix := sign + '0'.repeat(1000)
		for suffix, expected in {
			'_A': 'math.big: Invalid character 95'
			'/0': 'math.big: Invalid character 47'
			':0': 'math.big: Invalid character 58'
			'A_': 'math.big: Invalid character 97 for base 10'
		} {
			input := prefix + suffix
			if _ := integer_from_string(input) {
				assert false
			} else {
				assert err.msg() == expected
			}
			if _ := integer_from_radix(input, 10) {
				assert false
			} else {
				assert err.msg() == expected
			}
		}
	}
}
