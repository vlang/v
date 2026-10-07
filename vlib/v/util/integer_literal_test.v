module util

import strconv

fn test_v_literal_parse_base() {
	for value in ['', '0', '00', '010', '08', '+010', '-010', '123', '0_10'] {
		assert v_literal_parse_base(value) == 10
	}
	for value in ['0b10', '0B_10', '0o12', '0O_12', '0xA', '0X_A', '+0b10', '-0o12', '+0X_A'] {
		assert v_literal_parse_base(value) == 0
	}
}

fn test_v_literal_parse_base_preserves_decimal_and_explicit_bases() {
	for value, expected in {
		'010':    10
		'08':     8
		'0_10':   10
		'+010':   10
		'-010':   -10
		'0b1010': 10
		'0o12':   10
		'0xA':    10
		'-0X_A':  -10
	} {
		assert strconv.parse_int(value, v_literal_parse_base(value), 64)! == expected
	}
}
