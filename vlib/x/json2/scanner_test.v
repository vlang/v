module json2

fn test_str() {
	mut sc := Scanner{
		text: '"test"'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .str_
	assert tok.lit.len == 4
	assert tok.lit.bytestr() == 'test'
}

fn test_str_valid_unicode_escape() {
	mut sc := Scanner{
		text: r'"\u0048"'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .str_
	assert tok.lit.len == 1
	assert tok.lit.bytestr() == 'H'
}

fn test_str_valid_unicode_escape_2() {
	mut sc := Scanner{
		text: r'"\u2714"'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .str_
	assert tok.lit.len == 3
	assert tok.lit.bytestr() == '✔'
}

fn test_str_invalid_escape() {
	mut sc := Scanner{
		text: r'"\z"'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'invalid backslash escape'
}

fn test_str_invalid_must_be_escape() {
	for char in important_escapable_chars {
		mut sc := Scanner{
			text: [byte(`"`), `t`, char, `"`]
		}
		tok := sc.scan()
		assert tok.kind == .error
		assert tok.lit.bytestr() == 'character must be escaped with a backslash'
	}
}

fn test_str_invalid_unicode_escape() {
	mut sc := Scanner{
		text: r'"\u010G"'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == '`G` is not a hex digit'
}

fn test_str_invalid_unicode_escape_len() {
	mut sc := Scanner{
		text: r'"\u001"'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'unicode escape must have 4 hex digits'
}

fn test_str_invalid_uppercase_u() {
	mut sc := Scanner{
		text: r'"\U0000"'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'unicode endpoints must be in lowercase `u`'
}

fn test_str_missing_closing_bracket() {
	mut sc := Scanner{
		text: '"incomplete'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'missing double quotes in string closing'
}

fn test_int() {
	mut sc := Scanner{
		text: '10'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .int_
	assert tok.lit.len == 2
	assert tok.lit.bytestr() == '10'
}

fn test_int_negative() {
	mut sc := Scanner{
		text: '-10'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .int_
	assert tok.lit.len == 3
	assert tok.lit.bytestr() == '-10'
}

fn test_float() {
	mut sc := Scanner{
		text: '123.400'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .float
	assert tok.lit.len == 7
	assert tok.lit.bytestr() == '123.400'
}

fn test_float_negative() {
	mut sc := Scanner{
		text: '-123.400'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .float
	assert tok.lit.len == 8
	assert tok.lit.bytestr() == '-123.400'
}

fn test_int_exp() {
	mut sc := Scanner{
		text: '1E22'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .int_
	assert tok.lit.len == 4
	assert tok.lit.bytestr() == '1E22'
}

fn test_int_exp_negative() {
	mut sc := Scanner{
		text: '1E-2'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .int_
	assert tok.lit.len == 4
	assert tok.lit.bytestr() == '1E-2'
}

fn test_int_exp_positive() {
	mut sc := Scanner{
		text: '1E+2'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .int_
	assert tok.lit.len == 4
	assert tok.lit.bytestr() == '1E+2'
}

fn test_float_exp() {
	mut sc := Scanner{
		text: '123.456e78'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .float
	assert tok.lit.len == 10
	assert tok.lit.bytestr() == '123.456e78'
}

fn test_float_exp_negative() {
	mut sc := Scanner{
		text: '20.56e-5'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .float
	assert tok.lit.len == 8
	assert tok.lit.bytestr() == '20.56e-5'
}

fn test_float_exp_positive() {
	mut sc := Scanner{
		text: '20.56e+5'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .float
	assert tok.lit.len == 8
	assert tok.lit.bytestr() == '20.56e+5'
}

fn test_number_with_space() {
	mut sc := Scanner{
		text: ' 4'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .int_
	assert tok.lit.len == 1
	assert tok.lit.bytestr() == '4'
}

fn test_number_invalid_leading_zero() {
	mut sc := Scanner{
		text: '0010'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'leading zeroes in a number are not allowed'
}

fn test_number_invalid_leading_zero_negative() {
	mut sc := Scanner{
		text: '-0010'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'leading zeroes in a number are not allowed'
}

fn test_number_invalid_start_char() {
	mut sc := Scanner{
		text: '+1'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'invalid token `+`'
}

fn test_number_invalid_char() {
	mut sc := Scanner{
		text: '122x'.bytes()
	}
	sc.scan()
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'invalid token `x`'
}

fn test_number_invalid_char_float() {
	mut sc := Scanner{
		text: '122x.1'.bytes()
	}
	sc.scan()
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'invalid token `x`'
}

fn test_number_invalid_multiple_dot() {
	mut sc := Scanner{
		text: '122.108.10'.bytes()
	}
	sc.scan()
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'invalid token `.`'
}

fn test_number_invalid_exp() {
	mut sc := Scanner{
		text: '0.3e'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'invalid exponent'
}

fn test_number_invalid_exp_with_sign() {
	mut sc := Scanner{
		text: '0.3e+'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'invalid exponent'
}

fn test_number_invalid_zero_exp() {
	mut sc := Scanner{
		text: '0e'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'invalid exponent'
}

fn test_number_invalid_dot_exp() {
	mut sc := Scanner{
		text: '0.e'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'invalid float'
}

fn test_number_invalid_double_exp() {
	mut sc := Scanner{
		text: '2eE'.bytes()
	}
	sc.scan()
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'invalid token `E`'
}

fn test_null() {
	mut sc := Scanner{
		text: 'null'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .null
	assert tok.lit.len == 4
	assert tok.lit.bytestr() == 'null'
}

fn test_bool_true() {
	mut sc := Scanner{
		text: 'true'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .bool_
	assert tok.lit.len == 4
	assert tok.lit.bytestr() == 'true'
}

fn test_bool_false() {
	mut sc := Scanner{
		text: 'false'.bytes()
	}
	tok := sc.scan()
	assert tok.kind == .bool_
	assert tok.lit.len == 5
	assert tok.lit.bytestr() == 'false'
}
