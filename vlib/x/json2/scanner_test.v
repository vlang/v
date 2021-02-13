module json2

fn test_str() {
	mut sc := Scanner{ text: '"test"'.bytes() }
	tok := sc.scan()
	assert tok.kind == .str_
	assert tok.lit.len == 4
	assert tok.lit.bytestr() == 'test'
}

fn test_str_valid_escape() {
	mut sc := Scanner{ text: r'"t\nest"'.bytes() }
	tok := sc.scan()
	assert tok.kind == .str_
	assert tok.lit.len == 5
	assert tok.lit.bytestr() == 't\nest'
}

fn test_str_valid_unicode_escape() {
	mut sc := Scanner{ text: r'"\u0048"'.bytes() }
	tok := sc.scan()
	assert tok.kind == .str_
	assert tok.lit.len == 1
	assert tok.lit.bytestr() == 'H'
}

fn test_str_invalid_escape() {
	mut sc := Scanner{ text: r'"\z"'.bytes() }
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'invalid backslash escape'
}

fn test_str_invalid_unicode_escape() {
	mut sc := Scanner{ text: r'"\u010G"'.bytes() }
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == '`G` is not a hex digit'
}

fn test_str_invalid_unicode_escape_len() {
	mut sc := Scanner{ text: r'"\u001"'.bytes() }
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'unicode escape must be 4 characters'
}

fn test_str_invalid_uppercase_u() {
	mut sc := Scanner{ text: r'"\U0000"'.bytes() }
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'unicode endpoints must be in lowercase `u`'
}

fn test_str_missing_closing_bracket() {
	mut sc := Scanner{ text: r'"incomplete'.bytes() }
	tok := sc.scan()
	assert tok.kind == .error
	assert tok.lit.bytestr() == 'missing closing bracket in string'
}

fn test_int() {}
