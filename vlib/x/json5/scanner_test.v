module json5

fn test_scan_bom_is_skipped() {
	mut s := new_scanner('\uFEFF{ a: 1 }')
	tok := s.next()!
	assert tok.kind == .lcbr
}

fn test_scan_punctuation() {
	mut s := new_scanner('{}[]:,')
	expected := [TokenKind.lcbr, .rcbr, .lsbr, .rsbr, .colon, .comma, .eof]
	for kind in expected {
		assert s.next()!.kind == kind
	}
}

fn test_scan_keywords() {
	mut s := new_scanner('true false null Infinity NaN')
	assert s.next()!.kind == .bool
	assert s.next()!.kind == .bool
	assert s.next()!.kind == .null
	assert s.next()!.kind == .infinity
	assert s.next()!.kind == .nan
	assert s.next()!.kind == .eof
}

fn test_scan_bare_ident() {
	mut s := new_scanner('someKey $_x key9')
	assert s.next()!.kind == .ident
	assert s.next()!.lit == '$_x'
	assert s.next()!.lit == 'key9'
}

fn test_scan_unicode_ident() {
	mut s := new_scanner('ключ')
	tok := s.next()!
	assert tok.kind == .ident
	assert tok.lit == 'ключ'
}

fn test_scan_line_comment() {
	mut s := new_scanner('// skip me\n42')
	tok := s.next()!
	assert tok.kind == .number
	assert tok.lit == '42'
	assert tok.pos.line == 2
}

fn test_scan_block_comment() {
	mut s := new_scanner('/* a\nb */ 7')
	tok := s.next()!
	assert tok.lit == '7'
	assert tok.pos.line == 2
}

fn test_scan_unterminated_block_comment() {
	mut s := new_scanner('/* never closed')
	s.next() or {
		assert err is ParseError
		assert err.msg().contains('unterminated block comment')
		return
	}
	assert false, 'expected an unterminated block comment error'
}

fn test_scan_single_quoted_string() {
	mut s := new_scanner("'it\\'s'")
	assert s.next()!.lit == "it's"
}

fn test_scan_escape_sequences() {
	// Each key is the JSON5 escape text as it appears in a document; each value
	// is the rune the scanner must produce for it.
	cases := {
		'\\n':        '\n'
		'\\t':        '\t'
		'\\r':        '\r'
		'\\b':        '\b'
		'\\f':        '\f'
		'\\v':        '\v'
		'\\0':        '\x00'
		'\\x41':      'A'
		'\\xAB':      '\u00AB'
		'\\u0041':    'A'
		'\\uABCD':    '\uABCD'
		'\\u{ABCD}':  '\uABCD'
		'\\u{1F600}': '\U0001F600'
		'\\q':        'q'
	}
	for escape, expected in cases {
		mut s := new_scanner('"' + escape + '"')
		tok := s.next()!
		assert tok.kind == .str, 'escape ${escape}: got kind ${tok.kind}'
		assert tok.lit == expected, 'escape ${escape}: got `${tok.lit}`'
	}
}

fn test_scan_line_continuation() {
	mut s := new_scanner('"a\\\nb"')
	assert s.next()!.lit == 'ab'
}

fn test_scan_unterminated_string() {
	mut s := new_scanner('"abc')
	s.next() or {
		assert err.msg().contains('unterminated string')
		return
	}
	assert false, 'expected an unterminated string error'
}

fn test_scan_numbers() {
	cases := ['0', '-1', '+1', '.5', '5.', '1.5', '1e3', '1E+3', '1e-3', '0x1f', '0XFF', '0xAB',
		'0XCD', '+0xAD', '-0xBC']
	for source in cases {
		mut s := new_scanner(source)
		tok := s.next()!
		assert tok.kind == .number, 'source ${source} scanned as ${tok.kind}'
		assert tok.lit == source
	}
}

fn test_scan_leading_zero_is_allowed() {
	mut s := new_scanner('007')
	assert s.next()!.lit == '007'
}

fn test_scan_signed_infinity() {
	mut s := new_scanner('-Infinity')
	tok := s.next()!
	assert tok.kind == .infinity
	assert tok.lit == '-Infinity'
}

fn test_scan_hex_without_digits() {
	mut s := new_scanner('0x')
	s.next() or {
		assert err.msg().contains('hexadecimal literal has no digits')
		return
	}
	assert false, 'expected a hex digit error'
}

fn test_scan_number_without_digits() {
	mut s := new_scanner('-')
	s.next() or {
		assert err.msg().contains('number has no digits')
		return
	}
	assert false, 'expected a missing digits error'
}

fn test_scan_exponent_without_digits() {
	mut s := new_scanner('1e')
	s.next() or {
		assert err.msg().contains('exponent has no digits')
		return
	}
	assert false, 'expected an exponent error'
}

fn test_scan_unexpected_token() {
	mut s := new_scanner('@')
	s.next() or {
		assert err is ParseError
		return
	}
	assert false, 'expected an unexpected token error'
}

fn test_scan_unicode_whitespace() {
	// A no-break space and a zero-width no-break space (BOM) are JSON5
	// whitespace, and both may appear between tokens.
	mut s := new_scanner('\u00A0\uFEFF 1')
	assert s.next()!.lit == '1'
}
