module json2

fn test_scan_bom_is_skipped() {
	mut s := new_json5_scanner('\uFEFF{ a: 1 }')
	tok := s.next()!
	assert tok.kind == .lcbr
	assert tok.pos.offset == 3
}

fn test_scan_source_offsets_include_unicode_and_original_escape_lengths() {
	input := "\uFEFF/* 🎼 */ {ключ: '\\x41', other: false}"
	mut s := new_json5_scanner(input)
	for {
		tok := s.next()!
		if tok.kind == .eof {
			assert tok.pos.offset == input.len
			break
		}
		if tok.kind == .str {
			assert tok.lit == 'A'
			assert input[tok.pos.offset..s.offset] == "'\\x41'"
		}
		if tok.kind == .bool {
			position := input.index('false') or { panic('missing value') }
			assert tok.pos.offset == position
		}
	}
}

fn test_scan_source_offsets_preserve_invalid_utf8_width() {
	input := [u8(`'`), 0xff, `'`, ` `, `1`].bytestr()
	mut s := new_json5_scanner(input)
	assert s.next()!.kind == .str
	assert s.next()!.pos.offset == 4
}

fn test_scan_counts_all_json5_line_terminators() {
	for separator in ['\n', '\r', '\r\n', '\u2028', '\u2029'] {
		mut s := new_json5_scanner('// heading' + separator + separator + '  false')
		tok := s.next()!
		assert tok.kind == .bool
		assert tok.pos.line == 3
		assert tok.pos.col == 3
	}
}

fn test_scan_accepts_all_unicode_space_separators() {
	for code in [0x20, 0xa0, 0x1680, 0x2000, 0x2001, 0x2002, 0x2003, 0x2004, 0x2005, 0x2006, 0x2007,
		0x2008, 0x2009, 0x200a, 0x202f, 0x205f, 0x3000] {
		space := rune(code).str()
		assert decode[int](space + '1' + space, json5: true)! == 1
		mut s := new_json5_scanner(space + '1' + space + 'true' + space)
		assert s.next()!.lit == '1'
		assert s.next()!.kind == .bool
		assert s.next()!.kind == .eof
	}
	mut s := new_json5_scanner('\u00851')
	s.next() or {
		assert err is JsonScanError
		return
	}
	assert false, 'U+0085 is not JSON5 whitespace'
}

fn test_scan_punctuation() {
	mut s := new_json5_scanner('{}[]:,')
	expected := [Json5TokenKind.lcbr, .rcbr, .lsbr, .rsbr, .colon, .comma, .eof]
	for kind in expected {
		assert s.next()!.kind == kind
	}
}

fn test_scan_keywords() {
	mut s := new_json5_scanner('true false null Infinity NaN')
	assert s.next()!.kind == .bool
	assert s.next()!.kind == .bool
	assert s.next()!.kind == .null
	assert s.next()!.kind == .infinity
	assert s.next()!.kind == .nan
	assert s.next()!.kind == .eof
}

fn test_scan_bare_ident() {
	mut s := new_json5_scanner('someKey $_x key9')
	assert s.next()!.kind == .ident
	assert s.next()!.lit == '$_x'
	assert s.next()!.lit == 'key9'
}

fn test_scan_unicode_ident() {
	mut s := new_json5_scanner('ключ')
	tok := s.next()!
	assert tok.kind == .ident
	assert tok.lit == 'ключ'
}

fn test_scan_unicode_escapes_in_identifiers_keep_source_positions() {
	cases := {
		r'\u0061':              'a'
		r'a\u0062':             'ab'
		r'\u0024\u005f\u0030':  '$_0'
		r'\u00e9clair':         'éclair'
		r'clé\u0031':           'clé1'
		r'a\u0301\u200c\u200d': 'a\u0301\u200c\u200d'
		r'\u2165':              '\u2165'
		r'a\u0903':             'a\u0903'
		r'a\u1ab0':             'a\u1ab0'
		r'a\u0660':             'a\u0660'
		r'a\u203f':             'a\u203f'
		r'\u0074rue':           'true'
		r'f\u0061lse':          'false'
		r'\u006eull':           'null'
		r'Inf\u0069nity':       'Infinity'
		r'N\u0061N':            'NaN'
	}
	for source, expected in cases {
		mut s := new_json5_scanner(source + ': false')
		tok := s.next()!
		assert tok.kind == .ident
		assert tok.lit == expected
		assert tok.pos.offset == 0
		assert tok.pos.col == 1
		assert tok.length == source.len
		colon := s.next()!
		assert colon.kind == .colon
		assert colon.pos.offset == source.len
		assert colon.pos.col == source.runes().len + 1
		assert colon.length == 1
	}
}

fn test_scan_rejects_invalid_identifier_escapes() {
	for source in [r'\u0030', r'\u0301', r'\u0903', r'\u203f', r'\u0660', r'\u200c', r'\u0020',
		r'\uD800', r'\uDC00', r'a\u00b2', r'a\u0488', r'a\u0020', r'a\u000a', r'a\u002f', r'a\u005c',
		r'\x61', r'a\x62', r'\U0061', r'\u12', r'a\u00ZZ', r'\u{61}', r'a\', r'\'] {
		mut s := new_json5_scanner(source)
		s.next() or {
			assert err is JsonScanError
			continue
		}
		assert false, 'expected invalid identifier escape to fail: ${source}'
	}
	mut s := new_json5_scanner('// heading\n  a' + r'\u0020')
	s.next() or {
		if err is JsonScanError {
			assert err.line == 2
			assert err.character == 4
		} else {
			assert false, err.msg()
		}
		return
	}
	assert false, 'expected an invalid identifier escape error'
}

fn test_scan_line_comment() {
	mut s := new_json5_scanner('// skip me\n42')
	tok := s.next()!
	assert tok.kind == .number
	assert tok.lit == '42'
	assert tok.pos.line == 2
}

fn test_scan_block_comment() {
	mut s := new_json5_scanner('/* a\nb */ 7')
	tok := s.next()!
	assert tok.lit == '7'
	assert tok.pos.line == 2
}

fn test_scan_unterminated_block_comment() {
	mut s := new_json5_scanner('/* never closed')
	s.next() or {
		assert err is JsonScanError
		assert err.msg().contains('unterminated block comment')
		return
	}
	assert false, 'expected an unterminated block comment error'
}

fn test_scan_single_quoted_string() {
	mut s := new_json5_scanner("'it\\'s'")
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
		mut s := new_json5_scanner('"' + escape + '"')
		tok := s.next()!
		assert tok.kind == .str, 'escape ${escape}: got kind ${tok.kind}'
		assert tok.lit == expected, 'escape ${escape}: got `${tok.lit}`'
	}
}

fn test_scan_rejects_decimal_digit_escapes() {
	for quote in ['"', "'"] {
		for digit in 0 .. 10 {
			for escape in [if digit == 0 { '\\00' } else { '\\' + digit.str() }, '\\0' + digit.str()] {
				mut s := new_json5_scanner(quote + escape + quote)
				s.next() or {
					assert err is JsonScanError
					assert err.msg().contains('digit')
					continue
				}
				assert false, 'expected digit escape ${escape} to fail'
			}
		}
		mut s := new_json5_scanner(quote + '\\0q\\x31\\u0032' + quote)
		assert s.next()!.lit == '\x00q12'
	}
}

fn test_scan_line_continuation() {
	for quote in ['"', "'"] {
		for separator in ['\n', '\r', '\r\n', '\u2028', '\u2029'] {
			mut s := new_json5_scanner('// heading\n  ' + quote + 'a\\' + separator + 'b' + quote + ' true')
			assert s.next()!.lit == 'ab'
			next := s.next()!
			assert next.kind == .bool
			assert next.pos.line == 3
			assert next.pos.col == 4
		}
	}
}

fn test_scan_rejects_unescaped_newlines_in_strings() {
	for quote in ['"', "'"] {
		for separator in ['\n', '\r', '\r\n'] {
			mut s := new_json5_scanner('// heading\n  ' + quote + 'a' + separator + 'b' + quote)
			s.next() or {
				if err is JsonScanError {
					assert err.message == 'unescaped line terminator in string'
					assert err.line == 2
					assert err.character == 5
				} else {
					assert false, err.msg()
				}
				continue
			}
			assert false, 'expected an unescaped line terminator error'
		}
	}
}

fn test_scan_accepts_unescaped_unicode_line_separators_in_strings() {
	for quote in ['"', "'"] {
		for separator in ['\u2028', '\u2029'] {
			mut s := new_json5_scanner(quote + 'a' + separator + 'b' + quote + ' true')
			assert s.next()!.lit == 'a' + separator + 'b'
			next := s.next()!
			assert next.kind == .bool
			assert next.pos.line == 2
			assert next.pos.col == 4
		}
	}
}

fn test_scan_unterminated_string() {
	mut s := new_json5_scanner('"abc')
	s.next() or {
		assert err.msg().contains('unterminated string')
		return
	}
	assert false, 'expected an unterminated string error'
}

fn test_scan_numbers() {
	cases := ['0', '-0', '+0', '0.', '0.5', '0.05', '0e01', '0.e+02', '.05', '-1', '+1', '.5',
		'5.', '1.5', '1e3', '1E+3', '1e-3', '0x1f', '0XFF', '0xAB', '0XCD', '+0xAD', '-0xBC', '0x00',
		'-0X01']
	for source in cases {
		mut s := new_json5_scanner(source)
		tok := s.next()!
		assert tok.kind == .number, 'source ${source} scanned as ${tok.kind}'
		assert tok.lit == source
		assert tok.length == source.len
	}
}

fn test_scan_rejects_leading_zero_decimals() {
	for sign in ['', '+', '-'] {
		for digits in ['00', '01', '007', '08', '09', '00.5', '01.5', '00e1', '01e+2'] {
			mut s := new_json5_scanner('// heading\n  ' + sign + digits)
			s.next() or {
				if err is JsonScanError {
					assert err.message.contains('leading zero')
					assert err.line == 2
					assert err.character == 3
				} else {
					assert false, err.msg()
				}
				continue
			}
			assert false, 'expected leading-zero decimal to fail: ${sign}${digits}'
		}
	}
}

fn test_scan_signed_infinity() {
	mut s := new_json5_scanner('-Infinity')
	tok := s.next()!
	assert tok.kind == .infinity
	assert tok.lit == '-Infinity'
}

fn test_scan_hex_without_digits() {
	mut s := new_json5_scanner('0x')
	s.next() or {
		assert err.msg().contains('hexadecimal literal has no digits')
		return
	}
	assert false, 'expected a hex digit error'
}

fn test_scan_number_without_digits() {
	mut s := new_json5_scanner('-')
	s.next() or {
		assert err.msg().contains('number has no digits')
		return
	}
	assert false, 'expected a missing digits error'
}

fn test_scan_exponent_without_digits() {
	mut s := new_json5_scanner('1e')
	s.next() or {
		assert err.msg().contains('exponent has no digits')
		return
	}
	assert false, 'expected an exponent error'
}

fn test_scan_unexpected_token() {
	mut s := new_json5_scanner('@')
	s.next() or {
		assert err is JsonScanError
		return
	}
	assert false, 'expected an unexpected token error'
}

fn test_scan_unicode_whitespace() {
	// A no-break space and a zero-width no-break space (BOM) are JSON5
	// whitespace, and both may appear between tokens.
	mut s := new_json5_scanner('\u00A0\uFEFF 1')
	assert s.next()!.lit == '1'
}
