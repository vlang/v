// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import strconv

struct Scanner {
mut:
	text []byte
	pos  int
	line int
	col  int
}

enum TokenKind {
	none_
	error
	str_
	float
	int_
	null
	bool_
	eof
	comma = 44
	colon = 58
	lsbr = 91
	rsbr = 93
	lcbr = 123
	rcbr = 125
}

struct Token {
	lit  []byte
	kind TokenKind
	line int
	col  int
}

const (
	// list of characters commonly used in JSON.
	char_list                 = [`{`, `}`, `[`, `]`, `,`, `:`]
	// list of newlines to check when moving to a new position.
	newlines                  = [`\r`, `\n`, `\t`]
	// list of escapable that needs to be escaped inside a JSON string.
	// double quotes and forward slashes are excluded intentionally since
	// they have their own separate checks for it in order to pass the
	// JSON test suite (https://github.com/nst/JSONTestSuite/).
	important_escapable_chars = [`\b`, `\f`, `\n`, `\r`, `\t`]
	// list of valid unicode escapes aside from \u{4-hex digits}
	valid_unicode_escapes     = [`b`, `f`, `n`, `r`, `t`, `\\`, `"`, `/`]
	// used for transforming escapes into valid unicode (eg. n => \n)
	unicode_transform_escapes = map{
		98:  `\b`
		102: `\f`
		110: `\n`
		114: `\r`
		116: `\t`
		92:  `\\`
		34:  `"`
		47:  `/`
	}
	exp_signs                 = [byte(`-`), `+`]
)

// move_pos proceeds to the next position.
fn (mut s Scanner) move_pos() {
	s.move(true, true)
}

// move_pos_with_newlines is the same as move_pos but only enables newline checking.
fn (mut s Scanner) move_pos_with_newlines() {
	s.move(false, true)
}

fn (mut s Scanner) move(include_space bool, include_newlines bool) {
	s.pos++
	if s.pos < s.text.len {
		if include_newlines && s.text[s.pos] in json2.newlines {
			s.line++
			s.col = 0
			if s.text[s.pos] == `\r` && s.pos + 1 < s.text.len && s.text[s.pos + 1] == `\n` {
				s.pos++
			}
			for s.pos < s.text.len && s.text[s.pos] in json2.newlines {
				s.move_pos()
			}
		} else if include_space && s.text[s.pos] == ` ` {
			s.pos++
			s.col++
			for s.pos < s.text.len && s.text[s.pos] == ` ` {
				s.move_pos()
			}
		}
	} else {
		s.col++
	}
}

// error returns an error token.
fn (s Scanner) error(description string) Token {
	return s.tokenize(description.bytes(), .error)
}

// tokenize returns a token based on the given lit and kind.
fn (s Scanner) tokenize(lit []byte, kind TokenKind) Token {
	return Token{
		lit: lit
		kind: kind
		col: s.col
		line: s.line
	}
}

// text_scan scans and returns a string token.
[manualfree]
fn (mut s Scanner) text_scan() Token {
	mut has_closed := false
	mut chrs := []byte{}
	for {
		s.move(false, false)
		if s.pos >= s.text.len {
			break
		}
		ch := s.text[s.pos]
		if (s.pos - 1 >= 0 && s.text[s.pos - 1] != `\\`) && ch == `"` {
			has_closed = true
			break
		} else if (s.pos - 1 >= 0 && s.text[s.pos - 1] != `\\`)
			&& ch in json2.important_escapable_chars {
			return s.error('character must be escaped with a backslash')
		} else if (s.pos == s.text.len - 1 && ch == `\\`) || ch == byte(0) {
			return s.error('invalid backslash escape')
		} else if s.pos + 1 < s.text.len && ch == `\\` {
			peek := s.text[s.pos + 1]
			if peek in json2.valid_unicode_escapes {
				chrs << json2.unicode_transform_escapes[int(peek)]
				s.move(false, false)
				continue
			} else if peek == `u` {
				if s.pos + 5 < s.text.len {
					s.move(false, false)
					mut codepoint := []byte{}
					codepoint_start := s.pos
					for s.pos < s.text.len && s.pos < codepoint_start + 4 {
						s.move(false, false)
						if s.text[s.pos] == `"` {
							break
						} else if !s.text[s.pos].is_hex_digit() {
							return s.error('`${s.text[s.pos].ascii_str()}` is not a hex digit')
						}
						codepoint << s.text[s.pos]
					}
					if codepoint.len != 4 {
						return s.error('unicode escape must have 4 hex digits')
					}
					val := u32(strconv.parse_uint(codepoint.bytestr(), 16, 32))
					converted := utf32_to_str(val)
					converted_bytes := converted.bytes()
					chrs << converted_bytes
					unsafe {
						converted.free()
						converted_bytes.free()
						codepoint.free()
					}
					continue
				} else {
					return s.error('incomplete unicode escape')
				}
			} else if peek == `U` {
				return s.error('unicode endpoints must be in lowercase `u`')
			} else if peek == byte(229) {
				return s.error('unicode endpoint not allowed')
			} else {
				return s.error('invalid backslash escape')
			}
		}
		chrs << ch
	}
	tok := s.tokenize(chrs, .str_)
	s.move_pos()
	if !has_closed {
		return s.error('missing double quotes in string closing')
	}
	return tok
}

// num_scan scans and returns an int/float token.
fn (mut s Scanner) num_scan() Token {
	// analyze json number structure
	// -[digit][?[dot][digit]][?[E/e][?-/+][digit]]
	mut is_fl := false
	mut dot_index := -1
	mut digits := []byte{}
	if s.text[s.pos] == `-` {
		digits << `-`
		if !s.text[s.pos + 1].is_digit() {
			return s.invalid_token()
		}
		s.move_pos_with_newlines()
	}
	if s.text[s.pos] == `0` && (s.pos + 1 < s.text.len && s.text[s.pos + 1].is_digit()) {
		return s.error('leading zeroes in a number are not allowed')
	}
	for s.pos < s.text.len && (s.text[s.pos].is_digit() || (!is_fl && s.text[s.pos] == `.`)) {
		digits << s.text[s.pos]
		if s.text[s.pos] == `.` {
			is_fl = true
			dot_index = digits.len - 1
		}
		s.move_pos_with_newlines()
	}
	if dot_index + 1 < s.text.len && digits[dot_index + 1..].len == 0 {
		return s.error('invalid float')
	}
	if s.pos < s.text.len && (s.text[s.pos] == `e` || s.text[s.pos] == `E`) {
		digits << s.text[s.pos]
		s.move_pos_with_newlines()
		if s.pos < s.text.len && s.text[s.pos] in json2.exp_signs {
			digits << s.text[s.pos]
			s.move_pos_with_newlines()
		}
		mut exp_digits_count := 0
		for s.pos < s.text.len && s.text[s.pos].is_digit() {
			digits << s.text[s.pos]
			exp_digits_count++
			s.move_pos_with_newlines()
		}
		if exp_digits_count == 0 {
			return s.error('invalid exponent')
		}
	}
	kind := if is_fl { TokenKind.float } else { TokenKind.int_ }
	return s.tokenize(digits, kind)
}

// invalid_token returns an error token with the invalid token message.
fn (s Scanner) invalid_token() Token {
	return s.error('invalid token `${s.text[s.pos].ascii_str()}`')
}

// scan returns a token based on the scanner's current position.
[manualfree]
fn (mut s Scanner) scan() Token {
	for s.pos < s.text.len && s.text[s.pos] == ` ` {
		s.pos++
	}
	if s.pos >= s.text.len {
		return s.tokenize([]byte{}, .eof)
	} else if s.pos + 3 < s.text.len && (s.text[s.pos] == `t` || s.text[s.pos] == `n`) {
		ident := s.text[s.pos..s.pos + 4].bytestr()
		if ident == 'true' || ident == 'null' {
			mut kind := TokenKind.null
			if ident == 'true' {
				kind = .bool_
			}
			unsafe { ident.free() }
			val := s.text[s.pos..s.pos + 4]
			tok := s.tokenize(val, kind)
			s.move_pos()
			s.move_pos()
			s.move_pos()
			s.move_pos()
			return tok
		}
		unsafe { ident.free() }
		return s.invalid_token()
	} else if s.pos + 4 < s.text.len && s.text[s.pos] == `f` {
		ident := s.text[s.pos..s.pos + 5].bytestr()
		if ident == 'false' {
			unsafe { ident.free() }
			val := s.text[s.pos..s.pos + 5]
			tok := s.tokenize(val, .bool_)
			s.move_pos()
			s.move_pos()
			s.move_pos()
			s.move_pos()
			s.move_pos()
			return tok
		}
		unsafe { ident.free() }
		return s.invalid_token()
	} else if s.text[s.pos] in json2.char_list {
		chr := s.text[s.pos]
		tok := s.tokenize([]byte{}, TokenKind(int(chr)))
		s.move_pos()
		return tok
	} else if s.text[s.pos] == `"` {
		return s.text_scan()
	} else if s.text[s.pos].is_digit() || s.text[s.pos] == `-` {
		return s.num_scan()
	} else {
		return s.invalid_token()
	}
}
