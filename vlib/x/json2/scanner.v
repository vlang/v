// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import strconv

struct Scanner {
mut:
	text     []byte
	pos      int
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
	true_
	false_
	eof
	comma = 44
	colon = 58
	lsbr = 91
	rsbr = 93
	lcbr = 123
	rcbr = 125
}

struct Error {
	description string
	line int
	col int
}

struct Token {
	lit  []byte
	kind TokenKind
	line int
	col  int
}

const (
	char_list = [`{`, `}`, `[`, `]`, `,`, `:`]
	newlines = [`\r`, `\n`, byte(9), `\t`]
	num_indicators = [`-`, `+`]
	important_escapable_chars = [byte(9), 10, 0]
	invalid_unicode_endpoints = [byte(9), 229]
	valid_unicode_escapes = [`b`, `f`, `n`, `r`, `t`, `\\`, `"`, `/`]
	unicode_escapes = {
		98: `\b`
		102: `\f`
		110: `\n`
		114: `\r`
		116: `\t`
		92: `\\`
		34: `"`
		47: `/`
	}
)

fn (mut s Scanner) move_pos() {
	s.pos++
	if s.pos < s.text.len {
		if s.text[s.pos] in json2.newlines {
			s.line++
			s.col = 0
			if s.text[s.pos] == `\r` && s.pos + 1 < s.text.len && s.text[s.pos + 1] == `\n` {
				s.pos++
			}
			for s.pos < s.text.len && s.text[s.pos] in json2.newlines {
				s.move_pos()
			}
		} else if s.text[s.pos] == ` ` {
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

fn (mut s Scanner) move_pos_upto(num int) {
	for i := 0; i < num; i++ {
		s.move_pos()
	}
}

fn (s Scanner) error(description string) Token {
	return s.tokenize(description.bytes(), .error)
}

fn (s Scanner) tokenize(lit []byte, kind TokenKind) Token {
	return Token{
		lit: lit
		kind: kind
		col: s.col
		line: s.line
	}
}

fn (mut s Scanner) text_scan() Token {
	mut has_closed := false
	mut chrs := []byte{}
	for {
		s.move_pos()
		if s.pos >= s.text.len {
			break
		}
		ch := s.text[s.pos]
		if ((s.pos - 1 >= 0 && s.text[s.pos - 1] != `/`) || s.pos == 0) && ch in json2.important_escapable_chars {
			return s.error('character must be escaped with a backslash')
		} else if s.pos == s.text.len - 1 && ch == `\\` {
			return s.error('invalid backslash escape')
		} else if s.pos + 1 < s.text.len && ch == `\\` {
			peek := s.text[s.pos + 1]
			if peek in json2.valid_unicode_escapes {
				chrs << unicode_escapes[int(peek)]
				s.move_pos()
				continue
			} else if peek == `u` {
				if s.pos + 5 < s.text.len {
					s.move_pos()
					mut codepoint := []byte{}
					codepoint_start := s.pos
					for s.pos < s.text.len && s.pos < codepoint_start + 4 {
						s.move_pos()
						if s.text[s.pos] == `"` {
							break
						} else if !s.text[s.pos].is_hex_digit() {
							return s.error('`${s.text[s.pos].ascii_str()}` is not a hex digit')
						}
						codepoint << s.text[s.pos]
					}
					if codepoint.len != 4 {
						return s.error('unicode escape must be 4 characters')
					}
					chrs << byte(strconv.parse_int(codepoint.bytestr(), 16, 0))
					continue
				} else {
					return s.error('incomplete unicode escape')
				}
			} else if peek == `U` {
				return s.error('unicode endpoints must be in lowercase `u`')
			} else if peek in json2.invalid_unicode_endpoints {
				return s.error('unicode endpoint not allowed')
			} else {
				return s.error('invalid backslash escape')
			}
		} else if ch == `"` {
			has_closed = true
			break
		}
		chrs << ch
	}
	s.move_pos()
	if !has_closed {
		return s.error('missing closing bracket in string')
	}
	return s.tokenize(chrs, .str_)
}

fn (mut s Scanner) num_scan() Token {
	is_minus := s.text[s.pos] == `-`

	// analyze json number structure
	// -[digit][?[dot][digit]][?[E/e][?-/+][digit]]
	mut is_fl := false
	mut digits := []byte{}
	if is_minus {
		digits << `-`
		s.move_pos()
	}
	if s.text[s.pos] == `0` && (s.pos + 1 < s.text.len && s.text[s.pos + 1].is_digit()) {
		return s.error('leading zeroes in a number are not allowed')
	}
	for s.pos < s.text.len && (s.text[s.pos].is_digit() || (!is_fl && s.text[s.pos] == `.`)) {
		digits << s.text[s.pos]
		if s.text[s.pos] == `.` {
			is_fl = true
		}
		s.move_pos()
	}
	if s.pos < s.text.len && (s.text[s.pos] == `e` || s.text[s.pos] == `E`) {
		digits << s.text[s.pos]
		s.move_pos()
		if s.pos < s.text.len && s.text[s.pos] in [`-`, `+`] {
			digits << s.text[s.pos]
			s.move_pos()
		}
		mut exp_digits := []byte{}
		for s.pos < s.text.len && s.text[s.pos].is_digit() {
			exp_digits << s.text[s.pos]
			s.move_pos()
		}
		if exp_digits.len == 0 {
			return s.error('invalid exponent')
		}
		digits << exp_digits
		unsafe { exp_digits.free() }
	}
	kind := if is_fl { TokenKind.float } else { TokenKind.int_ }
	return s.tokenize(digits, kind)
}

fn (s Scanner) invalid_token() Token {
	return s.error('invalid token `${s.text[s.pos]}`')
}

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
				kind = .true_
			}
			unsafe { ident.free() }
			val := s.text[s.pos..s.pos + 4]
			s.move_pos_upto(4)
			return s.tokenize(val, kind)
		}
		unsafe { ident.free() }
		return s.invalid_token()
	} if s.pos + 4 < s.text.len && s.text[s.pos] == `f` {
		ident := s.text[s.pos..s.pos + 5].bytestr()
		if ident == 'false' {
			unsafe { ident.free() }
			val := s.text[s.pos..s.pos + 5]
			s.move_pos_upto(5)
			return s.tokenize(val, .false_)
		}
		unsafe { ident.free() }
		return s.invalid_token()
	} else if s.text[s.pos] in json2.char_list {
		tok := s.text[s.pos]
		s.move_pos()
		return s.tokenize([]byte{}, TokenKind(int(tok)))
	} else if s.text[s.pos] == `"` {
		return s.text_scan()
	} else if s.text[s.pos].is_digit() || s.text[s.pos] == `-` {
		return s.num_scan()
	} else {
		return s.invalid_token()
	}
}
