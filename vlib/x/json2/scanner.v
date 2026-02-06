// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import strconv

struct Scanner {
mut:
	text []u8
	pos  int // the position of the token in scanner text
	line int
	col  int
}

enum TokenKind {
	none
	error
	str
	float
	int
	null
	bool
	eof
	comma = 44  // ,
	colon = 58  // :
	lsbr  = 91  // [
	rsbr  = 93  // ]
	lcbr  = 123 // {
	rcbr  = 125 // }
}

pub struct Token {
	lit  []u8      // literal representation of the token
	kind TokenKind // the token number/enum; for quick comparisons
	line int       // the line in the source where the token occurred
	col  int       // the column in the source where the token occurred
}

// full_col returns the full column information which includes the length.
pub fn (t Token) full_col() int {
	return t.col + t.lit.len
}

// list of characters commonly used in JSON.
const char_list = [`{`, `}`, `[`, `]`, `,`, `:`]!
// list of newlines to check when moving to a new position.
const newlines = [`\r`, `\n`, `\t`]!
// list of escapable that needs to be escaped inside a JSON string.
// double quotes and forward slashes are excluded intentionally since
// they have their own separate checks for it in order to pass the
// JSON test suite (https://github.com/nst/JSONTestSuite/).
const important_escapable_chars = [`\b`, `\f`, `\n`, `\r`, `\t`]!
// list of valid unicode escapes aside from \u{4-hex digits}
const valid_unicode_escapes = [`b`, `f`, `n`, `r`, `t`, `\\`, `"`, `/`]!
// used for transforming escapes into valid unicode (eg. n => \n)
const unicode_transform_escapes = {
	98:  `\b`
	102: `\f`
	110: `\n`
	114: `\r`
	116: `\t`
	92:  `\\`
	34:  `"`
	47:  `/`
}
const exp_signs = [u8(`-`), `+`]!

// move_pos proceeds to the next position.
fn (mut s Scanner) move() {
	s.move_pos(true, true)
}

// move_pos_with_newlines is the same as move_pos but only enables newline checking.
fn (mut s Scanner) move_pos_with_newlines() {
	s.move_pos(false, true)
}

fn (mut s Scanner) move_pos(include_space bool, include_newlines bool) {
	s.pos++
	if s.pos < s.text.len {
		if include_newlines && s.text[s.pos] in newlines {
			s.line++
			s.col = 0
			if s.text[s.pos] == `\r` && s.pos + 1 < s.text.len && s.text[s.pos + 1] == `\n` {
				s.pos++
			}
			for s.pos < s.text.len && s.text[s.pos] in newlines {
				s.move()
			}
		} else if include_space && s.text[s.pos] == ` ` {
			s.pos++
			s.col++
			for s.pos < s.text.len && s.text[s.pos] == ` ` {
				s.move()
			}
		}
	} else {
		s.col++
	}
}

// error returns an error token.
fn (s &Scanner) error(description string) Token {
	return s.tokenize(description.bytes(), .error)
}

// tokenize returns a token based on the given lit and kind.
fn (s &Scanner) tokenize(lit []u8, kind TokenKind) Token {
	return Token{
		lit:  lit
		kind: kind
		col:  s.col
		line: s.line
	}
}

// text_scan scans and returns a string token.
@[manualfree]
fn (mut s Scanner) text_scan() Token {
	mut has_closed := false
	mut chrs := []u8{}
	for {
		s.pos++
		s.col++
		if s.pos >= s.text.len {
			break
		}
		ch := s.text[s.pos]
		if ch == `"` {
			has_closed = true
			break
		} else if ch in important_escapable_chars {
			return s.error('character must be escaped with a backslash, replace with: \\${valid_unicode_escapes[important_escapable_chars.index(ch)]}')
		} else if ch < 0x20 {
			return s.error('character must be escaped with a unicode escape, replace with: \\u${ch:04x}')
		} else if ch == `\\` {
			if s.pos == s.text.len - 1 {
				return s.error('incomplete backslash escape at end of JSON input')
			}

			peek := s.text[s.pos + 1]
			if peek in valid_unicode_escapes {
				chrs << unicode_transform_escapes[int(peek)]
				s.pos++
				s.col++
				continue
			} else if peek == `u` {
				if s.pos + 5 < s.text.len {
					s.pos++
					s.col++
					mut codepoint := []u8{}
					codepoint_start := s.pos
					for s.pos < s.text.len && s.pos < codepoint_start + 4 {
						s.pos++
						s.col++
						if s.text[s.pos] == `"` {
							break
						} else if !s.text[s.pos].is_hex_digit() {
							x := s.text[s.pos].ascii_str()
							return s.error('`${x}` is not a hex digit')
						}
						codepoint << s.text[s.pos]
					}
					if codepoint.len != 4 {
						return s.error('unicode escape must have 4 hex digits')
					}
					val := u32(strconv.parse_uint(codepoint.bytestr(), 16, 32) or { 0 })
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
			} else if peek == u8(229) {
				return s.error('unicode endpoint not allowed')
			} else {
				return s.error('invalid backslash escape')
			}
		}
		chrs << ch
	}
	tok := s.tokenize(chrs, .str)
	s.move()
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
	mut digits := []u8{}
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
		if s.pos < s.text.len && s.text[s.pos] in exp_signs {
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
	kind := if is_fl { TokenKind.float } else { TokenKind.int }
	return s.tokenize(digits, kind)
}

// invalid_token returns an error token with the invalid token message.
fn (s &Scanner) invalid_token() Token {
	if s.text[s.pos] >= 32 && s.text[s.pos] <= 126 {
		x := s.text[s.pos].ascii_str()
		return s.error('invalid token `${x}`')
	} else {
		x := s.text[s.pos].str_escaped()
		return s.error('invalid token `${x}`')
	}
}

// scan returns a token based on the scanner's current position.
// used to set the next token
@[manualfree]
fn (mut s Scanner) scan() Token {
	if s.pos < s.text.len && (s.text[s.pos] == ` ` || s.text[s.pos] in newlines) {
		s.move()
	}
	if s.pos >= s.text.len {
		return s.tokenize([]u8{}, .eof)
	} else if s.pos + 3 < s.text.len && (s.text[s.pos] == `t` || s.text[s.pos] == `n`) {
		ident := s.text[s.pos..s.pos + 4].bytestr()
		if ident == 'true' || ident == 'null' {
			mut kind := TokenKind.null
			if ident == 'true' {
				kind = .bool
			}
			unsafe { ident.free() }
			val := s.text[s.pos..s.pos + 4]
			tok := s.tokenize(val, kind)
			s.move() // n / t
			s.move() // u / r
			s.move() // l / u
			s.move() // l / e
			return tok
		}
		unsafe { ident.free() }
		return s.invalid_token()
	} else if s.pos + 4 < s.text.len && s.text[s.pos] == `f` {
		ident := s.text[s.pos..s.pos + 5].bytestr()
		if ident == 'false' {
			unsafe { ident.free() }
			val := s.text[s.pos..s.pos + 5]
			tok := s.tokenize(val, .bool)
			s.move() // f
			s.move() // a
			s.move() // l
			s.move() // s
			s.move() // e
			return tok
		}
		unsafe { ident.free() }
		return s.invalid_token()
	} else if s.text[s.pos] in char_list {
		chr := s.text[s.pos]
		tok := s.tokenize([]u8{}, unsafe { TokenKind(int(chr)) })
		s.move()
		return tok
	} else if s.text[s.pos] == `"` {
		return s.text_scan()
	} else if s.text[s.pos].is_digit() || s.text[s.pos] == `-` {
		return s.num_scan()
	} else {
		return s.invalid_token()
	}
}
