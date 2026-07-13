module scanner

import v3.token
import v3.pref

// Mode lists mode values used by scanner.
@[flag]
pub enum Mode {
	normal
	scan_comments
	skip_interpolation
}

// Scanner represents scanner data used by scanner.
pub struct Scanner {
	pref               &pref.Preferences
	mode               Mode
	skip_interpolation bool
mut:
	file        &token.File = unsafe { nil }
	insert_semi bool
pub mut:
	src                 string
	offset              int
	pos                 int
	lit                 string
	in_str_incomplete   bool
	in_str_inter        bool
	str_inter_cbr_depth int
	str_quote           u8
}

// peek_byte supports peek byte handling for Scanner.
@[direct_array_access; inline]
fn (s &Scanner) peek_byte(n int) u8 {
	idx := s.offset + n
	if idx < 0 || idx >= s.src.len {
		return 0
	}
	return s.src[idx]
}

// new_scanner supports new scanner handling for scanner.
pub fn new_scanner(prefs &pref.Preferences, mode Mode) Scanner {
	unsafe {
		return Scanner{
			pref:               prefs
			mode:               mode
			skip_interpolation: mode.has(.skip_interpolation)
		}
	}
}

// init supports init handling for Scanner.
pub fn (mut s Scanner) init(file &token.File, src string) {
	s.offset = 0
	s.pos = 0
	s.lit = ''
	s.insert_semi = false
	s.in_str_incomplete = false
	s.in_str_inter = false
	s.str_inter_cbr_depth = 0
	s.str_quote = 0
	s.file = unsafe { file }
	s.src = src
}

@[inline]
fn (s &Scanner) source_lit(start int, end int) string {
	if end <= start {
		return ''
	}
	unsafe {
		return tos(s.src.str + start, end - start)
	}
}

// scan_char_literal reads scan char literal input for scanner.
fn (mut s Scanner) scan_char_literal(quote u8) token.Token {
	s.offset++
	for s.offset < s.src.len {
		c2 := s.src[s.offset]
		if c2 == quote {
			break
		}
		if c2 == `\\` && s.offset + 1 < s.src.len {
			s.offset += 2
			continue
		}
		s.offset++
	}
	mut end := s.offset
	if s.offset < s.src.len && s.src[s.offset] == quote {
		end = s.offset
		s.offset++
	}
	s.lit = s.source_lit(s.pos + 1, end)
	s.insert_semi = true
	return .char
}

// current_file returns current file data for Scanner.
pub fn (s &Scanner) current_file() &token.File {
	return unsafe { s.file }
}

// scan supports scan handling for Scanner.
@[direct_array_access]
pub fn (mut s Scanner) scan() token.Token {
	if s.in_str_incomplete {
		s.in_str_incomplete = false
		s.pos = s.offset
		s.string_literal(false, s.str_quote)
		s.lit = s.source_lit(s.pos, s.offset)
		return .string
	}
	start:
	s.whitespace()
	if s.offset == s.src.len {
		s.lit = ''
		if s.insert_semi {
			s.insert_semi = false
			return .semicolon
		}
		return .eof
	}
	c := s.src[s.offset]
	s.pos = s.offset
	s.lit = ''
	preserve_insert_semi := s.insert_semi
	s.insert_semi = false
	if c == `\n` {
		s.lit = ''
		return .semicolon
	} else if c == `/` {
		c2 := s.peek_byte(1)
		if c2 == `/` || c2 == `*` {
			if preserve_insert_semi {
				s.insert_semi = true
			}
			s.comment()
			if !s.mode.has(.scan_comments) {
				unsafe {
					goto start
				}
			}
			s.lit = s.source_lit(s.pos, s.offset)
			return .comment
		} else if c2 == `=` {
			s.offset += 2
			return .div_assign
		}
		s.offset++
		return .div
	} else if c >= `0` && c <= `9` {
		s.number()
		s.lit = s.source_lit(s.pos, s.offset)
		s.insert_semi = true
		return .number
	} else if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || c == `_` || c == `@` {
		s.offset++
		if c == `@` && s.peek_byte(0) == `[` {
			s.offset++
			return .attribute
		}
		for s.offset < s.src.len {
			c3 := s.src[s.offset]
			if (c3 >= `a` && c3 <= `z`) || (c3 >= `A` && c3 <= `Z`)
				|| (c3 >= `0` && c3 <= `9`) || c3 == `_` {
				s.offset++
				continue
			}
			break
		}
		s.lit = s.source_lit(s.pos, s.offset)
		if s.lit == 'c' && s.offset < s.src.len && s.src[s.offset] == `'` {
			s.pos = s.offset
			tok := s.scan_char_literal(`'`)
			s.lit = 'c:${s.lit}'
			return tok
		}
		if s.lit == 'r' && s.offset < s.src.len
			&& (s.src[s.offset] == `'` || s.src[s.offset] == `"`) {
			quote := s.src[s.offset]
			s.offset++
			if !s.in_str_inter {
				s.str_quote = quote
			}
			s.string_literal(true, quote)
			s.lit = s.source_lit(s.pos, s.offset)
			s.insert_semi = true
			return .string
		}
		tok := token.Token.from_string_tinyv(s.lit)
		if tok in [.key_break, .key_continue, .key_nil, .key_none, .key_return, .key_false, .key_true,
			.name] {
			s.insert_semi = true
		}
		return tok
	} else if c == `'` || c == `"` {
		s.offset++
		if !s.in_str_inter {
			s.str_quote = c
		}
		s.string_literal(s.in_str_inter || (s.offset >= 2 && s.src[s.offset - 2] == `r`), c)
		s.lit = s.source_lit(s.pos, s.offset)
		s.insert_semi = true
		return .string
	} else if c == `\`` {
		return s.scan_char_literal(c)
	}
	s.lit = ''
	s.offset++
	match c {
		`.` {
			c2 := s.peek_byte(0)
			if c2 >= `0` && c2 <= `9` {
				s.number()
				s.lit = s.source_lit(s.pos, s.offset)
				return .number
			} else if c2 == `.` {
				s.offset++
				if s.peek_byte(0) == `.` {
					s.offset++
					return .ellipsis
				}
				return .dotdot
			}
			return .dot
		}
		`:` {
			if s.peek_byte(0) == `=` {
				s.offset++
				return .decl_assign
			}
			return .colon
		}
		`!` {
			c2 := s.peek_byte(0)
			if c2 == `=` {
				s.offset++
				return .ne
			} else if c2 == `i` {
				c3 := s.peek_byte(1)
				c4 := s.peek_byte(2)
				c4_is_space := c4 == ` ` || c4 == `\t`
				if c3 == `n` && c4_is_space {
					s.offset += 2
					return .not_in
				} else if c3 == `s` && c4_is_space {
					s.offset += 2
					return .not_is
				}
			}
			s.insert_semi = true
			return .not
		}
		`=` {
			if s.peek_byte(0) == `=` {
				s.offset++
				return .eq
			}
			return .assign
		}
		`+` {
			c2 := s.peek_byte(0)
			if c2 == `+` {
				s.offset++
				return .inc
			} else if c2 == `=` {
				s.offset++
				return .plus_assign
			}
			return .plus
		}
		`-` {
			c2 := s.peek_byte(0)
			if c2 == `-` {
				s.offset++
				return .dec
			} else if c2 == `=` {
				s.offset++
				return .minus_assign
			}
			return .minus
		}
		`%` {
			if s.peek_byte(0) == `=` {
				s.offset++
				return .mod_assign
			}
			return .mod
		}
		`*` {
			if s.peek_byte(0) == `=` {
				s.offset++
				return .mul_assign
			}
			return .mul
		}
		`^` {
			if s.peek_byte(0) == `=` {
				s.offset++
				return .xor_assign
			}
			return .xor
		}
		`&` {
			c2 := s.peek_byte(0)
			if c2 == `&` {
				if s.peek_byte(1) == `=` {
					s.offset += 2
					return .and_assign
				}
				s.offset++
				return .and
			} else if c2 == `=` {
				s.offset++
				return .and_assign
			}
			return .amp
		}
		`|` {
			c2 := s.peek_byte(0)
			if c2 == `|` {
				if s.peek_byte(1) == `=` {
					s.offset += 2
					return .or_assign
				}
				s.offset++
				return .logical_or
			} else if c2 == `=` {
				s.offset++
				return .or_assign
			}
			return .pipe
		}
		`<` {
			c2 := s.peek_byte(0)
			if c2 == `<` {
				s.offset++
				if s.peek_byte(0) == `=` {
					s.offset++
					return .left_shift_assign
				}
				return .left_shift
			} else if c2 == `=` {
				s.offset++
				return .le
			} else if c2 == `-` {
				s.offset++
				return .arrow
			}
			return .lt
		}
		`>` {
			c2 := s.peek_byte(0)
			if c2 == `>` {
				s.offset++
				c3 := s.peek_byte(0)
				if c3 == `>` {
					s.offset++
					if s.peek_byte(0) == `=` {
						s.offset++
						return .right_shift_unsigned_assign
					}
					return .right_shift_unsigned
				} else if c3 == `=` {
					s.offset++
					return .right_shift_assign
				}
				return .right_shift
			} else if c2 == `=` {
				s.offset++
				return .ge
			}
			return .gt
		}
		`#` {
			if s.peek_byte(0) == `[` {
				s.offset++
				// gated index `a#[..]`; the parser reads the marker from lit
				s.lit = '#'
				return .lsbr
			}
			start := s.offset
			for s.offset < s.src.len && s.src[s.offset] != `\n` {
				s.offset++
			}
			s.lit = s.source_lit(start, s.offset).trim_space()
			s.insert_semi = true
			return .hash
		}
		`~` {
			return .bit_not
		}
		`,` {
			return .comma
		}
		`$` {
			if s.in_str_inter {
				return .str_dollar
			}
			return .dollar
		}
		`{` {
			if s.in_str_inter {
				s.str_inter_cbr_depth++
			}
			return .lcbr
		}
		`}` {
			if s.in_str_inter {
				s.str_inter_cbr_depth--
				if s.str_inter_cbr_depth == 0 {
					s.in_str_incomplete = true
					s.in_str_inter = false
				}
			}
			s.insert_semi = true
			return .rcbr
		}
		`(` {
			return .lpar
		}
		`)` {
			s.insert_semi = true
			return .rpar
		}
		`[` {
			s.lit = ''
			return .lsbr
		}
		`]` {
			s.insert_semi = true
			return .rsbr
		}
		`;` {
			return .semicolon
		}
		`?` {
			s.insert_semi = true
			return .question
		}
		else {
			return .unknown
		}
	}
}

@[direct_array_access]
fn (mut s Scanner) whitespace() {
	for s.offset < s.src.len {
		c := s.src[s.offset]
		if c == ` ` || c == `\t` || c == `\r` {
			s.offset++
			continue
		} else if c == `\n` {
			if s.insert_semi {
				return
			}
			s.offset++
			continue
		}
		break
	}
}

@[direct_array_access]
fn (mut s Scanner) line() {
	for s.offset < s.src.len {
		if s.src[s.offset] == `\n` {
			break
		}
		s.offset++
	}
}

@[direct_array_access]
fn (mut s Scanner) comment() {
	s.offset++
	if s.offset >= s.src.len {
		return
	}
	c := s.src[s.offset]
	if c == `/` {
		s.line()
	} else if c == `*` {
		s.offset++
		mut ml_comment_depth := 1
		for s.offset < s.src.len {
			c2 := s.src[s.offset]
			c3 := s.peek_byte(1)
			if c2 == `\n` {
				s.offset++
			} else if c2 == `/` && c3 == `*` {
				s.offset += 2
				ml_comment_depth++
			} else if c2 == `*` && c3 == `/` {
				s.offset += 2
				ml_comment_depth--
				if ml_comment_depth == 0 {
					break
				}
			} else {
				s.offset++
			}
		}
	}
}

@[direct_array_access]
fn (mut s Scanner) string_literal(scan_as_raw bool, c_quote u8) {
	if scan_as_raw {
		for s.offset < s.src.len {
			c := s.src[s.offset]
			if c == c_quote {
				break
			}
			if c == `\n` {
				s.offset++
				continue
			}
			s.offset++
		}
		if s.offset < s.src.len {
			s.offset++
		}
		return
	}
	for s.offset < s.src.len {
		c := s.src[s.offset]
		if c == `\\` {
			s.offset += 2
			continue
		} else if c == `\n` {
			s.offset++
			continue
		} else if c == `$` && s.peek_byte(1) == `{` {
			s.in_str_inter = true
			if s.skip_interpolation {
				s.str_inter_cbr_depth++
				s.offset += 2
				continue
			} else {
				return
			}
		} else if s.skip_interpolation && s.in_str_inter {
			if c == `{` {
				s.str_inter_cbr_depth++
			} else if c == `}` {
				s.str_inter_cbr_depth--
				if s.str_inter_cbr_depth == 0 {
					s.in_str_inter = false
				}
			}
		} else if c == c_quote && !s.in_str_inter {
			s.offset++
			break
		}
		s.offset++
	}
}

@[direct_array_access]
fn (mut s Scanner) number() {
	if s.offset < s.src.len && s.src[s.offset] == `0` {
		s.offset++
		c := s.peek_byte(0)
		if c == `b` || c == `B` {
			s.offset++
			for s.offset < s.src.len {
				c2 := s.src[s.offset]
				if c2 == `0` || c2 == `1` || c2 == `_` {
					s.offset++
					continue
				}
				return
			}
			return
		} else if c == `x` || c == `X` {
			s.offset++
			for s.offset < s.src.len {
				c2 := s.src[s.offset]
				if (c2 >= `0` && c2 <= `9`) || (c2 >= `a` && c2 <= `f`)
					|| (c2 >= `A` && c2 <= `F`) || c2 == `_` {
					s.offset++
					continue
				}
				return
			}
			return
		} else if c == `o` || c == `O` {
			s.offset++
			for s.offset < s.src.len {
				c2 := s.src[s.offset]
				if (c2 >= `0` && c2 <= `7`) || c2 == `_` {
					s.offset++
					continue
				}
				return
			}
			return
		}
	}
	mut has_decimal := false
	mut has_exponent := false
	for s.offset < s.src.len {
		c := s.src[s.offset]
		if (c >= `0` && c <= `9`) || c == `_` {
			s.offset++
			continue
		} else if !has_decimal && c == `.` {
			next := s.peek_byte(1)
			mut has_dot_exponent := false
			if next == `e` || next == `E` {
				after_exp := s.peek_byte(2)
				if after_exp == `+` || after_exp == `-` {
					after_sign := s.peek_byte(3)
					has_dot_exponent = after_sign >= `0` && after_sign <= `9`
				} else {
					has_dot_exponent = after_exp >= `0` && after_exp <= `9`
				}
			}
			if next != `.` && ((next >= `0` && next <= `9`) || has_dot_exponent) {
				has_decimal = true
				s.offset++
				continue
			}
		} else if !has_exponent && (c == `e` || c == `E`) {
			has_exponent = true
			s.offset++
			if s.offset < s.src.len && (s.src[s.offset] == `+` || s.src[s.offset] == `-`) {
				s.offset++
			}
			continue
		}
		break
	}
}
