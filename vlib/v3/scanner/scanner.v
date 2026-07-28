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

// Diagnostic describes a lexical error at a byte offset in the current source.
pub struct Diagnostic {
pub:
	offset  int
	end     int
	message string
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
	in_str_inter_format bool
	str_inter_cbr_depth int
	str_quote           u8
	diagnostics         []Diagnostic
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
	s.in_str_inter_format = false
	s.str_inter_cbr_depth = 0
	s.str_quote = 0
	s.diagnostics = []Diagnostic{}
	s.file = unsafe { file }
	s.src = src
}

fn (mut s Scanner) error(message string, offset int) {
	s.diagnostics << Diagnostic{
		offset:  offset
		end:     offset + 1
		message: message
	}
}

fn (mut s Scanner) error_span(message string, start int, end int) {
	s.diagnostics << Diagnostic{
		offset:  start
		end:     end
		message: message
	}
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
	start := s.pos
	s.offset++
	mut closed := false
	for s.offset < s.src.len {
		c2 := s.src[s.offset]
		if c2 == quote {
			closed = true
			break
		}
		if c2 == `\\` {
			if s.offset + 1 >= s.src.len {
				s.error('unterminated escape in character literal', s.offset)
				s.offset = s.src.len
				break
			}
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
	if !closed {
		s.error('unterminated character literal', start)
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
	if s.offset >= s.src.len {
		s.offset = s.src.len
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
			if s.peek_byte(0) == `*` {
				s.offset++
				if s.peek_byte(0) == `=` {
					s.offset++
					return .power_assign
				}
				return .power
			}
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
					s.in_str_inter_format = false
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
			if c >= 0x80 {
				end := int_min(s.pos + utf8_char_len(c), s.src.len)
				character := s.source_lit(s.pos, end)
				s.offset = end
				s.error('invalid character `${character}`', s.pos)
				return .unknown
			}
			s.error('unknown character `${c.ascii_str()}`', s.pos)
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
			} else if c2 == `/` && c3 == `*` && s.peek_byte(2) != `/` {
				// A `/*` only opens a nested comment when it is not immediately
				// followed by `/`. This keeps the `//*/` / `/*/` idioms (used to
				// close a block comment) from being misread as a nested opener.
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
		if ml_comment_depth > 0 {
			s.error('unterminated block comment', s.pos)
		}
	}
}

@[direct_array_access]
fn (mut s Scanner) string_literal(scan_as_raw bool, c_quote u8) {
	if scan_as_raw {
		mut closed := false
		for s.offset < s.src.len {
			c := s.src[s.offset]
			if c == c_quote {
				closed = true
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
		if !closed {
			s.error('unterminated raw string literal', s.pos)
		}
		return
	}
	for s.offset < s.src.len {
		c := s.src[s.offset]
		if c == `\\` {
			if s.offset + 1 >= s.src.len {
				s.error('unterminated escape in string literal', s.offset)
				s.offset = s.src.len
				return
			}
			s.check_string_escape(s.offset)
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
			return
		}
		s.offset++
	}
	s.error('unfinished string literal', s.src.len)
}

fn (mut s Scanner) check_string_escape(backslash_offset int) {
	escape_offset := backslash_offset + 1
	escape := s.src[escape_offset]
	if !is_known_string_escape(escape) {
		s.error('`${escape.ascii_str()}` unknown escape sequence', escape_offset)
		return
	}
	digits, message := match escape {
		`x` { 2, r'`\x` used without two following hex digits' }
		`u` { 4, r'`\u` incomplete 16 bit unicode character value' }
		`U` { 8, r'`\U` incomplete 32 bit unicode character value' }
		else { return }
	}
	mut value := u32(0)
	for i in 0 .. digits {
		index := escape_offset + 1 + i
		if index >= s.src.len || !s.src[index].is_hex_digit() {
			s.error(message, escape_offset)
			return
		}
		value = (value << 4) | string_escape_hex_value(s.src[index])
	}
	if escape != `x` && (value > 0x10ffff || (value >= 0xd800 && value <= 0xdfff)) {
		end := escape_offset + 1 + digits
		literal := s.source_lit(backslash_offset, end)
		s.error('invalid unicode point `${literal}`', end)
	}
}

fn is_known_string_escape(c u8) bool {
	return (c >= `0` && c <= `9`) || c == `\n`
		|| c in [`x`, `u`, `e`, `n`, `r`, `t`, `v`, `a`, `f`, `b`, `\\`, `\``, `$`, `@`, `?`, `{`, `}`, `'`, `"`, `U`]
}

fn string_escape_hex_value(c u8) u32 {
	if c >= `0` && c <= `9` {
		return u32(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return u32(c - `a`) + 10
	}
	return u32(c - `A`) + 10
}

@[direct_array_access]
fn (mut s Scanner) number() {
	if s.offset + 1 < s.src.len && s.src[s.offset] == `0` {
		c := s.peek_byte(1)
		if c == `b` || c == `B` {
			s.offset += 2
			digits := s.consume_digits(2)
			if s.offset < s.src.len && s.src[s.offset].is_alnum() {
				s.error('this binary number has unsuitable digit `${s.src[s.offset].ascii_str()}`',
					s.offset)
				s.consume_invalid_numeric_suffix()
			} else if digits == 0 {
				s.error('number part of this binary is not provided', s.pos + 1)
			}
			return
		} else if c == `x` || c == `X` {
			s.offset += 2
			digits := s.consume_digits(16)
			if s.offset < s.src.len && s.src[s.offset].is_alnum() {
				s.error('this hexadecimal number has unsuitable digit `${s.src[s.offset].ascii_str()}`',
					s.offset)
				s.consume_invalid_numeric_suffix()
			} else if digits == 0 {
				s.error('number part of this hexadecimal is not provided', s.pos + 1)
			}
			return
		} else if c == `o` || c == `O` {
			s.offset += 2
			digits := s.consume_digits(8)
			if s.offset < s.src.len && s.src[s.offset].is_alnum() {
				s.error('this octal number has unsuitable digit `${s.src[s.offset].ascii_str()}`',
					s.offset)
				s.consume_invalid_numeric_suffix()
			} else if digits == 0 {
				s.error('number part of this octal is not provided', s.pos + 1)
			}
			return
		}
	}
	s.consume_digits(10)
	mut has_fraction := false
	if s.offset < s.src.len && s.src[s.offset] == `.` && s.peek_byte(1) != `.` {
		next := s.peek_byte(1)
		if (next >= `0` && next <= `9`) || next == `e` || next == `E` {
			has_fraction = true
			s.offset++
			if next >= `0` && next <= `9` {
				s.consume_digits(10)
			}
		}
	}
	mut has_exponent := false
	mut exponent_has_no_digits := false
	mut exponent_pos := 0
	if s.offset < s.src.len && (s.src[s.offset] == `e` || s.src[s.offset] == `E`) {
		has_exponent = true
		exponent_pos = s.offset
		s.offset++
		if s.offset < s.src.len && (s.src[s.offset] == `+` || s.src[s.offset] == `-`) {
			s.offset++
		}
		if s.consume_digits(10) == 0 {
			if s.in_str_inter_format {
				s.offset = exponent_pos
				return
			}
			exponent_has_no_digits = true
		}
	}
	if s.offset < s.src.len && s.src[s.offset] == `.` && s.peek_byte(1).is_digit() {
		message := if has_exponent {
			'exponential part should be integer'
		} else if has_fraction {
			'too many decimal points in number'
		} else {
			''
		}
		if message.len > 0 {
			s.error(message, s.offset)
			s.offset++
			s.consume_digits(10)
		}
	}
	if !s.in_str_inter_format && !s.is_interpolation_format_number() && s.offset < s.src.len
		&& s.src[s.offset].is_letter() {
		invalid_digit_offset := s.offset
		invalid_digit := s.src[s.offset]
		s.consume_invalid_numeric_suffix()
		invalid_ident := s.number_prefixed_identifier_name(s.pos, s.offset)
		if exponent_has_no_digits {
			s.error('this number has unsuitable digit `${invalid_digit.ascii_str()}`',
				invalid_digit_offset)
		} else if invalid_ident.len > 0 {
			message := 'identifier name `${invalid_ident}` cannot start with a number'
			s.error_span(message, s.pos, s.offset)
		} else {
			s.error('this number has unsuitable digit `${invalid_digit.ascii_str()}`',
				invalid_digit_offset)
		}
	} else if exponent_has_no_digits {
		s.error('exponent has no digits', exponent_pos)
	}
}

fn (s &Scanner) number_prefixed_identifier_name(start int, end int) string {
	if end <= start || !s.src[start].is_digit() {
		return ''
	}
	mut ident_start := start
	for ident_start < end && (s.src[ident_start].is_digit() || s.src[ident_start] == `_`) {
		ident_start++
	}
	if ident_start >= end || !s.src[ident_start].is_letter() {
		return ''
	}
	for i in ident_start .. end {
		if !s.src[i].is_alnum() && s.src[i] != `_` {
			return ''
		}
	}
	if s.next_non_space_char(end) !in [`:`, `=`, `,`, `)`, `]`, `}`, `.`, `;`, 0] {
		return ''
	}
	return s.source_lit(start, end)
}

fn (s &Scanner) next_non_space_char(start int) u8 {
	for i in start .. s.src.len {
		if s.src[i] !in [` `, `\t`, `\r`, `\n`] {
			return s.src[i]
		}
	}
	return 0
}

fn (s &Scanner) is_interpolation_format_number() bool {
	if !s.in_str_inter || s.pos <= 0 {
		return false
	}
	mut index := s.pos - 1
	for index >= 0 && s.src[index] in [` `, `\t`] {
		index--
	}
	if index >= 0 && s.src[index] in [`+`, `-`] {
		index--
		for index >= 0 && s.src[index] in [` `, `\t`] {
			index--
		}
	}
	return index >= 0 && s.src[index] == `:`
}

fn (mut s Scanner) consume_invalid_numeric_suffix() {
	for s.offset < s.src.len && (s.src[s.offset].is_alnum() || s.src[s.offset] == `_`) {
		s.offset++
	}
}

@[direct_array_access]
fn (mut s Scanner) consume_digits(base int) int {
	mut digits := 0
	mut previous_underscore := false
	for s.offset < s.src.len {
		c := s.src[s.offset]
		if digit_value(c) < base {
			digits++
			previous_underscore = false
			s.offset++
			continue
		}
		if c == `_` {
			if digits == 0 || previous_underscore {
				s.error('numeric separators must occur between digits', s.offset)
			}
			previous_underscore = true
			s.offset++
			continue
		}
		break
	}
	if previous_underscore {
		s.error('numeric literal cannot end with a separator', s.offset - 1)
	}
	return digits
}

@[inline]
fn digit_value(c u8) int {
	if c >= `0` && c <= `9` {
		return int(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return int(c - `a`) + 10
	}
	if c >= `A` && c <= `F` {
		return int(c - `A`) + 10
	}
	return 256
}
