// Copyright (c) 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json5

import encoding.utf8

// rune literals for the ASCII characters the scanner tests against. Runes are
// compared as `int` because V promotes a `rune` literal comparison to a string
// comparison, which is both slower and wrong for code points above 0x7F.
// ASCII code points the scanner compares against. They are compared as `int`
// because V promotes a `rune` comparison to a string comparison, which is both
// slower and wrong for code points above 0x7F.
const r_star = 0x2A
const r_plus = 0x2B
const r_upper_a = 0x41
const r_comma = 0x2C
const r_minus = 0x2D
const r_dot = 0x2E
const r_slash = 0x2F
const r_zero = 0x30
const r_upper_e = 0x45
const r_upper_f = 0x46
const r_upper_i = 0x49
const r_upper_n = 0x4E
const r_colon = 0x3A
const r_semicolon = 0x3B
const r_nine = 0x39
const r_lt = 0x3C
const r_upper_x = 0x58
const r_lbkt = 0x5B
const r_backslash = 0x5C
const r_rbkt = 0x5D
const r_underscr = 0x5F
const r_lower_a = 0x61
const r_lower_b = 0x62
const r_lower_e = 0x65
const r_lower_f = 0x66
const r_lower_n = 0x6E
const r_lower_r = 0x72
const r_lower_t = 0x74
const r_lower_u = 0x75
const r_lower_v = 0x76
const r_lower_x = 0x78
const r_lcbr = 0x7B
const r_rcbr = 0x7D
const r_quote_dbl = 0x22
const r_dollar = 0x24
const r_apostroph = 0x27
const r_space = 0x20
const r_tab = 0x09
const r_lf = 0x0A
const r_vt = 0x0B
const r_ff = 0x0C
const r_cr = 0x0D
const r_nbsp = 0xA0
const r_zwnj = 0x200C
const r_zwj = 0x200D
const r_line_sep = 0x2028
const r_para_sep = 0x2029
const r_bom = 0xFEFF

// TokenKind identifies the kind of a JSON5 token.
pub enum TokenKind {
	none
	error
	comment
	str
	ident
	number
	bool
	null
	infinity
	nan
	comma = 0x2C // ,
	colon = 0x3A // :
	lsbr  = 0x5B // [
	rsbr  = 0x5D // ]
	lcbr  = 0x7B // {
	rcbr  = 0x7D // }
	eof
}

// Pos is a location inside the scanned text.
pub struct Pos {
pub:
	line int
	col  int
}

// Token is a single lexical unit produced by the Scanner.
pub struct Token {
pub:
	kind TokenKind
	lit  string // decoded value for strings, raw source text otherwise
	pos  Pos
}

// Scanner tokenizes JSON5 text. Whitespace and comments are treated as trivia
// and skipped; every other token is reported to the caller.
pub struct Scanner {
pub:
	text []rune
mut:
	pos  int
	line int = 1
	col  int = 1
}

// new_scanner creates a Scanner over `text`, dropping a leading byte order mark.
pub fn new_scanner(text string) &Scanner {
	mut runes := text.runes()
	if runes.len > 0 && int(runes[0]) == r_bom {
		runes = runes[1..]
	}
	return &Scanner{
		text: runes
	}
}

// peek returns the rune `offset` positions ahead, or -1 at the end of the input.
fn (s &Scanner) peek(offset int) rune {
	i := s.pos + offset
	if i < 0 || i >= s.text.len {
		return -1
	}
	return s.text[i]
}

// peek_code returns the code point `offset` positions ahead, or -1 at the end of
// the input.
fn (s &Scanner) peek_code(offset int) int {
	r := s.peek(offset)
	return if r == -1 { -1 } else { int(r) }
}

// advance consumes the current rune and keeps the line/column counters in sync.
fn (mut s Scanner) advance() rune {
	if s.pos >= s.text.len {
		return -1
	}
	ch := s.text[s.pos]
	s.pos++
	if int(ch) == r_lf {
		s.line++
		s.col = 1
	} else {
		s.col++
	}
	return ch
}

// position returns the current position.
fn (s &Scanner) position() Pos {
	return Pos{
		line: s.line
		col:  s.col
	}
}

// is_digit reports whether `code` is an ASCII decimal digit.
fn is_digit(code int) bool {
	return code >= r_zero && code <= r_nine
}

// is_hex_digit reports whether `code` is an ASCII hexadecimal digit.
fn is_hex_digit(code int) bool {
	return (code >= r_zero && code <= r_nine) || (code >= r_lower_a && code <= r_lower_f)
		|| (code >= r_upper_a && code <= r_upper_f)
}

// hex_digit_value returns the numeric value of an ASCII hexadecimal digit.
fn hex_digit_value(code int) int {
	if code <= r_nine {
		return code - r_zero
	}
	if code <= r_upper_f {
		return code - r_upper_a + 10
	}
	return code - r_lower_a + 10
}

// is_whitespace reports whether `code` is JSON5 whitespace: the ASCII space
// characters, the no-break space, and any unicode space separator.
fn is_whitespace(code int) bool {
	return code == r_space || code == r_tab || code == r_lf || code == r_cr
		|| code == r_vt || code == r_ff || code == r_nbsp || code == r_bom
		|| (code > 0x2000 && utf8.is_space(rune(code)))
}

// is_line_terminator reports whether `code` ends a line for the purposes of `//`
// comments and escaped newlines in strings.
fn is_line_terminator(code int) bool {
	return code == r_lf || code == r_cr || code == r_line_sep || code == r_para_sep
}

// is_ident_start reports whether `code` may start an unquoted key.
fn is_ident_start(code int) bool {
	return code == r_dollar || code == r_underscr || utf8.is_letter(rune(code))
}

// is_ident_part reports whether `code` may continue an unquoted key. On top of
// `is_ident_start` it allows digits, combining marks and the two joiner runes
// used by non-latin scripts.
fn is_ident_part(code int) bool {
	if is_ident_start(code) || is_digit(code) {
		return true
	}
	return (code >= 0x0300 && code <= 0x036F) // combining diacritical marks
		|| code == r_zwnj || code == r_zwj || utf8.is_number(rune(code))
}

// skip_trivia consumes whitespace and comments.
fn (mut s Scanner) skip_trivia() ! {
	for s.pos < s.text.len {
		code := int(s.text[s.pos])
		if is_whitespace(code) {
			s.advance()
			continue
		}
		if code == r_slash && s.peek_code(1) == r_slash {
			s.advance()
			s.advance()
			for s.pos < s.text.len && !is_line_terminator(int(s.text[s.pos])) {
				s.advance()
			}
			continue
		}
		if code == r_slash && s.peek_code(1) == r_star {
			start := s.position()
			s.advance()
			s.advance()
			mut closed := false
			for s.pos < s.text.len {
				if int(s.text[s.pos]) == r_star && s.peek_code(1) == r_slash {
					s.advance()
					s.advance()
					closed = true
					break
				}
				s.advance()
			}
			if !closed {
				return syntax_error('unterminated block comment', start.line, start.col)
			}
			continue
		}
		return
	}
}

// next returns the next token, skipping whitespace and comments.
pub fn (mut s Scanner) next() !Token {
	s.skip_trivia()!
	start := s.position()
	if s.pos >= s.text.len {
		return Token{
			kind: .eof
			pos:  start
		}
	}
	code := int(s.text[s.pos])
	match code {
		r_lcbr, r_rcbr, r_lbkt, r_rbkt, r_comma, r_colon {
			ch := s.advance()
			return Token{
				kind: unsafe { TokenKind(code) }
				lit:  ch.str()
				pos:  start
			}
		}
		r_quote_dbl {
			return s.scan_string(start, r_quote_dbl)
		}
		r_apostroph {
			return s.scan_string(start, r_apostroph)
		}
		else {}
	}
	if is_digit(code) || code == r_minus || code == r_plus || code == r_dot {
		return s.scan_number(start)
	}
	if is_ident_start(code) {
		return s.scan_ident(start)
	}
	return syntax_error('unexpected token `${s.text[s.pos].str()}`', start.line, start.col)
}

// scan_string scans a single- or double-quoted string and returns its decoded
// value.
fn (mut s Scanner) scan_string(start Pos, quote_code int) !Token {
	s.advance() // opening quote
	mut out := []rune{}
	for {
		if s.pos >= s.text.len {
			return syntax_error('unterminated string', start.line, start.col)
		}
		code := int(s.text[s.pos])
		if code == quote_code {
			s.advance()
			break
		}
		if code == r_backslash {
			s.advance()
			if s.pos >= s.text.len {
				return syntax_error('unterminated escape sequence', start.line, start.col)
			}
			esc := int(s.text[s.pos])
			if is_line_terminator(esc) {
				// A backslash before a line terminator is a line continuation: it
				// and the terminator contribute nothing to the value.
				if esc == r_cr && s.peek_code(1) == r_lf {
					s.advance()
				}
				s.advance()
				continue
			}
			s.advance()
			match esc {
				r_lower_n { out << rune(0x0A) }
				r_lower_t { out << rune(0x09) }
				r_lower_r { out << rune(0x0D) }
				r_lower_b { out << rune(0x08) }
				r_lower_f { out << rune(0x0C) }
				r_lower_v { out << rune(0x0B) }
				r_zero { out << rune(0x00) }
				r_lower_x { out << s.scan_hex_digits(2, start)! }
				r_lower_u { out << s.scan_unicode_escape(start)! }
				else {
					// JSON5 lets any character be escaped, and the escape yields
					// the escaped character itself.
					out << rune(esc)
				}
			}
			continue
		}
		out << s.advance()
	}
	return Token{
		kind: .str
		lit:  out.string()
		pos:  start
	}
}

// scan_hex_digits reads `count` hexadecimal digits and returns them as a rune.
fn (mut s Scanner) scan_hex_digits(count int, start Pos) !rune {
	mut value := u32(0)
	for _ in 0 .. count {
		if s.pos >= s.text.len {
			return syntax_error('incomplete hexadecimal escape', start.line, start.col)
		}
		code := int(s.text[s.pos])
		if !is_hex_digit(code) {
			return syntax_error('`${s.text[s.pos].str()}` is not a hexadecimal digit',
				s.line, s.col)
		}
		value = value << 4 | u32(hex_digit_value(code))
		s.advance()
	}
	return rune(value)
}

// scan_unicode_escape reads either `\uXXXX` or the ES6 `\u{...}` form.
fn (mut s Scanner) scan_unicode_escape(start Pos) !rune {
	if s.peek_code(0) == r_lcbr {
		s.advance()
		mut value := u32(0)
		mut digits := 0
		for s.pos < s.text.len && int(s.text[s.pos]) != r_rcbr {
			code := int(s.text[s.pos])
			if !is_hex_digit(code) {
				return syntax_error('`${s.text[s.pos].str()}` is not a hexadecimal digit',
					s.line, s.col)
			}
			value = value << 4 | u32(hex_digit_value(code))
			digits++
			s.advance()
		}
		if s.pos >= s.text.len {
			return syntax_error('unterminated `\\u{...}` escape', start.line, start.col)
		}
		s.advance() // `}`
		if digits == 0 {
			return syntax_error('empty `\\u{...}` escape', start.line, start.col)
		}
		if value > 0x10FFFF || value >= 0xD800 && value <= 0xDFFF {
			return syntax_error('`\\u{...}` escape is out of the unicode range', start.line,
				start.col)
		}
		return rune(value)
	}
	first := s.scan_hex_digits(4, start)!
	if first >= 0xD800 && first <= 0xDBFF {
		if s.peek_code(0) != r_backslash || s.peek_code(1) != r_lower_u {
			return syntax_error('high surrogate requires a low surrogate escape', start.line,
				start.col)
		}
		s.advance()
		s.advance()
		second := s.scan_hex_digits(4, start)!
		if second < 0xDC00 || second > 0xDFFF {
			return syntax_error('high surrogate requires a low surrogate escape', start.line,
				start.col)
		}
		return rune(0x10000 + (u32(first) - 0xD800) * 0x400 + u32(second) - 0xDC00)
	}
	if first >= 0xDC00 && first <= 0xDFFF {
		return syntax_error('low surrogate requires a preceding high surrogate', start.line,
			start.col)
	}
	return first
}

// scan_ident scans an unquoted key, or one of the keywords `true`, `false`,
// `null`, `Infinity` and `NaN`.
fn (mut s Scanner) scan_ident(start Pos) !Token {
	mut out := []rune{}
	for s.pos < s.text.len && is_ident_part(int(s.text[s.pos])) {
		out << s.advance()
	}
	lit := out.string()
	kind := match lit {
		'true', 'false' { TokenKind.bool }
		'null' { TokenKind.null }
		'Infinity' { TokenKind.infinity }
		'NaN' { TokenKind.nan }
		else { TokenKind.ident }
	}
	return Token{
		kind: kind
		lit:  lit
		pos:  start
	}
}

// scan_number scans a JSON5 number: an optional sign, then either `Infinity`,
// `NaN`, a hexadecimal integer, or a decimal value with an optional leading or
// trailing dot and an optional exponent.
fn (mut s Scanner) scan_number(start Pos) !Token {
	mut out := []rune{}
	code := int(s.text[s.pos])
	if code == r_plus || code == r_minus {
		out << s.advance()
	}
	// `Infinity` and `NaN` may carry a sign; they are scanned as identifiers so
	// that a bare occurrence in key position still lexes as a key.
	if s.pos >= s.text.len {
		return syntax_error('number has no digits', start.line, start.col)
	}
	first := int(s.text[s.pos])
	if first == r_upper_i || first == r_upper_n {
		tok := s.scan_ident(start)!
		out << tok.lit.runes()
		return Token{
			kind: tok.kind
			lit:  out.string()
			pos:  start
		}
	}
	if int(s.text[s.pos]) == r_zero && (s.peek_code(1) == r_lower_x || s.peek_code(1) == r_upper_x) {
		out << s.advance()
		out << s.advance()
		mut digits := 0
		for s.pos < s.text.len && is_hex_digit(int(s.text[s.pos])) {
			out << s.advance()
			digits++
		}
		if digits == 0 {
			return syntax_error('hexadecimal literal has no digits', start.line, start.col)
		}
		return Token{
			kind: .number
			lit:  out.string()
			pos:  start
		}
	}
	mut int_digits := 0
	for s.pos < s.text.len && is_digit(int(s.text[s.pos])) {
		out << s.advance()
		int_digits++
	}
	// A dot is part of the number both as a decimal separator and as a leading
	// dot (`.5`), and it may also be trailing (`5.`).
	if s.pos < s.text.len && int(s.text[s.pos]) == r_dot {
		out << s.advance()
		for s.pos < s.text.len && is_digit(int(s.text[s.pos])) {
			out << s.advance()
			int_digits++
		}
	}
	if int_digits == 0 {
		return syntax_error('number has no digits', start.line, start.col)
	}
	if s.pos < s.text.len && (int(s.text[s.pos]) == r_lower_e || int(s.text[s.pos]) == r_upper_e) {
		out << s.advance()
		if s.pos < s.text.len && (int(s.text[s.pos]) == r_minus || int(s.text[s.pos]) == r_plus) {
			out << s.advance()
		}
		mut exp_digits := 0
		for s.pos < s.text.len && is_digit(int(s.text[s.pos])) {
			out << s.advance()
			exp_digits++
		}
		if exp_digits == 0 {
			return syntax_error('exponent has no digits', start.line, start.col)
		}
	}
	return Token{
		kind: .number
		lit:  out.string()
		pos:  start
	}
}
