// Copyright (c) 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

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

// Json5TokenKind identifies the kind of a JSON5 token.
pub enum Json5TokenKind {
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

// Json5Pos is a location inside the scanned text.
pub struct Json5Pos {
	offset int // Byte offset in the original input, including a leading BOM.
pub:
	line int
	col  int
}

// Json5Token is a single lexical unit produced by the Json5Scanner.
pub struct Json5Token {
pub:
	kind   Json5TokenKind
	lit    string // Decoded value for strings and identifiers, raw source text otherwise.
	pos    Json5Pos
	length int // Token length in source bytes, before decoding escapes.
}

// Json5Scanner tokenizes JSON5 text. Whitespace and comments are treated as trivia
// and skipped; every other token is reported to the caller.
pub struct Json5Scanner {
	source string
pub:
	text []rune
mut:
	pos    int
	offset int
	line   int = 1
	col    int = 1
}

// new_json5_scanner creates a Json5Scanner over `text`, dropping a leading byte order mark.
pub fn new_json5_scanner(text string) &Json5Scanner {
	mut runes := text.runes()
	if runes.len > 0 && int(runes[0]) == r_bom {
		runes.delete(0)
	}
	return &Json5Scanner{
		source: text
		text:   runes
		offset: if text.starts_with('\uFEFF') { 3 } else { 0 }
	}
}

// peek returns the rune `offset` positions ahead, or -1 at the end of the input.
fn (s &Json5Scanner) peek(offset int) rune {
	i := s.pos + offset
	if i < 0 || i >= s.text.len {
		return -1
	}
	return s.text[i]
}

// peek_code returns the code point `offset` positions ahead, or -1 at the end of
// the input.
fn (s &Json5Scanner) peek_code(offset int) int {
	r := s.peek(offset)
	return if r == -1 { -1 } else { int(r) }
}

// advance consumes the current rune and keeps the line/column counters in sync.
fn (mut s Json5Scanner) advance() rune {
	if s.pos >= s.text.len {
		return -1
	}
	ch := s.text[s.pos]
	if int(ch) == 0xfffd && !s.source[s.offset..].starts_with('\uFFFD') {
		// string.runes replaces each invalid source byte with a replacement rune.
		s.offset++
	} else {
		s.offset += ch.length_in_bytes()
	}
	s.pos++
	if json5_is_line_terminator(int(ch)) {
		if int(ch) != r_lf || s.pos < 2 || int(s.text[s.pos - 2]) != r_cr {
			s.line++
		}
		s.col = 1
	} else {
		s.col++
	}
	return ch
}

// position returns the current position.
fn (s &Json5Scanner) position() Json5Pos {
	return Json5Pos{
		offset: s.offset
		line:   s.line
		col:    s.col
	}
}

// json5_is_digit reports whether `code` is an ASCII decimal digit.
fn json5_is_digit(code int) bool {
	return code >= r_zero && code <= r_nine
}

// json5_is_hex_digit reports whether `code` is an ASCII hexadecimal digit.
fn json5_is_hex_digit(code int) bool {
	return (code >= r_zero && code <= r_nine) || (code >= r_lower_a && code <= r_lower_f)
		|| (code >= r_upper_a && code <= r_upper_f)
}

// json5_hex_digit_value returns the numeric value of an ASCII hexadecimal digit.
fn json5_hex_digit_value(code int) int {
	if code <= r_nine {
		return code - r_zero
	}
	if code <= r_upper_f {
		return code - r_upper_a + 10
	}
	return code - r_lower_a + 10
}

// json5_is_whitespace reports whether `code` is JSON5 whitespace: the ASCII space
// characters, the no-break space, and any unicode space separator.
fn json5_is_whitespace(code int) bool {
	return code == r_space || code == r_tab || code == r_lf || code == r_cr
		|| code == r_vt || code == r_ff || code == r_nbsp || code == r_bom
		|| (code > 0xff && utf8.is_space(rune(code)))
}

// json5_is_line_terminator reports whether `code` ends a line for the purposes of `//`
// comments and escaped newlines in strings.
fn json5_is_line_terminator(code int) bool {
	return code == r_lf || code == r_cr || code == r_line_sep || code == r_para_sep
}

// json5_is_ident_start reports whether `code` may start an unquoted key.
fn json5_is_ident_start(code int) bool {
	if code < 0 || code > 0x10ffff {
		return false
	}
	return code == r_dollar || code == r_underscr || utf8.is_letter(rune(code))
		|| json5_in_unicode_ranges(code, json5_letter_number_ranges)
}

// json5_is_ident_part reports whether `code` may continue an unquoted key. On top of
// `json5_is_ident_start` it allows Mn/Mc marks, Nd digits, Pc connectors and joiners.
fn json5_is_ident_part(code int) bool {
	if json5_is_ident_start(code) || json5_is_digit(code) {
		return true
	}
	return code == r_zwnj || code == r_zwj
		|| json5_in_unicode_ranges(code, json5_identifier_part_ranges)
}

// json5_in_unicode_ranges searches sorted inclusive start/end pairs.
fn json5_in_unicode_ranges(code int, ranges [][2]int) bool {
	mut low := 0
	mut high := ranges.len
	for low < high {
		middle := low + (high - low) / 2
		if code < ranges[middle][0] {
			high = middle
		} else if code > ranges[middle][1] {
			low = middle + 1
		} else {
			return true
		}
	}
	return false
}

// skip_trivia consumes whitespace and comments.
fn (mut s Json5Scanner) skip_trivia() ! {
	for s.pos < s.text.len {
		code := int(s.text[s.pos])
		if json5_is_whitespace(code) {
			s.advance()
			continue
		}
		if code == r_slash && s.peek_code(1) == r_slash {
			s.advance()
			s.advance()
			for s.pos < s.text.len && !json5_is_line_terminator(int(s.text[s.pos])) {
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
				return json5_syntax_error('unterminated block comment', start.line, start.col)
			}
			continue
		}
		return
	}
}

// next returns the next token, decoding string and identifier escapes and skipping trivia.
@[manualfree]
pub fn (mut s Json5Scanner) next() !Json5Token {
	s.skip_trivia()!
	start := s.position()
	if s.pos >= s.text.len {
		return Json5Token{
			kind: .eof
			pos:  start
		}
	}
	code := int(s.text[s.pos])
	match code {
		r_lcbr, r_rcbr, r_lbkt, r_rbkt, r_comma, r_colon {
			ch := s.advance()
			return Json5Token{
				kind:   unsafe { Json5TokenKind(code) }
				lit:    ch.str()
				pos:    start
				length: s.offset - start.offset
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
	if json5_is_digit(code) || code == r_minus || code == r_plus || code == r_dot {
		return s.scan_number(start)
	}
	if json5_is_ident_start(code) || code == r_backslash {
		return s.scan_ident(start)
	}
	return json5_syntax_error('unexpected token `${s.text[s.pos].str()}`', start.line, start.col)
}

// scan_string scans a single- or double-quoted string and returns its decoded
// value.
@[manualfree]
fn (mut s Json5Scanner) scan_string(start Json5Pos, quote_code int) !Json5Token {
	s.advance() // opening quote
	mut out := []u8{cap: 32}
	unsafe { out.flags |= .noslices }
	// The returned UTF-8 view borrows this explicitly owned scratch buffer.
	mut utf8_buffer := unsafe { malloc_noscan(5) }
	defer {
		unsafe {
			out.free()
			free(utf8_buffer)
		}
	}
	for {
		if s.pos >= s.text.len {
			return json5_syntax_error('unterminated string', start.line, start.col)
		}
		code := int(s.text[s.pos])
		mut character := rune(code)
		if code == quote_code {
			s.advance()
			break
		}
		if code == r_backslash {
			s.advance()
			if s.pos >= s.text.len {
				return json5_syntax_error('unterminated escape sequence', start.line, start.col)
			}
			esc := int(s.text[s.pos])
			if json5_is_digit(esc) && (esc != r_zero || json5_is_digit(s.peek_code(1))) {
				return json5_syntax_error('invalid decimal digit in string escape', s.line, s.col)
			}
			if json5_is_line_terminator(esc) {
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
				r_lower_n { character = rune(0x0A) }
				r_lower_t { character = rune(0x09) }
				r_lower_r { character = rune(0x0D) }
				r_lower_b { character = rune(0x08) }
				r_lower_f { character = rune(0x0C) }
				r_lower_v { character = rune(0x0B) }
				r_zero { character = rune(0x00) }
				r_lower_x { character = s.scan_hex_digits(2, start)! }
				r_lower_u { character = s.scan_unicode_escape(start)! }
				else {
					// Other non-digit characters yield the escaped character itself.
					character = rune(esc)
				}
			}
		} else {
			// JSON5 permits literal U+2028/U+2029 in strings (specification section 5.2).
			if code == r_lf || code == r_cr {
				return json5_syntax_error('unescaped line terminator in string', s.line, s.col)
			}
			character = s.advance()
		}
		// This view borrows only the scratch UTF-8 buffer and is copied immediately.
		encoded := unsafe { utf32_to_str_no_malloc(u32(character), mut utf8_buffer) }
		for byte in encoded {
			out << byte
		}
	}
	return Json5Token{
		kind:   .str
		lit:    out.bytestr()
		pos:    start
		length: s.offset - start.offset
	}
}

// scan_hex_digits reads `count` hexadecimal digits and returns them as a rune.
fn (mut s Json5Scanner) scan_hex_digits(count int, start Json5Pos) !rune {
	mut value := u32(0)
	for _ in 0 .. count {
		if s.pos >= s.text.len {
			return json5_syntax_error('incomplete hexadecimal escape', start.line, start.col)
		}
		code := int(s.text[s.pos])
		if !json5_is_hex_digit(code) {
			return json5_syntax_error('`${s.text[s.pos].str()}` is not a hexadecimal digit',
				s.line, s.col)
		}
		value = value << 4 | u32(json5_hex_digit_value(code))
		s.advance()
	}
	return rune(value)
}

// scan_unicode_escape reads either `\uXXXX` or the ES6 `\u{...}` form.
fn (mut s Json5Scanner) scan_unicode_escape(start Json5Pos) !rune {
	if s.peek_code(0) == r_lcbr {
		s.advance()
		mut value := u32(0)
		mut digits := 0
		for s.pos < s.text.len && int(s.text[s.pos]) != r_rcbr {
			code := int(s.text[s.pos])
			if !json5_is_hex_digit(code) {
				return json5_syntax_error('`${s.text[s.pos].str()}` is not a hexadecimal digit',
					s.line, s.col)
			}
			value = value << 4 | u32(json5_hex_digit_value(code))
			digits++
			s.advance()
		}
		if s.pos >= s.text.len {
			return json5_syntax_error('unterminated `\\u{...}` escape', start.line, start.col)
		}
		s.advance() // `}`
		if digits == 0 {
			return json5_syntax_error('empty `\\u{...}` escape', start.line, start.col)
		}
		if value > 0x10FFFF || value >= 0xD800 && value <= 0xDFFF {
			return json5_syntax_error('`\\u{...}` escape is out of the unicode range', start.line,
				start.col)
		}
		return rune(value)
	}
	first := s.scan_hex_digits(4, start)!
	if first >= 0xD800 && first <= 0xDBFF {
		if s.peek_code(0) != r_backslash || s.peek_code(1) != r_lower_u {
			return json5_syntax_error('high surrogate requires a low surrogate escape', start.line,
				start.col)
		}
		s.advance()
		s.advance()
		second := s.scan_hex_digits(4, start)!
		if second < 0xDC00 || second > 0xDFFF {
			return json5_syntax_error('high surrogate requires a low surrogate escape', start.line,
				start.col)
		}
		return rune(0x10000 + (u32(first) - 0xD800) * 0x400 + u32(second) - 0xDC00)
	}
	if first >= 0xDC00 && first <= 0xDFFF {
		return json5_syntax_error('low surrogate requires a preceding high surrogate', start.line,
			start.col)
	}
	return first
}

// scan_ident scans an unquoted key, or one of the keywords `true`, `false`,
// `null`, `Infinity` and `NaN`.
@[manualfree]
fn (mut s Json5Scanner) scan_ident(start Json5Pos) !Json5Token {
	mut out := []u8{}
	unsafe { out.flags |= .noslices }
	// The returned UTF-8 view borrows this explicitly owned scratch buffer.
	mut utf8_buffer := unsafe { malloc_noscan(5) }
	mut has_escape := false
	defer {
		unsafe {
			out.free()
			free(utf8_buffer)
		}
	}
	for s.pos < s.text.len {
		code := int(s.text[s.pos])
		if code == r_backslash {
			escape := s.position()
			is_start := s.offset == start.offset
			if !has_escape {
				for i in start.offset .. s.offset {
					out << s.source[i]
				}
				has_escape = true
			}
			s.advance()
			if s.peek_code(0) != r_lower_u {
				return json5_syntax_error('identifier escape must use `\\uXXXX`', escape.line,
					escape.col)
			}
			s.advance()
			character := s.scan_hex_digits(4, escape)!
			valid := if is_start {
				json5_is_ident_start(int(character))
			} else {
				json5_is_ident_part(int(character))
			}
			if !valid {
				return json5_syntax_error('invalid character in identifier escape', escape.line,
					escape.col)
			}
			// This view borrows only the scratch UTF-8 buffer and is copied immediately.
			encoded := unsafe { utf32_to_str_no_malloc(u32(character), mut utf8_buffer) }
			for byte in encoded {
				out << byte
			}
		} else if json5_is_ident_part(code) {
			offset := s.offset
			s.advance()
			if has_escape {
				for i in offset .. s.offset {
					out << s.source[i]
				}
			}
		} else {
			break
		}
	}
	lit := if has_escape { out.bytestr() } else { s.source.substr(start.offset, s.offset) }
	// Escaped keywords are IdentifierNames for keys, rather than value literals.
	kind := if has_escape {
		Json5TokenKind.ident
	} else {
		match lit {
			'true', 'false' { Json5TokenKind.bool }
			'null' { Json5TokenKind.null }
			'Infinity' { Json5TokenKind.infinity }
			'NaN' { Json5TokenKind.nan }
			else { Json5TokenKind.ident }
		}
	}
	return Json5Token{
		kind:   kind
		lit:    lit
		pos:    start
		length: s.offset - start.offset
	}
}

// scan_number scans a JSON5 number: an optional sign, then either `Infinity`,
// `NaN`, a hexadecimal integer, or a decimal value with an optional leading or
// trailing dot and an optional exponent.
@[manualfree]
fn (mut s Json5Scanner) scan_number(start Json5Pos) !Json5Token {
	code := int(s.text[s.pos])
	if code == r_plus || code == r_minus {
		s.advance()
	}
	// `Infinity` and `NaN` may carry a sign; they are scanned as identifiers so
	// that a bare occurrence in key position still lexes as a key.
	if s.pos >= s.text.len {
		return json5_syntax_error('number has no digits', start.line, start.col)
	}
	first := int(s.text[s.pos])
	if first == r_upper_i || first == r_upper_n {
		tok := s.scan_ident(s.position())!
		defer {
			unsafe { tok.lit.free() }
		}
		return Json5Token{
			kind:   tok.kind
			lit:    s.source.substr(start.offset, s.offset)
			pos:    start
			length: s.offset - start.offset
		}
	}
	if int(s.text[s.pos]) == r_zero && (s.peek_code(1) == r_lower_x || s.peek_code(1) == r_upper_x) {
		s.advance()
		s.advance()
		mut digits := 0
		for s.pos < s.text.len && json5_is_hex_digit(int(s.text[s.pos])) {
			s.advance()
			digits++
		}
		if digits == 0 {
			return json5_syntax_error('hexadecimal literal has no digits', start.line, start.col)
		}
		return Json5Token{
			kind:   .number
			lit:    s.source.substr(start.offset, s.offset)
			pos:    start
			length: s.offset - start.offset
		}
	}
	if first == r_zero && json5_is_digit(s.peek_code(1)) {
		return json5_syntax_error('decimal literal has a leading zero', start.line, start.col)
	}
	mut int_digits := 0
	for s.pos < s.text.len && json5_is_digit(int(s.text[s.pos])) {
		s.advance()
		int_digits++
	}
	// A dot is part of the number both as a decimal separator and as a leading
	// dot (`.5`), and it may also be trailing (`5.`).
	if s.pos < s.text.len && int(s.text[s.pos]) == r_dot {
		s.advance()
		for s.pos < s.text.len && json5_is_digit(int(s.text[s.pos])) {
			s.advance()
			int_digits++
		}
	}
	if int_digits == 0 {
		return json5_syntax_error('number has no digits', start.line, start.col)
	}
	if s.pos < s.text.len && (int(s.text[s.pos]) == r_lower_e || int(s.text[s.pos]) == r_upper_e) {
		s.advance()
		if s.pos < s.text.len && (int(s.text[s.pos]) == r_minus || int(s.text[s.pos]) == r_plus) {
			s.advance()
		}
		mut exp_digits := 0
		for s.pos < s.text.len && json5_is_digit(int(s.text[s.pos])) {
			s.advance()
			exp_digits++
		}
		if exp_digits == 0 {
			return json5_syntax_error('exponent has no digits', start.line, start.col)
		}
	}
	return Json5Token{
		kind:   .number
		lit:    s.source.substr(start.offset, s.offset)
		pos:    start
		length: s.offset - start.offset
	}
}
