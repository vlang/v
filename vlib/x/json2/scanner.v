// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import io
import strconv

// JsonScanError describes a tokenization error reported by the iterative scanner APIs.
pub struct JsonScanError {
	Error
pub:
	message string

	line      int
	character int
}

fn (e JsonScanError) msg() string {
	return '${e.line}:${e.character}: Invalid json token: ${e.message}'
}

// Scanner tokenizes JSON from an in-memory string or byte slice.
pub struct Scanner {
mut:
	text []u8
	pos  int // the position of the token in scanner text
	line int = 1
	col  int = 1
}

// ReaderScanner tokenizes JSON incrementally from any io.Reader.
pub struct ReaderScanner {
mut:
	reader &io.BufferedReader
	peeked bool
	ch     u8
	line   int = 1
	col    int = 1
}

// ReaderScannerConfig configures a reader-backed JSON scanner.
@[params]
pub struct ReaderScannerConfig {
pub:
	reader      io.Reader
	buffer_size int = 128 * 1024
}

// TokenKind identifies the kind of a JSON token.
pub enum TokenKind {
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

// new_scanner creates an iterative scanner for an in-memory JSON string.
pub fn new_scanner(text string) Scanner {
	return Scanner{
		text: text.bytes()
		line: 1
		col:  1
	}
}

// new_scanner_from_bytes creates an iterative scanner for an in-memory JSON byte slice.
pub fn new_scanner_from_bytes(text []u8) Scanner {
	return Scanner{
		text: text
		line: 1
		col:  1
	}
}

// new_reader_scanner creates an iterative scanner that reads JSON tokens from an io.Reader.
pub fn new_reader_scanner(config ReaderScannerConfig) &ReaderScanner {
	return &ReaderScanner{
		reader: io.new_buffered_reader(reader: config.reader, cap: config.buffer_size)
		line:   1
		col:    1
	}
}

// free releases the reader scanner's internal buffer.
pub fn (mut s ReaderScanner) free() {
	s.reader.free()
}

pub struct Token {
pub:
	lit  []u8      // literal representation of the token
	kind TokenKind // the token number/enum; for quick comparisons
	line int       // the line in the source where the token occurred
	col  int       // the column in the source where the token occurred
}

// literal returns the token contents as a string.
pub fn (t Token) literal() string {
	return t.lit.bytestr()
}

// full_col returns the full column information which includes the length.
pub fn (t Token) full_col() int {
	return t.col + t.lit.len
}

// is_eof reports whether the token marks the end of the JSON stream.
pub fn (t Token) is_eof() bool {
	return t.kind == .eof
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

fn new_scan_error(message string, line int, col int) JsonScanError {
	return JsonScanError{
		message:   message
		line:      line
		character: col
	}
}

fn token_to_scan_error(token Token) JsonScanError {
	return new_scan_error(token.literal(), token.line, token.col)
}

fn important_escapable_char(ch u8) ?u8 {
	return match ch {
		`\b` { `b` }
		`\f` { `f` }
		`\n` { `n` }
		`\r` { `r` }
		`\t` { `t` }
		else { none }
	}
}

fn invalid_token_description(ch u8) string {
	if ch >= 32 && ch <= 126 {
		x := ch.ascii_str()
		return 'invalid token `${x}`'
	} else {
		x := ch.str_escaped()
		return 'invalid token `${x}`'
	}
}

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
		} else if escaped := important_escapable_char(ch) {
			return s.error('character must be escaped with a backslash, replace with: \\${escaped.ascii_str()}')
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
		if s.pos + 1 >= s.text.len || !s.text[s.pos + 1].is_digit() {
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
	return s.error(invalid_token_description(s.text[s.pos]))
}

// next returns the next JSON token from the in-memory scanner.
pub fn (mut s Scanner) next() !Token {
	tok := s.scan()
	if tok.kind == .error {
		return token_to_scan_error(tok)
	}
	return tok
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

fn (mut s ReaderScanner) tokenize(lit []u8, kind TokenKind, line int, col int) Token {
	return Token{
		lit:  lit
		kind: kind
		line: line
		col:  col
	}
}

fn (mut s ReaderScanner) has_next_byte() !bool {
	if s.peeked {
		return true
	}
	mut buf := [u8(0)]
	n := s.reader.read(mut buf) or {
		if err is io.Eof {
			return false
		}
		return err
	}
	if n == 0 {
		return false
	}
	s.ch = buf[0]
	s.peeked = true
	return true
}

fn (mut s ReaderScanner) peek_byte() !u8 {
	if !s.has_next_byte()! {
		return io.Eof{}
	}
	return s.ch
}

fn (mut s ReaderScanner) advance_position(ch u8) ! {
	if ch == `\r` {
		if s.has_next_byte()! && s.ch == `\n` {
			s.peeked = false
		}
	}
	if ch in newlines {
		s.line++
		s.col = 1
		return
	}
	s.col++
}

fn (mut s ReaderScanner) read_byte() !u8 {
	ch := s.peek_byte()!
	s.peeked = false
	s.advance_position(ch)!
	return ch
}

fn (mut s ReaderScanner) skip_whitespace() ! {
	for {
		if !s.has_next_byte()! {
			return
		}
		ch := s.ch
		if ch == ` ` || ch in newlines {
			_ = s.read_byte()!
			continue
		}
		return
	}
}

fn (mut s ReaderScanner) scan_ident(ident string, kind TokenKind, line int, col int) !Token {
	mut lit := []u8{}
	for expected in ident.bytes() {
		current_line, current_col := s.line, s.col
		ch := s.read_byte() or {
			if err is io.Eof {
				return new_scan_error('unexpected end of JSON input', current_line, current_col)
			}
			return err
		}
		if ch != expected {
			return new_scan_error(invalid_token_description(ch), current_line, current_col)
		}
		lit << ch
	}
	return s.tokenize(lit, kind, line, col)
}

@[manualfree]
fn (mut s ReaderScanner) text_scan(line int, col int) !Token {
	mut chrs := []u8{}
	_ = s.read_byte()! // opening quote
	for {
		current_line, current_col := s.line, s.col
		if !s.has_next_byte()! {
			return new_scan_error('missing double quotes in string closing', line, col)
		}
		ch := s.ch
		if ch == `"` {
			_ = s.read_byte()!
			break
		} else if escaped := important_escapable_char(ch) {
			return new_scan_error('character must be escaped with a backslash, replace with: \\${escaped.ascii_str()}',
				current_line, current_col)
		} else if ch < 0x20 {
			return new_scan_error('character must be escaped with a unicode escape, replace with: \\u${ch:04x}',
				current_line, current_col)
		} else if ch == `\\` {
			_ = s.read_byte()!
			escape_line, escape_col := s.line, s.col
			if !s.has_next_byte()! {
				return new_scan_error('incomplete backslash escape at end of JSON input',
					escape_line, escape_col)
			}
			peek := s.ch
			if peek in valid_unicode_escapes {
				chrs << unicode_transform_escapes[int(peek)]
				_ = s.read_byte()!
				continue
			} else if peek == `u` {
				_ = s.read_byte()!
				mut codepoint := []u8{}
				for _ in 0 .. 4 {
					digit_line, digit_col := s.line, s.col
					if !s.has_next_byte()! {
						return new_scan_error('incomplete unicode escape', escape_line, escape_col)
					}
					digit := s.ch
					if digit == `"` {
						return new_scan_error('unicode escape must have 4 hex digits', digit_line,
							digit_col)
					} else if !digit.is_hex_digit() {
						x := digit.ascii_str()
						return new_scan_error('`${x}` is not a hex digit', digit_line, digit_col)
					}
					codepoint << digit
					_ = s.read_byte()!
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
			} else if peek == `U` {
				return new_scan_error('unicode endpoints must be in lowercase `u`', escape_line,
					escape_col)
			} else if peek == u8(229) {
				return new_scan_error('unicode endpoint not allowed', escape_line, escape_col)
			} else {
				return new_scan_error('invalid backslash escape', escape_line, escape_col)
			}
		}
		chrs << ch
		_ = s.read_byte()!
	}
	return s.tokenize(chrs, .str, line, col)
}

fn (mut s ReaderScanner) num_scan(line int, col int) !Token {
	mut is_fl := false
	mut dot_index := -1
	mut digits := []u8{}
	if s.peek_byte()! == `-` {
		digits << `-`
		_ = s.read_byte()!
		if !s.has_next_byte()! {
			return new_scan_error('invalid token `-`', line, col)
		}
		next := s.ch
		if !next.is_digit() {
			return new_scan_error(invalid_token_description(next), s.line, s.col)
		}
	}
	if s.has_next_byte()! {
		first := s.ch
		if first == `0` {
			digits << first
			_ = s.read_byte()!
			if s.has_next_byte()! && s.ch.is_digit() {
				return new_scan_error('leading zeroes in a number are not allowed', line, col)
			}
		}
	}
	for {
		if !s.has_next_byte()! {
			break
		}
		ch := s.ch
		if ch.is_digit() || (!is_fl && ch == `.`) {
			digits << ch
			if ch == `.` {
				is_fl = true
				dot_index = digits.len - 1
			}
			_ = s.read_byte()!
			continue
		}
		break
	}
	if dot_index != -1 && digits[dot_index + 1..].len == 0 {
		return new_scan_error('invalid float', line, col)
	}
	if s.has_next_byte()! {
		ch := s.ch
		if ch == `e` || ch == `E` {
			digits << ch
			_ = s.read_byte()!
			if s.has_next_byte()! && s.ch in exp_signs {
				digits << s.ch
				_ = s.read_byte()!
			}
			mut exp_digits_count := 0
			for {
				if !s.has_next_byte()! {
					break
				}
				digit := s.ch
				if !digit.is_digit() {
					break
				}
				digits << digit
				exp_digits_count++
				_ = s.read_byte()!
			}
			if exp_digits_count == 0 {
				return new_scan_error('invalid exponent', line, col)
			}
		}
	}
	kind := if is_fl { TokenKind.float } else { TokenKind.int }
	return s.tokenize(digits, kind, line, col)
}

// next returns the next JSON token from the reader-backed scanner.
pub fn (mut s ReaderScanner) next() !Token {
	s.skip_whitespace()!
	line, col := s.line, s.col
	if !s.has_next_byte()! {
		return s.tokenize([]u8{}, .eof, line, col)
	}
	ch := s.ch
	if ch == `t` || ch == `n` {
		ident := if ch == `t` { 'true' } else { 'null' }
		kind := if ch == `t` { TokenKind.bool } else { TokenKind.null }
		return s.scan_ident(ident, kind, line, col)
	} else if ch == `f` {
		return s.scan_ident('false', .bool, line, col)
	} else if ch in char_list {
		_ = s.read_byte()!
		return s.tokenize([]u8{}, unsafe { TokenKind(int(ch)) }, line, col)
	} else if ch == `"` {
		return s.text_scan(line, col)
	} else if ch.is_digit() || ch == `-` {
		return s.num_scan(line, col)
	}
	return new_scan_error(invalid_token_description(ch), line, col)
}
