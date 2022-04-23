// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module scanner

import toml.input
import toml.token
import toml.util

pub const (
	digit_extras = [`_`, `.`, `x`, `o`, `b`, `e`, `E`]
	end_of_text  = u32(~0)
)

// Scanner contains the necessary fields for the state of the scan process.
// the task the scanner does is also refered to as "lexing" or "tokenizing".
// The Scanner methods are based on much of the work in `vlib/strings/textscanner`.
pub struct Scanner {
pub:
	config Config
	text   string // the input TOML text
mut:
	col        int // current column number (x coordinate)
	line_nr    int = 1 // current line number (y coordinate)
	pos        int // current flat/index position in the `text` field
	header_len int // Length, how many bytes of header was found
	// Quirks
	is_left_of_assign bool = true // indicates if the scanner is on the *left* side of an assignment
}

// State is a read-only copy of the scanner's internal state.
// See also `Scanner.state()`.
pub struct State {
pub:
	col     int // current column number (x coordinate)
	line_nr int = 1 // current line number (y coordinate)
	pos     int // current flat/index position in the `text` field
}

// Config is used to configure a Scanner instance.
// Only one of the fields `text` and `file_path` is allowed to be set at time of configuration.
pub struct Config {
pub:
	input               input.Config
	tokenize_formatting bool = true // if true, generate tokens for `\n`, ` `, `\t`, `\r` etc.
}

// new_scanner returns a new *heap* allocated `Scanner` instance, based on the file in config.input.file_path,
// or based on the text in config.input.text .
pub fn new_scanner(config Config) ?&Scanner {
	mut s := &Scanner{
		config: config
		text: config.input.read_input() ?
	}
	return s
}

// new_simple returns a new *stack* allocated `Scanner` instance.
pub fn new_simple(config Config) ?Scanner {
	return Scanner{
		config: config
		text: config.input.read_input() ?
	}
}

// new_simple_text returns a new *stack* allocated `Scanner` instance
// ready for parsing TOML in `text`.
pub fn new_simple_text(text string) ?Scanner {
	in_config := input.Config{
		text: text
	}
	config := Config{
		input: in_config
	}
	return Scanner{
		config: config
		text: config.input.read_input() ?
	}
}

// new_simple_file returns a new *stack* allocated `Scanner` instance
// ready for parsing TOML in file read from `path`.
pub fn new_simple_file(path string) ?Scanner {
	in_config := input.Config{
		file_path: path
	}
	config := Config{
		input: in_config
	}
	return Scanner{
		config: config
		text: config.input.read_input() ?
	}
}

// scan returns the next token from the input.
[direct_array_access]
pub fn (mut s Scanner) scan() ?token.Token {
	s.validate_and_skip_headers() ?

	for {
		c := s.next()
		byte_c := u8(c)
		if c == scanner.end_of_text {
			s.inc_line_number()
			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'reached EOF')
			return s.new_token(.eof, '', 1)
		}

		ascii := byte_c.ascii_str()
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'current char "$ascii"')

		if byte_c == u8(0x0) {
			s.reset()
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' NULL control character `$c.hex()` is not allowed at ($s.line_nr,$s.col) "$ascii" near ...${s.excerpt(s.pos, 5)}...')
		}

		is_sign := c == `+` || c == `-`

		// (+/-)nan & (+/-)inf
		peek_1 := s.peek(1)
		peek_2 := s.peek(2)
		is_nan := c == `n` && s.at() == `a` && peek_1 == `n`
		is_inf := !is_nan && c == `i` && s.at() == `n` && peek_1 == `f`
		is_signed_nan := is_sign && s.at() == `n` && peek_1 == `a` && peek_2 == `n`
		is_signed_inf := !is_signed_nan && is_sign && s.at() == `i` && peek_1 == `n`
			&& peek_2 == `f`
		if !s.is_left_of_assign && (is_nan || is_inf || is_signed_nan || is_signed_inf) {
			num := s.extract_nan_or_inf_number() ?
			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified a special number "$num" ($num.len)')
			return s.new_token(.number, num, num.len)
		}

		is_signed_number := is_sign && u8(s.at()).is_digit() && !u8(s.peek(-1)).is_digit()
		is_digit := byte_c.is_digit()
		if is_digit || is_signed_number {
			num := s.extract_number() ?
			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified a number "$num" ($num.len)')
			return s.new_token(.number, num, num.len)
		}

		if util.is_key_char(byte_c) {
			key := s.extract_key()
			if !s.is_left_of_assign && (key == 'true' || key == 'false') {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified a boolean "$key" ($key.len)')
				return s.new_token(.boolean, key, key.len)
			}
			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified a bare key "$key" ($key.len)')
			return s.new_token(.bare, key, key.len)
		}

		match rune(c) {
			` `, `\t`, `\n`, `\r` {
				if c == `\n` {
					s.inc_line_number()
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'incremented line nr to $s.line_nr')
				}
				// Date-Time in RFC 3339 is allowed to have a space between the date and time in supplement to the 'T'
				// so we allow space characters to slip through to the parser if the space is between two digits...
				// util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, '"'+u8(s.peek(-1)).ascii_str()+'" < "$ascii" > "'+u8(s.at()).ascii_str()+'"')
				if c == ` ` && u8(s.peek(-1)).is_digit() && u8(s.at()).is_digit() {
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified, what could be, a space between a RFC 3339 date and time ("$ascii") ($ascii.len)')
					return s.new_token(token.Kind.whitespace, ascii, ascii.len)
				}
				if s.config.tokenize_formatting {
					mut kind := token.Kind.whitespace
					if c == `\t` {
						kind = token.Kind.tab
					} else if c == `\r` {
						kind = token.Kind.cr
					} else if c == `\n` {
						kind = token.Kind.nl
					}
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified formatting character ("$ascii") ($ascii.len)')
					return s.new_token(kind, ascii, ascii.len)
				} else {
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping " ", "\\t" or "\\n" ("$ascii") ($ascii.len)')
				}
				continue
			}
			`-` {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified minus "$ascii" ($ascii.len)')
				return s.new_token(.minus, ascii, ascii.len)
			}
			`_` {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified underscore "$ascii" ($ascii.len)')
				return s.new_token(.underscore, ascii, ascii.len)
			}
			`+` {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified plus "$ascii" ($ascii.len)')
				return s.new_token(.plus, ascii, ascii.len)
			}
			`=` {
				s.is_left_of_assign = false
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified assignment "$ascii" ($ascii.len)')
				return s.new_token(.assign, ascii, ascii.len)
			}
			`"`, `'` { // ... some string "/'
				ident_string := s.extract_string() ?
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified quoted string `$ident_string`')
				return s.new_token(.quoted, ident_string, ident_string.len)
			}
			`#` {
				hash := s.ignore_line() ?
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified comment hash "$hash" ($hash.len)')
				return s.new_token(.hash, hash, hash.len + 1)
			}
			`{` {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified left curly bracket "$ascii" ($ascii.len)')
				return s.new_token(.lcbr, ascii, ascii.len)
			}
			`}` {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified right curly bracket "$ascii" ($ascii.len)')
				return s.new_token(.rcbr, ascii, ascii.len)
			}
			`[` {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified left square bracket "$ascii" ($ascii.len)')
				return s.new_token(.lsbr, ascii, ascii.len)
			}
			`]` {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified right square bracket "$ascii" ($ascii.len)')
				return s.new_token(.rsbr, ascii, ascii.len)
			}
			`:` {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified colon "$ascii" ($ascii.len)')
				return s.new_token(.colon, ascii, ascii.len)
			}
			`,` {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified comma "$ascii" ($ascii.len)')
				return s.new_token(.comma, ascii, ascii.len)
			}
			`.` {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified period "$ascii" ($ascii.len)')
				return s.new_token(.period, ascii, ascii.len)
			}
			else {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' could not scan character `$ascii` / $c at $s.pos ($s.line_nr,$s.col) near ...${s.excerpt(s.pos, 5)}...')
			}
		}
	}
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'unknown character code at $s.pos ($s.line_nr,$s.col) near ...${s.excerpt(s.pos,
		5)}...')
	return s.new_token(.unknown, '', 0)
}

// free frees all allocated resources.
[unsafe]
pub fn (mut s Scanner) free() {
	unsafe {
		s.text.free()
	}
}

// remaining returns how many characters remain in the text input.
[inline]
pub fn (s &Scanner) remaining() int {
	return s.text.len - s.pos
}

// next returns the next character code from the input text.
// next returns `end_of_text` if it can't reach the next character.
[direct_array_access; inline]
pub fn (mut s Scanner) next() u32 {
	if s.pos < s.text.len {
		opos := s.pos
		s.pos++
		s.col++
		c := s.text[opos]
		return c
	}
	return scanner.end_of_text
}

// skip skips one character ahead.
[inline]
pub fn (mut s Scanner) skip() {
	if s.pos + 1 < s.text.len {
		s.pos++
		s.col++
	}
}

// skip_n skips ahead `n` characters.
// If the skip goes out of bounds from the length of `Scanner.text`,
// the scanner position will be sat to the last character possible.
[inline]
pub fn (mut s Scanner) skip_n(n int) {
	s.pos += n
	if s.pos > s.text.len {
		s.pos = s.text.len
	}
	s.col = s.pos
}

// at returns the *current* character code from the input text.
// at returns `end_of_text` if it can't get the current character.
// unlike `next()`, `at()` does not change the state of the scanner.
[direct_array_access; inline]
pub fn (s &Scanner) at() u32 {
	if s.pos < s.text.len {
		return s.text[s.pos]
	}
	return scanner.end_of_text
}

// at_crlf returns `true` if the scanner is at a `\r` character
// and the next character is a `\n`.
fn (s Scanner) at_crlf() bool {
	return s.at() == `\r` && s.peek(1) == `\n`
}

// peek returns the character code from the input text at position + `n`.
// peek returns `end_of_text` if it can't peek `n` characters ahead.
[direct_array_access; inline]
pub fn (s &Scanner) peek(n int) u32 {
	if s.pos + n < s.text.len {
		// Allow peeking back - needed for spaces between date and time in RFC 3339 format :/
		if n - 1 < 0 && s.pos + n - 1 >= 0 {
			// util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'LOOKING BAAAA-AACK - OOOVER MY SHOOOOULDEEEER "${s.text[s.pos + n-1]}"')
			return s.text[s.pos + n - 1]
		}
		return s.text[s.pos + n]
	}
	return scanner.end_of_text
}

// reset resets the internal state of the scanner.
pub fn (mut s Scanner) reset() {
	s.pos = 0
	s.col = 0
	s.line_nr = 1
	s.header_len = 0
}

// new_token returns a new `token.Token`.
[inline]
fn (mut s Scanner) new_token(kind token.Kind, lit string, len int) token.Token {
	// println('new_token($lit)')
	mut col := s.col - len + 1
	if s.line_nr == 1 {
		col -= s.header_len
	}
	return token.Token{
		kind: kind
		lit: lit
		col: if col < 1 { 1 } else { col }
		line_nr: s.line_nr + 1
		pos: s.pos - s.header_len - len + 1
		len: len
	}
}

// ignore_line forwards the scanner to the end of the current line.
[direct_array_access; inline]
fn (mut s Scanner) ignore_line() ?string {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, ' ignoring until EOL...')
	start := s.pos
	for c := s.at(); c != scanner.end_of_text && c != `\n`; c = s.at() {
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping "${u8(c).ascii_str()} / $c"')
		if s.at_crlf() {
			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'letting `\\r\\n` slip through')
			break
		}
		s.next()
	}
	return s.text[start..s.pos]
}

// inc_line_number increases the internal line number.
[inline]
fn (mut s Scanner) inc_line_number() {
	s.col = 0
	s.line_nr++
	s.is_left_of_assign = true
}

// extract_key parses and returns a TOML key as a string.
[direct_array_access; inline]
fn (mut s Scanner) extract_key() string {
	s.pos--
	s.col--
	start := s.pos
	for s.pos < s.text.len {
		c := u8(s.at())
		if !(util.is_key_char(c) || c.is_digit() || c in [`_`, `-`]) {
			break
		}
		s.pos++
		s.col++
	}
	key := s.text[start..s.pos]
	return key
}

// extract_string collects and returns a string containing
// any bytes recognized as a TOML string.
// TOML strings are everything found between two double or single quotation marks (`"`/`'`).
[direct_array_access; inline]
fn (mut s Scanner) extract_string() ?string {
	// extract_string is called when the scanner has already reached
	// a byte that is the start of a string so we rewind it to start at the correct
	s.pos--
	s.col--
	quote := u8(s.at())
	start := s.pos
	mut lit := quote.ascii_str()

	is_multiline := s.text[s.pos + 1] == quote && s.text[s.pos + 2] == quote
	// Check for escaped multiline quote
	if is_multiline {
		mls := s.extract_multiline_string() ?
		return mls
	}

	for {
		s.pos++
		s.col++

		if s.pos >= s.text.len {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' unfinished single-line string literal `$quote.ascii_str()` started at $start ($s.line_nr,$s.col) "${u8(s.at()).ascii_str()}" near ...${s.excerpt(s.pos, 5)}...')
		}

		c := u8(s.at())
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'c: `$c.ascii_str()` / $c (quote type: $quote/$quote.ascii_str())')

		// Check for escaped chars
		if c == u8(92) {
			esc, skip := s.handle_escapes(quote, is_multiline)
			lit += esc
			if skip > 0 {
				s.pos += skip
				s.col += skip
				continue
			}
		}
		// Check for control characters (allow TAB)
		if util.is_illegal_ascii_control_character(c) {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' control character `$c.hex()` is not allowed at $start ($s.line_nr,$s.col) "${u8(s.at()).ascii_str()}" near ...${s.excerpt(s.pos, 5)}...')
		}

		if c == quote {
			s.pos++
			s.col++
			return lit + quote.ascii_str()
		}

		lit += c.ascii_str()

		// Don't eat multiple lines in single-line mode
		if lit.contains('\n') {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' unfinished single-line string literal `$quote.ascii_str()` started at $start ($s.line_nr,$s.col) "${u8(s.at()).ascii_str()}" near ...${s.excerpt(s.pos, 5)}...')
		}
	}
	return lit
}

// extract_multiline_string collects and returns a string containing
// any bytes recognized as a TOML string.
// TOML strings are everything found between two double or single quotation marks (`"`/`'`).
[direct_array_access; inline]
fn (mut s Scanner) extract_multiline_string() ?string {
	// extract_multiline_string is called from extract_string so we know the 3 first
	// characters is the quotes
	quote := u8(s.at())
	start := s.pos
	mut lit := quote.ascii_str() + quote.ascii_str() + quote.ascii_str()

	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'multi-line `$quote.ascii_str()${s.text[s.pos +
		1].ascii_str()}${s.text[s.pos + 2].ascii_str()}` string started at pos $start ($s.line_nr,$s.col) (quote type: $quote.ascii_str() / $quote)')

	s.pos += 2
	s.col += 2

	for {
		s.pos++
		s.col++

		if s.pos >= s.text.len {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' unfinished multi-line string literal ($quote.ascii_str()$quote.ascii_str()$quote.ascii_str()) started at $start ($s.line_nr,$s.col) "${u8(s.at()).ascii_str()}" near ...${s.excerpt(s.pos, 5)}...')
		}

		c := u8(s.at())
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'c: `$c.ascii_str()` / $c (quote type: $quote/$quote.ascii_str())')

		if c == `\n` {
			s.inc_line_number()
			lit += c.ascii_str()
			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'c: `\\n` / $c')
			continue
		}
		// Check for escaped chars
		if c == u8(92) {
			esc, skip := s.handle_escapes(quote, true)
			lit += esc
			if skip > 0 {
				s.pos += skip
				s.col += skip
				continue
			}
		}
		// Check for control characters (allow TAB)
		if util.is_illegal_ascii_control_character(c) {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' control character `$c.hex()` is not allowed at $start ($s.line_nr,$s.col) "${u8(s.at()).ascii_str()}" near ...${s.excerpt(s.pos, 5)}...')
		}

		if c == quote {
			if s.peek(1) == quote && s.peek(2) == quote {
				if s.peek(3) == scanner.end_of_text {
					s.pos += 3
					s.col += 3
					lit += quote.ascii_str() + quote.ascii_str() + quote.ascii_str()
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'returning at $c.ascii_str() `$lit`')
					return lit
				} else if s.peek(3) != quote {
					// lit += c.ascii_str()
					// lit += quote.ascii_str()
					s.pos += 3
					s.col += 3
					lit += quote.ascii_str() + quote.ascii_str() + quote.ascii_str()
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'returning at $c.ascii_str() `$lit`')
					return lit
				}
			}
		}
		lit += c.ascii_str()
	}
	return lit
}

// handle_escapes returns any escape character sequence.
// For escape sequence validation see `Checker.check_quoted_escapes`.
fn (mut s Scanner) handle_escapes(quote u8, is_multiline bool) (string, int) {
	c := u8(s.at())
	mut lit := c.ascii_str()
	is_literal_string := quote == `'`
	if !is_literal_string {
		if s.peek(1) == `u` && u8(s.peek(2)).is_hex_digit() && u8(s.peek(3)).is_hex_digit()
			&& u8(s.peek(4)).is_hex_digit() && u8(s.peek(5)).is_hex_digit() {
			lit += s.text[s.pos + 1..s.pos + 6] //.ascii_str()
			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'gulp escaped unicode `$lit`')
			return lit, 5
		} else if s.peek(1) == quote {
			if (!is_multiline && s.peek(2) == `\n`)
				|| (is_multiline && s.peek(2) == quote && s.peek(3) == quote && s.peek(4) == `\n`) {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'ignore special case escaped `$lit` at end of string')
				return '', 0
			}
			lit += quote.ascii_str()
			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'gulp escaped `$lit`')
			return lit, 1
		}
	}
	if is_literal_string {
		if s.peek(1) == quote {
			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'ignore escape `$lit${u8(s.peek(1)).ascii_str()}` in literal string')
			return '', 0
		}
	}

	lit += u8(s.peek(1)).ascii_str()
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'gulp escaped `$lit`')
	return lit, 1
}

// extract_number collects and returns a string containing
// any bytes recognized as a TOML number except for "(+/-)nan" and "(+/-)inf".
// TOML numbers can include digits 0-9 and `_`.
[direct_array_access; inline]
fn (mut s Scanner) extract_number() ?string {
	// extract_number is called when the scanner has already reached
	// a byte that is a number or +/- - so we rewind it to start at the correct
	// position to get the complete number. Even if it's only one digit
	s.pos--
	s.col--
	start := s.pos

	mut c := s.at()
	is_digit := u8(c).is_digit()
	if !(is_digit || c in [`+`, `-`]) {
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' ${u8(c).ascii_str()} is not a number at ${s.excerpt(s.pos, 10)}')
	}
	s.pos++
	s.col++
	for s.pos < s.text.len {
		c = s.at()
		// Handle signed exponent notation. I.e.: 3e2, 3E2, 3e-2, 3E+2, 3e0, 3.1e2, 3.1E2, -1E-1
		if c in [`e`, `E`] && s.peek(1) in [`+`, `-`] && u8(s.peek(2)).is_digit() {
			s.pos += 2
			s.col += 2
		}
		c = s.at()
		if !(u8(c).is_hex_digit() || c in scanner.digit_extras) || (c == `.` && s.is_left_of_assign) {
			break
		}
		s.pos++
		s.col++
	}
	key := s.text[start..s.pos]
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified number "$key" in range [$start .. $s.pos]')
	return key
}

// extract_nan_or_inf_number collects and returns a string containing
// any bytes recognized as infinity or not-a-number TOML numbers.
[direct_array_access; inline]
fn (mut s Scanner) extract_nan_or_inf_number() ?string {
	// extract_nan_or_inf_number is called when the scanner has already identified that
	// +/- or 'nan'/'inf' bytes is up but we rewind it to start at the correct position
	s.pos--
	s.col--
	start := s.pos

	mut c := s.at()
	if c !in [`+`, `-`, `n`, `i`] {
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' ${u8(c).ascii_str()} is not a number at ${s.excerpt(s.pos, 10)}')
	}
	s.pos++
	s.col++
	for s.pos < s.text.len {
		c = s.at()
		if c !in [`n`, `a`, `i`, `f`] {
			break
		}
		s.pos++
		s.col++
	}
	key := s.text[start..s.pos]
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'identified special number "$key" in range [$start .. $s.pos]')
	return key
}

// excerpt returns a string excerpt of the input text centered
// at `pos`. The `margin` argument defines how many chacters
// on each side of `pos` is returned
pub fn (s Scanner) excerpt(pos int, margin int) string {
	start := if pos > 0 && pos >= margin { pos - margin } else { 0 }
	end := if pos + margin < s.text.len { pos + margin } else { s.text.len }
	return s.text[start..end].replace('\n', r'\n')
}

// state returns a read-only view of the scanner's internal state.
pub fn (s Scanner) state() State {
	return State{
		col: s.col
		line_nr: s.line_nr
		pos: s.pos
	}
}

fn (mut s Scanner) validate_and_skip_headers() ? {
	// UTF-16 / UTF-32 headers (BE/LE)
	s.check_utf16_or_32_bom() ?

	// NICE-TO-HAVE-TODO Check other types of (UTF-?) headers and yield an error. TOML is UTF-8 only.

	// Skip optional UTF-8 heaser, if any.
	if s.at() == 0xEF && s.peek(1) == 0xBB && s.peek(2) == 0xBF {
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping UTF-8 byte order mark (BOM)')
		s.header_len = 3
		s.skip_n(s.header_len)
	}

	// Check after we've skipped UTF-8 BOM
	s.check_utf16_or_32_bom() ?
}

fn (mut s Scanner) check_utf16_or_32_bom() ? {
	if (s.at() == 0xFF && s.peek(1) == 0xFE && s.peek(2) == 0x00 && s.peek(3) == 0x00)
		|| (s.at() == 0x00 && s.peek(1) == 0x00 && s.peek(2) == 0xFE && s.peek(3) == 0xFF) {
		s.header_len = 4
		s.skip_n(s.header_len)
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' UTF-32 is not a valid TOML encoding at $s.pos ($s.line_nr,$s.col) near ...${s.excerpt(s.pos, 5)}...')
	}
	if (s.at() == 0xFE && s.peek(1) == 0xFF) || (s.at() == 0xFF && s.peek(1) == 0xFE) {
		s.header_len = 2
		s.skip_n(s.header_len)
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' UTF-16 is not a valid TOML encoding at $s.pos ($s.line_nr,$s.col) near ...${s.excerpt(s.pos, 5)}...')
	}
}
