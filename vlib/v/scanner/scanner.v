// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module scanner

import os
import strconv
import v.token
import v.pref
import v.util
import v.vet
import v.errors
import v.ast
import v.mathutil

const (
	single_quote = `'`
	double_quote = `"`
	// char used as number separator
	num_sep      = `_`
	b_lf         = 10
	b_cr         = 13
	backslash    = `\\`
)

[minify]
pub struct Scanner {
pub mut:
	file_path                   string // '/path/to/file.v'
	file_base                   string // 'file.v'
	text                        string // the whole text of the file
	pos                         int    // current position in the file, first character is s.text[0]
	line_nr                     int    // current line number
	last_nl_pos                 int = -1 // for calculating column
	is_crlf                     bool   // special check when computing columns
	is_inside_string            bool   // set to true in a string, *at the start* of an $var or ${expr}
	is_inter_start              bool   // for hacky string interpolation TODO simplify
	is_inter_end                bool
	is_enclosed_inter           bool
	line_comment                string
	last_lt                     int = -1 // position of latest <
	is_started                  bool
	is_print_line_on_error      bool
	is_print_colored_error      bool
	is_print_rel_paths_on_error bool
	quote                       u8  // which quote is used to denote current string: ' or "
	inter_quote                 u8
	nr_lines                    int // total number of lines in the source file that were scanned
	is_vh                       bool // Keep newlines
	is_fmt                      bool // Used for v fmt.
	comments_mode               CommentsMode
	is_inside_toplvl_statement  bool // *only* used in comments_mode: .toplevel_comments, toggled by parser
	all_tokens                  []token.Token // *only* used in comments_mode: .toplevel_comments, contains all tokens
	tidx                        int
	eofs                        int
	inter_cbr_count             int
	pref                        &pref.Preferences
	error_details               []string
	errors                      []errors.Error
	warnings                    []errors.Warning
	notices                     []errors.Notice
	vet_errors                  []vet.Error
	should_abort                bool // when too many errors/warnings/notices are accumulated, should_abort becomes true, and the scanner should stop
}

/*
How the .toplevel_comments mode works:

In this mode, the scanner scans *everything* at once, before parsing starts,
including all the comments, and stores the results in an buffer s.all_tokens.

Then .scan() just returns s.all_tokens[ s.tidx++ ] *ignoring* the
comment tokens. In other words, by default in this mode, the parser
*will not see any comments* inside top level statements, so it has
no reason to complain about them.

When the parser determines, that it is outside of a top level statement,
it tells the scanner to backtrack s.tidx to the current p.tok index,
then it changes .is_inside_toplvl_statement to false , and refills its
lookahead buffer (i.e. p.peek_tok), from the scanner.

In effect, from the parser's point of view, the next tokens, that it will
receive with p.next(), will be the same, as if comments are not ignored
anymore, *between* top level statements.

When the parser determines, that it is going again inside a top level
statement, it does the same, this time setting .is_inside_toplvl_statement
to true, again refilling the lookahead buffer => calling .next() in this
mode, will again ignore all the comment tokens, till the top level statement
is finished.
*/
// The different kinds of scanner modes:
//
// .skip_comments - simplest/fastest, just ignores all comments early.
// This mode is used by the compiler itself.
//
// .parse_comments is used by vfmt. Ideally it should handle inline /* */
// comments too, i.e. it returns every kind of comment as a new token.
//
// .toplevel_comments is used by vdoc, parses *only* top level ones
// that are *outside* structs/enums/fns.
pub enum CommentsMode {
	skip_comments
	parse_comments
	toplevel_comments
}

// new scanner from file.
pub fn new_scanner_file(file_path string, comments_mode CommentsMode, pref &pref.Preferences) ?&Scanner {
	if !os.is_file(file_path) {
		return error('$file_path is not a .v file')
	}
	raw_text := util.read_file(file_path) or { return err }
	mut s := &Scanner{
		pref: pref
		text: raw_text
		all_tokens: []token.Token{cap: raw_text.len / 3}
		is_print_line_on_error: true
		is_print_colored_error: true
		is_print_rel_paths_on_error: true
		is_fmt: pref.is_fmt
		comments_mode: comments_mode
		file_path: file_path
		file_base: os.base(file_path)
	}
	s.scan_all_tokens_in_buffer()
	return s
}

// new scanner from string.
pub fn new_scanner(text string, comments_mode CommentsMode, pref &pref.Preferences) &Scanner {
	mut s := &Scanner{
		pref: pref
		text: text
		all_tokens: []token.Token{cap: text.len / 3}
		is_print_line_on_error: true
		is_print_colored_error: true
		is_print_rel_paths_on_error: true
		is_fmt: pref.is_fmt
		comments_mode: comments_mode
		file_path: 'internal_memory'
		file_base: 'internal_memory'
	}
	s.scan_all_tokens_in_buffer()
	return s
}

[unsafe]
pub fn (mut s Scanner) free() {
	unsafe {
		// Note: s.text is not freed here, because it is shared with all other util.read_file instances,
		// and strings are not reference counted yet:
		// s.text.free()
		// .all_tokens however are not shared with anything, and can be freed:
		s.all_tokens.free()
	}
}

[inline]
fn (s &Scanner) should_parse_comment() bool {
	return (s.comments_mode == .parse_comments)
		|| (s.comments_mode == .toplevel_comments && !s.is_inside_toplvl_statement)
}

// Note: this is called by v's parser
pub fn (mut s Scanner) set_is_inside_toplevel_statement(newstate bool) {
	s.is_inside_toplvl_statement = newstate
}

pub fn (mut s Scanner) set_current_tidx(cidx int) {
	mut tidx := if cidx < 0 { 0 } else { cidx }
	tidx = if tidx > s.all_tokens.len { s.all_tokens.len } else { tidx }
	s.tidx = tidx
}

[inline]
fn (mut s Scanner) new_token(tok_kind token.Kind, lit string, len int) token.Token {
	cidx := s.tidx
	s.tidx++
	line_offset := if tok_kind == .hash { 0 } else { 1 }
	return token.Token{
		kind: tok_kind
		lit: lit
		line_nr: s.line_nr + line_offset
		col: mathutil.max(1, s.current_column() - len + 1)
		pos: s.pos - len + 1
		len: len
		tidx: cidx
	}
}

[inline]
fn (s &Scanner) new_eof_token() token.Token {
	return token.Token{
		kind: .eof
		lit: ''
		line_nr: s.line_nr + 1
		col: s.current_column()
		pos: s.pos
		len: 1
		tidx: s.tidx
	}
}

[inline]
fn (mut s Scanner) new_multiline_token(tok_kind token.Kind, lit string, len int, start_line int) token.Token {
	cidx := s.tidx
	s.tidx++
	return token.Token{
		kind: tok_kind
		lit: lit
		line_nr: start_line + 1
		col: mathutil.max(1, s.current_column() - len + 1)
		pos: s.pos - len + 1
		len: len
		tidx: cidx
	}
}

[direct_array_access; inline]
fn (mut s Scanner) ident_name() string {
	start := s.pos
	s.pos++
	for s.pos < s.text.len {
		c := s.text[s.pos]
		if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_` {
			s.pos++
			continue
		}
		break
	}
	name := s.text[start..s.pos]
	s.pos--
	return name
}

fn (s Scanner) num_lit(start int, end int) string {
	if s.is_fmt {
		return s.text[start..end]
	}
	unsafe {
		txt := s.text.str
		mut b := malloc_noscan(end - start + 1) // add a byte for the endstring 0
		mut i_no_sep := 0
		for i in start .. end {
			if txt[i] != scanner.num_sep {
				b[i_no_sep] = txt[i]
				i_no_sep++
			}
		}
		b[i_no_sep] = 0 // C string compatibility
		return b.vstring_with_len(i_no_sep)
	}
}

fn (mut s Scanner) ident_bin_number() string {
	mut has_wrong_digit := false
	mut first_wrong_digit_pos := 0
	mut first_wrong_digit := `\0`
	start_pos := s.pos
	s.pos += 2 // skip '0b'
	if s.pos < s.text.len && s.text[s.pos] == scanner.num_sep {
		s.error('separator `_` is only valid between digits in a numeric literal')
	}
	for s.pos < s.text.len {
		c := s.text[s.pos]
		if c == scanner.num_sep && s.text[s.pos - 1] == scanner.num_sep {
			s.error('cannot use `_` consecutively')
		}
		if !c.is_bin_digit() && c != scanner.num_sep {
			if (!c.is_digit() && !c.is_letter()) || s.is_inside_string {
				break
			} else if !has_wrong_digit {
				has_wrong_digit = true
				first_wrong_digit_pos = s.pos
				first_wrong_digit = c
			}
		}
		s.pos++
	}
	if s.text[s.pos - 1] == scanner.num_sep {
		s.pos--
		s.error('cannot use `_` at the end of a numeric literal')
	} else if start_pos + 2 == s.pos {
		s.pos-- // adjust error position
		s.error('number part of this binary is not provided')
	} else if has_wrong_digit {
		s.pos = first_wrong_digit_pos // adjust error position
		s.error('this binary number has unsuitable digit `$first_wrong_digit.str()`')
	}
	number := s.num_lit(start_pos, s.pos)
	s.pos--
	return number
}

[direct_array_access]
fn (mut s Scanner) ident_hex_number() string {
	mut has_wrong_digit := false
	mut first_wrong_digit_pos := 0
	mut first_wrong_digit := `\0`
	start_pos := s.pos
	if s.pos + 2 >= s.text.len {
		return '0x'
	}
	s.pos += 2 // skip '0x'
	if s.pos < s.text.len && s.text[s.pos] == scanner.num_sep {
		s.error('separator `_` is only valid between digits in a numeric literal')
	}
	for s.pos < s.text.len {
		c := s.text[s.pos]
		if c == scanner.num_sep && s.text[s.pos - 1] == scanner.num_sep {
			s.error('cannot use `_` consecutively')
		}
		if !c.is_hex_digit() && c != scanner.num_sep {
			if !c.is_letter() || s.is_inside_string {
				break
			} else if !has_wrong_digit {
				has_wrong_digit = true
				first_wrong_digit_pos = s.pos
				first_wrong_digit = c
			}
		}
		s.pos++
	}
	if s.text[s.pos - 1] == scanner.num_sep {
		s.pos--
		s.error('cannot use `_` at the end of a numeric literal')
	} else if start_pos + 2 == s.pos {
		s.pos-- // adjust error position
		s.error('number part of this hexadecimal is not provided')
	} else if has_wrong_digit {
		s.pos = first_wrong_digit_pos // adjust error position
		s.error('this hexadecimal number has unsuitable digit `$first_wrong_digit.str()`')
	}
	number := s.num_lit(start_pos, s.pos)
	s.pos--
	return number
}

fn (mut s Scanner) ident_oct_number() string {
	mut has_wrong_digit := false
	mut first_wrong_digit_pos := 0
	mut first_wrong_digit := `\0`
	start_pos := s.pos
	s.pos += 2 // skip '0o'
	if s.pos < s.text.len && s.text[s.pos] == scanner.num_sep {
		s.error('separator `_` is only valid between digits in a numeric literal')
	}
	for s.pos < s.text.len {
		c := s.text[s.pos]
		if c == scanner.num_sep && s.text[s.pos - 1] == scanner.num_sep {
			s.error('cannot use `_` consecutively')
		}
		if !c.is_oct_digit() && c != scanner.num_sep {
			if (!c.is_digit() && !c.is_letter()) || s.is_inside_string {
				break
			} else if !has_wrong_digit {
				has_wrong_digit = true
				first_wrong_digit_pos = s.pos
				first_wrong_digit = c
			}
		}
		s.pos++
	}
	if s.text[s.pos - 1] == scanner.num_sep {
		s.pos--
		s.error('cannot use `_` at the end of a numeric literal')
	} else if start_pos + 2 == s.pos {
		s.pos-- // adjust error position
		s.error('number part of this octal is not provided')
	} else if has_wrong_digit {
		s.pos = first_wrong_digit_pos // adjust error position
		s.error('this octal number has unsuitable digit `$first_wrong_digit.str()`')
	}
	number := s.num_lit(start_pos, s.pos)
	s.pos--
	return number
}

[direct_array_access]
fn (mut s Scanner) ident_dec_number() string {
	mut has_wrong_digit := false
	mut first_wrong_digit_pos := 0
	mut first_wrong_digit := `\0`
	start_pos := s.pos
	// scan integer part
	for s.pos < s.text.len {
		c := s.text[s.pos]
		if c == scanner.num_sep && s.text[s.pos - 1] == scanner.num_sep {
			s.error('cannot use `_` consecutively')
		}
		if !c.is_digit() && c != scanner.num_sep {
			if !c.is_letter() || c in [`e`, `E`] || s.is_inside_string {
				break
			} else if !has_wrong_digit {
				has_wrong_digit = true
				first_wrong_digit_pos = s.pos
				first_wrong_digit = c
			}
		}
		s.pos++
	}
	if s.text[s.pos - 1] == scanner.num_sep {
		s.pos--
		s.error('cannot use `_` at the end of a numeric literal')
	}
	mut call_method := false // true for, e.g., 5.str(), 5.5.str(), 5e5.str()
	mut is_range := false // true for, e.g., 5..10
	// scan fractional part
	if s.pos < s.text.len && s.text[s.pos] == `.` {
		s.pos++
		if s.pos < s.text.len {
			// 5.5, 5.5.str()
			if s.text[s.pos].is_digit() {
				for s.pos < s.text.len {
					c := s.text[s.pos]
					if !c.is_digit() {
						if !c.is_letter() || c in [`e`, `E`] || s.is_inside_string {
							// 5.5.str()
							if c == `.` && s.pos + 1 < s.text.len && s.text[s.pos + 1].is_letter() {
								call_method = true
							}
							break
						} else if !has_wrong_digit {
							has_wrong_digit = true
							first_wrong_digit_pos = s.pos
							first_wrong_digit = c
						}
					}
					s.pos++
				}
			} else if s.text[s.pos] == `.` {
				// 5.. (a range)
				is_range = true
				s.pos--
			} else if s.text[s.pos] in [`e`, `E`] {
				// 5.e5
			} else if s.text[s.pos].is_letter() {
				// 5.str()
				call_method = true
				s.pos--
			} else {
				// 5.
				mut symbol_length := 0
				for i := s.pos - 2; i > 0 && s.text[i - 1].is_digit(); i-- {
					symbol_length++
				}
				float_symbol := s.text[s.pos - 2 - symbol_length..s.pos - 1]
				s.warn('float literals should have a digit after the decimal point, e.g. `${float_symbol}.0`')
			}
		}
	}
	// scan exponential part
	mut has_exp := false
	if s.pos < s.text.len && s.text[s.pos] in [`e`, `E`] {
		has_exp = true
		s.pos++
		if s.pos < s.text.len && s.text[s.pos] in [`-`, `+`] {
			s.pos++
		}
		for s.pos < s.text.len {
			c := s.text[s.pos]
			if !c.is_digit() {
				if !c.is_letter() || s.is_inside_string {
					// 5e5.str()
					if c == `.` && s.pos + 1 < s.text.len && s.text[s.pos + 1].is_letter() {
						call_method = true
					}
					break
				} else if !has_wrong_digit {
					has_wrong_digit = true
					first_wrong_digit_pos = s.pos
					first_wrong_digit = c
				}
			}
			s.pos++
		}
	}
	if has_wrong_digit {
		// error check: wrong digit
		s.pos = first_wrong_digit_pos // adjust error position
		if !s.pref.translated {
			s.error('this number has unsuitable digit `$first_wrong_digit.str()`')
		}
	} else if s.text[s.pos - 1] in [`e`, `E`] {
		// error check: 5e
		s.pos-- // adjust error position
		s.error('exponent has no digits')
	} else if s.pos < s.text.len && s.text[s.pos] == `.` && !is_range && !call_method {
		// error check: 1.23.4, 123.e+3.4
		if has_exp {
			s.error('exponential part should be integer')
		} else {
			s.error('too many decimal points in number')
		}
	}
	number := s.num_lit(start_pos, s.pos)
	s.pos--
	return number
}

fn (mut s Scanner) ident_number() string {
	if s.expect('0b', s.pos) {
		return s.ident_bin_number()
	} else if s.expect('0x', s.pos) {
		return s.ident_hex_number()
	} else if s.expect('0o', s.pos) {
		return s.ident_oct_number()
	} else {
		return s.ident_dec_number()
	}
}

[direct_array_access; inline]
fn (mut s Scanner) skip_whitespace() {
	for s.pos < s.text.len {
		c := s.text[s.pos]
		if c == 9 {
			// tabs are most common
			s.pos++
			continue
		}
		if !(c == 32 || (c > 8 && c < 14) || (c == 0x85) || (c == 0xa0)) {
			return
		}
		c_is_nl := c == scanner.b_cr || c == scanner.b_lf
		if c_is_nl && s.is_vh {
			return
		}
		if s.pos + 1 < s.text.len && c == scanner.b_cr && s.text[s.pos + 1] == scanner.b_lf {
			s.is_crlf = true
		}
		// Count \r\n as one line
		if c_is_nl && !(s.pos > 0 && s.text[s.pos - 1] == scanner.b_cr && c == scanner.b_lf) {
			s.inc_line_number()
		}
		s.pos++
	}
}

fn (mut s Scanner) end_of_file() token.Token {
	s.eofs++
	if s.eofs > 50 {
		s.line_nr--
		panic(
			'the end of file `$s.file_path` has been reached 50 times already, the v parser is probably stuck.\n' +
			'This should not happen. Please report the bug here, and include the last 2-3 lines of your source code:\n' +
			'https://github.com/vlang/v/issues/new?labels=Bug&template=bug_report.md')
	}
	if s.pos != s.text.len && s.eofs == 1 {
		s.inc_line_number()
	}
	s.pos = s.text.len
	return s.new_eof_token()
}

fn (mut s Scanner) scan_all_tokens_in_buffer() {
	mut timers := util.get_timers()
	timers.measure_pause('PARSE')
	util.timing_start('SCAN')
	defer {
		util.timing_measure_cumulative('SCAN')
		timers.measure_resume('PARSE')
	}
	s.scan_remaining_text()
	s.tidx = 0
	$if debugscanner ? {
		for t in s.all_tokens {
			eprintln('> tidx:${t.tidx:-5} | kind: ${t.kind:-10} | lit: $t.lit')
		}
	}
}

fn (mut s Scanner) scan_remaining_text() {
	for {
		t := s.text_scan()
		if s.comments_mode == .skip_comments && t.kind == .comment {
			continue
		}
		s.all_tokens << t
		if t.kind == .eof || s.should_abort {
			break
		}
	}
}

[direct_array_access]
pub fn (mut s Scanner) scan() token.Token {
	for {
		cidx := s.tidx
		s.tidx++
		if cidx >= s.all_tokens.len || s.should_abort {
			return s.end_of_file()
		}
		if s.all_tokens[cidx].kind == .comment {
			if !s.should_parse_comment() {
				continue
			}
		}
		return s.all_tokens[cidx]
	}
	return s.new_eof_token()
}

[direct_array_access; inline]
pub fn (s &Scanner) peek_token(n int) token.Token {
	idx := s.tidx + n
	if idx >= s.all_tokens.len {
		return s.new_eof_token()
	}
	t := s.all_tokens[idx]
	return t
}

[direct_array_access; inline]
fn (s &Scanner) look_ahead(n int) u8 {
	if s.pos + n < s.text.len {
		return s.text[s.pos + n]
	} else {
		return `\0`
	}
}

[direct_array_access]
fn (mut s Scanner) text_scan() token.Token {
	// The for loop here is so that instead of doing
	// `return s.scan()` (which will use a new call stack frame),
	// text_scan can just do continue, keeping
	// memory & stack usage low.
	// That optimization mostly matters for long sections
	// of comments and string literals.
	for {
		if s.is_started {
			s.pos++
		} else {
			s.is_started = true
		}
		if !s.is_inside_string {
			s.skip_whitespace()
		}
		if s.pos >= s.text.len || s.should_abort {
			return s.end_of_file()
		}
		// End of $var, start next string
		if s.is_inter_end {
			if s.text[s.pos] == s.quote {
				s.is_inter_end = false
				return s.new_token(.string, '', 1)
			}
			s.is_inter_end = false
			ident_string := s.ident_string()
			return s.new_token(.string, ident_string, ident_string.len + 2) // + two quotes
		}
		s.skip_whitespace()
		// end of file
		if s.pos >= s.text.len {
			return s.end_of_file()
		}
		// handle each char
		c := s.text[s.pos]
		nextc := s.look_ahead(1)
		// name or keyword
		if util.is_name_char(c) {
			name := s.ident_name()
			// tmp hack to detect . in ${}
			// Check if not .eof to prevent panic
			next_char := s.look_ahead(1)
			kind := token.scanner_matcher.find(name)
			// '$type' '$struct'... will be recognized as ident (not keyword token)
			if kind != -1 && !(s.is_inter_start && next_char == s.quote) {
				return s.new_token(token.Kind(kind), name, name.len)
			}
			// 'asdf $b' => "b" is the last name in the string, dont start parsing string
			// at the next ', skip it
			if s.is_inside_string {
				if next_char == s.quote {
					s.is_inter_end = true
					s.is_inter_start = false
					s.is_inside_string = false
				}
			}
			// end of `$expr`
			// allow `'$a.b'` and `'$a.c()'`
			if s.is_inter_start && next_char == `\\`
				&& s.look_ahead(2) !in [`x`, `n`, `r`, `\\`, `t`, `e`, `"`, `'`] {
				s.warn('unknown escape sequence \\${s.look_ahead(2)}')
			}
			if s.is_inter_start && next_char == `(` {
				if s.look_ahead(2) != `)` {
					s.warn('use `\${f(expr)}` instead of `\$f(expr)`')
				}
			} else if s.is_inter_start && next_char != `.` {
				s.is_inter_end = true
				s.is_inter_start = false
			}
			return s.new_token(.name, name, name.len)
		} else if c.is_digit() || (c == `.` && nextc.is_digit()) {
			// `123`, `.123`
			if !s.is_inside_string {
				// In C ints with `0` prefix are octal (in V they're decimal), so discarding heading zeros is needed.
				mut start_pos := s.pos
				for start_pos < s.text.len && s.text[start_pos] == `0` {
					start_pos++
				}
				mut prefix_zero_num := start_pos - s.pos // how many prefix zeros should be jumped
				// for 0b, 0o, 0x the heading zero shouldn't be jumped
				if start_pos == s.text.len || (c == `0` && !s.text[start_pos].is_digit()) {
					prefix_zero_num--
				}
				s.pos += prefix_zero_num // jump these zeros
			}
			num := s.ident_number()
			return s.new_token(.number, num, num.len)
		}
		// Handle `'$fn()'`
		if c == `)` && s.is_inter_start {
			next_char := s.look_ahead(1)
			if next_char != `.` {
				s.is_inter_end = true
				s.is_inter_start = false
				if next_char == s.quote {
					s.is_inside_string = false
				}
				return s.new_token(.rpar, '', 1)
			}
		}
		// all other tokens
		match c {
			`+` {
				if nextc == `+` {
					s.pos++
					return s.new_token(.inc, '', 2)
				} else if nextc == `=` {
					s.pos++
					return s.new_token(.plus_assign, '', 2)
				}
				return s.new_token(.plus, '', 1)
			}
			`-` {
				if nextc == `-` {
					s.pos++
					return s.new_token(.dec, '', 2)
				} else if nextc == `=` {
					s.pos++
					return s.new_token(.minus_assign, '', 2)
				}
				return s.new_token(.minus, '', 1)
			}
			`*` {
				if nextc == `=` {
					s.pos++
					return s.new_token(.mult_assign, '', 2)
				}
				return s.new_token(.mul, '', 1)
			}
			`^` {
				if nextc == `=` {
					s.pos++
					return s.new_token(.xor_assign, '', 2)
				}
				return s.new_token(.xor, '', 1)
			}
			`%` {
				if nextc == `=` {
					s.pos++
					return s.new_token(.mod_assign, '', 2)
				}
				return s.new_token(.mod, '', 1)
			}
			`?` {
				return s.new_token(.question, '?', 1)
			}
			scanner.single_quote, scanner.double_quote {
				start_line := s.line_nr
				ident_string := s.ident_string()
				return s.new_multiline_token(.string, ident_string, ident_string.len + 2,
					start_line) // + two quotes
			}
			`\`` {
				// ` // apostrophe balance comment. do not remove
				ident_char := s.ident_char()
				return s.new_token(.chartoken, ident_char, ident_char.len + 2) // + two quotes
			}
			`(` {
				// TODO `$if vet {` for performance
				if s.pref.is_vet && s.text[s.pos + 1] == ` ` {
					s.vet_error('Looks like you are adding a space after `(`', .vfmt)
				}
				return s.new_token(.lpar, '', 1)
			}
			`)` {
				// TODO `$if vet {` for performance
				if s.pref.is_vet && s.text[s.pos - 1] == ` ` {
					s.vet_error('Looks like you are adding a space before `)`', .vfmt)
				}
				return s.new_token(.rpar, '', 1)
			}
			`[` {
				return s.new_token(.lsbr, '', 1)
			}
			`]` {
				return s.new_token(.rsbr, '', 1)
			}
			`{` {
				// Skip { in `${` in strings
				if s.is_inside_string {
					if s.text[s.pos - 1] == `$` {
						continue
					} else {
						s.inter_cbr_count++
					}
				}
				return s.new_token(.lcbr, '', 1)
			}
			`$` {
				if s.is_inside_string {
					return s.new_token(.str_dollar, '', 1)
				} else {
					return s.new_token(.dollar, '', 1)
				}
			}
			`}` {
				// s = `hello $name !`
				// s = `hello ${name} !`
				if s.is_enclosed_inter && s.inter_cbr_count == 0 {
					if s.pos < s.text.len - 1 {
						s.pos++
					} else {
						s.error('unfinished string literal')
					}
					if s.text[s.pos] == s.quote {
						s.is_inside_string = false
						s.is_enclosed_inter = false
						return s.new_token(.string, '', 1)
					}
					s.is_enclosed_inter = false
					ident_string := s.ident_string()
					return s.new_token(.string, ident_string, ident_string.len + 2) // + two quotes
				} else {
					if s.inter_cbr_count > 0 {
						s.inter_cbr_count--
					}
					return s.new_token(.rcbr, '', 1)
				}
			}
			`&` {
				if nextc == `=` {
					s.pos++
					return s.new_token(.and_assign, '', 2)
				}
				afternextc := s.look_ahead(2)
				if nextc == `&` && (afternextc.is_space() || afternextc == `!`) {
					s.pos++
					return s.new_token(.and, '', 2)
				}
				return s.new_token(.amp, '', 1)
			}
			`|` {
				if nextc == `|` {
					s.pos++
					return s.new_token(.logical_or, '', 2)
				}
				if nextc == `=` {
					s.pos++
					return s.new_token(.or_assign, '', 2)
				}
				return s.new_token(.pipe, '', 1)
			}
			`,` {
				return s.new_token(.comma, '', 1)
			}
			`@` {
				mut name := ''
				if nextc != `\0` {
					s.pos++
					name = s.ident_name()
				}
				if s.is_fmt {
					return s.new_token(.name, '@' + name, name.len + 1)
				}
				// @FN, @STRUCT, @MOD etc. See full list in token.valid_at_tokens
				if '@' + name in token.valid_at_tokens || name.starts_with('cc') { // `=@cccond` in inline assembly
					return s.new_token(.at, '@' + name, name.len + 1)
				}
				if !token.is_key(name) {
					mut at_error_msg := '@ must be used before keywords or compile time variables (e.g. `@type string` or `@FN`)'
					// If name is all uppercase, the user is probably looking for a compile time variable ("at-token")
					if name.is_upper() {
						at_error_msg += '\nAvailable compile time variables:\n$token.valid_at_tokens'
					}
					s.error(at_error_msg)
				}
				return s.new_token(.name, name, name.len)
			}
			`.` {
				if nextc == `.` {
					s.pos++
					if s.pos + 1 < s.text.len && s.text[s.pos + 1] == `.` {
						s.pos++
						return s.new_token(.ellipsis, '', 3)
					}
					return s.new_token(.dotdot, '', 2)
				}
				return s.new_token(.dot, '', 1)
			}
			`#` {
				// manage gated arrays/strings
				if nextc == `[` {
					s.pos++
					return s.new_token(.nilsbr, '', 2)
				}

				start := s.pos + 1
				s.ignore_line()
				if nextc == `!` {
					// treat shebang line (#!) as a comment
					comment := s.text[start - 1..s.pos].trim_space()
					// s.fgenln('// shebang line "$s.line_comment"')
					return s.new_token(.comment, comment, comment.len + 2)
				}
				hash := s.text[start..s.pos].trim_space()
				return s.new_token(.hash, hash, hash.len + 2)
			}
			`>` {
				if nextc == `=` {
					s.pos++
					return s.new_token(.ge, '', 2)
				} else if nextc == `>` {
					if s.pos + 2 < s.text.len {
						// an algorithm to decide it's generic or non-generic
						// such as `foo<Baz, Bar<int>>(a)` vs `a, b := Foo{}<Foo{}, bar>>(baz)`
						// @SleepyRoy if you have smarter algorithm :-)
						// almost correct heuristics: last <T> of generic cannot be extremely long
						// here we set the limit 100 which should be nice for real cases
						// e.g. ...Bar<int, []Foo<int>, Baz_, [20]f64, map[string][]bool>> =>
						// <int, Baz_, [20]f64, map[string][]bool => int, Baz_, f64, bool
						is_generic := if s.last_lt >= 0 && s.pos - s.last_lt < 100 {
							typs := s.text[s.last_lt + 1..s.pos].split(',').map(it.trim_space().trim_right('>').after(']'))
							// if any typ is neither Type nor builtin, then the case is non-generic
							typs.all(it.len > 0
								&& ((it[0].is_capital() && it[1..].bytes().all(it.is_alnum()
								|| it == `_`))
								|| ast.builtin_type_names_matcher.matches(it)))
						} else {
							false
						}
						if is_generic {
							return s.new_token(.gt, '', 1)
						} else if s.text[s.pos + 2] == `=` {
							s.pos += 2
							return s.new_token(.right_shift_assign, '', 3)
						} else if s.text[s.pos + 2] == `>` {
							if s.pos + 3 < s.text.len && s.text[s.pos + 3] == `=` {
								s.pos += 3
								return s.new_token(.unsigned_right_shift_assign, '', 4)
							}
							s.pos += 2
							return s.new_token(.unsigned_right_shift, '', 3)
						}
					}
					s.pos++
					return s.new_token(.right_shift, '', 2)
				}
				return s.new_token(.gt, '', 1)
			}
			`<` {
				if nextc == `=` {
					s.pos++
					return s.new_token(.le, '', 2)
				} else if nextc == `<` {
					if s.pos + 2 < s.text.len && s.text[s.pos + 2] == `=` {
						s.pos += 2
						return s.new_token(.left_shift_assign, '', 3)
					}
					s.pos++
					return s.new_token(.left_shift, '', 2)
				} else if nextc == `-` {
					s.pos++
					return s.new_token(.arrow, '', 2)
				} else {
					s.last_lt = s.pos
					return s.new_token(.lt, '', 1)
				}
			}
			`=` {
				if nextc == `=` {
					s.pos++
					return s.new_token(.eq, '', 2)
				} else {
					return s.new_token(.assign, '', 1)
				}
			}
			`:` {
				if nextc == `=` {
					s.pos++
					return s.new_token(.decl_assign, '', 2)
				} else {
					return s.new_token(.colon, '', 1)
				}
			}
			`;` {
				return s.new_token(.semicolon, '', 1)
			}
			`!` {
				if nextc == `=` {
					s.pos++
					return s.new_token(.ne, '', 2)
				} else if s.text.len > s.pos + 3 && nextc == `i` && s.text[s.pos + 2] == `n`
					&& s.text[s.pos + 3].is_space() {
					s.pos += 2
					return s.new_token(.not_in, '', 3)
				} else if s.text.len > s.pos + 3 && nextc == `i` && s.text[s.pos + 2] == `s`
					&& s.text[s.pos + 3].is_space() {
					s.pos += 2
					return s.new_token(.not_is, '', 3)
				} else {
					return s.new_token(.not, '!', 1)
				}
			}
			`~` {
				return s.new_token(.bit_not, '', 1)
			}
			`/` {
				if nextc == `=` {
					s.pos++
					return s.new_token(.div_assign, '', 2)
				}
				if nextc == `/` { // Single line comments
					start := s.pos + 1
					s.ignore_line()
					mut comment_line_end := s.pos
					if s.text[s.pos - 1] == scanner.b_cr {
						comment_line_end--
					} else {
						// fix line_nr, \n was read; the comment is marked on the next line
						s.pos--
						s.line_nr--
					}
					if s.should_parse_comment() {
						s.line_comment = s.text[start + 1..comment_line_end]
						mut comment := s.line_comment
						// Find out if this comment is on its own line (for vfmt)
						mut is_separate_line_comment := true
						for j := start - 2; j >= 0 && s.text[j] != scanner.b_lf; j-- {
							if s.text[j] !in [`\t`, ` `] {
								is_separate_line_comment = false
							}
						}
						if is_separate_line_comment {
							// Note: ´\x01´ is used to preserve the initial whitespace in comments
							//     that are on a separate line
							comment = '\x01' + comment
						}
						return s.new_token(.comment, comment, comment.len + 2)
					}
					// Skip the comment (return the next token)
					continue
				} else if nextc == `*` { // Multiline comments
					start := s.pos + 2
					start_line := s.line_nr
					mut nest_count := 1
					s.pos++
					// Skip comment
					for nest_count > 0 && s.pos < s.text.len - 1 {
						s.pos++
						if s.pos >= s.text.len {
							s.line_nr--
							s.error('comment not terminated')
						}
						if s.text[s.pos] == scanner.b_lf {
							s.inc_line_number()
							continue
						}
						if s.expect('/*', s.pos) {
							nest_count++
							continue
						}
						if s.expect('*/', s.pos) {
							nest_count--
						}
					}
					s.pos++
					if s.should_parse_comment() {
						mut comment := s.text[start..(s.pos - 1)].trim(' ')
						if !comment.contains('\n') {
							comment = '\x01' + comment
						}
						return s.new_multiline_token(.comment, comment, comment.len + 4,
							start_line)
					}
					// Skip if not in fmt mode
					continue
				}
				return s.new_token(.div, '', 1)
			}
			else {}
		}
		$if windows {
			if c == `\0` {
				return s.end_of_file()
			}
		}
		s.invalid_character()
		break
	}
	return s.end_of_file()
}

fn (mut s Scanner) invalid_character() {
	len := utf8_char_len(s.text[s.pos])
	end := mathutil.min(s.pos + len, s.text.len)
	c := s.text[s.pos..end]
	s.error('invalid character `$c`')
}

fn (s &Scanner) current_column() int {
	return s.pos - s.last_nl_pos
}

fn (s &Scanner) count_symbol_before(p int, sym u8) int {
	mut count := 0
	for i := p; i >= 0; i-- {
		if s.text[i] != sym {
			break
		}
		count++
	}
	return count
}

[direct_array_access]
fn (mut s Scanner) ident_string() string {
	lspos := token.Pos{
		line_nr: s.line_nr
		pos: s.pos
		col: s.pos - s.last_nl_pos - 1
	}
	q := s.text[s.pos]
	is_quote := q == scanner.single_quote || q == scanner.double_quote
	is_raw := is_quote && s.pos > 0 && s.text[s.pos - 1] == `r` && !s.is_inside_string
	is_cstr := is_quote && s.pos > 0 && s.text[s.pos - 1] == `c` && !s.is_inside_string
	if is_quote {
		if s.is_inside_string || s.is_enclosed_inter || s.is_inter_start {
			s.inter_quote = q
		} else {
			s.quote = q
		}
	}
	mut n_cr_chars := 0
	mut start := s.pos
	start_char := s.text[start]
	if start_char == s.quote
		|| (start_char == s.inter_quote && (s.is_inter_start || s.is_enclosed_inter)) {
		start++
	} else if start_char == scanner.b_lf {
		s.inc_line_number()
	}
	s.is_inside_string = false
	mut u_escapes_pos := []int{} // pos list of \uXXXX
	mut h_escapes_pos := []int{} // pos list of \xXX
	mut backslash_count := if start_char == scanner.backslash { 1 } else { 0 }
	for {
		s.pos++
		if s.pos >= s.text.len {
			if lspos.line_nr + 1 < s.line_nr {
				s.add_error_detail_with_pos('literal started here', lspos)
			}
			s.error('unfinished string literal')
			break
		}
		c := s.text[s.pos]
		prevc := s.text[s.pos - 1]
		if c == scanner.backslash {
			backslash_count++
		}
		// end of string
		if c == s.quote && (is_raw || backslash_count % 2 == 0) {
			// handle '123\\' backslash at the end
			break
		}
		if c == s.inter_quote && (s.is_inter_start || s.is_enclosed_inter) {
			break
		}
		if c == scanner.b_cr {
			n_cr_chars++
		}
		if c == scanner.b_lf {
			s.inc_line_number()
		}
		// Escape `\x` `\u`
		if backslash_count % 2 == 1 && !is_raw && !is_cstr {
			// Escape `\x`
			if c == `x` {
				if s.text[s.pos + 1] == s.quote || !(s.text[s.pos + 1].is_hex_digit()
					&& s.text[s.pos + 2].is_hex_digit()) {
					s.error(r'`\x` used without two following hex digits')
				}
				h_escapes_pos << s.pos - 1
			}
			// Escape `\u`
			if c == `u` {
				if s.text[s.pos + 1] == s.quote || s.text[s.pos + 2] == s.quote
					|| s.text[s.pos + 3] == s.quote || s.text[s.pos + 4] == s.quote
					|| !s.text[s.pos + 1].is_hex_digit() || !s.text[s.pos + 2].is_hex_digit()
					|| !s.text[s.pos + 3].is_hex_digit() || !s.text[s.pos + 4].is_hex_digit() {
					s.error(r'`\u` incomplete unicode character value')
				}
				u_escapes_pos << s.pos - 1
			}
		}
		// ${var} (ignore in vfmt mode) (skip \$)
		if prevc == `$` && c == `{` && !is_raw
			&& s.count_symbol_before(s.pos - 2, scanner.backslash) % 2 == 0 {
			s.is_inside_string = true
			s.is_enclosed_inter = true
			// so that s.pos points to $ at the next step
			s.pos -= 2
			break
		}
		// $var
		if prevc == `$` && util.is_name_char(c) && !is_raw
			&& s.count_symbol_before(s.pos - 2, scanner.backslash) % 2 == 0 {
			s.is_inside_string = true
			s.is_inter_start = true
			s.pos -= 2
			break
		}
		if c != scanner.backslash {
			backslash_count = 0
		}
	}
	mut lit := ''
	mut end := s.pos
	if s.is_inside_string {
		end++
	}
	if start <= s.pos {
		mut string_so_far := s.text[start..end]
		if !s.is_fmt && u_escapes_pos.len > 0 {
			string_so_far = s.decode_u_escapes(string_so_far, start, u_escapes_pos)
		}
		if !s.is_fmt && h_escapes_pos.len > 0 {
			string_so_far = decode_h_escapes(string_so_far, start, h_escapes_pos)
		}
		if n_cr_chars > 0 {
			string_so_far = string_so_far.replace('\r', '')
		}
		if string_so_far.contains('\\\n') {
			lit = trim_slash_line_break(string_so_far)
		} else {
			lit = string_so_far
		}
	}
	return lit
}

// only handle single-byte inline escapes like '\xc0'
fn decode_h_escapes(s string, start int, escapes_pos []int) string {
	if escapes_pos.len == 0 {
		return s
	}
	mut ss := []string{cap: escapes_pos.len * 2 + 1}
	ss << s[..escapes_pos.first() - start]
	for i, pos in escapes_pos {
		idx := pos - start
		end_idx := idx + 4 // "\xXX".len == 4
		// notice this function doesn't do any decoding... it just replaces '\xc0' with the byte 0xc0
		ss << [u8(strconv.parse_uint(s[idx + 2..end_idx], 16, 8) or { 0 })].bytestr()
		if i + 1 < escapes_pos.len {
			ss << s[end_idx..escapes_pos[i + 1] - start]
		} else {
			ss << s[end_idx..]
		}
	}
	return ss.join('')
}

// handle single-byte inline octal escapes like '\###'
fn decode_o_escapes(s string, start int, escapes_pos []int) string {
	if escapes_pos.len == 0 {
		return s
	}
	mut ss := []string{cap: escapes_pos.len}
	ss << s[..escapes_pos.first() - start] // everything before the first escape code position
	for i, pos in escapes_pos {
		idx := pos - start
		end_idx := idx + 4 // "\XXX".len == 4
		// notice this function doesn't do any decoding... it just replaces '\141' with the byte 0o141
		ss << [u8(strconv.parse_uint(s[idx + 1..end_idx], 8, 8) or { 0 })].bytestr()
		if i + 1 < escapes_pos.len {
			ss << s[end_idx..escapes_pos[i + 1] - start]
		} else {
			ss << s[end_idx..]
		}
	}
	return ss.join('')
}

// decode the flagged unicode escape sequences into their utf-8 bytes
fn (mut s Scanner) decode_u_escapes(str string, start int, escapes_pos []int) string {
	if escapes_pos.len == 0 {
		return str
	}
	mut ss := []string{cap: escapes_pos.len * 2 + 1}
	ss << str[..escapes_pos.first() - start]
	for i, pos in escapes_pos {
		idx := pos - start
		end_idx := idx + 6 // "\uXXXX".len == 6
		escaped_code_point := strconv.parse_uint(str[idx + 2..end_idx], 16, 32) or { 0 }
		// Check if Escaped Code Point is invalid or not
		if rune(escaped_code_point).length_in_bytes() == -1 {
			s.error('invalid unicode point `$str`')
		}
		ss << utf32_to_str(u32(escaped_code_point))
		if i + 1 < escapes_pos.len {
			ss << str[end_idx..escapes_pos[i + 1] - start]
		} else {
			ss << str[end_idx..]
		}
	}
	return ss.join('')
}

fn trim_slash_line_break(s string) string {
	mut start := 0
	mut ret_str := s
	for {
		idx := ret_str.index_after('\\\n', start)
		if idx != -1 {
			ret_str = ret_str[..idx] + ret_str[idx + 2..].trim_left(' \n\t\v\f\r')
			start = idx
		} else {
			break
		}
	}
	return ret_str
}

/// ident_char is called when a backtick "single-char" is parsed from the code
/// it is needed because some runes (chars) are written with escape sequences
/// the string it returns should be a standardized, simplified version of the character
/// as it would appear in source code
/// possibilities:
///   single chars like `a`, `b` => 'a', 'b'
///   escaped single chars like `\\`, `\``, `\n` => '\\', '`', '\n'
///   escaped single hex bytes like `\x01`, `\x61` => '\x01', 'a'
///   escaped unicode literals like `\u2605`
///   escaped utf8 runes in hex like `\xe2\x98\x85` => (★)
///   escaped utf8 runes in octal like `\342\230\205` => (★)
fn (mut s Scanner) ident_char() string {
	lspos := token.Pos{
		line_nr: s.line_nr
		pos: s.pos
		col: s.pos - s.last_nl_pos - 1
	}

	start := s.pos // the string position of the first backtick char
	slash := `\\`
	mut len := 0

	// set flags for advanced escapes first
	escaped_hex := s.expect('\\x', start + 1)
	escaped_unicode := s.expect('\\u', start + 1)
	escaped_octal := !escaped_hex && !escaped_unicode && s.expect('\\', start + 1)

	// walk the string to get characters up to the next backtick
	for {
		s.pos++
		if s.pos >= s.text.len {
			break
		}
		if s.text[s.pos] != slash {
			len++
		}
		double_slash := s.expect('\\\\', s.pos - 2)
		if s.text[s.pos] == `\`` && (s.text[s.pos - 1] != slash || double_slash) {
			// ` // apostrophe balance comment. do not remove
			if double_slash {
				len++
			}
			break
		}
	}
	len--
	mut c := s.text[start + 1..s.pos]
	if s.is_fmt {
		return c
	}
	if len != 1 {
		// the string inside the backticks is longer than one character
		// but we might only have one rune... attempt to decode escapes
		// if the content expresses an escape code, it will have an even number of characters
		// e.g. (octal) \141 (hex) \x61 or (unicode) \u2605
		// we don't handle binary escape codes in rune literals
		orig := c
		if (c.len % 2 == 0) && (escaped_hex || escaped_unicode || escaped_octal) {
			if escaped_unicode {
				// there can only be one, so attempt to decode it now
				c = s.decode_u_escapes(c, 0, [0])
			} else {
				// find escape sequence start positions
				mut escapes_pos := []int{}
				for i, v in c {
					if v == `\\` {
						escapes_pos << i
					}
				}
				if escaped_hex {
					c = decode_h_escapes(c, 0, escapes_pos)
				} else {
					c = decode_o_escapes(c, 0, escapes_pos)
				}
			}
		}

		u := c.runes()
		if u.len != 1 {
			if escaped_hex || escaped_unicode {
				s.error('invalid character literal `$orig` => `$c` ($u) (escape sequence did not refer to a singular rune)')
			} else if u.len == 0 {
				s.add_error_detail_with_pos('use quotes for strings, backticks for characters',
					lspos)
				s.error('invalid empty character literal `$orig`')
			} else {
				s.add_error_detail_with_pos('use quotes for strings, backticks for characters',
					lspos)
				s.error('invalid character literal `$orig` => `$c` ($u) (more than one character)')
			}
		}
	}
	// Escapes a `'` character
	if c == "'" {
		return '\\' + c
	}
	return c
}

[direct_array_access; inline]
fn (s &Scanner) expect(want string, start_pos int) bool {
	end_pos := start_pos + want.len
	if start_pos < 0 || end_pos < 0 || start_pos >= s.text.len || end_pos > s.text.len {
		return false
	}
	for pos in start_pos .. end_pos {
		if s.text[pos] != want[pos - start_pos] {
			return false
		}
	}
	return true
}

[inline]
fn (mut s Scanner) ignore_line() {
	s.eat_to_end_of_line()
	s.inc_line_number()
}

[direct_array_access; inline]
fn (mut s Scanner) eat_to_end_of_line() {
	for s.pos < s.text.len && s.text[s.pos] != scanner.b_lf {
		s.pos++
	}
}

[inline]
fn (mut s Scanner) inc_line_number() {
	s.last_nl_pos = mathutil.min(s.text.len - 1, s.pos)
	if s.is_crlf {
		s.last_nl_pos++
	}
	s.line_nr++
	if s.line_nr > s.nr_lines {
		s.nr_lines = s.line_nr
	}
}

pub fn (mut s Scanner) note(msg string) {
	pos := token.Pos{
		line_nr: s.line_nr
		pos: s.pos
	}
	if s.pref.output_mode == .stdout && !s.pref.check_only {
		util.show_compiler_message('notice:', pos: pos, file_path: s.file_path, message: msg)
	} else {
		s.notices << errors.Notice{
			file_path: s.file_path
			pos: pos
			reporter: .scanner
			message: msg
		}
	}
}

// call this *before* calling error or warn
pub fn (mut s Scanner) add_error_detail(msg string) {
	s.error_details << msg
}

pub fn (mut s Scanner) add_error_detail_with_pos(msg string, pos token.Pos) {
	s.add_error_detail(util.formatted_error('details:', msg, s.file_path, pos))
}

fn (mut s Scanner) eat_details() string {
	mut details := ''
	if s.error_details.len > 0 {
		details = '\n' + s.error_details.join('\n')
		s.error_details = []
	}
	return details
}

pub fn (mut s Scanner) warn(msg string) {
	if s.pref.warns_are_errors {
		s.error(msg)
		return
	}
	pos := token.Pos{
		line_nr: s.line_nr
		pos: s.pos
		col: s.current_column() - 1
	}
	details := s.eat_details()
	if s.pref.output_mode == .stdout && !s.pref.check_only {
		util.show_compiler_message('warning:',
			pos: pos
			file_path: s.file_path
			message: msg
			details: details
		)
	} else {
		if s.pref.message_limit >= 0 && s.warnings.len >= s.pref.message_limit {
			s.should_abort = true
			return
		}
		s.warnings << errors.Warning{
			file_path: s.file_path
			pos: pos
			reporter: .scanner
			message: msg
			details: details
		}
	}
}

pub fn (mut s Scanner) error(msg string) {
	pos := token.Pos{
		line_nr: s.line_nr
		pos: s.pos
		col: s.current_column() - 1
	}
	details := s.eat_details()
	if s.pref.output_mode == .stdout && !s.pref.check_only {
		util.show_compiler_message('error:',
			pos: pos
			file_path: s.file_path
			message: msg
			details: details
		)
		exit(1)
	} else {
		if s.pref.fatal_errors {
			util.show_compiler_message('error:',
				pos: pos
				file_path: s.file_path
				message: msg
				details: details
			)
			exit(1)
		}
		if s.pref.message_limit >= 0 && s.errors.len >= s.pref.message_limit {
			s.should_abort = true
			return
		}
		s.errors << errors.Error{
			file_path: s.file_path
			pos: pos
			reporter: .scanner
			message: msg
			details: details
		}
	}
}

fn (mut s Scanner) vet_error(msg string, fix vet.FixKind) {
	ve := vet.Error{
		message: msg
		file_path: s.file_path
		pos: token.Pos{
			line_nr: s.line_nr
			col: s.current_column() - 1
		}
		kind: .error
		fix: fix
		typ: .default
	}
	s.vet_errors << ve
}

fn (mut s Scanner) trace(fbase string, message string) {
	if s.file_base == fbase {
		println('> s.trace | ${fbase:-10s} | $message')
	}
}
