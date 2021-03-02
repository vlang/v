// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module scanner

import os
import v.token
import v.pref
import v.util
import v.vet
import v.errors

const (
	single_quote = `\'`
	double_quote = `"`
	// char used as number separator
	num_sep      = `_`
)

pub struct Scanner {
pub mut:
	file_path         string // '/path/to/file.v'
	file_base         string // 'file.v'
	text              string // the whole text of the file
	pos               int    // current position in the file, first character is s.text[0]
	line_nr           int    // current line number
	last_nl_pos       int    // for calculating column
	is_inside_string  bool   // set to true in a string, *at the start* of an $var or ${expr}
	is_inter_start    bool   // for hacky string interpolation TODO simplify
	is_inter_end      bool
	is_enclosed_inter bool
	line_comment      string
	// prev_tok                 TokenKind
	is_started                  bool
	is_print_line_on_error      bool
	is_print_colored_error      bool
	is_print_rel_paths_on_error bool
	quote                       byte // which quote is used to denote current string: ' or "
	inter_quote                 byte
	line_ends                   []int // the positions of source lines ends   (i.e. \n signs)
	nr_lines                    int   // total number of lines in the source file that were scanned
	is_vh                       bool  // Keep newlines
	is_fmt                      bool  // Used for v fmt.
	comments_mode               CommentsMode
	is_inside_toplvl_statement  bool // *only* used in comments_mode: .toplevel_comments, toggled by parser
	all_tokens                  []token.Token // *only* used in comments_mode: .toplevel_comments, contains all tokens
	tidx                        int
	eofs                        int
	pref                        &pref.Preferences
	errors                      []errors.Error
	warnings                    []errors.Warning
	vet_errors                  []vet.Error
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
pub fn new_scanner_file(file_path string, comments_mode CommentsMode, pref &pref.Preferences) &Scanner {
	if !os.exists(file_path) {
		verror("$file_path doesn't exist")
	}
	raw_text := util.read_file(file_path) or {
		verror(err)
		return voidptr(0)
	}
	mut s := &Scanner{
		pref: pref
		text: raw_text
		is_print_line_on_error: true
		is_print_colored_error: true
		is_print_rel_paths_on_error: true
		is_fmt: pref.is_fmt
		comments_mode: comments_mode
		file_path: file_path
		file_base: os.base(file_path)
	}
	s.init_scanner()
	return s
}

// new scanner from string.
pub fn new_scanner(text string, comments_mode CommentsMode, pref &pref.Preferences) &Scanner {
	mut s := &Scanner{
		pref: pref
		text: text
		is_print_line_on_error: true
		is_print_colored_error: true
		is_print_rel_paths_on_error: true
		is_fmt: pref.is_fmt
		comments_mode: comments_mode
		file_path: 'internal_memory'
		file_base: 'internal_memory'
	}
	s.init_scanner()
	return s
}

fn (mut s Scanner) init_scanner() {
	util.get_timers().measure_pause('PARSE')
	s.scan_all_tokens_in_buffer(s.comments_mode)
	util.get_timers().measure_resume('PARSE')
}

[unsafe]
pub fn (mut s Scanner) free() {
	unsafe {
		s.text.free()
	}
}

[inline]
fn (s &Scanner) should_parse_comment() bool {
	return (s.comments_mode == .parse_comments)
		|| (s.comments_mode == .toplevel_comments && !s.is_inside_toplvl_statement)
}

// NB: this is called by v's parser
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
		pos: s.pos - len + 1
		len: len
		tidx: cidx
	}
}

[inline]
fn (mut s Scanner) ident_name() string {
	start := s.pos
	s.pos++
	for s.pos < s.text.len && (util.is_name_char(s.text[s.pos]) || s.text[s.pos].is_digit()) {
		s.pos++
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
		mut b := malloc(end - start + 1) // add a byte for the endstring 0
		mut i1 := 0
		for i := start; i < end; i++ {
			if txt[i] != scanner.num_sep {
				b[i1] = txt[i]
				i1++
			}
		}
		b[i1] = 0 // C string compatibility
		return b.vstring_with_len(i1)
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
		s.error('this number has unsuitable digit `$first_wrong_digit.str()`')
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

[inline]
fn (mut s Scanner) skip_whitespace() {
	// if s.is_vh { println('vh') return }
	for s.pos < s.text.len && s.text[s.pos].is_space() {
		if util.is_nl(s.text[s.pos]) && s.is_vh {
			return
		}
		// Count \r\n as one line
		if util.is_nl(s.text[s.pos]) && !s.expect('\r\n', s.pos - 1) {
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

pub fn (mut s Scanner) scan_all_tokens_in_buffer(mode CommentsMode) {
	// s.scan_all_tokens_in_buffer is used mainly by vdoc,
	// in order to implement the .toplevel_comments mode.
	util.timing_start('SCAN')
	defer {
		util.timing_measure_cumulative('SCAN')
	}
	oldmode := s.comments_mode
	s.comments_mode = mode
	s.scan_remaining_text()
	s.comments_mode = oldmode
	s.tidx = 0
	$if debugscanner ? {
		for t in s.all_tokens {
			eprintln('> tidx:${t.tidx:-5} | kind: ${t.kind:-10} | lit: $t.lit')
		}
	}
}

pub fn (mut s Scanner) scan_remaining_text() {
	for {
		t := s.text_scan()
		if s.comments_mode == .skip_comments && t.kind == .comment {
			continue
		}
		s.all_tokens << t
		if t.kind == .eof {
			break
		}
	}
}

pub fn (mut s Scanner) scan() token.Token {
	return s.buffer_scan()
}

pub fn (mut s Scanner) buffer_scan() token.Token {
	for {
		cidx := s.tidx
		s.tidx++
		if cidx >= s.all_tokens.len {
			return s.end_of_file()
		}
		if s.all_tokens[cidx].kind == .comment {
			if !s.should_parse_comment() {
				continue
			}
		}
		return s.all_tokens[cidx]
	}
	return s.new_token(.eof, '', 1)
}

[inline]
pub fn (s &Scanner) peek_token(n int) token.Token {
	idx := s.tidx + n
	if idx >= s.all_tokens.len {
		return s.new_eof_token()
	}
	t := s.all_tokens[idx]
	return t
}

[inline]
fn (s &Scanner) look_ahead(n int) byte {
	if s.pos + n < s.text.len {
		return s.text[s.pos + n]
	} else {
		return `\0`
	}
}

fn (mut s Scanner) text_scan() token.Token {
	// The for loop here is so that instead of doing
	// `return s.scan()` (which will use a new call stack frame),
	// text_scan can just do continue, keeping
	// memory & stack usage low.
	// That optimization mostly matters for long sections
	// of comments and string literals.
	for {
		// if s.comments_mode == .parse_comments {
		// println('\nscan()')
		// }
		// if s.line_comment != '' {
		// s.fgenln('// LC "$s.line_comment"')
		// s.line_comment = ''
		// }
		if s.is_started {
			s.pos++
		} else {
			s.is_started = true
		}
		if !s.is_inside_string {
			s.skip_whitespace()
		}
		if s.pos >= s.text.len {
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
			kind := token.keywords[name]
			if kind != .unknown {
				return s.new_token(kind, name, name.len)
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
				&& s.look_ahead(2) !in [`x`, `n`, `r`, `\\`, `t`, `e`, `"`, `\'`] {
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
			if s.pos == 0 && next_char == ` ` {
				// If a single letter name at the start of the file, increment
				// Otherwise the scanner would be stuck at s.pos = 0
				s.pos++
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
				return s.new_token(.question, '', 1)
			}
			scanner.single_quote, scanner.double_quote {
				ident_string := s.ident_string()
				return s.new_token(.string, ident_string, ident_string.len + 2) // + two quotes
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
					continue
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
				if s.is_enclosed_inter {
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
					return s.new_token(.rcbr, '', 1)
				}
			}
			`&` {
				if nextc == `=` {
					s.pos++
					return s.new_token(.and_assign, '', 2)
				}
				afternextc := s.look_ahead(2)
				if nextc == `&` && afternextc.is_space() {
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
				if '@' + name in token.valid_at_tokens {
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
			/*
			case `\r`:
		if nextc == `\n` {
			s.pos++
			s.last_nl_pos = s.pos
			return s.new_token(.nl, '')
		}
	 }
	case `\n`:
		s.last_nl_pos = s.pos
		return s.new_token(.nl, '')
	 }
			*/
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
				start := s.pos + 1
				s.ignore_line()
				if nextc == `!` {
					// treat shebang line (#!) as a comment
					comment := s.text[start - 1..s.pos].trim_space()
					// s.fgenln('// shebang line "$s.line_comment"')
					return s.new_token(.comment, comment, comment.len + 2)
				}
				hash := s.text[start..s.pos].trim_space()
				return s.new_token(.hash, hash, hash.len)
			}
			`>` {
				if nextc == `=` {
					s.pos++
					return s.new_token(.ge, '', 2)
				} else if nextc == `>` {
					if s.pos + 2 < s.text.len && s.text[s.pos + 2] == `=` {
						s.pos += 2
						return s.new_token(.right_shift_assign, '', 3)
					}
					s.pos++
					return s.new_token(.right_shift, '', 2)
				} else {
					return s.new_token(.gt, '', 1)
				}
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
					return s.new_token(.not, '', 1)
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
				if nextc == `/` {
					start := s.pos + 1
					s.ignore_line()
					mut comment_line_end := s.pos
					if s.text[s.pos - 1] == `\r` {
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
						for j := start - 2; j >= 0 && s.text[j] != `\n`; j-- {
							if s.text[j] !in [`\t`, ` `] {
								is_separate_line_comment = false
							}
						}
						if is_separate_line_comment {
							// NB: ´\x01´ is used to preserve the initial whitespace in comments
							//     that are on a separate line
							comment = '\x01' + comment
						}
						return s.new_token(.comment, comment, comment.len + 2)
					}
					// s.fgenln('// ${s.prev_tok.str()} "$s.line_comment"')
					// Skip the comment (return the next token)
					continue
				}
				// Multiline comments
				if nextc == `*` {
					start := s.pos + 2
					start_line := s.line_nr
					mut nest_count := 1
					// Skip comment
					for nest_count > 0 && s.pos < s.text.len - 1 {
						s.pos++
						if s.pos >= s.text.len {
							s.line_nr--
							s.error('comment not terminated')
						}
						if s.text[s.pos] == `\n` {
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
	end := util.imin(s.pos + len, s.text.len)
	c := s.text[s.pos..end]
	s.error('invalid character `$c`')
}

fn (s &Scanner) current_column() int {
	return s.pos - s.last_nl_pos
}

fn (s &Scanner) count_symbol_before(p int, sym byte) int {
	mut count := 0
	for i := p; i >= 0; i-- {
		if s.text[i] != sym {
			break
		}
		count++
	}
	return count
}

fn (mut s Scanner) ident_string() string {
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
	// if s.file_path.contains('string_test') {
	// println('\nident_string() at char=${s.text[s.pos].str()}')
	// println('linenr=$s.line_nr quote=  $qquote ${qquote.str()}')
	// }
	mut n_cr_chars := 0
	mut start := s.pos
	if s.text[start] == s.quote
		|| (s.text[start] == s.inter_quote && (s.is_inter_start || s.is_enclosed_inter)) {
		start++
	}
	s.is_inside_string = false
	slash := `\\`
	for {
		s.pos++
		if s.pos >= s.text.len {
			s.error('unfinished string literal')
			break
		}
		c := s.text[s.pos]
		prevc := s.text[s.pos - 1]
		// end of string
		if c == s.quote && (prevc != slash || (prevc == slash && s.text[s.pos - 2] == slash)) {
			// handle '123\\'  slash at the end
			break
		}
		if c == s.inter_quote && (s.is_inter_start || s.is_enclosed_inter) {
			break
		}
		if c == `\r` {
			n_cr_chars++
		}
		if c == `\n` {
			s.inc_line_number()
		}
		// Don't allow \0
		if c == `0` && s.pos > 2 && prevc == slash {
			if (s.pos < s.text.len - 1 && s.text[s.pos + 1].is_digit())
				|| s.count_symbol_before(s.pos - 1, slash) % 2 == 0 {
			} else if !is_cstr && !is_raw {
				s.error(r'cannot use `\0` (NULL character) in the string literal')
			}
		}
		// Don't allow \x00
		if c == `0` && s.pos > 5 && s.expect('\\x0', s.pos - 3) {
			if s.count_symbol_before(s.pos - 3, slash) % 2 == 0 {
			} else if !is_cstr && !is_raw {
				s.error(r'cannot use `\x00` (NULL character) in the string literal')
			}
		}
		// Escape `\x` `\u`
		if prevc == slash && !is_raw && !is_cstr && s.count_symbol_before(s.pos - 2, slash) % 2 == 0 {
			// Escape `\x`
			if c == `x` && (s.text[s.pos + 1] == s.quote || !s.text[s.pos + 1].is_hex_digit()) {
				s.error(r'`\x` used with no following hex digits')
			}
			// Escape `\u`
			if c == `u` && (s.text[s.pos + 1] == s.quote
				|| s.text[s.pos + 2] == s.quote || s.text[s.pos + 3] == s.quote
				|| s.text[s.pos + 4] == s.quote || !s.text[s.pos + 1].is_hex_digit()
				|| !s.text[s.pos + 2].is_hex_digit()
				|| !s.text[s.pos + 3].is_hex_digit()
				|| !s.text[s.pos + 4].is_hex_digit()) {
				s.error(r'`\u` incomplete unicode character value')
			}
		}
		// ${var} (ignore in vfmt mode) (skip \$)
		if prevc == `$` && c == `{` && !is_raw && s.count_symbol_before(s.pos - 2, slash) % 2 == 0 {
			s.is_inside_string = true
			s.is_enclosed_inter = true
			// so that s.pos points to $ at the next step
			s.pos -= 2
			break
		}
		// $var
		if prevc == `$` && util.is_name_char(c) && !is_raw
			&& s.count_symbol_before(s.pos - 2, slash) % 2 == 0 {
			s.is_inside_string = true
			s.is_inter_start = true
			s.pos -= 2
			break
		}
	}
	mut lit := ''
	mut end := s.pos
	if s.is_inside_string {
		end++
	}
	if start <= s.pos {
		mut string_so_far := s.text[start..end]
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

fn (mut s Scanner) ident_char() string {
	start := s.pos
	slash := `\\`
	mut len := 0
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
	c := s.text[start + 1..s.pos]
	if len != 1 {
		u := c.ustring()
		if u.len != 1 {
			s.error('invalid character literal (more than one character)\n' +
				'use quotes for strings, backticks for characters')
		}
	}
	// Escapes a `'` character
	return if c == "'" { '\\' + c } else { c }
}

[inline]
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

[inline]
fn (mut s Scanner) eat_to_end_of_line() {
	for s.pos < s.text.len && s.text[s.pos] != `\n` {
		s.pos++
	}
}

[inline]
fn (mut s Scanner) inc_line_number() {
	s.last_nl_pos = s.pos
	s.line_nr++
	s.line_ends << s.pos
	if s.line_nr > s.nr_lines {
		s.nr_lines = s.line_nr
	}
}

pub fn (mut s Scanner) warn(msg string) {
	if s.pref.warns_are_errors {
		s.error(msg)
		return
	}
	pos := token.Position{
		line_nr: s.line_nr
		pos: s.pos
	}
	if s.pref.output_mode == .stdout {
		eprintln(util.formatted_error('warning:', msg, s.file_path, pos))
	} else {
		s.warnings << errors.Warning{
			file_path: s.file_path
			pos: pos
			reporter: .scanner
			message: msg
		}
	}
}

pub fn (mut s Scanner) error(msg string) {
	pos := token.Position{
		line_nr: s.line_nr
		pos: s.pos
	}
	if s.pref.output_mode == .stdout {
		eprintln(util.formatted_error('error:', msg, s.file_path, pos))
		exit(1)
	} else {
		if s.pref.fatal_errors {
			exit(1)
		}
		s.errors << errors.Error{
			file_path: s.file_path
			pos: pos
			reporter: .scanner
			message: msg
		}
	}
}

fn (mut s Scanner) vet_error(msg string, fix vet.FixKind) {
	ve := vet.Error{
		message: msg
		file_path: s.file_path
		pos: token.Position{
			line_nr: s.line_nr
		}
		kind: .error
		fix: fix
	}
	s.vet_errors << ve
}

pub fn verror(s string) {
	util.verror('scanner error', s)
}

pub fn (mut s Scanner) codegen(newtext string) {
	$if debug_codegen ? {
		eprintln('scanner.codegen:\n $newtext')
	}
	// codegen makes sense only during normal compilation
	// feeding code generated V code to vfmt or vdoc will
	// cause them to output/document ephemeral stuff.
	if s.comments_mode == .skip_comments {
		s.all_tokens.delete_last() // remove .eof from end of .all_tokens
		s.text += newtext
		old_tidx := s.tidx
		s.tidx = s.all_tokens.len
		s.scan_remaining_text()
		s.tidx = old_tidx
	}
}

fn (mut s Scanner) trace(fbase string, message string) {
	if s.file_base == fbase {
		println('> s.trace | ${fbase:-10s} | $message')
	}
}
