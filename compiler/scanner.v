// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import os
import strings

struct Scanner {
mut:
	file_path      string
	text           string
	pos            int
	line_nr        int
	inside_string  bool
	dollar_start   bool // for hacky string interpolation TODO simplify
	dollar_end     bool
	debug          bool
	line_comment   string
	started        bool
	// vfmt fields
	fmt_out        strings.Builder
	fmt_indent     int
	fmt_line_empty bool
	prev_tok Token 
}

fn new_scanner(file_path string) *Scanner {
	if !os.file_exists(file_path) {
		panic('"$file_path" doesn\'t exist')
	}

	mut raw_text := os.read_file(file_path) or {
		panic('scanner: failed to open "$file_path"')
		return &Scanner{}
	}

	// BOM check
	if raw_text.len >= 3 {
		c_text := raw_text.cstr()

		if c_text[0] == 0xEF && c_text[1] == 0xBB && c_text[2] == 0xBF {
			// skip three BOM bytes
			offset_from_begin := 3
			raw_text = tos(c_text[offset_from_begin], C.strlen(c_text) - offset_from_begin)
		}
	}

	text := raw_text

	scanner := &Scanner {
		file_path: file_path
		text: text
		fmt_out: strings.new_builder(1000)
	}

	return scanner
}

// TODO remove once multiple return values are implemented
struct ScanRes {
	tok Token
	lit string
}

fn scan_res(tok Token, lit string) ScanRes {
	return ScanRes{tok, lit}
}

fn is_white(c byte) bool {
	return c.is_white()
}

fn is_nl(c byte) bool {
	return c == `\r` || c == `\n`
}

fn (s mut Scanner) ident_name() string {
	start := s.pos
	for {
		s.pos++
		if s.pos >= s.text.len {
			break
		}
		c := s.text[s.pos]
		if !is_name_char(c) && !c.is_digit() {
			break
		}
	}
	name := s.text.substr(start, s.pos)
	s.pos--
	return name
}

fn (s mut Scanner) ident_number() string {
	start := s.pos
	is_hex := s.pos + 1 < s.text.len && s.text[s.pos] == `0` && s.text[s.pos + 1] == `x`
	is_oct := !is_hex && s.text[s.pos] == `0`
	mut is_float := false
	for {
		s.pos++
		if s.pos >= s.text.len {
			break
		}
		c := s.text[s.pos]
		if c == `.` {
			is_float = true
		}
		is_good_hex := is_hex && (c == `x`  || (c >= `a` && c <= `f`)  || (c >= `A` && c <= `F`))
		// 1e+3, 1e-3, 1e3
		if !is_hex && c == `e` && s.pos + 1 < s.text.len {
			next := s.text[s.pos + 1]
			if next == `+` || next == `-` || next.is_digit() {
				s.pos++
				continue
			}
		}
		if !c.is_digit() && c != `.` && !is_good_hex {
			break
		}
		// 1..9
		if c == `.` && s.pos + 1 < s.text.len && s.text[s.pos + 1] == `.` {
			break
		}
		if is_oct && c >= `8` && !is_float {
			s.error('malformed octal constant')
		}
	}
	number := s.text.substr(start, s.pos)
	s.pos--
	return number
}

fn (s Scanner) has_gone_over_line_end() bool {
	mut i := s.pos-1
	for i >= 0 && !is_white(s.text[i]) {
		i--
	}
	for i >= 0 && is_white(s.text[i]) {
		if is_nl(s.text[i]) {
			return true
		}
		i--
	}
	return false
}

fn (s mut Scanner) skip_whitespace() {
	for s.pos < s.text.len && is_white(s.text[s.pos]) {
		if is_nl(s.text[s.pos]) {
			// Count \r\n as one line 
			if !(s.text[s.pos] == `\n` && s.pos > 0 && s.text[s.pos-1] == `\r`) { 
				s.line_nr++
			} 
		}
		s.pos++
	}
}

fn (s mut Scanner) get_var_name(pos int) string {
	mut pos_start := pos

	for ; pos_start >= 0 && s.text[pos_start] != `\n` && s.text[pos_start] != `;`; pos_start-- {}
	pos_start++
	return s.text.substr(pos_start, pos)
}

// CAO stands for Compound Assignment Operators  (e.g '+=' )
/* 
fn (s mut Scanner) cao_change(operator string) {
	s.text = s.text.substr(0, s.pos - operator.len) + ' = ' + s.get_var_name(s.pos - operator.len) + ' ' + operator + ' ' + s.text.substr(s.pos + 1, s.text.len)
}
*/ 

fn (s mut Scanner) scan() ScanRes {
	if s.line_comment != '' { 
		//s.fgenln('// LOL "$s.line_comment"')
		//s.line_comment = '' 
	} 

	if s.started {
		s.pos++
	}

	s.started = true
	if s.pos >= s.text.len {
		return scan_res(.eof, '')
	}

	// skip whitespace
	if !s.inside_string {
		s.skip_whitespace()
	}

	// End of $var, start next string
	if s.dollar_end {
		if s.text[s.pos] == `\'` {
			s.dollar_end = false
			return scan_res(.str, '')
		}
		s.dollar_end = false
		return scan_res(.str, s.ident_string())
	}
	s.skip_whitespace()

	// end of file
	if s.pos >= s.text.len {
		return scan_res(.eof, '')
	}

	// handle each char
	c := s.text[s.pos]
	mut nextc := `\0`
	if s.pos + 1 < s.text.len {
		nextc = s.text[s.pos + 1]
	}

	// name or keyword
	if is_name_char(c) {
		name := s.ident_name()
		// tmp hack to detect . in ${}
		// Check if not .eof to prevent panic
		next_char := if s.pos + 1 < s.text.len { s.text[s.pos + 1] } else { `\0` }
		if is_key(name) {
			return scan_res(key_to_token(name), '')
		}
		// 'asdf $b' => "b" is the last name in the string, dont start parsing string
		// at the next ', skip it
		if s.inside_string {
			if next_char == `\'` {
				s.pos++
				s.dollar_start = false
				s.inside_string = false
			}
		}
		if s.dollar_start && next_char != `.` {
			s.dollar_end = true
			s.dollar_start = false
		}
		if s.pos == 0 && next_char == ` ` {
			s.pos++
			//If a single letter name at the start of the file, increment
			//Otherwise the scanner would be stuck at s.pos = 0
		}
		return scan_res(.name, name)
	}
	// `123`, `.123`
	else if c.is_digit() || c == `.` && nextc.is_digit() {
		num := s.ident_number()
		return scan_res(.integer, num)
	}
	// all other tokens
	switch c {
	case `+`:
		if nextc == `+` {
			s.pos++
			return scan_res(.inc, '')
		}
		else if nextc == `=` {
			s.pos++
			return scan_res(.plus_assign, '')
		}
		return scan_res(.plus, '')
	case `-`:
		if nextc == `-` {
			s.pos++
			return scan_res(.dec, '')
		}
		else if nextc == `=` {
			s.pos++
			return scan_res(.minus_assign, '')
		}
		return scan_res(.minus, '')
	case `*`:
		if nextc == `=` {
			s.pos++
			return scan_res(.mult_assign, '')
		}
		return scan_res(.mul, '')
	case `^`:
		if nextc == `=` {
			s.pos++
			return scan_res(.xor_assign, '')
		}
		return scan_res(.xor, '')
	case `%`:
		if nextc == `=` {
			s.pos++
			return scan_res(.mod_assign, '')
		}
		return scan_res(.mod, '')
	case `?`:
		return scan_res(.question, '')
	case `\'`:
		return scan_res(.str, s.ident_string())
		// TODO allow double quotes
		// case QUOTE:
		// return scan_res(.str, s.ident_string())
	case `\``:
		return scan_res(.chartoken, s.ident_char())
	case `(`:
		return scan_res(.lpar, '')
	case `)`:
		return scan_res(.rpar, '')
	case `[`:
		return scan_res(.lsbr, '')
	case `]`:
		return scan_res(.rsbr, '')
	case `{`:
		// Skip { in ${ in strings
		if s.inside_string {
			return s.scan()
		}
		return scan_res(.lcbr, '')
	case `$`:
		return scan_res(.dollar, '')
	case `}`:
		// s = `hello $name !`
		// s = `hello ${name} !`
		if s.inside_string {
			s.pos++
			// TODO UN.neEDED?
			if s.text[s.pos] == `\'` {
				s.inside_string = false
				return scan_res(.str, '')
			}
			return scan_res(.str, s.ident_string())
		}
		else {
			return scan_res(.rcbr, '')
		}
	case `&`:
		if nextc == `=` {
			s.pos++
			return scan_res(.and_assign, '')
		}
		if nextc == `&` {
			s.pos++
			return scan_res(.and, '')
		}
		return scan_res(.amp, '')
	case `|`:
		if nextc == `|` {
			s.pos++
			return scan_res(.logical_or, '')
		}
		if nextc == `=` {
			s.pos++
			return scan_res(.or_assign, '')
		}
		return scan_res(.pipe, '')
	case `,`:
		return scan_res(.comma, '')
	case `\r`:
		if nextc == `\n` {
			s.pos++
			return scan_res(.nl, '')
		}
	case `\n`:
		return scan_res(.nl, '')
	case `.`:
		if nextc == `.` {
			s.pos++
			return scan_res(.dotdot, '')
		}
		return scan_res(.dot, '')
	case `#`:
		start := s.pos + 1
		for s.pos < s.text.len && s.text[s.pos] != `\n` {
			s.pos++
		}
		s.line_nr++
		hash := s.text.substr(start, s.pos)
		return scan_res(.hash, hash.trim_space())
	case `>`:
		if nextc == `=` {
			s.pos++
			return scan_res(.ge, '')
		}
		else if nextc == `>` {
			if s.pos + 2 < s.text.len && s.text[s.pos + 2] == `=` {
				s.pos += 2
				return scan_res(.righ_shift_assign, '')
			}
			s.pos++
			return scan_res(.righ_shift, '')
		}
		else {
			return scan_res(.gt, '')
		}
	case `<`:
		if nextc == `=` {
			s.pos++
			return scan_res(.le, '')
		}
		else if nextc == `<` {
			if s.pos + 2 < s.text.len && s.text[s.pos + 2] == `=` {
				s.pos += 2
				return scan_res(.left_shift_assign, '')
			}
			s.pos++
			return scan_res(.left_shift, '')
		}
		else {
			return scan_res(.lt, '')
		}
	case `=`:
		if nextc == `=` {
			s.pos++
			return scan_res(.eq, '')
		}
		else if nextc == `>` {
			s.pos++
			return scan_res(.arrow, '')
		} 
		else {
			return scan_res(.assign, '')
		}
	case `:`:
		if nextc == `=` {
			s.pos++
			return scan_res(.decl_assign, '')
		}
		else {
			return scan_res(.colon, '')
		}
	case `;`:
		return scan_res(.semicolon, '')
	case `!`:
		if nextc == `=` {
			s.pos++
			return scan_res(.ne, '')
		}
		else {
			return scan_res(.not, '')
		}
	case `~`:
		return scan_res(.bit_not, '')
	case `/`:
		if nextc == `=` {
			s.pos++
			return scan_res(.div_assign, '')
		}
		if nextc == `/` {
			start := s.pos + 1
			for s.pos < s.text.len && s.text[s.pos] != `\n`{
				s.pos++
			}
			s.line_nr++
			s.line_comment = s.text.substr(start + 1, s.pos)
			s.line_comment = s.line_comment.trim_space()
			s.fgenln('// ${s.prev_tok.str()} "$s.line_comment"')
			// Skip the comment (return the next token) 
			return s.scan()
		}
		// Multiline comments
		if nextc == `*` {
			start := s.pos
			mut nest_count := 1
			// Skip comment
			for nest_count > 0 {
				s.pos++
				if s.pos >= s.text.len {
					s.line_nr--
					s.error('comment not terminated')
				}
				if s.text[s.pos] == `\n` {
					s.line_nr++
					continue
				}
				if s.text[s.pos] == `/` && s.text[s.pos + 1] == `*` {
					nest_count++
					continue
				}
				if s.text[s.pos] == `*` && s.text[s.pos + 1] == `/` {
					nest_count--
				}
			}
			s.pos++
			end := s.pos + 1
			comm := s.text.substr(start, end)
			s.fgenln(comm)
			// Skip if not in fmt mode
			return s.scan()
		}
		return scan_res(.div, '')
	}
	$if windows {
		if c == `\0` {
			return scan_res(.eof, '')
		} 
	} 
	println('(char code=$c) pos=$s.pos len=$s.text.len')
	mut msg := 'invalid character `${c.str()}`' 
	if c == `"` {
		msg += ', use \' to denote strings' 
	} 
	s.error(msg) 
	return scan_res(.eof, '')
}

fn (s &Scanner) error(msg string) {
	file := s.file_path.all_after('/')
	println('panic: $file:${s.line_nr + 1}')
	println(msg)
	exit(1)
}

// println('array out of bounds $idx len=$a.len')
// This is really bad. It needs a major clean up
fn (s mut Scanner) ident_string() string {
	// println("\nidentString() at char=", string(s.text[s.pos]),
	// "chard=", s.text[s.pos], " pos=", s.pos, "txt=", s.text[s.pos:s.pos+7])
	mut start := s.pos
	s.inside_string = false
	slash := `\\`
	for {
		s.pos++
		if s.pos >= s.text.len {
			break
		}
		c := s.text[s.pos]
		prevc := s.text[s.pos - 1]
		// end of string
		if c == `\'` && (prevc != slash || (prevc == slash && s.text[s.pos - 2] == slash)) {
			// handle '123\\'  slash at the end
			break
		}
		if c == `\n` {
			s.line_nr++
		}
		// Don't allow \0
		if c == `0` && s.pos > 2 && s.text[s.pos - 1] == `\\` {
			s.error('0 character in a string literal')
		}
		// Don't allow \x00
		if c == `0` && s.pos > 5 && s.text[s.pos - 1] == `0` && s.text[s.pos - 2] == `x` &&
		s.text[s.pos - 3] == `\\` {
			s.error('0 character in a string literal')
		}
		// ${var}
		if c == `{` && prevc == `$` {
			s.inside_string = true
			// so that s.pos points to $ at the next step
			s.pos -= 2
			break
		}
		// $var
		if (c.is_letter() || c == `_`) && prevc == `$` {
			s.inside_string = true
			s.dollar_start = true
			// println('setting s.dollar=true pos=$s.pos')
			s.pos -= 2
			break
		}
	}
	mut lit := ''
	if s.text[start] == `\'` {
		start++
	}
	mut end := s.pos
	if s.inside_string {
		end++
	}
	if start > s.pos{}
	else {
		lit = s.text.substr(start, end)
	}
	return lit
}

fn (s mut Scanner) ident_char() string {
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
		double_slash := s.text[s.pos - 1] == slash && s.text[s.pos - 2] == slash
		if s.text[s.pos] == `\`` && (s.text[s.pos - 1] != slash || double_slash) {
			if double_slash {
				len++
			}
			break
		}
	}
	len--
	c := s.text.substr(start + 1, s.pos)
	if len != 1 {
		u := c.ustring()
		if u.len != 1 {
		s.error('invalid character literal (more than one character: $len)')
		}
	}
	return c
}

fn (p mut Parser) peek() Token {
	for {
		tok := p.scanner.peek()
		if tok != .nl {
			return tok
		}
	}
}

fn (s mut Scanner) peek() Token {
	pos := s.pos
	line := s.line_nr
	inside_string := s.inside_string
	dollar_start := s.dollar_start
	dollar_end := s.dollar_end
	res := s.scan()
	tok := res.tok
	s.pos = pos
	s.line_nr = line
	s.inside_string = inside_string
	s.dollar_start = dollar_start
	s.dollar_end = dollar_end
	return tok
}

fn (s mut Scanner) debug_tokens() {
	s.pos = 0
	s.debug = true
	for {
		res := s.scan()
		tok := res.tok
		lit := res.lit
		print(tok.str())
		if lit != '' {
			println(' `$lit`')
		}
		else {
			println('')
		}
		if tok == .eof {
			break
		}
	}
}

fn is_name_char(c byte) bool {
	return c.is_letter() || c == `_`
}

fn (s mut Scanner) get_opening_bracket() int {
	mut pos := s.pos
	mut parentheses := 0
	mut inside_string := false

	for pos > 0 && s.text[pos] != `\n` {
		if s.text[pos] == `)` && !inside_string {
			parentheses++
		}
		if s.text[pos] == `(` && !inside_string {
			parentheses--
		}
		if s.text[pos] == `\'` && s.text[pos - 1] != `\\` && s.text[pos - 1] != `\`` {
			inside_string = !inside_string
		}
		if parentheses == 0 {
			break
		}
		pos--
	}
	return pos
}

// Foo { bar: 3, baz: 'hi' } => '{ bar: 3, baz: "hi" }'
fn (s mut Scanner) create_type_string(T Type, name string) {
	line := s.line_nr
	inside_string := s.inside_string
	mut newtext := '\'{ '
	start := s.get_opening_bracket() + 1
	end := s.pos
	for i, field in T.fields {
		if i != 0 {
			newtext += ', '
		}
		newtext += '$field.name: ' + '$${name}.${field.name}'
	}
	newtext += ' }\''
	s.text = s.text.substr(0, start) + newtext + s.text.substr(end, s.text.len)
	s.pos = start - 2
	s.line_nr = line
	s.inside_string = inside_string
}

fn (p mut Parser) create_type_string(T Type, name string) {
	p.scanner.create_type_string(T, name)
}
