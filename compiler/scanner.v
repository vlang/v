// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

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
	is_fmt         bool
	// vfmt fields
	fmt_out        StringBuilder
	fmt_indent     int
	fmt_line_empty bool
}

const (
	SINGLE_QUOTE = `\'`
	QUOTE        = `"`
)

fn new_scanner(file_path string) *Scanner {
	if !os.file_exists(file_path) {
		panic('"$file_path" doesn\'t exist')
	}
	scanner := &Scanner {
		file_path: file_path
		text: os.read_file(file_path)
		fmt_out: new_string_builder(1000)
	}
	// println('new scanner "$file_path" txt.len=$scanner.text.len')
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
	i := int(c)
	return i == 12 || i == 10
}

fn (s mut Scanner) ident_name() string {
	start := s.pos
	for {
		s.pos++
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
	is_hex := s.text[s.pos] == `0` && s.text[s.pos + 1] == `x`
	is_oct := !is_hex && s.text[s.pos] == `0`
	mut is_float := false
	for {
		s.pos++
		c := s.text[s.pos]
		if c == `.` {
			is_float = true
		}
		is_good_hex := is_hex && (c == `x` || c == `u` || (c >= `a` && c <= `f`))
		// 1e+3, 1e-3, 1e3
		if !is_hex && c == `e` {
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
		if c == `.` && s.text[s.pos + 1] == `.` {
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

fn (s mut Scanner) skip_whitespace() {
	for s.pos < s.text.len && is_white(s.text[s.pos]) {
		if is_nl(s.text[s.pos]) {
			s.line_nr++
			if s.is_fmt {
				return
			}
		}
		s.pos++
	}
	// if s.pos == s.text.len {
	// return scan_res(EOF, '')
	// }
}

fn (s mut Scanner) scan() ScanRes {
	// if s.file_path == 'd.v' {
	// println('\nscan()')
	// }
	// if s.started {
	if s.pos > 0 {
		// || (s.pos == 0 && s.text.len > 0 && s.text[s.pos] == `\n`) {
		s.pos++
	}
	s.started = true
	if s.pos >= s.text.len {
		return scan_res(EOF, '')
	}
	// skip whitespace
	if !s.inside_string {
		s.skip_whitespace()
	}
	if s.is_fmt && s.text[s.pos] == `\n` {
		return scan_res(NL, '')
	}
	// End of $var, start next string
	if !s.is_fmt && s.dollar_end {
		// fmt.Println("end of $var, get string", s.pos, string(s.text[s.pos]))
		if s.text[s.pos] == SINGLE_QUOTE {
			// fmt.Println("ENDDD")
			s.dollar_end = false
			return scan_res(STRING, '')
		}
		s.dollar_end = false
		return scan_res(STRING, s.ident_string())
	}
	s.skip_whitespace()
	// println('ws skipped')
	// end of file
	if s.pos >= s.text.len {
		// println('scan(): returning EOF (pos >= len)')
		return scan_res(EOF, '')
	}
	// println('!!!!! HANDLE CHAR pos=$s.pos')
	// handle each char
	c := s.text[s.pos]
	mut nextc := `\0`
	if s.pos + 1 < s.text.len {
		nextc = s.text[s.pos + 1]
	}
	// name or keyword
	if is_name_char(c) {
		name := s.ident_name()
		next_char := s.text[s.pos + 1]// tmp hack to detect . in ${}
		// println('!!! got name=$name next_char=$next_char')
		if is_key(name) {
			// println('IS KEY')
			// tok := (key_to_token(name))
			// println(tok.str())
			return scan_res(key_to_token(name), '')
		}
		// 'asdf $b' => "b" is the last name in the string, dont start parsing string
		// at the next ', skip it
		if s.inside_string {
			// println('is_letter inside string! nextc=${nextc.str()}')
			if s.text[s.pos + 1] == SINGLE_QUOTE {
				// println('var is last before QUOTE')
				s.pos++
				s.dollar_start = false
				s.inside_string = false
			}
		}
		if s.dollar_start && next_char != `.` {
			// println('INSIDE STRING .dollar var=$name')
			s.dollar_end = true
			s.dollar_start = false
		}
		return scan_res(NAME, name)
	}
	// number, `.123`
	else if c.is_digit() || c == `.` && nextc.is_digit() {
		num := s.ident_number()
		return scan_res(INT, num)
	}
	// all other tokens
	switch c {
	case `+`:
		if nextc == `+` {
			s.pos++
			return scan_res(INC, '')
		}
		else if nextc == `=` {
			s.pos++
			return scan_res(PLUS_ASSIGN, '')
		}
		return scan_res(PLUS, '')
	case `-`:
		if nextc == `-` {
			s.pos++
			return scan_res(DEC, '')
		}
		else if nextc == `=` {
			s.pos++
			return scan_res(MINUS_ASSIGN, '')
		}
		return scan_res(MINUS, '')
	case `*`:
		if nextc == `=` {
			s.pos++
			return scan_res(MULT_ASSIGN, '')
		}
		return scan_res(MUL, '')
	case `^`:
		if nextc == `=` {
			s.pos++
			return scan_res(XOR_ASSIGN, '')
		}
		return scan_res(XOR, '')
	case `%`:
		if nextc == `=` {
			s.pos++
			return scan_res(MOD_ASSIGN, '')
		}
		return scan_res(MOD, '')
	case `?`:
		return scan_res(QUESTION, '')
	case SINGLE_QUOTE:
		return scan_res(STRING, s.ident_string())
		// TODO allow double quotes
		// case QUOTE:
		// return scan_res(STRING, s.ident_string())
	case `\``:
		return scan_res(CHAR, s.ident_char())
	case `(`:
		return scan_res(LPAR, '')
	case `)`:
		return scan_res(RPAR, '')
	case `[`:
		return scan_res(LSBR, '')
	case `]`:
		return scan_res(RSBR, '')
	case `{`:
		// Skip { in ${ in strings
		if s.inside_string {
			return s.scan()
		}
		return scan_res(LCBR, '')
	case `$`:
		return scan_res(DOLLAR, '')
	case `}`:
		// s = `hello $name kek`
		// s = `hello ${name} kek`
		if s.inside_string {
			s.pos++
			// TODO UNNEEDED?
			if s.text[s.pos] == SINGLE_QUOTE {
				s.inside_string = false
				return scan_res(STRING, '')
			}
			return scan_res(STRING, s.ident_string())
		}
		else {
			return scan_res(RCBR, '')
		}
	case `&`:
		if nextc == `=` {
			s.pos++
			return scan_res(AND_ASSIGN, '')
		}
		if s.text[s.pos + 1] == `&` {
			s.pos++
			return scan_res(AND, '')
		}
		return scan_res(AMP, '')
	case `|`:
		if s.text[s.pos + 1] == `|` {
			s.pos++
			return scan_res(OR, '')
		}
		if nextc == `=` {
			s.pos++
			return scan_res(OR_ASSIGN, '')
		}
		return scan_res(PIPE, '')
	case `,`:
		return scan_res(COMMA, '')
	case `\r`:
		if nextc == `\n` {
			s.pos++
			return scan_res(NL, '')
		}
	case `\n`:
		return scan_res(NL, '')
	case `.`:
		if s.text[s.pos + 1] == `.` {
			s.pos++
			return scan_res(DOTDOT, '')
		}
		return scan_res(DOT, '')
	case `#`:
		start := s.pos + 1
		for s.text[s.pos] != `\n` {
			s.pos++
		}
		s.line_nr++
		hash := s.text.substr(start, s.pos)
		if s.is_fmt {
			// fmt needs NL after #
			s.pos--
		}
		return scan_res(HASH, hash.trim_space())
	case `@`:
		start := s.pos + 1
		for s.text[s.pos] != `\n` {
			s.pos++
		}
		s.line_nr++
		at := s.text.substr(start, s.pos)
		return scan_res(AT, at.trim_space())
	case `>`:
		if s.text[s.pos + 1] == `=` {
			s.pos++
			return scan_res(GE, '')
		}
		else if s.text[s.pos + 1] == `>` {
			if s.text[s.pos + 2] == `=` {
				s.pos += 2
				return scan_res(RIGHT_SHIFT_ASSIGN, '')
			}
			s.pos++
			return scan_res(RIGHT_SHIFT, '')
		}
		else {
			return scan_res(GT, '')
		}
	case `<`:
		if s.text[s.pos + 1] == `=` {
			s.pos++
			return scan_res(LE, '')
		}
		else if s.text[s.pos + 1] == `<` {
			if s.text[s.pos + 2] == `=` {
				s.pos += 2
				return scan_res(LEFT_SHIFT_ASSIGN, '')
			}
			s.pos++
			return scan_res(LEFT_SHIFT, '')
		}
		else {
			return scan_res(LT, '')
		}
	case `=`:
		if s.text[s.pos + 1] == `=` {
			s.pos++
			return scan_res(EQ, '')
		}
		else {
			return scan_res(ASSIGN, '')
		}
	case `:`:
		if s.text[s.pos + 1] == `=` {
			s.pos++
			return scan_res(DECL_ASSIGN, '')
		}
		else {
			return scan_res(COLON, '')
		}
	case `;`:
		return scan_res(SEMICOLON, '')
	case `!`:
		if s.text[s.pos + 1] == `=` {
			s.pos++
			return scan_res(NE, '')
		}
		else {
			return scan_res(NOT, '')
		}
	case `~`:
		return scan_res(BIT_NOT, '')
	case `/`:
		if nextc == `=` {
			s.pos++
			return scan_res(DIV_ASSIGN, '')
		}
		if s.text[s.pos + 1] == `/` {
			// debug("!!!!!!GOT LINE COM")
			start := s.pos + 1
			for s.text[s.pos] != `\n` {
				s.pos++
			}
			s.line_nr++
			s.line_comment = s.text.substr(start + 1, s.pos)
			s.line_comment = s.line_comment.trim_space()
			s.fgenln('// $s.line_comment')
			if s.is_fmt {
				// fmt needs NL after comment
				s.pos--
			}
			else {
				// Skip comment
				return s.scan()
			}
			return scan_res(LINE_COM, s.line_comment)
		}
		// Multiline comments
		if s.text[s.pos + 1] == `*` {
			start := s.pos
			// Skip comment
			for ! (s.text[s.pos] == `*` && s.text[s.pos + 1] == `/`) {
				s.pos++
				if s.pos >= s.text.len {
					s.line_nr--
					s.error('comment not terminated')
				}
				if s.text[s.pos] == `\n` {
					s.line_nr++
				}
			}
			s.pos++
			end := s.pos + 1
			comm := s.text.substr(start, end)
			s.fgenln(comm)
			if s.is_fmt {
				return scan_res(MLINE_COM, comm)
			}
			// Skip if not in fmt mode
			return s.scan()
		}
		return scan_res(DIV, '')
	}
	println('(char code=$c) pos=$s.pos len=$s.text.len')
	s.error('invalid character `${c.str()}`')
	return scan_res(EOF, '')
}

fn (s &Scanner) error(msg string) {
	file := s.file_path.all_after('/')
	println('panic: $file:${s.line_nr + 1}')
	println(msg)
	// os.print_backtrace()
	// println(file)
	// println(s.file_path)
	exit(1)
}

// println('array out of bounds $idx len=$a.len')
// This is really bad. It needs a major clean up
fn (s mut Scanner) ident_string() string {
	// println("\nidentString() at char=", string(s.text[s.pos]),
	// "chard=", s.text[s.pos], " pos=", s.pos, "txt=", s.text[s.pos:s.pos+7])
	debug := s.file_path.contains('test_test')
	if debug {
		println('identStr() $s.file_path line=$s.line_nr pos=$s.pos')
	}
	mut start := s.pos
	s.inside_string = false
	slash := `\\`
	for {
		s.pos++
		if s.pos >= s.text.len {
			break
		}
		c := s.text[s.pos]
		if debug {
			println(c.str())
		}
		prevc := s.text[s.pos - 1]
		// end of string
		if c == SINGLE_QUOTE && (prevc != slash || (prevc == slash && s.text[s.pos - 2] == slash)) {
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
		if !s.is_fmt && c == `{` && prevc == `$` {
			s.inside_string = true
			// fmt.Println("breaking out of is()")
			// so that s.pos points to $ at the next step
			s.pos -= 2
			// fmt.Println("break pos=", s.pos, "c=", string(s.text[s.pos]), "d=", s.text[s.pos])
			break
		}
		// $var
		// if !s.is_fmt && c != `{` && c != ` ` && ! (c >= `0` && c <= `9`)  && prevc == `$` {
		if !s.is_fmt && (c.is_letter() || c == `_`) && prevc == `$` {
			s.inside_string = true
			s.dollar_start = true
			// println('setting s.dollar=true pos=$s.pos')
			s.pos -= 2
			break
		}
	}
	mut lit := ''
	if s.text[start] == SINGLE_QUOTE {
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
	// if lit.contains('\n') {
	// println('\nstring lit="$lit" pos=$s.pos line=$s.line_nr')
	// }
	/* 
	for c in lit {
		if s.file_path.contains('range_test') {
			println('!')
			println(c)
		}
	}
*/
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
		s.error('invalid character literal (more than one character: $len)')
	}
	return c
}

fn (p mut Parser) peek() Token {
	for {
		tok := p.scanner.peek()
		if tok != NL {
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
	// /////
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
	fname := s.file_path.all_after('/')
	println('\n===DEBUG TOKENS $fname ============')
	// allToks := ''
	s.debug = true
	for {
		res := s.scan()
		tok := res.tok
		lit := res.lit
		// printiln(tok)
		print(tok.str())
		// allToks += tok.String()
		if lit != '' {
			println(' `$lit`')
			// allToks += " `" + lit + "`"
		}
		else {
			println('')
		}
		// allToks += "\n"
		if tok == EOF {
			println('============ END OF DEBUG TOKENS ==================')
			// fmt.Println("========"+s.file+"========\n", allToks)
			break
		}
	}
}

fn is_name_char(c byte) bool {
	return c.is_letter() || c == `_`
}

