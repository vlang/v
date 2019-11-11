// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module compiler

import strings
import os

[if vfmt]
fn (scanner mut Scanner) fgen(s_ string) {
	mut s := s_
	if scanner.fmt_line_empty {
		s = strings.repeat(`\t`, scanner.fmt_indent) + s
	}
	
	//scanner.fmt_out << s
	scanner.fmt_out.write(s)
	scanner.fmt_line_empty = false
}

[if vfmt]
fn (scanner mut Scanner) fgenln(s_ string) {
	mut s := s_
	if scanner.fmt_line_empty {
		s = strings.repeat(`\t`, scanner.fmt_indent) + s
	}
	//scanner.fmt_out << s
	//scanner.fmt_out << '\n'
	scanner.fmt_out.writeln(s)
	scanner.fmt_line_empty = true
}


[if vfmt]
fn (p mut Parser) fgen(s string) {
	if p.pass != .main {
		return
	}	
	p.scanner.fgen(s)
}

[if vfmt]
fn (p mut Parser) fspace() {
	if p.first_pass() {
		return
	}	
	p.fgen(' ')
}


[if vfmt]
fn (p mut Parser) fgenln(s string) {
	if p.pass != .main {
		return
	}	
	p.scanner.fgenln(s)
}

/*
fn (p mut Parser) peek() TokenKind {
	for {
		p.cgen.line = p.scanner.line_nr + 1
		tok := p.scanner.peek()
		if tok != .nl {
			return tok
		}
	}
	return .eof // TODO can never get here - v doesn't know that
}
*/

[if vfmt]
fn (p mut Parser) fmt_inc() {
	if p.pass != .main {
		return
	}	
	p.scanner.fmt_indent++
}

[if vfmt]
fn (p mut Parser) fmt_dec() {
	if p.pass != .main {
		return
	}	
	p.scanner.fmt_indent--
}

[if vfmt]
fn (p mut Parser) fnext() {
	if p.tok == .eof {
		return
	}
	if p.tok == .rcbr && !p.inside_if_expr && p.prev_tok != .lcbr {
		p.fmt_dec()
	}
	mut s := p.strtok()
	// Need to reconstruct an interpolated string from multiple string and
	// dollar tokens.
	// 'abc $name zxc' => ['abc', $, name, 'zxc'] => 'abc'$name'zxc'
	// need to remove the extra '
	if p.tok == .str && p.peek() == .dollar {
		s = s[..s.len - 1]
		p.fmt_dollar = true
	}	
	else if p.tok == .str && p.fmt_dollar {
		s = s[1..]
		p.fmt_dollar = false
	}	
	p.fgen(s)
	// vfmt: increase indentation on `{` unless it's `{}`
	if p.tok == .lcbr && !p.inside_if_expr && p.peek() != .rcbr {
		p.fgenln('')
		p.fmt_inc()
	}
}


[if vfmt]
fn (p mut Parser) gen_fmt() {
	if p.pass != .main {
		return
	}
	if p.file_name == '' {
		return
	}	
	s := p.scanner.fmt_out.str().trim_space()
	//s := p.scanner.fmt_out.join('').trim_space()
	if s == '' {
		return
	}	
	println('GENERATING ${p.file_name}.V')
	out := os.create('/var/tmp/fmt/' + p.file_name) or {
		verror('failed to create fmt.v')
		return
	}
	out.writeln(s)//p.scanner.fmt_out.str().trim_space())
	out.close()
}

