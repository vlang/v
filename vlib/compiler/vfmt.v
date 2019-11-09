// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module compiler

import strings

[if vfmt]
fn (scanner mut Scanner) fgen(s_ string) {
	mut s := s_
	if scanner.fmt_line_empty {
		s = strings.repeat(`\t`, scanner.fmt_indent) + s
	}
	scanner.fmt_out.write(s)
	scanner.fmt_line_empty = false
}

[if vfmt]
fn (scanner mut Scanner) fgenln(s_ string) {
	mut s := s_
	if scanner.fmt_line_empty {
		s = strings.repeat(`\t`, scanner.fmt_indent) + s
	}
	scanner.fmt_out.writeln(s)
	scanner.fmt_line_empty = true
}


[if vfmt]
fn (p mut Parser) fgen(s string) {
	}
[if vfmt]
fn (p mut Parser) fgen2(s string) {
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
	p.fgen2(' ')
}


[if vfmt]
fn (p mut Parser) fgenln(s string) {
	}

[if vfmt]
fn (p mut Parser) fgenln2(s string) {
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
	if p.tok == .rcbr {
		p.fmt_dec()
	}
	p.fgen2(p.strtok())
	// vfmt: increase indentation on `{` unless it's `{}`
	if p.tok == .lcbr { //&& p.scanner.pos + 1 < p.scanner.text.len && p.scanner.text[p.scanner.pos + 1] != `}` {
		p.fgenln2('')
		p.fmt_inc()
	}
}

