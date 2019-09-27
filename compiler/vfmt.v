// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import strings

// fmt helpers
fn (scanner mut Scanner) fgen(s_ string) {
	mut s := s_
	if scanner.fmt_line_empty {
		s = strings.repeat(`\t`, scanner.fmt_indent) + s
	}
	scanner.fmt_out.write(s)
	scanner.fmt_line_empty = false
}

fn (scanner mut Scanner) fgenln(s_ string) {
	mut s := s_
	if scanner.fmt_line_empty {
		s = strings.repeat(`\t`, scanner.fmt_indent) + s
	}
	scanner.fmt_out.writeln(s)
	scanner.fmt_line_empty = true
}

fn (p mut Parser) fgen(s string) {
	p.scanner.fgen(s)
}

fn (p mut Parser) fspace() {
	p.fgen(' ')
}

fn (p mut Parser) fgenln(s string) {
	p.scanner.fgenln(s)
}

/*
fn (p mut Parser) peek() Token {
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

fn (p mut Parser) fmt_inc() {
	p.scanner.fmt_indent++
}

fn (p mut Parser) fmt_dec() {
	p.scanner.fmt_indent--
}

