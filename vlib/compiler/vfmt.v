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
	if scanner.fmt_line_empty && scanner.fmt_indent > 0 {
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
fn (p mut Scanner) init_fmt() {
	// Right now we can't do `$if vfmt {`, so I'm using
	// a conditional function init_fmt to set this flag.
	// This function will only be called if `-d vfmt` is passed.
	p.is_fmt = true
}

[if vfmt]
fn (p mut Parser) fnext() {
	//if p.tok == .eof {
		//println('eof ret')
		//return
	//}
	if p.tok == .rcbr && !p.inside_if_expr && p.prev_tok != .lcbr {
		p.fmt_dec()
	}
	mut s := p.strtok()
	if p.tok != .eof {
	p.fgen(s)
	}
	// vfmt: increase indentation on `{` unless it's `{}`
	if p.tok == .lcbr && !p.inside_if_expr && p.peek() != .rcbr {
		p.fgenln('')
		p.fmt_inc()
	}
	
	// Skip comments and add them to vfmt output
	if p.tokens[p.token_idx].tok in [.line_comment, .mline_comment] {
		// Newline before the comment and after consts and closing }
		if p.inside_const {
			p.fgenln('\n')
		}	
		if p.tok == .rcbr {
			p.fgenln('')
		}	
		for p.token_idx < p.tokens.len - 1 {
			tok := p.tokens[p.token_idx].tok
			if tok != .line_comment && tok != .mline_comment {
				break
			}	
			comment_token := p.tokens[p.token_idx]
			comment := comment_token.lit
			if p.token_idx > 0 && comment_token.line_nr > p.tokens[p.token_idx-1].line_nr {
				//p.fgenln('')
			}	
			if tok == .line_comment {
				p.fgen('// ' + comment)
			}	else {
				p.fgen(comment)
			}	
			if p.token_idx > 0 &&
				comment_token.line_nr < p.tokens[p.token_idx+1].line_nr
			{
				p.fgenln('')
			}
			p.token_idx++
		}	
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
	//s := p.scanner.fmt_out.str().replace('\n\n\n', '\n').trim_space()
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

