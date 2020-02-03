// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module compiler

import strings
import os

[if vfmt]
fn (scanner mut Scanner) fgen(s_ string) {
	mut s := s_
	if s != ' ' {
		//s = s.trim_space()
	}
	if scanner.fmt_line_empty {
		s = strings.repeat(`\t`, scanner.fmt_indent) + s.trim_left(' ')
	}
	scanner.fmt_lines << s
	//scanner.fmt_out << s
	//scanner.fmt_out.write(s)
	scanner.fmt_line_empty = false
}

[if vfmt]
fn (scanner mut Scanner) fgenln(s_ string) {
	mut s := s_.trim_right(' ')
	if scanner.fmt_line_empty && scanner.fmt_indent > 0 {
		s = strings.repeat(`\t`, scanner.fmt_indent) + s
	}
	scanner.fmt_lines << s
	//println('s="$s"')
	//scanner.fmt_lines << '//!'
	scanner.fmt_lines << '\n'
	//scanner.fmt_out.writeln(s)
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
fn (p mut Parser) fspace_or_newline() {
	if p.first_pass() {
		return
	}
	if p.token_idx >= 2 && p.tokens[p.token_idx-1].line_nr !=
		p.tokens[p.token_idx-2].line_nr {
		p.fgen_nl()
	} else {
		p.fgen(' ')
	}
}


[if vfmt]
fn (p mut Parser) fgenln(s string) {
	if p.pass != .main {
		return
	}
	p.scanner.fgenln(s)
}

[if vfmt]
fn (p mut Parser) fgen_nl() {
	if p.pass != .main {
		return
	}

	//println(p.tok)
	// Don't insert a newline after a comment
	/*
	if p.token_idx>0 && p.tokens[p.token_idx-1].tok == .line_comment &&
	p.tokens[p.token_idx].tok != .line_comment {
		p.scanner.fgenln('notin')
		return
	}
	*/

	///if p.token_idx > 0 && p.token_idx < p.tokens.len  &&
	// Previous token is a comment, and NL has already been generated?
	// Don't generate a second NL.
	if p.scanner.fmt_lines.len > 0 && p.scanner.fmt_lines.last() == '\n' &&
		p.token_idx > 2 &&
		p.tokens[p.token_idx-2].tok == .line_comment
	{
		//if p.fileis('parser.v') {
		//println(p.scanner.line_nr.str() + ' '  +p.tokens[p.token_idx-2].str())
		//}
		return
	}

	p.scanner.fgen_nl()
}

[if vfmt]
fn (scanner mut Scanner) fgen_nl() {
	//scanner.fmt_lines << ' fgen_nl'
	//scanner.fmt_lines << '//fgen_nl\n'
	scanner.fmt_lines << '\n'
	//scanner.fmt_out.writeln('')
	scanner.fmt_line_empty = true
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
fn (s mut Scanner) init_fmt() {
	// Right now we can't do `$if vfmt {`, so I'm using
	// a conditional function init_fmt to set this flag.
	// This function will only be called if `-d vfmt` is passed.
	s.is_fmt = true
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
	s := p.strtok()
	if p.tok != .eof {
	p.fgen(s)
	}
	// vfmt: increase indentation on `{` unless it's `{}`
	inc_indent := false
	if p.tok == .lcbr && !p.inside_if_expr  && p.peek() != .rcbr {
		p.fgen_nl()
		p.fmt_inc()
	}
	if p.token_idx >= p.tokens.len { 
		return 
	}
	// Skip comments and add them to vfmt output
	if p.tokens[p.token_idx].tok in [.line_comment, .mline_comment] {
		// Newline before the comment and after consts and closing }
		if p.inside_const {
			//p.fgen_nl()
			//p.fgen_nl()
		}
		//is_rcbr := p.tok == .rcbr
		for p.token_idx < p.tokens.len - 1 {
			i := p.token_idx
			tok := p.tokens[p.token_idx].tok
			if tok != .line_comment && tok != .mline_comment {
				break
			}
			comment_token := p.tokens[i]
			next := p.tokens[i+1]
			comment_on_new_line := i == 0 ||
				comment_token.line_nr > p.tokens[i-1].line_nr
			//prev_token := p.tokens[p.token_idx - 1]
			comment := comment_token.lit
			// Newline before the comment, but not between two // comments,
			// and not right after `{`, there's already a newline there
			if i > 0 && ((p.tokens[i-1].tok != .line_comment &&
				p.tokens[i-1].tok != .lcbr &&
				comment_token.line_nr > p.tokens[i-1].line_nr) ||
				p.tokens[i-1].tok == .hash) { // TODO not sure why this is needed, newline wasn't added after a hash
				p.fgen_nl()
			}
			if i > 0 && p.tokens[i-1].tok == .rcbr && p.scanner.fmt_indent == 0 {
				p.fgen_nl()
			}
			if tok == .line_comment {
				if !comment_on_new_line { //prev_token.line_nr < comment_token.line_nr {
					p.fgen(' ')
				}
				p.fgen('// ' + comment)
				/*
				if false && i > 0 {
				p.fgen(
'pln=${p.tokens[i-1].line_nr} ${comment_token.str()} ' +
'line_nr=$comment_token.line_nr  next=${next.str()}  next_line_nr=$next.line_nr')
}
*/

			}	else {
				// /**/ comment
				p.fgen(comment)
			}
			//if next.tok == .line_comment &&	comment_token.line_nr <	next.line_nr	{
			if comment_token.line_nr <	next.line_nr	{
				//p.fgenln('nextcm')
				p.fgen_nl()
			}
			p.token_idx++
		}

		if inc_indent {
			p.fgen_nl()
		}
	}
}

[if vfmt]
fn (p mut Parser) fremove_last() {
	if p.scanner.fmt_lines.len > 0 {
		p.scanner.fmt_lines[p.scanner.fmt_lines.len-1] = ''
	}
}

[if vfmt]
fn (p &Parser) gen_fmt() {
	if p.pass != .main {
		return
	}
	//println('gen fmt name=$p.file_name path=$p.file_path')
	if p.file_name == '' {
		return
	}
	is_all := p.v.v_fmt_all
	vfmt_file := p.v.v_fmt_file
	if p.file_path != vfmt_file && !is_all {
		// skip everything except the last file (given by the CLI argument)
		return
	}
	//s := p.scanner.fmt_out.str().replace('\n\n\n', '\n').trim_space()
	//s := p.scanner.fmt_out.str().trim_space()
	//p.scanner.fgenln('// nice')
	mut s := p.scanner.fmt_lines.join('')
/*.replace_each([
		'\n\n\n\n', '\n\n',
		' \n', '\n',
		') or{', ') or {',
	])
	*/
	//.replace('\n\n\n\n', '\n\n')

	s = s.replace(' \n', '\n')
	s = s.replace(')  or {', ') or {')
	s = s.replace(') or{', ') or {')
	s = s.replace(')or{', ') or {')
	s = s.replace('or{', 'or {')
	s = s.replace('}}\n', '}\n\t}\n')

	if s == '' {
		return
	}
	//files := ['get_type.v']
	if p.file_path.contains('compiler/vfmt.v') {return}
	//if !(p.file_name in files) { return }
	if is_all {
		if p.file_path.len > 0 {
			path := write_formatted_source( p.file_name, s )
			os.cp( path, p.file_path ) or { panic(err) }
			eprintln('Written fmt file to: $p.file_path')
		}
	}
	if p.file_path == vfmt_file {
		res_path := write_formatted_source( p.file_name, s )
		mut vv := p.v
		vv.v_fmt_file_result = res_path
	}
}

fn write_formatted_source(file_name string, s string) string {
	path := os.tmpdir() + '/' + file_name
	mut out := os.create(path) or {
		verror('failed to create file $path')
		return ''
	}
	//eprintln('replacing ${p.file_path} ...\n')
	out.writeln(s.trim_space())//p.scanner.fmt_out.str().trim_space())
	out.close()
	return path
}
