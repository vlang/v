// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import x.toml.ast
import x.toml.token
import x.toml.scanner

// Scanner contains the necessary fields for the state of the scan process.
// the task the scanner does is also refered to as "lexing" or "tokenizing".
// The Scanner methods are based on much of the work in `vlib/strings/textscanner`.
pub struct Parser {
pub:
	config Config
mut:
	scanner &scanner.Scanner

	prev_tok token.Token
	tok token.Token
	peek_tok token.Token
}

// Config is used to configure a Scanner instance.
// Only one of the fields `text` and `file_path` is allowed to be set at time of configuration.
pub struct Config {
pub:
	scanner &scanner.Scanner
}

pub fn new_parser(config Config) Parser {
	return Parser{
		config: config
		scanner: config.scanner
	}
}

pub fn (mut p Parser) init() {
	p.tok = p.scanner.scan()
	p.peek_tok = p.scanner.scan()
}

pub fn (mut p Parser) parse() &ast.Root {
	mut root := &ast.Root{}
	mut parent := root
	p.init()
	for p.tok.kind != .eof {
		p.next()
		match p.tok.kind {
			.assign {
				parent.children << p.assign()
			}
			.hash {
				parent.children << p.comment()
			}
			.name {
				parent.children << p.identifier()
			}
			.string {
				parent.children << p.assign()
			}
			.eof {
				parent.children << p.eof()
			}
			else {
				panic(@MOD + '.' + @FN + ' could not parse ${p.tok.kind} ("${p.tok.lit}") token \n$p.tok') //\n$p.prev_tok\n$p.peek_tok\n$p.scanner')
			}
		}

	}
	return root
}

fn (mut p Parser) next() {
	p.prev_tok = p.tok
	p.tok = p.peek_tok
	p.peek_tok = p.scanner.scan()
}

pub fn (mut p Parser) comment() &ast.Comment {
	//println('parsed "${p.tok.lit}"')
	return &ast.Comment{
		text: p.tok.lit
		pos: p.tok.position()
	}
}

pub fn (mut p Parser) identifier() &ast.Identifier {
	//println('parsed comment "${p.tok.lit}"')
	return &ast.Identifier{
		text: p.tok.lit
		pos: p.tok.position()
	}
}

pub fn (mut p Parser) assign() &ast.Assign {
	//println('parsed "${p.tok.lit}"')
	return &ast.Assign {
		text: p.tok.lit
		pos: p.tok.position()
	}
}

pub fn (mut p Parser) sstring() &ast.String {
	//println('parsed "${p.tok.lit}"')
	return &ast.String {
		text: p.tok.lit
		pos: p.tok.position()
	}
}

pub fn (mut p Parser) eof() &ast.EOF {
	//println('parsed "${p.tok.lit}"')
	return &ast.EOF {
		pos: p.tok.position()
	}
}
