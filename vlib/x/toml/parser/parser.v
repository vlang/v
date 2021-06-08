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

pub fn (mut p Parser) parse() &ast.Root {
	mut root := &ast.Root{}
	for p.tok.kind != .eof {
		p.next()
		if p.tok.kind == .hash {
			root.children << p.comment()
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
	//println('parsed comment "${p.tok.lit}"')
	p.next()
	return &ast.Comment{
		text: p.tok.lit
		pos: p.tok.position()
	}
}
