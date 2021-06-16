// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import x.toml.ast
import x.toml.util
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
	tok      token.Token
	peek_tok token.Token

	root &ast.Root = &ast.Root{}
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
	p.next()
}

pub fn (mut p Parser) parse() &ast.Root {
	p.init()
	p.root.table = p.table()
	return p.root
}

fn (mut p Parser) next() {
	p.prev_tok = p.tok
	p.tok = p.peek_tok
	p.peek_tok = p.scanner.scan()
}

fn (mut p Parser) expect(expected_token token.Kind) {
	if p.tok.kind == expected_token {
		p.next()
	} else {
		panic(@MOD + '.' + @STRUCT + '.' + @FN +
			' expected token "$expected_token" but found "$p.peek_tok.kind"')
	}
}

pub fn (mut p Parser) table() ast.Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing table...')
	mut table := map[string]ast.Value{}
	for p.tok.kind != .eof {
		p.next()
		match p.tok.kind {
			.hash {
				// TODO table.comments << p.comment()
				c := p.comment()
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping comment "$c.text"')
			}
			.number {
				if p.peek_tok.kind == .assign {
					key, val := p.key_value()
					table[key.str()] = val
				}
			}
			.bare, .quoted {
				if p.peek_tok.kind == .assign {
					key, val := p.key_value()
					table[key.str()] = val
				}
			}
			.lsbr {
				if p.peek_tok.kind == .lsbr {
					// p.array()
				} else {
					key := p.key()
					table[key.str()] = p.table()
				}
			}
			.eof {
				// parent.children << p.eof()
			}
			else {
				panic(@MOD + '.' + @STRUCT + '.' + @FN +
					' could not parse $p.tok.kind ("$p.tok.lit") token \n$p.tok') //\n$p.prev_tok\n$p.peek_tok\n$p.scanner')
			}
		}
	}
	return ast.Value(table)
}

pub fn (mut p Parser) comment() ast.Comment {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed hash comment "#$p.tok.lit"')
	return ast.Comment{
		text: p.tok.lit
		pos: p.tok.position()
	}
}

pub fn (mut p Parser) key() ast.Key {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing key...')

	p.expect(.lsbr) // '[' bracket
	key := match p.tok.kind {
		.bare {
			bare := p.bare()
			ast.Key(bare)
		}
		.quoted {
			quoted := p.quoted()
			ast.Key(quoted)
		}
		else {
			panic(@MOD + '.' + @STRUCT + '.' + @FN + ' key expected .bare or .quoted')
			ast.Key(ast.Bare{}) // TODO workaround bug
		}
	}
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed key "$p.tok.lit"')
	p.next()
	// p.expect(.rsbr) // ']' bracket
	return key

	/*
	util.printdbg(@MOD +'.' + @STRUCT + '.' + @FN, 'parsed key "$p.tok.lit"')
	panic(@MOD + '.' + @STRUCT + '.' + @FN + ' could not parse ${p.tok.kind} ("${p.tok.lit}") token \n$p.tok')
	return ast.Key(ast.Bare{})*/
}

pub fn (mut p Parser) key_value() (ast.Key, ast.Value) {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing key value pair...')
	// println('parsed comment "${p.tok.lit}"')
	// mut key := ast.Key{}

	key := match p.tok.kind {
		.bare, .number {
			ast.Key(p.bare())
		}
		.quoted {
			ast.Key(p.quoted())
		}
		else {
			panic(@MOD + '.' + @STRUCT + '.' + @FN +
				' key expected .bare or .quoted got "$p.tok.kind"')
			ast.Key(ast.Bare{}) // TODO workaround bug
		}
	}
	p.next()
	p.expect(.assign) // Assignment operator

	// mut value := ast.Value{}
	value := match p.tok.kind {
		.number {
			p.number()
		}
		.quoted {
			ast.Value(p.quoted())
		}
		else {
			panic(@MOD + '.' + @STRUCT + '.' + @FN + ' value expected .quoted got "$p.tok.kind"')
			ast.Value(ast.Quoted{}) // TODO workaround bug
		}
	}
	/*
	if value is ast.Err {
		panic(@MOD + '.' + @STRUCT + '.' + @FN + ' expected .quoted value')
	}*/
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed key value pair. "$key" = "$value"')
	return key, value
}

pub fn (mut p Parser) bare() ast.Bare {
	return ast.Bare{
		text: p.tok.lit
		pos: p.tok.position()
	}
}

/*
pub fn (mut p Parser) assign() ast.Assign {
	return &ast.Assign {
		text: p.tok.lit
		pos: p.tok.position()
	}
}
*/

pub fn (mut p Parser) number() ast.Value {
	// Date/Time
	mut lit := p.tok.lit
	pos := p.tok.position()
	p.expect(.number)
	if p.tok.kind == .minus {
		lit += p.tok.lit
		p.expect(.minus)
		lit += p.tok.lit
		p.expect(.number)
		lit += p.tok.lit
		p.expect(.minus)
		lit += p.tok.lit
		p.expect(.number)
		// TODO Offset Date-Time
		// TODO Local Date-Time
		date := ast.Date{
			text: lit
			pos: pos
		}
		return ast.Value(date)
	} else if p.tok.kind == .colon {
		p.expect(.colon)
		p.expect(.number)
		p.expect(.colon)
		p.expect(.number)
		// TODO Milliseconds
		time := ast.Time{
			text: lit
			pos: pos
		}
		return ast.Value(time)
	}
	num := ast.Number{
		text: lit
		pos: pos
	}
	return ast.Value(num)
}

pub fn (mut p Parser) quoted() ast.Quoted {
	return ast.Quoted{
		text: p.tok.lit
		pos: p.tok.position()
	}
}

pub fn (mut p Parser) eof() ast.EOF {
	return ast.EOF{
		pos: p.tok.position()
	}
}
