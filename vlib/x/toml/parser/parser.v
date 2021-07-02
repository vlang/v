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

fn (mut p Parser) check(check_token token.Kind) {
	if p.tok.kind == check_token {
		p.next()
	} else {
		panic(@MOD + '.' + @STRUCT + '.' + @FN +
			' expected token "$check_token" but found "$p.tok.kind"')
	}
}

fn (mut p Parser) is_at(expected_token token.Kind) bool {
	return p.tok.kind == expected_token
}

fn (mut p Parser) expect(expected_token token.Kind) {
	if p.tok.kind == expected_token {
		return
	} else {
		panic(@MOD + '.' + @STRUCT + '.' + @FN +
			' expected token "$expected_token" but found "$p.tok.kind"')
	}
}

pub fn (mut p Parser) table() ast.Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing table...')
	mut table := map[string]ast.Value{}
	for p.tok.kind != .eof {
		p.next()
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing token "$p.tok.kind"')
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

	p.check(.lsbr) // '[' bracket
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
	p.expect(.rsbr) // ']' bracket
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
	p.check(.assign) // Assignment operator

	// mut value := ast.Value{}
	value := match p.tok.kind {
		.number {
			p.number()
		}
		.quoted {
			ast.Value(p.quoted())
		}
		.boolean {
			ast.Value(p.boolean())
		}
		else {
			panic(@MOD + '.' + @STRUCT + '.' + @FN +
				' value expected .boolean, .quoted or .number got "$p.tok.kind"')
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

pub fn (mut p Parser) boolean() ast.Bool {
	if p.tok.lit !in ['true', 'false'] {
		panic(@MOD + '.' + @STRUCT + '.' + @FN +
			' expected literal to be either `true` or `false` got "$p.tok.kind"')
	}
	return ast.Bool{
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
	if p.peek_tok.kind == .minus {
		return ast.Value(p.date())
	} else if p.peek_tok.kind == .colon {
		return ast.Value(p.time())
	}
	num := ast.Number{
		text: lit
		pos: pos
	}
	return ast.Value(num)
}

pub fn (mut p Parser) date() ast.Date {
	// Date
	mut lit := p.tok.lit
	pos := p.tok.position()

	p.check(.number)
	lit += p.tok.lit
	p.check(.minus)
	lit += p.tok.lit
	p.check(.number)
	lit += p.tok.lit
	p.check(.minus)
	lit += p.tok.lit
	p.expect(.number)
	// Look for any THH:MM:SS
	if p.peek_tok.kind == .bare && p.peek_tok.lit.starts_with('T') {
		p.next() // Advance to token with Txx
		time := p.time()
		// Parse offset TODO
		if p.peek_tok.kind == .minus {
		}
	}
	// TODO Offset Date-Time
	// TODO Local Date-Time
	return ast.Date{
		text: lit
		pos: pos
	}
}

pub fn (mut p Parser) time() ast.Time {
	// Time
	mut lit := p.tok.lit
	pos := p.tok.position()

	if p.is_at(.bare) && lit.starts_with('T') {
		lit = lit.all_after('T')
		p.next()
	} else {
		p.check(.number)
	}
	lit += p.tok.lit
	p.check(.colon)
	lit += p.tok.lit
	p.check(.number)
	lit += p.tok.lit
	p.check(.colon)
	lit += p.tok.lit
	p.expect(.number)

	// TODO Milliseconds
	return ast.Time{
		text: lit
		pos: pos
	}
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
