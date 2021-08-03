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
	p.root.table = ast.Value(map[string]ast.Value{})
}

pub fn (mut p Parser) parse() &ast.Root {
	p.init()
	// p.root.table = ast.Value(p.table(''))
	p.root.table = ast.Value(p.root_table())
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

pub fn (mut p Parser) root_table() map[string]ast.Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing root table...')

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
			.bare, .quoted, .boolean { // NOTE .boolean allows for use of "true" and "false" as table keys
				if p.peek_tok.kind == .assign {
					key, val := p.key_value()
					table[key.str()] = val
				}
			}
			.lsbr {
				key := p.bracket_key()
				key_str := key.str()
				if key_str == '' {
					panic(@MOD + '.' + @STRUCT + '.' + @FN +
						'  could not parse $p.tok.kind ("$p.tok.lit") token \n$p.tok empty keys are not supported')
				}
				ks := key_str.split('.')
				mut t := map[string]ast.Value{}
				if ks.len > 1 { // Has "." dot separators
					// TODO fix dot/nested lookup
					panic(@MOD + '.' + @STRUCT + '.' + @FN +
						' nested keys like "$key_str" is not supported')
					// t = p.find_table(key_str)
				}
				p.table(mut t)
				if ks.len == 1 { // Has "." dot separators
					table[key_str] = ast.Value(t)
				} else {
					panic(@MOD + '.' + @STRUCT + '.' + @FN +
						' could not parse $p.tok.kind ("$p.tok.lit") token \n$p.tok unknown table key "$key_str"')
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
	return table
}

pub fn (mut p Parser) table(mut t map[string]ast.Value) {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing into table...')

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
					t[key.str()] = val
				}
			}
			.bare, .quoted {
				if p.peek_tok.kind == .assign {
					key, val := p.key_value()
					t[key.str()] = val
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
		if p.peek_tok.kind == .lsbr {
			return
		}
	}
	// return table
}

pub fn (mut p Parser) array() []ast.Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing array...')
	mut arr := []ast.Value{}
	p.expect(.lsbr) // '[' bracket
	for p.tok.kind != .eof {
		p.next()
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing token "$p.tok.kind"')
		match p.tok.kind {
			/*.hash {
				// TODO table.comments << p.comment()
				c := p.comment()
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping comment "$c.text"')
			}*/
			.number {
				val := p.number()
				arr << val
			}
			.quoted {
				if p.peek_tok.kind == .assign {
					quoted := p.quoted()
					arr << ast.Value(quoted)
				}
			}
			.comma {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping comma array value seperator "$p.tok.lit"')
				continue
			}
			.rsbr {
				return arr
			}
			else {
				panic(@MOD + '.' + @STRUCT + '.' + @FN +
					' could not parse $p.tok.kind ("$p.tok.lit") token \n$p.tok') //\n$p.prev_tok\n$p.peek_tok\n$p.scanner')
			}
		}
	}
	return arr
}

pub fn (mut p Parser) comment() ast.Comment {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed hash comment "#$p.tok.lit"')
	return ast.Comment{
		text: p.tok.lit
		pos: p.tok.position()
	}
}

pub fn (mut p Parser) bracket_key() ast.Key {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing bracketed key...')

	p.check(.lsbr) // '[' bracket
	key := p.key()
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed key "$p.tok.lit"')
	p.next()
	p.expect(.rsbr) // ']' bracket
	return key

	/*
	util.printdbg(@MOD +'.' + @STRUCT + '.' + @FN, 'parsed key "$p.tok.lit"')
	panic(@MOD + '.' + @STRUCT + '.' + @FN + ' could not parse ${p.tok.kind} ("${p.tok.lit}") token \n$p.tok')
	return ast.Key(ast.Bare{})*/
}

pub fn (mut p Parser) key() ast.Key {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing key...')

	key := match p.tok.kind {
		.bare {
			ast.Key(p.bare())
		}
		.quoted, .boolean {
			ast.Key(p.quoted())
		}
		else {
			panic(@MOD + '.' + @STRUCT + '.' + @FN + ' key expected .bare, .quoted or .boolean')
			ast.Key(ast.Bare{}) // TODO workaround bug
		}
	}

	/*
	NOTE kept for eased debugging
	util.printdbg(@MOD +'.' + @STRUCT + '.' + @FN, 'parsed key "$p.tok.lit"')
	panic(@MOD + '.' + @STRUCT + '.' + @FN + ' could not parse ${p.tok.kind} ("${p.tok.lit}") token \n$p.tok')
	return ast.Key(ast.Bare{})
	*/

	return key
}

pub fn (mut p Parser) key_value() (ast.Key, ast.Value) {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing key value pair...')
	// println('parsed comment "${p.tok.lit}"')

	key := p.key()
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
		.lsbr {
			ast.Value(p.array())
		}
		else {
			panic(@MOD + '.' + @STRUCT + '.' + @FN +
				' value expected .boolean, .quoted, .lsbr or .number got "$p.tok.kind"')
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

pub fn (mut p Parser) quoted() ast.Quoted {
	return ast.Quoted{
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

pub fn (mut p Parser) eof() ast.EOF {
	return ast.EOF{
		pos: p.tok.position()
	}
}

fn (mut p Parser) table_exists(key string) bool {
	if key == '' {
		return true
	}
	mut t := p.root.table as map[string]ast.Value
	ks := key.split('.')
	for i in 0 .. ks.len {
		k := ks[i]
		if k in t {
			if t[k] is map[string]ast.Value {
				continue
			} else {
				return false
			}
		} else {
			return false
		}
	}
	return true
}

fn (mut p Parser) find_table(key string) map[string]ast.Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'locating "$key" ...')
	mut t := p.root.table as map[string]ast.Value
	if key == '' {
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, ' key is blank returning root ...')
		return t
	}
	ks := key.split('.')
	for i in 0 .. ks.len {
		k := ks[i]
		if k in t {
			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'located "$k" ...')
			if t[k] is map[string]ast.Value {
				t = t[k] as map[string]ast.Value
			} else {
				panic(@MOD + '.' + @STRUCT + '.' + @FN + ' "$k" is not a table')
			}
		} else {
			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'allocating new table for "$k" ...')
			t[k] = map[string]ast.Value{}
		}
	}
	return t
}
