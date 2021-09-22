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

	prev_tok  token.Token
	tok       token.Token
	peek_tok  token.Token
	skip_next bool
	// Array of Tables state
	last_aot       string
	last_aot_index int
	// Root of the tree
	root &ast.Root = &ast.Root{}
}

// Config is used to configure a Scanner instance.
// Only one of the fields `text` and `file_path` is allowed to be set at time of configuration.
pub struct Config {
pub:
	scanner &scanner.Scanner
}

// new_parser returns a new, stack allocated, `Parser`.
pub fn new_parser(config Config) Parser {
	return Parser{
		config: config
		scanner: config.scanner
	}
}

// init initializes the parser.
pub fn (mut p Parser) init() {
	p.next()
}

// parse starts parsing the input and returns the root
// of the generated AST.
pub fn (mut p Parser) parse() &ast.Root {
	p.init()
	p.root.table = ast.Value(p.root_table())
	return p.root
}

// next forwards the parser to the next token.
fn (mut p Parser) next() {
	p.prev_tok = p.tok
	p.tok = p.peek_tok
	p.peek_tok = p.scanner.scan()
}

// check returns true if the current token's `Kind` is equal that of `expected_token`.
fn (mut p Parser) check(check_token token.Kind) {
	if p.tok.kind == check_token {
		p.next()
	} else {
		panic(@MOD + '.' + @STRUCT + '.' + @FN +
			' expected token "$check_token" but found "$p.tok.kind" in this (excerpt): "...${p.excerpt()}..."')
	}
}

// check_one_of returns true if the current token's `Kind` is equal that of `expected_token`.
fn (mut p Parser) check_one_of(tokens []token.Kind) {
	if p.tok.kind in tokens {
		p.next()
	} else {
		panic(@MOD + '.' + @STRUCT + '.' + @FN +
			' expected one of $tokens but found "$p.tok.kind" in this (excerpt): "...${p.excerpt()}..."')
	}
}

// is_at returns true if the token kind is equal to `expected_token`.
fn (mut p Parser) is_at(expected_token token.Kind) bool {
	return p.tok.kind == expected_token
}

// expect will panic if the token kind is not equal to `expected_token`.
fn (mut p Parser) expect(expected_token token.Kind) {
	if p.tok.kind == expected_token {
		return
	} else {
		panic(@MOD + '.' + @STRUCT + '.' + @FN +
			' expected token "$expected_token" but found "$p.tok.kind" in this text "...${p.excerpt()}..."')
	}
}

// find_in_table returns a reference to a map if found in `table` given a "flat path" key ('aa.bb.cc').
// If some segments of the key does not exist in the input map find_in_table will
// allocate a new map for the segment. This behavior is needed because you can
// reference maps by multiple keys "flat path" (separated by "." periods) in TOML documents.
pub fn (mut p Parser) find_in_table(mut table map[string]ast.Value, key_str string) ?&map[string]ast.Value {
	// NOTE This code is the result of much trial and error.
	// I'm still not quite sure *exactly* why it works. All I can leave here is a hope
	// that this kind of minefield someday will be easier in V :)
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'locating "$key_str" in map ${ptr_str(table)}')
	mut t := &map[string]ast.Value{}
	unsafe {
		t = &table
	}
	mut ks := key_str.split('.')
	unsafe {
		for k in ks {
			if k in t.keys() {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'found key "$k" in $t.keys()')
				if val := t[k] or {
					panic(@MOD + '.' + @STRUCT + '.' + @FN +
						' this should never happen. Key "$k" was checked before access')
				}
				{
					if val is map[string]ast.Value {
						// unsafe {
						t = &(t[k] as map[string]ast.Value)
						//}
					} else {
						return error(@MOD + '.' + @STRUCT + '.' + @FN + ' "$k" is not a map')
					}
				}
			} else {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'no key "$k" found, allocating new map "$k" in map ${ptr_str(t)}"')
				// unsafe {
				t[k] = map[string]ast.Value{}
				t = &(t[k] as map[string]ast.Value)
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'allocated new map ${ptr_str(t)}"')
				//}
			}
		}
	}
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'returning map ${ptr_str(t)}"')
	return t
}

pub fn (mut p Parser) nested_key() string {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing nested key...')
	key := p.key()
	mut text := key.str()
	for p.peek_tok.kind == .period {
		p.next() // .
		p.check(.period)
		next_key := p.key()
		text += '.' + next_key.text
	}
	p.next()
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed nested key `$text` now at "$p.tok.kind" "$p.tok.lit"')
	return text
}

// root_table parses next tokens into a map of `ast.Value`s.
// The V map type is corresponding to a "table" in TOML.
pub fn (mut p Parser) root_table() map[string]ast.Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing root table...')

	mut table := map[string]ast.Value{}

	for p.tok.kind != .eof {
		if !p.skip_next {
			p.next()
		} else {
			p.skip_next = false
		}

		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing token "$p.tok.kind" "$p.tok.lit"')
		match p.tok.kind {
			.hash {
				// TODO table.comments << p.comment()
				c := p.comment()
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping comment "$c.text"')
			}
			//.whitespace, .tab, .nl {
			//	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping "$p.tok.kind "$p.tok.lit"')
			//}
			.bare, .quoted, .boolean, .number, .underscore { // NOTE .boolean allows for use of "true" and "false" as table keys
				if p.peek_tok.kind == .assign
					|| (p.tok.kind == .number && p.peek_tok.kind == .minus) {
					key, val := p.key_value()
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'inserting @1 "$key.str()" = $val.to_json() into ${ptr_str(table)}')
					table[key.str()] = val
				} else if p.peek_tok.kind == .period {

					mut text := p.nested_key()

					p.check(.assign)
					val := p.value()

					//table.value(text)
					mut ks := text.split('.')
					last := ks.last()
					ks.delete_last()

					mut t := p.find_in_table(mut table, ks.join('.')) or { panic(err) }
					//println(@MOD + '.' + @STRUCT + '.' + @FN + ' inserting key $last in ${ptr_str(t)}')
					unsafe {
						util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'inserting @2 "$last" = $val.to_json() into ${ptr_str(t)}')
						t[last] = val
					}
				} else {
					panic(@MOD + '.' + @STRUCT + '.' + @FN + ' dead end at "$p.tok.kind" "$p.tok.lit"')
				}
			}
			.lsbr {
				p.check(.lsbr) // '[' bracket

				if p.tok.kind == .lsbr {
					p.array_of_tables(mut &table)
					p.skip_next = true // skip calling p.next() in coming iteration
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'leaving double bracket at "$p.tok.kind "$p.tok.lit". NEXT is "$p.peek_tok.kind "$p.peek_tok.lit"')
				} else if p.peek_tok.kind == .period {
					mut text := p.nested_key()
					//util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed nested key `$text` now at "$p.tok.kind" "$p.tok.lit"')

					p.expect(.rsbr)

					mut ks := text.split('.')

					/*
					table.exist()
					table.value(text)
					*/

					/*
					///

					last := ks.last()
					ks.delete_last()

					mut t := p.find_in_table(mut table, ks.join('.')) or { panic(err) }
					t[ks.last()] = ast.Value(p.root_table())

					///
					*/


					mut t := p.find_in_table(mut table, text) or { panic(err) }
					p.table(mut t)
					// println(@MOD + '.' + @STRUCT + '.' + @FN + ' inserting into key ${ks.last()} ')
					unsafe {
						val := ast.Value(t)
						util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'inserting @3 "$ks.last()" = $val.to_json() into ${ptr_str(t)}')
						t[ks.last()] = val
					}
				} else {
					key := p.key()
					p.next()
					p.expect(.rsbr)

					///
					/*
					table[key.str()] = ast.Value(p.root_table())
					*/
					///

					mut t := map[string]ast.Value{}
					p.table(mut t)
					val := ast.Value(t)
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'inserting @4 "$key.str()" = $val.to_json() into ${ptr_str(table)}')
					table[key.str()] = val

				}
			}
			.eof {
				return table
			}
			else {
				panic(@MOD + '.' + @STRUCT + '.' + @FN +
					' could not parse $p.tok.kind ("$p.tok.lit") in this (excerpt): "...${p.excerpt()}..." token:\n$p.tok')
			}
		}
	}
	return table
}

// excerpt returns a string of the characters surrounding `Parser.tok.pos`
fn (mut p Parser) excerpt() string {
	return p.scanner.excerpt(p.tok.pos, 10)
}

// table parses next tokens into a map of `ast.Value`s.
// The V map type is corresponding to a "table" in TOML.
pub fn (mut p Parser) table(mut tbl map[string]ast.Value) {
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
			//.whitespace, .tab, .nl {
			//	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping "$p.tok.kind "$p.tok.lit"')
			//}
			.comma {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping comma table value seperator "$p.tok.lit"')
				continue
			}
			.rcbr {
				// p.expect(.rsbr) // ']' bracket
				//$if debug {
				//	flat := arr.str().replace('\n', r'\n')
				//	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed table: $flat . Currently @ token "$p.tok.kind"')
				//}
				return
			}
			.bare, .quoted, .boolean, .number, .underscore {
				if p.peek_tok.kind == .assign {
					key, val := p.key_value()
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'inserting @5 "$key.str()" = $val.to_json() into ${ptr_str(tbl)}')
					tbl[key.str()] = val

				} else if p.peek_tok.kind == .period {
					mut text := p.nested_key()

					p.check(.assign)

					val := p.value()

					mut ks := text.split('.')
					last := ks.last()
					ks.delete_last()

					mut t := p.find_in_table(mut tbl, ks.join('.')) or { panic(err) }
					unsafe {
						util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'inserting @6 "$last" = $val.to_json() into ${ptr_str(t)}')
						t[last] = val
					}
				} else {
					panic(@MOD + '.' + @STRUCT + '.' + @FN + ' dead end at "$p.tok.kind" "$p.tok.lit"')
				}
			}
			.lsbr {

				///

				//tbl[key.str()] = ast.Value(p.root_table())

				///

				p.check(.lsbr) // '[' bracket

				if p.peek_tok.kind == .period {
					mut text := p.nested_key()
					//util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing nested key `$text` now at "$p.tok.kind" "$p.tok.lit"')

					p.expect(.rsbr)

					ks := text.split('.')

					mut t := p.find_in_table(mut tbl, text) or { panic(err) }
					p.table(mut t)
					unsafe {
						val := ast.Value(t)
						util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'inserting @7 "$ks.last()" = $val.to_json() into ${ptr_str(t)}')
						t[ks.last()] = val //ast.Value(t)
					}
				} else {
					key := p.key()
					p.next()
					p.expect(.rsbr)

					///
					val := ast.Value(p.root_table())
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'inserting @8 "$key.str()" = $val.to_json() into ${ptr_str(tbl)}')
					tbl[key.str()] = val
					///

					//mut t := map[string]ast.Value{}
					//p.table(mut t)
					//tbl[key.str()] = ast.Value(t)

				}

			}
			.eof {
				return
			}
			else {
				panic(@MOD + '.' + @STRUCT + '.' + @FN +
					' could not parse $p.tok.kind ("$p.tok.lit") in this (excerpt): "...${p.excerpt()}..." token \n$p.tok')
			}
		}
		if p.peek_tok.kind == .lsbr {
			return
		}
	}
}

// array_of_tables parses next tokens into an array of `ast.Value`s.
pub fn (mut p Parser) array_of_tables(mut table map[string]ast.Value) {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing array of tables "$p.tok.kind" "$p.tok.lit"')
	// NOTE this is starting to get ugly. TOML isn't simple at this point
	p.check(.lsbr) // '[' bracket

	// [[key.key]] horror
	if p.peek_tok.kind == .period {
		p.double_array_of_tables(mut table)
		return
	}

	key := p.key()
	p.next()
	p.check(.rsbr)
	p.check(.rsbr)

	if key.str() in table.keys() {
		if table[key.str()] is []ast.Value {
			unsafe {
				arr := &(table[key.str()] as []ast.Value)
				arr << p.double_bracket_array()
				table[key.str()] = arr
			}
		} else {
			panic(@MOD + '.' + @STRUCT + '.' + @FN +
				' table[$key.str()] is not an array. (excerpt): "...${p.excerpt()}..."')
		}
	} else {
		table[key.str()] = p.double_bracket_array()
	}
	p.last_aot = key.str()
	p.last_aot_index = 0
}

// double_array_of_tables parses next tokens into an array of tables of arrays of `ast.Value`s...
pub fn (mut p Parser) double_array_of_tables(mut table map[string]ast.Value) {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing array of tables of arrays "$p.tok.kind" "$p.tok.lit"')

	key := p.key()
	mut key_str := key.str()
	for p.peek_tok.kind == .period {
		p.next() // .
		p.check(.period)
		next_key := p.key()
		// p.expect(.bare)
		key_str += '.' + next_key.text
	}

	p.next()
	p.check(.rsbr)
	p.check(.rsbr)

	ks := key_str.split('.')

	if ks.len != 2 {
		panic(@MOD + '.' + @STRUCT + '.' + @FN +
			' nested array of tables does not support more than 2 levels. (excerpt): "...${p.excerpt()}..."')
	}

	first := ks[0]
	last := ks[1]

	// NOTE this is starting to get EVEN uglier. TOML is not at all simple at this point...
	if p.last_aot != first {
		table[first] = []ast.Value{}
		p.last_aot = first
		mut t_arr := &(table[p.last_aot] as []ast.Value)
		t_arr << map[string]ast.Value{}
		p.last_aot_index = 0
		//panic(@MOD + '.' + @STRUCT + '.' + @FN +
		//	' last accessed key "$p.last_aot" is not "$first". (excerpt): "...${p.excerpt()}..."')
	}

	mut t_arr := &(table[p.last_aot] as []ast.Value)
	mut t_map := t_arr[p.last_aot_index] // or { panic(@MOD + '.' + @STRUCT + '.' + @FN + ' OUCH' }
	mut t := &(t_map as map[string]ast.Value)

	if last in t.keys() {
		if t[last] is []ast.Value {
			unsafe {
				arr := &(t[last] as []ast.Value)
				arr << p.double_bracket_array()
				t[last] = arr
			}
		} else {
			panic(@MOD + '.' + @STRUCT + '.' + @FN +
				' t[$last] is not an array. (excerpt): "...${p.excerpt()}..."')
		}
	} else {
		t[last] = p.double_bracket_array()
	}
}

// array parses next tokens into an array of `ast.Value`s.
pub fn (mut p Parser) double_bracket_array() []ast.Value {
	mut arr := []ast.Value{}
	for p.tok.kind in [.bare, .quoted, .boolean, .number] && p.peek_tok.kind == .assign {
		mut tbl := map[string]ast.Value{}
		key, val := p.key_value()
		tbl[key.str()] = val
		arr << tbl
		p.next()
	}
	// p.next()
	return arr
}

// array parses next tokens into an array of `ast.Value`s.
pub fn (mut p Parser) array() []ast.Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing array...')
	mut arr := []ast.Value{}
	p.expect(.lsbr) // '[' bracket
	for p.tok.kind != .eof {
		p.next()
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing token "$p.tok.kind" "$p.tok.lit"')
		match p.tok.kind {
			.boolean {
				arr << ast.Value(p.boolean())
			}
			.comma {
				// if p.peek_tok.kind == .lsbr {
				//	p.next()
				//}
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping comma array value seperator "$p.tok.lit"')
				continue
			}
			.eof {
				// End Of File
			}
			.hash {
				// TODO array.comments << p.comment()
				c := p.comment()
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping comment "$c.text"')
			}
			.lcbr {
				mut t := map[string]ast.Value{}
				p.table(mut t)
				// table[key_str] = ast.Value(t)
				ast.Value(t)
			}
			.number {
				val := p.number_or_date()
				arr << val
			}
			.quoted {
				arr << ast.Value(p.quoted())
			}
			.lsbr {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing array in array "$p.tok.lit"')
				arr << ast.Value(p.array())
			}
			.rsbr {
				break
			}
			else {
				panic(@MOD + '.' + @STRUCT + '.' + @FN +
					' could not parse $p.tok.kind ("$p.tok.lit") in this (excerpt): "...${p.excerpt()}..." token \n$p.tok')
			}
		}
	}
	p.expect(.rsbr) // ']' bracket
	$if debug {
		flat := arr.str().replace('\n', r'\n')
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed array: $flat . Currently @ token "$p.tok.kind"')
	}
	return arr
}

// comment returns an `ast.Comment` type.
pub fn (mut p Parser) comment() ast.Comment {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed hash comment "#$p.tok.lit"')
	return ast.Comment{
		text: p.tok.lit
		pos: p.tok.position()
	}
}

// key parses and returns an `ast.Key` type.
// Keys are the token(s) appearing before an assignment operator (=).
pub fn (mut p Parser) key() ast.Key {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing key from "$p.tok.lit" ...')

	mut key := ast.Key(ast.Null{})
	if p.tok.kind == .number {
		if p.peek_tok.kind == .minus {
			mut lits := p.tok.lit
			pos := p.tok.position()
			for p.peek_tok.kind != .assign {
				p.next()
				lits += p.tok.lit
			}
			return ast.Key(ast.Bare{
				text: lits
				pos: pos
			})
		}
		// number := p.number() as ast.Number
		key = ast.Key(p.number())
	} else {
		key = match p.tok.kind {
			.bare, .underscore {
				ast.Key(p.bare())
			}
			.boolean {
				ast.Key(p.boolean())
			}
			.quoted {
				ast.Key(p.quoted())
			}
			else {
				panic(@MOD + '.' + @STRUCT + '.' + @FN +
					' key expected .bare, .number, .quoted or .boolean but got "$p.tok.kind"')
				ast.Key(ast.Bare{}) // TODO workaround bug
			}
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

// key_value parses and returns a pair `ast.Key` and `ast.Value` type.
// see also `key()` and `value()`
pub fn (mut p Parser) key_value() (ast.Key, ast.Value) {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing key value pair...')
	key := p.key()
	p.next()
	p.check(.assign) // Assignment operator
	value := p.value()
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed key value pair. "$key" = ${value.to_json()}')
	return key, value
}

// value parses and returns an `ast.Value` type.
// values are the token(s) appearing after an assignment operator (=).
pub fn (mut p Parser) value() ast.Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing value...')
	// println('parsed comment "${p.tok.lit}"')

	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing token "$p.tok.kind" "$p.tok.lit"')
	// mut value := ast.Value{}
	value := match p.tok.kind {
		.number {
			p.number_or_date()
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
		.lcbr {
			// TODO make table olt for inline tables
			mut t := map[string]ast.Value{}
			p.table(mut t)
			// table[key_str] = ast.Value(t)
			ast.Value(t)
		}
		else {
			panic(@MOD + '.' + @STRUCT + '.' + @FN +
				' value expected .boolean, .quoted, .lsbr, .lcbr or .number got "$p.tok.kind" "$p.tok.lit"')
			ast.Value(ast.Null{}) // TODO workaround bug
		}
	}
	/*
	if value is ast.Null {
		panic(@MOD + '.' + @STRUCT + '.' + @FN + ' expected .quoted value')
	}*/
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed value ${value.to_json()}')
	return value
}

// number_or_date parses and returns an `ast.Value` type as
// one of [`ast.Date`, `ast.Time`, `ast.DateTime`, `ast.Number`]
pub fn (mut p Parser) number_or_date() ast.Value {
	// Handle Date/Time
	if p.peek_tok.kind == .minus || p.peek_tok.kind == .colon {
		date_time_type := p.date_time()
		match date_time_type {
			ast.Date {
				return ast.Value(date_time_type as ast.Date)
			}
			ast.Time {
				return ast.Value(date_time_type as ast.Time)
			}
			ast.DateTime {
				return ast.Value(date_time_type as ast.DateTime)
			}
		}
	}
	return ast.Value(p.number())
}

// bare parses and returns an `ast.Bare` type.
pub fn (mut p Parser) bare() ast.Bare {
	return ast.Bare{
		text: p.tok.lit
		pos: p.tok.position()
	}
}

// quoted parses and returns an `ast.Quoted` type.
pub fn (mut p Parser) quoted() ast.Quoted {
	return ast.Quoted{
		text: p.tok.lit
		pos: p.tok.position()
	}
}

// boolean parses and returns an `ast.Bool` type.
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

// number parses and returns an `ast.Number` type.
pub fn (mut p Parser) number() ast.Number {
	return ast.Number{
		text: p.tok.lit
		pos: p.tok.position()
	}
}

// date_time parses dates and time in RFC 3339 format.
// https://datatracker.ietf.org/doc/html/rfc3339
pub fn (mut p Parser) date_time() ast.DateTimeType {
	// Date and/or Time
	mut lit := ''
	pos := p.tok.position()
	mut date := ast.Date{}
	mut time := ast.Time{}

	if p.peek_tok.kind == .minus {
		date = p.date()
		lit += date.text
		// Look for any THH:MM:SS or <space>HH:MM:SS
		if (p.peek_tok.kind == .bare && (p.peek_tok.lit.starts_with('T')
			|| p.peek_tok.lit.starts_with('t'))) || p.peek_tok.kind == .whitespace {
			p.next() // Advance to token with Txx or whitespace special case
			if p.tok.lit.starts_with('T') || p.tok.lit.starts_with('t') {
				lit += p.tok.lit[0].ascii_str() //'T' or 't'
			} else {
				lit += p.tok.lit
				p.next()
			}
			time = p.time()
			lit += time.text

			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed date-time: "$lit"')
			return ast.DateTime{
				text: lit
				pos: pos
				date: date
				time: time
			}
		}
	} else if p.peek_tok.kind == .colon {
		time = p.time()
		return time
	}

	return ast.Date{
		text: lit
		pos: pos
	}
}

// date parses and returns an `ast.Date` type.
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

	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed date: "$lit"')
	return ast.Date{
		text: lit
		pos: pos
	}
}

// time parses and returns an `ast.Time` type.
pub fn (mut p Parser) time() ast.Time {
	// Time
	mut lit := p.tok.lit
	pos := p.tok.position()

	if p.is_at(.bare) && (lit.starts_with('T') || lit.starts_with('t')) {
		if p.tok.lit.starts_with('T') {
			lit = lit.all_after('T')
		} else if p.tok.lit.starts_with('t') {
			lit = lit.all_after('t')
		}
		p.next()
	} else {
		p.check(.number)
	}
	lit += p.tok.lit
	p.check(.colon)
	lit += p.tok.lit
	p.check(.number)
	lit += p.tok.lit
	// TODO does TOML even have optional seconds?
	// if p.peek_tok.kind == .colon {
	p.check(.colon)
	lit += p.tok.lit
	p.expect(.number)
	//}

	// Optional milliseconds
	if p.peek_tok.kind == .period {
		p.next()
		lit += p.tok.lit // lit += '.'
		p.check(.period)
		lit += p.tok.lit
		p.expect(.number)
	}

	// Parse offset
	if p.peek_tok.kind == .minus || p.peek_tok.kind == .plus {
		p.next()
		lit += p.tok.lit // lit += '-'
		p.check_one_of([.minus, .plus])
		lit += p.tok.lit
		p.check(.number)
		lit += p.tok.lit
		p.check(.colon)
		lit += p.tok.lit
		p.expect(.number)
	} else if p.peek_tok.kind == .bare && (p.peek_tok.lit == 'Z' || p.peek_tok.lit == 'z') {
		p.next()
		lit += p.tok.lit
		p.expect(.bare)
	}

	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed time: "$lit"')
	return ast.Time{
		text: lit
		pos: pos
	}
}

// eof returns an `ast.EOF` type.
pub fn (mut p Parser) eof() ast.EOF {
	return ast.EOF{
		pos: p.tok.position()
	}
}

/*
fn (mut p Parser) table_exists(key string) bool {
	if key == '' {
		return true // root table
	}
	mut t := p.root.table as map[string]ast.Value
	return p.table_exists_r(key, t)
}

fn (mut p Parser) table_exists_r(key string, table map[string]ast.Value) bool {
	ks := key.split('.')
	for i in 0 .. ks.len {
		k := ks[i]
		if k in table.keys() {
			val := table[k] or { ast.Null{} }
			if val is map[string]ast.Value {
				return p.table_exists_r(ks[1..].join('.'), val)
			} else {
				return false
			}
		} else {
			return false
		}
	}
	return true
}
*/
