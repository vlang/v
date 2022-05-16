// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import toml.ast
import toml.checker
import toml.decoder
import toml.util
import toml.token
import toml.scanner

pub const (
	all_formatting            = [token.Kind.whitespace, .tab, .cr, .nl]
	space_formatting          = [token.Kind.whitespace, .tab]
	keys_and_space_formatting = [token.Kind.whitespace, .tab, .minus, .bare, .quoted, .boolean,
		.number, .underscore]
)

type DottedKey = []string

pub fn (dk DottedKey) str() string {
	return dk.join('.')
}

// starts_with returns true if the dotted key starts with the same key entries as `target`.
fn (dk DottedKey) starts_with(target DottedKey) bool {
	if dk.len >= target.len {
		for i := 0; i < target.len; i++ {
			if dk[i] != target[i] {
				return false
			}
		}
		return true
	}
	return false
}

// has returns true if the array contains `target`.
fn (a []DottedKey) has(target DottedKey) bool {
	for dk in a {
		if dk == target {
			return true
		}
	}
	return false
}

// Parser contains the necessary fields for keeping the state of the parsing process.
pub struct Parser {
pub:
	config Config
mut:
	scanner   &scanner.Scanner
	prev_tok  token.Token
	tok       token.Token
	peek_tok  token.Token
	tokens    []token.Token // To be able to peek more than one token ahead.
	skip_next bool
	// The root map (map is called table in TOML world)
	root_map                          map[string]ast.Value
	root_map_key                      DottedKey
	explicit_declared                 []DottedKey
	explicit_declared_array_of_tables []DottedKey
	implicit_declared                 []DottedKey
	// Array of Tables state
	last_aot       DottedKey
	last_aot_index int
	// Root of the tree
	ast_root &ast.Root = &ast.Root{}
}

// Config is used to configure a Parser instance.
// `run_checks` is used to en- or disable running of the strict `checker.Checker` type checks.
// `decode_values` is used to en- or disable decoding of values with the `decoder.Decoder`.
pub struct Config {
pub:
	scanner       &scanner.Scanner
	run_checks    bool = true
	decode_values bool = true
}

// new_parser returns a new, stack allocated, `Parser`.
pub fn new_parser(config Config) Parser {
	return Parser{
		config: config
		scanner: config.scanner
	}
}

// init initializes the parser.
pub fn (mut p Parser) init() ? {
	p.root_map = map[string]ast.Value{}
	p.tokens << p.scanner.scan()?
	p.next()?
}

// run_checker validates the parsed `ast.Value` nodes in the
// the generated AST.
fn (mut p Parser) run_checker() ? {
	if p.config.run_checks {
		chckr := checker.Checker{
			scanner: p.scanner
		}
		chckr.check(p.root_map)?
		for comment in p.ast_root.comments {
			chckr.check_comment(comment)?
		}
	}
}

// run_decoder decodes values in the parsed `ast.Value` nodes in the
// the generated AST.
fn (mut p Parser) run_decoder() ? {
	if p.config.decode_values {
		dcoder := decoder.Decoder{
			scanner: p.scanner
		}
		dcoder.decode(mut p.root_map)?
	}
}

// parse starts parsing the input and returns the root
// of the generated AST.
pub fn (mut p Parser) parse() ?&ast.Root {
	p.init()?
	p.root_table()?
	p.run_checker()?
	p.run_decoder()?
	p.ast_root.table = p.root_map
	return p.ast_root
}

// next forwards the parser to the next token.
fn (mut p Parser) next() ? {
	p.prev_tok = p.tok
	p.tok = p.peek_tok
	if p.tokens.len > 0 {
		p.peek_tok = p.tokens.first()
		p.tokens.delete(0)
		p.peek(1)?
	} else {
		p.peek(1)?
		p.peek_tok = p.tokens.first()
		p.tokens.delete(0)
	}
}

// peek peeks forward `n` tokens.
// peek returns `.unknown` if it can not peek ahead long enough.
fn (mut p Parser) peek(n int) ?token.Token {
	if n < 0 {
		return error(@MOD + '.' + @STRUCT + '.' + @FN + ' peeking backwards is not supported.')
	}
	if n == 0 {
		return p.peek_tok
	} else {
		// n >= 1
		if n <= p.tokens.len {
			return p.tokens[n - 1]
		} else {
			mut token := token.Token{}
			mut count := n - p.tokens.len
			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'buffering $count tokens...')
			for token.kind != .eof && count != 0 {
				token = p.scanner.scan()?
				p.tokens << token
				count--
			}
			return token
		}
	}
}

// check forwards the parser to the next token if the current
// token's `Kind` is equal that of `check_token`.
fn (mut p Parser) check(check_token token.Kind) ? {
	if p.tok.kind == check_token {
		p.next()?
	} else {
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' expected token "$check_token" but found "$p.tok.kind" in this (excerpt): "...${p.excerpt()}..."')
	}
}

// peek_for_correct_line_ending_or_fail peeks past any formatting tokens
// and return an error if the next token is not one of [.cr, .nl, .hash, .eof].
fn (mut p Parser) peek_for_correct_line_ending_or_fail() ? {
	// Disallow anything else than [.cr, .nl, .hash, .eof] after any space formatting.
	peek_tok, _ := p.peek_over(1, parser.space_formatting)?
	if peek_tok.kind !in [.cr, .nl, .hash, .eof] {
		p.next()? // Forward to the peek_tok
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' unexpected EOL "$p.tok.kind" "$p.tok.lit" expected one of [.cr, .nl, .hash, .eof] at this (excerpt): "...${p.excerpt()}..."')
	}
}

// check_one_of forwards the parser to the next token if the current
// token's `Kind` can be found in `tokens`. Otherwise it returns an error.
fn (mut p Parser) check_one_of(tokens []token.Kind) ? {
	if p.tok.kind in tokens {
		p.next()?
	} else {
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' expected one of $tokens but found "$p.tok.kind" in this (excerpt): "...${p.excerpt()}..."')
	}
}

// ignore_while forwards the parser to the next token as long as the current
// token's `Kind` can be found in `tokens`. This is helpful for ignoring
// a stream of formatting tokens.
fn (mut p Parser) ignore_while(tokens []token.Kind) {
	if p.tok.kind in tokens {
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'ignoring "$p.tok.kind" ...')
		p.next() or { return }
		p.ignore_while(tokens)
	}
}

// ignore_while_peek forwards the parser to the next token as long as `peek_tok`
// token's `Kind` can be found in `tokens`. This is helpful for ignoring
// a stream of formatting tokens.
// In contrast to `ignore_while`, `ignore_while_peek` compares on `peek_tok` this is
// sometimes necessary since not all parser calls forward using the `next()` call.
fn (mut p Parser) ignore_while_peek(tokens []token.Kind) {
	for p.peek_tok.kind in tokens {
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'eating "$p.tok.kind" ...')
		p.next() or { return }
	}
}

// peek_over peeks ahead from token starting at `i` skipping over
// any `token.Kind`s found in `tokens`. `peek_over` returns the next token *not*
// found in `tokens`.
fn (mut p Parser) peek_over(i int, tokens []token.Kind) ?(token.Token, int) {
	mut peek_tok := p.peek_tok

	// Peek ahead as far as we can from token at `i` while the peeked
	// token is found in `tokens`.
	mut peek_i := i
	for peek_tok.kind in tokens {
		peek_tok = p.peek(peek_i)?
		peek_i++
	}
	return peek_tok, peek_i
}

// is_at returns true if the token kind is equal to `expected_token`.
fn (mut p Parser) is_at(expected_token token.Kind) bool {
	return p.tok.kind == expected_token
}

// expect will error if the token kind is not equal to `expected_token`.
fn (mut p Parser) expect(expected_token token.Kind) ? {
	if p.tok.kind == expected_token {
		return
	} else {
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' expected token "$expected_token" but found "$p.tok.kind" in this text "...${p.excerpt()}..."')
	}
}

// build_abs_dotted_key returns the absolute dotted key path.
fn (p Parser) build_abs_dotted_key(key DottedKey) DottedKey {
	if p.root_map_key.len > 0 {
		mut abs_dotted_key := DottedKey([]string{})
		abs_dotted_key << p.root_map_key
		abs_dotted_key << key
		return abs_dotted_key
	}
	return key
}

// todo_msvc_astring2dkey worksaround a MSVC compile error.
// TODO remove.
fn todo_msvc_astring2dkey(s []string) DottedKey {
	return s
}

// check_explicitly_declared returns an error if `key` has been explicitly declared.
fn (p Parser) check_explicitly_declared(key DottedKey) ? {
	if p.explicit_declared.len > 0 && p.explicit_declared.has(key) {
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' key `$key.str()` is already explicitly declared. Unexpected redeclaration at "$p.tok.kind" "$p.tok.lit" in this (excerpt): "...${p.excerpt()}..."')
	}
}

// check_explicitly_declared_array_of_tables returns an error if `key` has been
// explicitly declared as an array of tables.
fn (p Parser) check_explicitly_declared_array_of_tables(key DottedKey) ? {
	if p.explicit_declared_array_of_tables.len > 0 && p.explicit_declared_array_of_tables.has(key) {
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' key `$key.str()` is already an explicitly declared array of tables. Unexpected redeclaration at "$p.tok.kind" "$p.tok.lit" in this (excerpt): "...${p.excerpt()}..."')
	}
}

// check_implicitly_declared returns an error if `key` has been implicitly declared.
fn (p Parser) check_implicitly_declared(key DottedKey) ? {
	if p.implicit_declared.len > 0 && p.implicit_declared.has(key) {
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' key `$key.str()` is already implicitly declared. Unexpected redeclaration at "$p.tok.kind" "$p.tok.lit" in this (excerpt): "...${p.excerpt()}..."')
	}
}

// find_table returns a reference to a map if found in the *root* table given a "dotted" key (`a.b.c`).
// If some segments of the key does not exist in the root table find_table will
// allocate a new map for each segment. This behavior is needed because you can
// reference maps by multiple keys "dotted" (separated by "." periods) in TOML documents.
// See also `find_in_table`.
pub fn (mut p Parser) find_table() ?&map[string]ast.Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'locating "$p.root_map_key" in map ${ptr_str(p.root_map)}')
	mut t := unsafe { &p.root_map }
	if p.root_map_key.len == 0 {
		return t
	}

	return p.find_in_table(mut t, p.root_map_key)
}

// allocate_table allocates all tables in "dotted" `key` (`a.b.c`) in the *root* table.
pub fn (mut p Parser) allocate_table(key DottedKey) ? {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'allocating "$key" in map ${ptr_str(p.root_map)}')
	mut t := unsafe { &p.root_map }
	if key.len == 0 {
		return
	}
	p.allocate_in_table(mut t, key)?
}

// sub_table_key returns the logic parts of a dotted key (`a.b.c`) for
// use with the `find_sub_table` method.
pub fn (mut p Parser) sub_table_key(key DottedKey) (DottedKey, DottedKey) {
	last := [key.last()]
	first := key[..key.len - 1]
	return first, last
}

// find_sub_table returns a reference to a map if found in the *root* table given a "dotted" key (`a.b.c`).
// If some segments of the key does not exist in the input map find_sub_table will
// allocate a new map for the segment. This behavior is needed because you can
// reference maps by multiple keys "dotted" (separated by "." periods) in TOML documents.
// See also `find_in_table`.
pub fn (mut p Parser) find_sub_table(key DottedKey) ?&map[string]ast.Value {
	mut ky := DottedKey([]string{})
	ky << p.root_map_key
	ky << key
	if p.root_map_key.len == 0 {
		ky = key
	}
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'locating "$ky" in map ${ptr_str(p.root_map)}')
	mut t := unsafe { &p.root_map }
	if ky.len == 0 {
		return t
	}

	return p.find_in_table(mut t, ky)
}

// find_in_table returns a reference to a map if found in `table` given a "dotted" key (`a.b.c`).
// If some segments of the key does not exist in the input map find_in_table will
// allocate a new map for the segment. This behavior is needed because you can
// reference maps by multiple keys "dotted" (separated by "." periods) in TOML documents.
pub fn (mut p Parser) find_in_table(mut table map[string]ast.Value, key DottedKey) ?&map[string]ast.Value {
	// NOTE This code is the result of much trial and error.
	// I'm still not quite sure *exactly* why it works. All I can leave here is a hope
	// that this kind of minefield someday will be easier in V :)
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'locating "$key" in map ${ptr_str(table)}')
	mut t := unsafe { &table }
	unsafe {
		for k in key {
			if val := t[k] {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'found key "$k" in $t.keys()')
				if val is map[string]ast.Value {
					t = &(val as map[string]ast.Value)
				} else {
					return error(@MOD + '.' + @STRUCT + '.' + @FN +
						' "$k" in "$key" is not a map but `$val.type_name()`')
				}
			} else {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'no key "$k" in "$key" found, allocating new map at key "$k" in map ${ptr_str(t)}"')
				t[k] = map[string]ast.Value{}
				t = &(t[k] as map[string]ast.Value)
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'allocated new map ${ptr_str(t)}"')
			}
		}
	}
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'returning map ${ptr_str(t)}"')
	return t
}

// find_array_of_tables returns an array if found in the root table based on the parser's
// last encountered "Array Of Tables" key.
// If the state key does not exist find_array_in_table will return an error.
pub fn (mut p Parser) find_array_of_tables() ?[]ast.Value {
	mut t := unsafe { &p.root_map }
	mut key := p.last_aot
	if key.len > 1 {
		key = DottedKey([key[0]])
	}
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'locating "$key" in map ${ptr_str(t)}')
	unsafe {
		if val := t[key.str()] {
			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'found key "$key" in $t.keys()')
			if val is []ast.Value {
				arr := (val as []ast.Value)
				return arr
			}
		}
	}
	return error(@MOD + '.' + @STRUCT + '.' + @FN + 'no key `$key` found in map ${ptr_str(t)}"')
}

// allocate_in_table allocates all tables in "dotted" `key` (`a.b.c`) in `table`.
pub fn (mut p Parser) allocate_in_table(mut table map[string]ast.Value, key DottedKey) ? {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'allocating "$key" in map ${ptr_str(table)}')
	mut t := unsafe { &table }
	unsafe {
		for k in key {
			if val := t[k] {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'found key "$k" in $t.keys()')
				if val is map[string]ast.Value {
					t = &(val as map[string]ast.Value)
				} else {
					return error(@MOD + '.' + @STRUCT + '.' + @FN +
						' "$k" in "$key" is not a map ($val.type_name())')
				}
			} else {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'no key "$k" in "$key" found, allocating new map at key "$k" in map ${ptr_str(t)}"')
				t[k] = map[string]ast.Value{}
				t = &(t[k] as map[string]ast.Value)
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'allocated new map ${ptr_str(t)}"')
			}
		}
	}
}

// dotted_key returns a string of the next tokens parsed as
// sub/nested/path keys (e.g. `a.b.c`). In TOML, this form of key is referred to as a "dotted" key.
pub fn (mut p Parser) dotted_key() ?DottedKey {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing dotted key...')
	mut dotted_key := DottedKey([]string{})
	key := p.key()?
	p.ignore_while_peek(parser.space_formatting)
	dotted_key << key.str()
	for p.peek_tok.kind == .period {
		p.next()? // .
		p.check(.period)?
		p.ignore_while(parser.space_formatting)
		next_key := p.key()?
		dotted_key << next_key.text
		p.ignore_while_peek(parser.space_formatting)
	}
	p.next()?
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed dotted key `$dotted_key` now at "$p.tok.kind" "$p.tok.lit"')
	return dotted_key
}

// root_table parses next tokens into the root map of `ast.Value`s.
// The V `map` type is corresponding to a "table" in TOML.
pub fn (mut p Parser) root_table() ? {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing root table...')

	for p.tok.kind != .eof {
		if !p.skip_next {
			p.next()?
		} else {
			p.skip_next = false
		}

		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing token "$p.tok.kind" "$p.tok.lit"')
		match p.tok.kind {
			.hash {
				c := p.comment()
				p.ast_root.comments << c
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping comment "$c.text"')
			}
			.whitespace, .tab, .nl, .cr {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping formatting "$p.tok.kind" "$p.tok.lit"')
				continue
			}
			.bare, .quoted, .number, .minus, .underscore {
				// Peek forward as far as we can skipping over space formatting tokens.
				peek_tok, _ := p.peek_over(1, parser.keys_and_space_formatting)?

				if peek_tok.kind == .period {
					dotted_key, val := p.dotted_key_value()?

					sub_table, key := p.sub_table_key(dotted_key)

					// NOTE these are *relatively* costly checks. In general - and by specification,
					// TOML documents are expected to be "small" so this shouldn't be a problem. Famous last words.
					for explicit_key in p.explicit_declared {
						// Check for key re-defining:
						// https://github.com/iarna/toml-spec-tests/blob/1880b1a/errors/inline-table-imutable-1.toml

						if p.build_abs_dotted_key(sub_table) == explicit_key {
							return error(@MOD + '.' + @STRUCT + '.' + @FN +
								' key `$sub_table` has already been explicitly declared. Unexpected redeclaration at "$p.tok.kind" "$p.tok.lit" in this (excerpt): "...${p.excerpt()}..."')
						}
						if explicit_key.len == 1 || explicit_key == p.root_map_key {
							continue
						}
						// Check for "table injection":
						// https://github.com/BurntSushi/toml-test/blob/576db85/tests/invalid/table/injection-1.toml
						// https://github.com/BurntSushi/toml-test/blob/576db85/tests/invalid/table/injection-2.toml
						if p.build_abs_dotted_key(sub_table).starts_with(explicit_key) {
							return error(@MOD + '.' + @STRUCT + '.' + @FN +
								' key `$dotted_key` has already been explicitly declared. Unexpected redeclaration at "$p.tok.kind" "$p.tok.lit" in this (excerpt): "...${p.excerpt()}..."')
						}
					}

					// Register implicit declaration
					mut dotted_key_copy := dotted_key.clone()
					dotted_key_copy.pop()
					implicit_keys := todo_msvc_astring2dkey(dotted_key_copy)
					mut abs_dotted_key := p.build_abs_dotted_key(implicit_keys)
					if !p.implicit_declared.has(abs_dotted_key) {
						p.implicit_declared << abs_dotted_key
					}

					t := p.find_sub_table(sub_table)?
					unsafe {
						util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'setting "$key" = $val in table ${ptr_str(t)}')
						t[key.str()] = val
					}
				} else {
					p.ignore_while(parser.space_formatting)
					key, val := p.key_value()?

					t := p.find_table()?
					unsafe {
						util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'setting "$key.str()" = $val in table ${ptr_str(t)}')
						key_str := key.str()
						if _ := t[key_str] {
							return error(@MOD + '.' + @STRUCT + '.' + @FN +
								' key "$key" is already initialized with a value. At "$p.tok.kind" "$p.tok.lit" in this (excerpt): "...${p.excerpt()}..."')
						}
						t[key_str] = val
					}
				}
				p.peek_for_correct_line_ending_or_fail()?
			}
			.lsbr {
				p.check(.lsbr)? // '[' bracket
				mut peek_tok := p.peek_tok

				// Disallow `[ [table]]`
				if p.tok.kind in parser.space_formatting {
					peek_tok, _ = p.peek_over(1, parser.space_formatting)?
					if peek_tok.kind == .lsbr {
						return error(@MOD + '.' + @STRUCT + '.' + @FN +
							' unexpected "$p.tok.kind" "$p.tok.lit" at this (excerpt): "...${p.excerpt()}..."')
					}
				}

				// Allow `[ d.e.f]`
				p.ignore_while(parser.space_formatting)

				// Peek forward as far as we can skipping over space formatting tokens.
				peek_tok, _ = p.peek_over(1, parser.keys_and_space_formatting)?

				if p.tok.kind == .lsbr {
					// Parse `[[table]]`
					p.array_of_tables(mut &p.root_map)?
					p.skip_next = true // skip calling p.next() in coming iteration
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'leaving double bracket at "$p.tok.kind" "$p.tok.lit". NEXT is "$p.peek_tok.kind "$p.peek_tok.lit"')
				} else if peek_tok.kind == .period {
					// Parse `[d.e.f]`
					dotted_key := p.dotted_key()?

					// So apparently TOML is a *very* key context sensitive language...
					// [[table]]   <- parsed previously
					//   ...
					// [table.key] <- parser is here
					//
					// `table.key` now shape shifts into being a *double array of tables* key...
					// ... but with a different set of rules - making it hard to reuse the code we already have for that ...
					// See `testdata/array_of_tables_edge_case_<N>_test.toml` for the type of constructs parsed.
					if p.last_aot.len == 1 && dotted_key.len > 1
						&& dotted_key[0] == p.last_aot.str() {
						// Disallow re-declaring the key
						p.check_explicitly_declared_array_of_tables(dotted_key)?
						p.check(.rsbr)?
						p.ignore_while(parser.space_formatting)
						arr := p.find_array_of_tables()?
						if val := arr[p.last_aot_index] {
							if val is map[string]ast.Value {
								mut m := map[string]ast.Value{}
								p.table_contents(mut m)?
								unsafe {
									mut mut_val := &val
									if dotted_key.len == 2 {
										// [table.key]
										mut_val[dotted_key[1].str()] = m
									} else {
										// [table.key.key.etc]
										mut dotted_key_copy := dotted_key.clone()
										dotted_key_copy.delete(0)
										new_key := todo_msvc_astring2dkey(dotted_key_copy)
										sub_table, key := p.sub_table_key(new_key)
										t := p.find_in_table(mut mut_val, sub_table)?
										util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN,
											'setting "$key" = $val in table ${ptr_str(t)}')
										t[new_key.last().str()] = m
									}
								}
							} else {
								return error(@MOD + '.' + @STRUCT + '.' + @FN +
									' "$p.last_aot_index" in array is not a map but `${typeof(val).name}`')
							}
						}
						continue
					}

					// Disallow re-declaring the key
					p.check_explicitly_declared(dotted_key)?
					p.explicit_declared << dotted_key
					// ... also check implicitly declared keys
					p.check_implicitly_declared(dotted_key)?

					p.ignore_while(parser.space_formatting)

					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'setting root map key to `$dotted_key` at "$p.tok.kind" "$p.tok.lit"')
					p.root_map_key = dotted_key
					p.allocate_table(p.root_map_key)?
					p.expect(.rsbr)?
					p.peek_for_correct_line_ending_or_fail()?
				} else {
					// Parse `[key]`
					key := p.key()?
					dotted_key := DottedKey([key.str()])

					// Disallow re-declaring the key
					p.check_explicitly_declared(dotted_key)?
					p.explicit_declared << dotted_key

					// Check for footgun redeclaration in this odd way:
					// [[tbl]]
					// [tbl]
					if p.last_aot == dotted_key {
						return error(@MOD + '.' + @STRUCT + '.' + @FN +
							' key `$dotted_key` has already been explicitly declared. Unexpected redeclaration at "$p.tok.kind" "$p.tok.lit" in this (excerpt): "...${p.excerpt()}..."')
					}

					// Allow [ key ]
					p.ignore_while(parser.space_formatting)

					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'setting root map key to `$dotted_key` at "$p.tok.kind" "$p.tok.lit"')
					p.root_map_key = dotted_key
					p.allocate_table(p.root_map_key)?
					p.next()?
					p.expect(.rsbr)?
					p.peek_for_correct_line_ending_or_fail()?
				}
			}
			.eof {
				return
			}
			else {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' could not parse "$p.tok.kind" "$p.tok.lit" in this (excerpt): "...${p.excerpt()}..."')
			}
		}
	}
}

// excerpt returns a string of the characters surrounding `Parser.tok.pos`
fn (p Parser) excerpt() string {
	return p.scanner.excerpt(p.tok.pos, 10)
}

// table_contents parses next tokens into a map of `ast.Value`s.
// The V `map` type is corresponding to a "table" in TOML.
pub fn (mut p Parser) table_contents(mut tbl map[string]ast.Value) ? {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing table contents...')

	for p.tok.kind != .eof {
		if p.peek_tok.kind == .lsbr {
			return
		}
		if !p.skip_next {
			p.next()?
		} else {
			p.skip_next = false
		}

		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing token "$p.tok.kind" "$p.tok.lit"')
		match p.tok.kind {
			.hash {
				c := p.comment()
				p.ast_root.comments << c
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping comment "$c.text"')
			}
			.whitespace, .tab, .nl, .cr {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping formatting "$p.tok.kind" "$p.tok.lit"')
				continue
			}
			.bare, .quoted, .number, .minus, .underscore {
				// Peek forward as far as we can skipping over space formatting tokens.
				peek_tok, _ := p.peek_over(1, parser.keys_and_space_formatting)?

				if peek_tok.kind == .period {
					dotted_key, val := p.dotted_key_value()?

					sub_table, key := p.sub_table_key(dotted_key)

					t := p.find_in_table(mut tbl, sub_table)?
					unsafe {
						util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'setting "$key" = $val in table ${ptr_str(t)}')
						t[key.str()] = val
					}
				} else {
					p.ignore_while(parser.space_formatting)
					key, val := p.key_value()?

					unsafe {
						util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'setting "$key.str()" = $val in table ${ptr_str(tbl)}')
						key_str := key.str()
						if _ := tbl[key_str] {
							return error(@MOD + '.' + @STRUCT + '.' + @FN +
								' key "$key" is already initialized with a value. At "$p.tok.kind" "$p.tok.lit" in this (excerpt): "...${p.excerpt()}..."')
						}
						tbl[key_str] = val
					}
				}
				p.peek_for_correct_line_ending_or_fail()?
			}
			.eof {
				break
			}
			else {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' could not parse "$p.tok.kind" "$p.tok.lit" in this (excerpt): "...${p.excerpt()}..."')
			}
		}
	}
}

// inline_table parses next tokens into a map of `ast.Value`s.
// The V map type is corresponding to a "table" in TOML.
pub fn (mut p Parser) inline_table(mut tbl map[string]ast.Value) ? {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing inline table into ${ptr_str(tbl)}...')

	mut previous_token_was_value := false
	for p.tok.kind != .eof {
		p.next()?
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing token "$p.tok.kind"')

		if previous_token_was_value {
			p.ignore_while(parser.space_formatting)
			if p.tok.kind != .rcbr {
				p.expect(.comma)?
			}
			previous_token_was_value = false
		}

		match p.tok.kind {
			.whitespace, .tab {
				/*
				if !p.scanner.config.tokenize_formatting {
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping "$p.tok.kind" "$p.tok.lit"')
					continue
				}*/
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping formatting "$p.tok.kind" "$p.tok.lit"')
				continue
			}
			.comma {
				p.ignore_while_peek(parser.space_formatting)
				if p.peek_tok.kind in [.comma, .rcbr] {
					p.next()? // Forward to the peek_tok
					return error(@MOD + '.' + @STRUCT + '.' + @FN +
						' unexpected "$p.tok.kind" "$p.tok.lit" at this (excerpt): "...${p.excerpt()}..."')
				}
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping comma table value seperator "$p.tok.lit"')
				continue
			}
			.rcbr {
				// '}' bracket
				return
			}
			.bare, .quoted, .number, .minus, .underscore {
				// Peek forward as far as we can skipping over space formatting tokens.
				peek_tok, _ := p.peek_over(1, parser.space_formatting)?

				if peek_tok.kind == .period {
					dotted_key, val := p.dotted_key_value()?

					sub_table, key := p.sub_table_key(dotted_key)

					mut t := p.find_in_table(mut tbl, sub_table)?
					unsafe {
						util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'inserting @6 "$key" = $val into ${ptr_str(t)}')
						t[key.str()] = val
					}
				} else {
					p.ignore_while(parser.space_formatting)
					key, val := p.key_value()?
					key_str := key.str()
					if _ := tbl[key_str] {
						return error(@MOD + '.' + @STRUCT + '.' + @FN +
							' key "$key_str" is already initialized with a value. At "$p.tok.kind" "$p.tok.lit" in this (excerpt): "...${p.excerpt()}..."')
					}
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'inserting @5 "$key_str" = $val into ${ptr_str(tbl)}')
					tbl[key_str] = val
				}
				previous_token_was_value = true
			}
			else {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' unexpected "$p.tok.kind" "$p.tok.lit" at this (excerpt): "...${p.excerpt()}..."')
			}
		}
	}
	// Make sure the inline-table actually use the return at .rcbr match branch.
	return error(@MOD + '.' + @STRUCT + '.' + @FN +
		' unexpected end of inline-table "$p.tok.kind" "$p.tok.lit" at this (excerpt): "...${p.excerpt()}..."')
}

// array_of_tables parses next tokens into an array of `ast.Value`s.
pub fn (mut p Parser) array_of_tables(mut table map[string]ast.Value) ? {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing array of tables "$p.tok.kind" "$p.tok.lit"')
	// NOTE this is starting to get ugly. TOML isn't simple at this point
	p.check(.lsbr)? // '[' bracket

	// Allow [[ key]]
	p.ignore_while(parser.space_formatting)
	peek_tok, _ := p.peek_over(1, parser.space_formatting)?
	p.ignore_while(parser.space_formatting)

	// [[key.key]] horror
	if peek_tok.kind == .period {
		p.double_array_of_tables(mut table)?
		return
	}

	key := p.key()?
	p.next()?

	// Allow [[key ]]
	p.ignore_while(parser.space_formatting)

	p.check(.rsbr)?
	p.peek_for_correct_line_ending_or_fail()?
	p.expect(.rsbr)?

	p.ignore_while(parser.all_formatting)

	dotted_key := DottedKey([key.str()])
	dotted_key_str := dotted_key.str()

	// Disallow re-declaring the key
	p.check_explicitly_declared(dotted_key)?

	unsafe {
		if val := table[dotted_key_str] {
			if val is []ast.Value {
				arr := &(table[dotted_key_str] as []ast.Value)
				arr << p.array_of_tables_contents()?
				table[dotted_key_str] = arr
			} else {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' table[$dotted_key_str] is not an array. (excerpt): "...${p.excerpt()}..."')
			}
		} else {
			table[dotted_key_str] = p.array_of_tables_contents()?
		}
	}
	p.last_aot = dotted_key

	unsafe {
		arr := &(table[p.last_aot.str()] as []ast.Value)
		p.last_aot_index = arr.len - 1
	}
}

// array_of_tables_contents parses next tokens into an array of `ast.Value`s.
pub fn (mut p Parser) array_of_tables_contents() ?[]ast.Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing contents from "$p.tok.kind" "$p.tok.lit"')
	mut tbl := map[string]ast.Value{}

	p.table_contents(mut tbl)?

	mut arr := []ast.Value{}
	arr << tbl
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed array of tables ${ast.Value(arr)}. leaving at "$p.tok.kind" "$p.tok.lit"')
	return arr
}

// double_array_of_tables parses next tokens into an array of tables of arrays of `ast.Value`s...
pub fn (mut p Parser) double_array_of_tables(mut table map[string]ast.Value) ? {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing nested array of tables "$p.tok.kind" "$p.tok.lit"')

	dotted_key := p.dotted_key()?
	p.ignore_while(parser.space_formatting)

	p.check(.rsbr)?
	p.expect(.rsbr)?

	p.ignore_while(parser.all_formatting)

	if dotted_key.len != 2 {
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' nested array of tables does not support more than 2 levels. (excerpt): "...${p.excerpt()}..."')
	}

	p.check_explicitly_declared(dotted_key)?

	if !p.explicit_declared_array_of_tables.has(dotted_key) {
		p.explicit_declared_array_of_tables << dotted_key
	}

	first := DottedKey([dotted_key[0]]) // The array that holds the entries
	last := DottedKey([dotted_key[1]]) // The key the parsed array data should be added to

	mut t_arr := &[]ast.Value(0)
	mut t_map := ast.Value(ast.Null{})

	unsafe {
		// NOTE this is starting to get EVEN uglier. TOML is not *at all* simple at this point...
		if first != p.last_aot {
			util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, '$first != $p.last_aot')
			// Implicit allocation
			if p.last_aot.len == 0 {
				p.last_aot = first
				mut nm := &p.root_map
				if first.str() in table.keys() {
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'adding to existing table entry at `$first`.')
					nm = &(table[first.str()] as map[string]ast.Value)
				} else {
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'implicit allocation of map for `$first` in dotted key `$dotted_key`.')
					nm = &map[string]ast.Value{}
					// We register this implicit allocation as *explicit* to be able to catch
					// special cases like:
					// https://github.com/BurntSushi/toml-test/blob/576db852/tests/invalid/table/array-implicit.toml
					p.explicit_declared << first
				}

				nm[last.str()] = []ast.Value{}
				table[first.str()] = ast.Value(nm)

				t_arr = &(nm[last.str()] as []ast.Value)
				t_arr << p.array_of_tables_contents()?
				return
			} else {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' nested array of tables key "$first" does not match "$p.last_aot". (excerpt): "...${p.excerpt()}..."')
			}
		}

		t_arr = &(table[p.last_aot.str()] as []ast.Value)
		t_map = ast.Value(map[string]ast.Value{})
		if p.last_aot_index < t_arr.len {
			t_map = t_arr[p.last_aot_index]
		}

		mut t := &(t_map as map[string]ast.Value)

		if val := t[last.str()] {
			if val is []ast.Value {
				arr := &(val as []ast.Value)
				arr << p.double_array_of_tables_contents(dotted_key)?
				t[last.str()] = arr
			} else {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' t[$last.str()] is not an array. (excerpt): "...${p.excerpt()}..."')
			}
		} else {
			t[last.str()] = p.double_array_of_tables_contents(dotted_key)?
		}
		if t_arr.len == 0 {
			t_arr << t
			p.last_aot_index = t_arr.len - 1
		}
	}
}

// double_array_of_tables_contents parses next tokens into an array of `ast.Value`s.
pub fn (mut p Parser) double_array_of_tables_contents(target_key DottedKey) ?[]ast.Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing contents from "$p.tok.kind" "$p.tok.lit"')
	mut tbl := map[string]ast.Value{}

	mut implicit_allocation_key := DottedKey([]string{})
	mut peeked_over := 0
	mut peek_tok := p.peek_tok

	for p.tok.kind != .eof {
		p.next()?
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing token "$p.tok.kind"')
		p.ignore_while(parser.all_formatting)

		// Peek forward as far as we can skipping over space formatting tokens.
		peek_tok, peeked_over = p.peek_over(1, parser.space_formatting)?
		// Peek for occurrence of `[[`
		if peek_tok.kind == .lsbr {
			peek_tok, peeked_over = p.peek_over(peeked_over + 1, parser.space_formatting)?
			if peek_tok.kind == .lsbr {
				mut arr := []ast.Value{}
				arr << tbl
				return arr
			}
		}

		match p.tok.kind {
			.bare, .quoted, .number, .minus, .underscore {
				// Peek forward as far as we can skipping over space formatting tokens.
				peek_tok, _ = p.peek_over(1, parser.space_formatting)?

				if peek_tok.kind == .period {
					mut dotted_key, val := p.dotted_key_value()?

					if implicit_allocation_key.len > 0 {
						dotted_key.insert(0, implicit_allocation_key)
					}
					sub_table, key := p.sub_table_key(dotted_key)

					mut t := p.find_in_table(mut tbl, sub_table)?
					unsafe {
						util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'inserting @6 "$key" = $val into ${ptr_str(t)}')
						t[key.str()] = val
					}
				} else {
					key, val := p.key_value()?

					mut t := unsafe { &tbl }
					if implicit_allocation_key.len > 0 {
						t = p.find_in_table(mut tbl, implicit_allocation_key)?
					}
					unsafe {
						util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'inserting @7 "$key" = $val into ${ptr_str(t)}')
						t[key.str()] = val
					}
				}
			}
			.lsbr {
				p.check(.lsbr)? // '[' bracket
				peek_tok = p.peek_tok

				// Allow `[ d.e.f]`
				p.ignore_while(parser.space_formatting)

				// Peek forward as far as we can skipping over space formatting tokens.
				peek_tok, _ = p.peek_over(1, parser.space_formatting)?

				if peek_tok.kind == .period {
					// Parse `[d.e.f]`
					p.ignore_while(parser.space_formatting)
					dotted_key := p.dotted_key()?
					implicit_allocation_key = dotted_key
					if dotted_key.len > 2 {
						implicit_allocation_key = dotted_key[2..]
					}
					p.ignore_while(parser.space_formatting)
					util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'keys are: dotted `$dotted_key`, target `$target_key`, implicit `$implicit_allocation_key` at "$p.tok.kind" "$p.tok.lit"')
					p.expect(.rsbr)?
					p.peek_for_correct_line_ending_or_fail()?
					p.explicit_declared << dotted_key
					continue
				} else {
					return error(@MOD + '.' + @STRUCT + '.' + @FN +
						' could not parse "$p.tok.kind" "$p.tok.lit" in this (excerpt): "...${p.excerpt()}..."')
				}
			}
			else {
				break
			}
		}
	}
	mut arr := []ast.Value{}
	arr << tbl
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed array of tables ${ast.Value(arr)}. leaving at "$p.tok.kind" "$p.tok.lit"')
	return arr
}

// array parses next tokens into an array of `ast.Value`s.
pub fn (mut p Parser) array() ?[]ast.Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing array...')
	mut arr := []ast.Value{}
	p.expect(.lsbr)? // '[' bracket
	mut previous_token_was_value := false
	for p.tok.kind != .eof {
		p.next()?
		util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing token "$p.tok.kind" "$p.tok.lit"')

		if previous_token_was_value {
			p.ignore_while(parser.all_formatting)
			if p.tok.kind != .rsbr && p.tok.kind != .hash {
				p.expect(.comma)?
			}
			previous_token_was_value = false
		}

		match p.tok.kind {
			.boolean {
				arr << ast.Value(p.boolean()?)
				previous_token_was_value = true
			}
			.comma {
				p.ignore_while_peek(parser.space_formatting)
				// Trailing commas before array close is allowed
				// so we do not do `if p.peek_tok.kind == .rsbr { ... }`

				// Check for known errors:
				if p.peek_tok.kind in [.comma, .bare] {
					p.next()? // Forward to the peek_tok
					return error(@MOD + '.' + @STRUCT + '.' + @FN +
						' unexpected "$p.tok.kind" "$p.tok.lit" at this (excerpt): "...${p.excerpt()}..."')
				}
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping comma table value seperator "$p.tok.lit"')
				continue
			}
			.eof {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' could not parse array. Reached EOF "$p.tok.kind" "$p.tok.lit" ("$p.tok.lit") in this (excerpt): "...${p.excerpt()}..."')
			}
			.hash {
				c := p.comment()
				p.ast_root.comments << c
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'skipping comment "$c.text"')
			}
			.lcbr {
				p.ignore_while(parser.space_formatting)
				mut t := map[string]ast.Value{}
				p.inline_table(mut t)?
				arr << ast.Value(t)
				previous_token_was_value = true
			}
			.number {
				val := p.number_or_date()?
				arr << val
				previous_token_was_value = true
			}
			.quoted {
				arr << ast.Value(p.quoted())
				previous_token_was_value = true
			}
			.lsbr {
				util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing array in array "$p.tok.kind" "$p.tok.lit"')
				arr << ast.Value(p.array()?)
				previous_token_was_value = true
			}
			.rsbr {
				break
			}
			else {
				error(@MOD + '.' + @STRUCT + '.' + @FN +
					' could not parse  "$p.tok.kind" "$p.tok.lit" ("$p.tok.lit") in this (excerpt): "...${p.excerpt()}..."')
			}
		}
	}
	p.expect(.rsbr)? // ']' bracket
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
		pos: p.tok.pos()
	}
}

// key parse and returns an `ast.Key` type.
// Keys are the token(s) appearing before an assignment operator (=).
pub fn (mut p Parser) key() ?ast.Key {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing key from "$p.tok.lit" ...')

	mut key := ast.Key(ast.Null{})
	if p.tok.kind == .number {
		if p.peek_tok.kind == .minus {
			mut lits := p.tok.lit
			pos := p.tok.pos()
			for p.peek_tok.kind != .assign && p.peek_tok.kind != .period && p.peek_tok.kind != .rsbr {
				p.next()?
				if p.tok.kind !in parser.space_formatting {
					lits += p.tok.lit
				}
			}
			return ast.Key(ast.Bare{
				text: lits
				pos: pos
			})
		}
		key = ast.Key(p.number())
	} else {
		key = match p.tok.kind {
			.bare, .underscore, .minus {
				ast.Key(p.bare()?)
			}
			.boolean {
				ast.Key(p.boolean()?)
			}
			.quoted {
				ast.Key(p.quoted())
			}
			else {
				ast.Key(ast.Null{})
			}
		}
	}

	// NOTE kept for eased debugging
	// util.printdbg(@MOD +'.' + @STRUCT + '.' + @FN, 'parsed key "$p.tok.lit"')
	// panic(@MOD + '.' + @STRUCT + '.' + @FN + ' could not parse ${p.tok.kind} ("${p.tok.lit}") token \n$p.tok')
	// return ast.Key(ast.Bare{})

	if key is ast.Null {
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' key expected .bare, .underscore, .number, .quoted or .boolean but got "$p.tok.kind"')
	}

	// A few small exceptions that can't easily be done via `checker` or `decoder` *after* the
	// main table has been build since information like `is_multiline` is lost when using the key.text as a
	// V `map` key directly.
	if key is ast.Quoted {
		if p.config.run_checks {
			quoted := key as ast.Quoted
			if quoted.is_multiline {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' multiline string as key is not allowed. (excerpt): "...${p.excerpt()}..."')
			}
			chckr := checker.Checker{
				scanner: p.scanner
			}
			chckr.check_quoted(quoted)?
		}
		if p.config.decode_values {
			mut quoted := key as ast.Quoted
			decoder.decode_quoted_escapes(mut quoted)?
			key = ast.Key(quoted)
		}
	}

	return key
}

// key_value parse and returns a pair `ast.Key` and `ast.Value` type.
// see also `key()` and `value()`
pub fn (mut p Parser) key_value() ?(ast.Key, ast.Value) {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing key value pair...')
	key := p.key()?
	p.next()?
	p.ignore_while(parser.space_formatting)
	p.check(.assign)? // Assignment operator
	p.ignore_while(parser.space_formatting)
	value := p.value()?
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed key value pair. `$key = $value`')

	p.explicit_declared << p.build_abs_dotted_key(DottedKey([
		key.str(),
	]))

	return key, value
}

// dotted_key_value parse and returns a pair `DottedKey` and `ast.Value` type.
// see also `key()` and `value()`
pub fn (mut p Parser) dotted_key_value() ?(DottedKey, ast.Value) {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing dotted key value pair...')
	p.ignore_while(parser.space_formatting)
	dotted_key := p.dotted_key()?
	p.ignore_while(parser.space_formatting)
	p.check(.assign)?
	p.ignore_while(parser.space_formatting)
	value := p.value()?
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed dotted key value pair `$dotted_key = $value`...')

	p.explicit_declared << p.build_abs_dotted_key(dotted_key)

	return dotted_key, value
}

// value parse and returns an `ast.Value` type.
// values are the token(s) appearing after an assignment operator (=).
pub fn (mut p Parser) value() ?ast.Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsing value from token "$p.tok.kind" "$p.tok.lit"...')
	mut value := ast.Value(ast.Null{})

	if p.tok.kind == .number {
		number_or_date := p.number_or_date()?
		value = number_or_date
	} else {
		value = match p.tok.kind {
			.quoted {
				ast.Value(p.quoted())
			}
			.boolean {
				ast.Value(p.boolean()?)
			}
			.lsbr {
				ast.Value(p.array()?)
			}
			.lcbr {
				p.ignore_while(parser.space_formatting)
				mut t := map[string]ast.Value{}
				p.inline_table(mut t)?
				ast.Value(t)
			}
			else {
				ast.Value(ast.Null{})
			}
		}
		if value is ast.Null {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' value expected .boolean, .quoted, .lsbr, .lcbr or .number got "$p.tok.kind" "$p.tok.lit" in this (excerpt): "...${p.excerpt()}..."')
		}
	}
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed "$p.tok.kind" as value $value')
	return value
}

// number_or_date parse and returns an `ast.Value` type as
// one of [`ast.Date`, `ast.Time`, `ast.DateTime`, `ast.Number`]
pub fn (mut p Parser) number_or_date() ?ast.Value {
	// Handle Date/Time
	if p.peek_tok.kind == .minus || p.peek_tok.kind == .colon {
		date_time_type := p.date_time()?
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

// bare parse and returns an `ast.Bare` type.
pub fn (mut p Parser) bare() ?ast.Bare {
	mut lits := p.tok.lit
	pos := p.tok.pos()
	for p.peek_tok.kind != .assign && p.peek_tok.kind != .period && p.peek_tok.kind != .rsbr
		&& p.peek_tok.kind !in parser.space_formatting {
		p.next()?
		if p.tok.kind == .bare || p.tok.kind == .minus || p.tok.kind == .underscore {
			lits += p.tok.lit
			continue
		}
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' bare key expected .bare, .minus, or .underscore but got "$p.tok.kind"')
	}
	return ast.Bare{
		text: lits
		pos: pos
	}
}

// quoted parse and returns an `ast.Quoted` type.
pub fn (mut p Parser) quoted() ast.Quoted {
	// To get more info about the quote type and enable better checking,
	// the scanner is returning the literal *with* single- or double-quotes.
	mut quote := p.tok.lit[0]
	is_multiline := p.tok.lit.len >= 6 && p.tok.lit[1] == quote && p.tok.lit[2] == quote
	mut lit := p.tok.lit[1..p.tok.lit.len - 1]
	if is_multiline {
		lit = p.tok.lit[3..p.tok.lit.len - 3]
		// From https://toml.io/en/v1.0.0#string
		// "Multi-line literal strings [...] A newline immediately following the opening
		// delimiter will be trimmed. All other content between the delimiters
		// is interpreted as-is without modification."
		if lit.len > 0 && lit[0] == `\n` {
			lit = lit[1..]
		}
	}
	return ast.Quoted{
		text: lit
		pos: p.tok.pos()
		quote: quote
		is_multiline: is_multiline
	}
}

// boolean parse and returns an `ast.Bool` type.
pub fn (mut p Parser) boolean() ?ast.Bool {
	if p.tok.lit !in ['true', 'false'] {
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' expected literal to be either `true` or `false` got "$p.tok.kind"')
	}
	return ast.Bool{
		text: p.tok.lit
		pos: p.tok.pos()
	}
}

// number parse and returns an `ast.Number` type.
pub fn (mut p Parser) number() ast.Number {
	return ast.Number{
		text: p.tok.lit
		pos: p.tok.pos()
	}
}

// date_time parses dates and time in RFC 3339 format.
// https://datatracker.ietf.org/doc/html/rfc3339
pub fn (mut p Parser) date_time() ?ast.DateTimeType {
	// Date and/or Time
	mut lit := ''
	pos := p.tok.pos()
	mut date := ast.Date{}
	mut time := ast.Time{}

	if p.peek_tok.kind == .minus {
		date = p.date()?
		lit += date.text
		// Look for any THH:MM:SS or <space>HH:MM:SS
		if (p.peek_tok.kind == .bare && (p.peek_tok.lit.starts_with('T')
			|| p.peek_tok.lit.starts_with('t'))) || p.peek_tok.kind == .whitespace {
			p.next()? // Advance to token with Txx or whitespace special case
			if p.tok.lit.starts_with('T') || p.tok.lit.starts_with('t') {
				lit += p.tok.lit[0].ascii_str() //'T' or 't'
			} else {
				lit += p.tok.lit
				p.next()?
			}
			time = p.time()?
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
		time = p.time()?
		return time
	}

	return ast.Date{
		text: lit
		pos: pos
	}
}

// date parse and returns an `ast.Date` type.
pub fn (mut p Parser) date() ?ast.Date {
	// Date
	mut lit := p.tok.lit
	pos := p.tok.pos()

	p.check(.number)?
	lit += p.tok.lit
	p.check(.minus)?
	lit += p.tok.lit
	p.check(.number)?
	lit += p.tok.lit
	p.check(.minus)?
	lit += p.tok.lit
	p.expect(.number)?

	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'parsed date: "$lit"')
	return ast.Date{
		text: lit
		pos: pos
	}
}

// time parse and returns an `ast.Time` type.
pub fn (mut p Parser) time() ?ast.Time {
	// Time
	mut lit := p.tok.lit
	pos := p.tok.pos()

	if p.is_at(.bare) && (lit.starts_with('T') || lit.starts_with('t')) {
		if p.tok.lit.starts_with('T') {
			lit = lit.all_after('T')
		} else if p.tok.lit.starts_with('t') {
			lit = lit.all_after('t')
		}
		p.next()?
	} else {
		p.check(.number)?
	}
	lit += p.tok.lit
	p.check(.colon)?
	lit += p.tok.lit
	p.check(.number)?
	lit += p.tok.lit
	// TODO does TOML even have optional seconds?
	// if p.peek_tok.kind == .colon {
	p.check(.colon)?
	lit += p.tok.lit
	p.expect(.number)?
	//}

	// Optional milliseconds
	if p.peek_tok.kind == .period {
		p.next()?
		lit += p.tok.lit // lit += '.'
		p.check(.period)?
		lit += p.tok.lit
		p.expect(.number)?
	}

	// Parse offset
	if p.peek_tok.kind == .minus || p.peek_tok.kind == .plus {
		p.next()?
		lit += p.tok.lit // lit += '-'
		p.check_one_of([.minus, .plus])?
		lit += p.tok.lit
		p.check(.number)?
		lit += p.tok.lit
		p.check(.colon)?
		lit += p.tok.lit
		p.expect(.number)?
	} else if p.peek_tok.kind == .bare && (p.peek_tok.lit == 'Z' || p.peek_tok.lit == 'z') {
		p.next()?
		lit += p.tok.lit
		p.expect(.bare)?
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
		pos: p.tok.pos()
	}
}
