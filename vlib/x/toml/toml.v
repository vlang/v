// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module toml

import os
import x.toml.ast
import x.toml.util
import x.toml.input
import x.toml.scanner
import x.toml.parser

pub type Any = []Any | bool | f64 | i64 | map[string]Any | string // TODO add more builtin types - or use json2.Any + Date etc. ??

pub fn (a Any) string() string {
	return a as string
}

// Config is used to configure the toml parser.
// Only one of the fields `text` or `file_path`, is allowed to be set at time of configuration.
pub struct Config {
pub:
	text           string // TOML text
	file_path      string // '/path/to/file.toml'
	parse_comments bool
}

// Doc is a representation of a TOML document.
// A document can be constructed from a `string` buffer or from a file path
pub struct Doc {
pub:
	ast &ast.Root
}

// parse_file parses the TOML file in `path`.
pub fn parse_file(path string) Doc {
	input_config := input.Config{
		file_path: path
	}
	scanner_config := scanner.Config{
		input: input_config
	}
	parser_config := parser.Config{
		scanner: scanner.new_scanner(scanner_config)
	}
	mut p := parser.new_parser(parser_config)
	return Doc{
		ast: p.parse()
	}
}

// parse_text parses the TOML document provided in `text`.
pub fn parse_text(text string) Doc {
	input_config := input.Config{
		text: text
	}
	scanner_config := scanner.Config{
		input: input_config
	}
	parser_config := parser.Config{
		scanner: scanner.new_scanner(scanner_config)
	}
	mut p := parser.new_parser(parser_config)
	return Doc{
		ast: p.parse()
	}
}

// parse is a convenience function that parses the TOML document provided in `input`.
// parse automatically try to determine if type of `input` is a file or text.
// For explicit parsing of input see `parse_file` or `parse_text`.
pub fn parse(toml string) Doc {
	mut input_config := input.Config{}
	if os.is_file(toml) {
		input_config = input.Config{
			file_path: toml
		}
	} else {
		input_config = input.Config{
			text: toml
		}
	}

	scanner_config := scanner.Config{
		input: input_config
	}
	parser_config := parser.Config{
		scanner: scanner.new_scanner(scanner_config)
	}
	mut p := parser.new_parser(parser_config)
	return Doc{
		ast: p.parse()
	}
}

// value queries a value from the TOML document.
pub fn (d Doc) value(key string) Any {
	values := d.ast.table as map[string]ast.Value
	// any_values := d.ast_to_any(values) as map[string]Any
	return d.get_map_value_as_any(values, key)
}

// ast_to_any_value converts `from` ast.Value to toml.Any value.
fn (d Doc) ast_to_any(value ast.Value) Any {
	// `match` isn't currently very suitable for these types of sum type constructs...
	if value is ast.Quoted {
		return Any((value as ast.Quoted).text)
	} else if value is ast.Number {
		str := (value as ast.Number).text
		if str.contains('.') {
			return Any(str.f64())
		}
		return Any(str.i64())
	} else if value is ast.Bool {
		str := (value as ast.Bool).text
		if str == 'true' {
			return Any(true)
		}
		return Any(false)
	} else if value is map[string]ast.Value {
		m := (value as map[string]ast.Value)
		mut am := map[string]Any{}
		for k, v in m {
			am[k] = d.ast_to_any(v)
		}
		return am
		// return d.get_map_value(m, key_split[1..].join('.'))
	} else if value is []ast.Value {
		a := (value as []ast.Value)
		mut aa := []Any{}
		for val in a {
			aa << d.ast_to_any(val)
		}
		return aa
	}

	// TODO add more types
	panic(@MOD + '.' + @STRUCT + '.' + @FN + ' can\'t convert "$value"')
	return Any('')
}

fn (d Doc) get_map_value_as_any(values map[string]ast.Value, key string) Any {
	key_split := key.split('.')
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, ' getting "${key_split[0]}"')
	if key_split[0] in values.keys() {
		value := values[key_split[0]] or {
			panic(@MOD + '.' + @STRUCT + '.' + @FN + ' key "$key" does not exist')
		}
		// `match` isn't currently very suitable for these types of sum type constructs...
		if value is map[string]ast.Value {
			m := (value as map[string]ast.Value)
			next_key := key_split[1..].join('.')
			if next_key == '' {
				return d.ast_to_any(value)
			}
			return d.get_map_value_as_any(m, next_key)
		}
		/*
		else if value is []ast.Value {
			a := (value as []ast.Value)
			mut aa := []Any
			for val in a {
				aa << d.ast_to_any(a)
			}
			return aa
		}*/
		return d.ast_to_any(value)
	}
	panic(@MOD + '.' + @STRUCT + '.' + @FN + ' key "$key" does not exist')
}

/*
fn (d Doc) get_map_value(values map[string]Any, key string) Any {
	key_split := key.split('.')
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, ' getting "${key_split[0]}"')
	value := values[key_split[0]]
	// `match` isn't currently very suitable for these types of sum type constructs...
	if value is map[string]Any {
		m := (value as map[string]Any)
		return d.get_map_value(m, key_split[1..].join('.'))
	} else if value is []Any {
		// TODO array support
	}

	return value
}
*/

/*
// map_value queries a value from `value_map`.
fn (d Doc) get_map_value(value_map map[string]ast.Value, key string) Any {
	key_split := key.split('.')
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, ' getting "${key_split[0]}"')
	value := value_map[key_split[0]]
	// `match` isn't currently very suitable for these types of sum type constructs...
	if value is ast.Quoted {
		return Any((value as ast.Quoted).text)
	} else if value is ast.Number {
		str := (value as ast.Number).text
		if str.contains('.') {
			return Any(str.f64())
		}
		return Any(str.i64())
	} else if value is ast.Bool {
		str := (value as ast.Bool).text
		if str == 'true' {
			return Any(true)
		}
		return Any(false)
	} else if value is map[string]ast.Value {
		m := (value as map[string]ast.Value)
		return d.get_map_value(m, key_split[1..].join('.'))
	} else if value is []ast.Value {
		a := (value as []ast.Value)
		for val in a {
		}
		return d.get_array_value(m, key_split[1..].join('.'))
	}
	// TODO add more types
	panic(@MOD + '.' + @STRUCT + '.' + @FN + ' can\'t convert "$value"')
	return Any('')
}
*/
