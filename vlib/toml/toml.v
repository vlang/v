// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module toml

import toml.ast
import toml.util
import toml.input
import toml.scanner
import toml.parser
import time

// Null is used in sumtype checks as a "default" value when nothing else is possible.
pub struct Null {
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
pub fn parse_file(path string) ?Doc {
	input_config := input.Config{
		file_path: path
	}
	scanner_config := scanner.Config{
		input: input_config
	}
	parser_config := parser.Config{
		scanner: scanner.new_scanner(scanner_config) ?
	}
	mut p := parser.new_parser(parser_config)
	ast := p.parse() ?
	return Doc{
		ast: ast
	}
}

// parse_text parses the TOML document provided in `text`.
pub fn parse_text(text string) ?Doc {
	input_config := input.Config{
		text: text
	}
	scanner_config := scanner.Config{
		input: input_config
	}
	parser_config := parser.Config{
		scanner: scanner.new_scanner(scanner_config) ?
	}
	mut p := parser.new_parser(parser_config)
	ast := p.parse() ?
	return Doc{
		ast: ast
	}
}

// parse parses the TOML document provided in `toml`.
// parse automatically try to determine if the type of `toml` is a file or text.
// For explicit parsing of input types see `parse_file` or `parse_text`.
pub fn parse(toml string) ?Doc {
	mut input_config := input.auto_config(toml) ?
	scanner_config := scanner.Config{
		input: input_config
	}
	parser_config := parser.Config{
		scanner: scanner.new_scanner(scanner_config) ?
	}
	mut p := parser.new_parser(parser_config)
	ast := p.parse() ?
	return Doc{
		ast: ast
	}
}

// to_json returns a compact json string of the complete document.
pub fn (d Doc) to_json() string {
	return d.ast.to_json()
}

// value queries a value from the TOML document.
pub fn (d Doc) value(key string) Any {
	values := d.ast.table as map[string]ast.Value
	// any_values := d.ast_to_any(values) as map[string]Any
	return d.get_map_value_as_any(values, key)
}

// ast_to_any_value converts `from` ast.Value to toml.Any value.
fn (d Doc) ast_to_any(value ast.Value) Any {
	// `match` isn't currently very suitable for further unwrapping sumtypes in the if's...
	if value is ast.Date || value is ast.Time || value is ast.DateTime {
		mut tim := time.Time{}
		if value is ast.Date {
			date_str := (value as ast.Date).text

			tim = time.parse_rfc3339(date_str) or {
				return Any(Null{})
				// TODO decide this
				// panic(@MOD + '.' + @STRUCT + '.' + @FN +
				//	' failed converting "$date_str" to rfc3339: $err')
			}
		} else if value is ast.Time {
			time_str := (value as ast.Time).text

			tim = time.parse_rfc3339(time_str) or {
				return Any(Null{})
				// TODO decide this
				// panic(@MOD + '.' + @STRUCT + '.' + @FN +
				//	' failed converting "$time_str" to rfc3339: $err')
			}
		} else {
			// value is ast.DateTime
			datetime_str := (value as ast.DateTime).text

			tim = time.parse_rfc3339(datetime_str) or {
				return Any(Null{})
				// TODO decide this
				// panic(@MOD + '.' + @STRUCT + '.' + @FN +
				//	' failed converting "$datetime_str" to rfc3339: $err')
			}
		}
		return Any(tim)
	}

	match value {
		ast.Quoted {
			return Any((value as ast.Quoted).text)
		}
		ast.Number {
			str := (value as ast.Number).text
			if str.contains('.') {
				return Any(str.f64())
			}
			return Any(str.i64())
		}
		ast.Bool {
			str := (value as ast.Bool).text
			if str == 'true' {
				return Any(true)
			}
			return Any(false)
		}
		map[string]ast.Value {
			m := (value as map[string]ast.Value)
			mut am := map[string]Any{}
			for k, v in m {
				am[k] = d.ast_to_any(v)
			}
			return am
			// return d.get_map_value(m, key_split[1..].join('.'))
		}
		[]ast.Value {
			a := (value as []ast.Value)
			mut aa := []Any{}
			for val in a {
				aa << d.ast_to_any(val)
			}
			return aa
		}
		else {
			return Any(Null{})
		}
	}

	return Any(Null{})
	// TODO decide this
	// panic(@MOD + '.' + @STRUCT + '.' + @FN + ' can\'t convert "$value"')
	// return Any('')
}

// get_map_value_as_any returns the value found at `key` in the map `values` as `Any` type.
fn (d Doc) get_map_value_as_any(values map[string]ast.Value, key string) Any {
	key_split := key.split('.')
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, ' getting "${key_split[0]}"')
	if key_split[0] in values.keys() {
		value := values[key_split[0]] or {
			return Any(Null{})
			// TODO decide this
			// panic(@MOD + '.' + @STRUCT + '.' + @FN + ' key "$key" does not exist')
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
		return d.ast_to_any(value)
	}
	return Any(Null{})
	// TODO decide this
	// panic(@MOD + '.' + @STRUCT + '.' + @FN + ' key "$key" does not exist')
}
