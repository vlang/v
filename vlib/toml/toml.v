// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module toml

import toml.ast
import toml.input
import toml.scanner
import toml.parser
import strconv

// Null is used in sumtype checks as a "default" value when nothing else is possible.
pub struct Null {
}

// DateTime is the representation of an RFC 3339 datetime string.
pub struct DateTime {
	datetime string
}

pub fn (dt DateTime) str() string {
	return dt.datetime
}

// Date is the representation of an RFC 3339 date-only string.
pub struct Date {
	date string
}

pub fn (d Date) str() string {
	return d.date
}

// Time is the representation of an RFC 3339 time-only string.
pub struct Time {
	time string
}

pub fn (t Time) str() string {
	return t.time
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

// parse_dotted_key converts `key` string to an array of strings.
// parse_dotted_key preserves strings delimited by both `"` and `'`.
pub fn parse_dotted_key(key string) ?[]string {
	mut out := []string{}
	mut buf := ''
	mut in_string := false
	mut delim := byte(` `)
	for ch in key {
		if ch in [`"`, `'`] {
			if !in_string {
				delim = ch
			}
			in_string = !in_string && ch == delim
			if !in_string {
				if buf != '' && buf != ' ' {
					out << buf
				}
				buf = ''
				delim = ` `
			}
			continue
		}
		buf += ch.ascii_str()
		if !in_string && ch == `.` {
			if buf != '' && buf != ' ' {
				out << buf[..buf.len - 1]
			}
			buf = ''
			continue
		}
	}
	if buf != '' && buf != ' ' {
		out << buf
	}
	if in_string {
		return error(@FN +
			': could not parse key, missing closing string delimiter `$delim.ascii_str()`')
	}
	return out
}

// to_any converts the `Doc` to toml.Any type.
pub fn (d Doc) to_any() Any {
	return d.ast_to_any(d.ast.table)
}

// value queries a value from the TOML document.
// `key` should be in "dotted" form (`a.b.c`).
// `key` supports quoted keys like `a."b.c"`.
pub fn (d Doc) value(key string) Any {
	values := d.ast.table as map[string]ast.Value
	key_split := parse_dotted_key(key) or { return Any(Null{}) }
	return d.value_(values, key_split)
}

// value_ returns the value found at `key` in the map `values` as `Any` type.
fn (d Doc) value_(values map[string]ast.Value, key []string) Any {
	value := values[key[0]] or {
		return Any(Null{})
		// TODO decide this
		// panic(@MOD + '.' + @STRUCT + '.' + @FN + ' key "$key[0]" does not exist')
	}
	// `match` isn't currently very suitable for these types of sum type constructs...
	if value is map[string]ast.Value {
		if key.len <= 1 {
			return d.ast_to_any(value)
		}
		m := (value as map[string]ast.Value)
		return d.value_(m, key[1..])
	}
	return d.ast_to_any(value)
}

// ast_to_any converts `from` ast.Value to toml.Any value.
fn (d Doc) ast_to_any(value ast.Value) Any {
	match value {
		ast.Date {
			return Any(Date{value.text})
		}
		ast.Time {
			return Any(Time{value.text})
		}
		ast.DateTime {
			return Any(DateTime{value.text})
		}
		ast.Quoted {
			return Any(value.text)
		}
		ast.Number {
			if value.text.contains('.') || value.text.to_lower().contains('e') {
				return Any(value.text.f64())
			}
			v := strconv.parse_int(value.text, 0, 0) or { i64(0) }
			return Any(v)
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
