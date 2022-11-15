// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module toml

import toml.ast
import toml.input
import toml.scanner
import toml.parser

// Null is used in sumtype checks as a "default" value when nothing else is possible.
pub struct Null {
}

// decode decodes a TOML `string` into the target type `T`.
pub fn decode<T>(toml_txt string) !T {
	doc := parse_text(toml_txt)!
	mut typ := T{}
	typ.from_toml(doc.to_any())
	return typ
}

// encode encodes the type `T` into a TOML string.
// Currently encode expects the method `.to_toml()` exists on `T`.
pub fn encode<T>(typ T) string {
	return typ.to_toml()
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
	ast &ast.Root = unsafe { nil }
}

// parse_file parses the TOML file in `path`.
pub fn parse_file(path string) !Doc {
	input_config := input.Config{
		file_path: path
	}
	scanner_config := scanner.Config{
		input: input_config
	}
	parser_config := parser.Config{
		scanner: scanner.new_scanner(scanner_config)!
	}
	mut p := parser.new_parser(parser_config)
	ast := p.parse()!
	return Doc{
		ast: ast
	}
}

// parse_text parses the TOML document provided in `text`.
pub fn parse_text(text string) !Doc {
	input_config := input.Config{
		text: text
	}
	scanner_config := scanner.Config{
		input: input_config
	}
	parser_config := parser.Config{
		scanner: scanner.new_scanner(scanner_config)!
	}
	mut p := parser.new_parser(parser_config)
	ast := p.parse()!
	return Doc{
		ast: ast
	}
}

// parse parses the TOML document provided in `toml`.
// parse automatically try to determine if the type of `toml` is a file or text.
// For explicit parsing of input types see `parse_file` or `parse_text`.
[deprecated: 'use parse_file or parse_text instead']
[deprecated_after: '2022-06-18']
pub fn parse(toml string) !Doc {
	mut input_config := input.auto_config(toml)!
	scanner_config := scanner.Config{
		input: input_config
	}
	parser_config := parser.Config{
		scanner: scanner.new_scanner(scanner_config)!
	}
	mut p := parser.new_parser(parser_config)
	ast := p.parse()!
	return Doc{
		ast: ast
	}
}

// parse_dotted_key converts `key` string to an array of strings.
// parse_dotted_key preserves strings delimited by both `"` and `'`.
pub fn parse_dotted_key(key string) ![]string {
	mut out := []string{}
	mut buf := ''
	mut in_string := false
	mut delim := u8(` `)
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
				buf = buf[..buf.len - 1]
				if buf != '' && buf != ' ' {
					out << buf
				}
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
			': could not parse key, missing closing string delimiter `${delim.ascii_str()}`')
	}
	return out
}

// parse_array_key converts `key` string to a key and index part.
fn parse_array_key(key string) (string, int) {
	mut index := -1
	mut k := key
	if k.contains('[') {
		index = k.all_after('[').all_before(']').int()
		if k.starts_with('[') {
			k = '' // k.all_after(']')
		} else {
			k = k.all_before('[')
		}
	}
	return k, index
}

// to_any converts the `Doc` to toml.Any type.
pub fn (d Doc) to_any() Any {
	return ast_to_any(d.ast.table)
}

// reflect returns `T` with `T.<field>`'s value set to the
// value of any 1st level TOML key by the same name.
pub fn (d Doc) reflect<T>() T {
	return d.to_any().reflect<T>()
}

// value queries a value from the TOML document.
// `key` supports a small query syntax scheme:
// Maps can be queried in "dotted" form e.g. `a.b.c`.
// quoted keys are supported as `a."b.c"` or `a.'b.c'`.
// Arrays can be queried with `a[0].b[1].[2]`.
pub fn (d Doc) value(key string) Any {
	key_split := parse_dotted_key(key) or { return toml.null }
	return d.value_(d.ast.table, key_split)
}

pub const null = Any(Null{})

pub fn (d Doc) value_opt(key string) !Any {
	key_split := parse_dotted_key(key) or { return error('invalid dotted key') }
	x := d.value_(d.ast.table, key_split)
	if x is Null {
		return error('no value for key')
	}
	return x
}

// value_ returns the value found at `key` in the map `values` as `Any` type.
fn (d Doc) value_(value ast.Value, key []string) Any {
	if key.len == 0 {
		return toml.null
	}
	mut ast_value := ast.Value(ast.Null{})
	k, index := parse_array_key(key[0])

	if k == '' {
		a := value as []ast.Value
		ast_value = a[index] or { return toml.null }
	}

	if value is map[string]ast.Value {
		ast_value = value[k] or { return toml.null }
		if index > -1 {
			a := ast_value as []ast.Value
			ast_value = a[index] or { return toml.null }
		}
	}

	if key.len <= 1 {
		return ast_to_any(ast_value)
	}
	match ast_value {
		map[string]ast.Value, []ast.Value {
			return d.value_(ast_value, key[1..])
		}
		else {
			return ast_to_any(value)
		}
	}
}

// ast_to_any converts `from` ast.Value to toml.Any value.
pub fn ast_to_any(value ast.Value) Any {
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
			val_text := value.text
			if val_text == 'inf' || val_text == '+inf' || val_text == '-inf' {
				// NOTE values taken from strconv
				if !val_text.starts_with('-') {
					// strconv.double_plus_infinity
					return Any(u64(0x7FF0000000000000))
				} else {
					// strconv.double_minus_infinity
					return Any(u64(0xFFF0000000000000))
				}
			}
			if val_text == 'nan' || val_text == '+nan' || val_text == '-nan' {
				return Any('nan')
			}
			if !val_text.starts_with('0x')
				&& (val_text.contains('.') || val_text.to_lower().contains('e')) {
				return Any(value.f64())
			}
			return Any(value.i64())
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
				am[k] = ast_to_any(v)
			}
			return am
			// return d.get_map_value(m, key_split[1..].join('.'))
		}
		[]ast.Value {
			a := (value as []ast.Value)
			mut aa := []Any{}
			for val in a {
				aa << ast_to_any(val)
			}
			return aa
		}
		else {
			return toml.null
		}
	}

	return toml.null
	// TODO decide this
	// panic(@MOD + '.' + @STRUCT + '.' + @FN + ' can\'t convert "$value"')
	// return Any('')
}
