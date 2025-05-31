// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module toml

import toml.ast
import toml.input
import toml.scanner
import toml.parser
import maps

// Null is used in sumtype checks as a "default" value when nothing else is possible.
pub struct Null {
}

// decode decodes a TOML `string` into the target type `T`.
// If `T` has a custom `.from_toml()` method, it will be used instead of the default.
pub fn decode[T](toml_txt string) !T {
	doc := parse_text(toml_txt)!
	mut typ := T{}
	$for method in T.methods {
		$if method.name == 'from_toml' {
			typ.$method(doc.to_any())
			return typ
		}
	}
	$if T !is $struct {
		return error('toml.decode: expected struct, found ${T.name}')
	}
	decode_struct[T](doc.to_any(), mut typ)
	return typ
}

fn decode_struct[T](doc Any, mut typ T) {
	$for field in T.fields {
		mut field_name := field.name
		mut skip := false
		for attr in field.attrs {
			if attr == 'skip' {
				skip = true
				break
			}
			if attr.starts_with('toml:') {
				field_name = attr.all_after(':').trim_space()
			}
		}
		value := doc.value(field_name)
		// only set the field's value when value != null and !skip, else field got it's default value
		if !skip && value != null {
			$if field.is_enum {
				typ.$(field.name) = value.int()
			} $else $if field.typ is string {
				typ.$(field.name) = value.string()
			} $else $if field.typ is bool {
				typ.$(field.name) = value.bool()
			} $else $if field.typ is int {
				typ.$(field.name) = value.int()
			} $else $if field.typ is i64 {
				typ.$(field.name) = value.i64()
			} $else $if field.typ is u64 {
				typ.$(field.name) = value.u64()
			} $else $if field.typ is f32 {
				typ.$(field.name) = value.f32()
			} $else $if field.typ is f64 {
				typ.$(field.name) = value.f64()
			} $else $if field.typ is DateTime {
				typ.$(field.name) = value.datetime()
			} $else $if field.typ is Date {
				typ.$(field.name) = value.date()
			} $else $if field.typ is Time {
				typ.$(field.name) = value.time()
			} $else $if field.typ is Any {
				typ.$(field.name) = value
			} $else $if field.is_array {
				arr := value.array()
				match field.typ {
					[]string { typ.$(field.name) = arr.as_strings() }
					[]int { typ.$(field.name) = arr.map(it.int()) }
					[]i64 { typ.$(field.name) = arr.map(it.i64()) }
					[]u64 { typ.$(field.name) = arr.map(it.u64()) }
					[]f32 { typ.$(field.name) = arr.map(it.f32()) }
					[]f64 { typ.$(field.name) = arr.map(it.f64()) }
					[]bool { typ.$(field.name) = arr.map(it.bool()) }
					[]DateTime { typ.$(field.name) = arr.map(it.datetime()) }
					[]Date { typ.$(field.name) = arr.map(it.date()) }
					[]Time { typ.$(field.name) = arr.map(it.time()) }
					[]Any { typ.$(field.name) = arr }
					else {}
				}
			} $else $if field.is_map {
				mut mmap := value.as_map()
				match field.typ {
					map[string]string {
						typ.$(field.name) = mmap.as_strings()
					}
					// Should be cleaned up to use the more modern lambda syntax
					// |k, v| k, v.int()
					// Unfortunately lambdas have issues with multiple return at the time of writing
					map[string]int {
						typ.$(field.name) = maps.to_map[string, Any, string, int](mmap,
							fn (k string, v Any) (string, int) {
							return k, v.int()
						})
					}
					map[string]i64 {
						typ.$(field.name) = maps.to_map[string, Any, string, i64](mmap,
							fn (k string, v Any) (string, i64) {
							return k, v.i64()
						})
					}
					map[string]u64 {
						typ.$(field.name) = maps.to_map[string, Any, string, u64](mmap,
							fn (k string, v Any) (string, u64) {
							return k, v.u64()
						})
					}
					map[string]f32 {
						typ.$(field.name) = maps.to_map[string, Any, string, f32](mmap,
							fn (k string, v Any) (string, f32) {
							return k, v.f32()
						})
					}
					map[string]f64 {
						typ.$(field.name) = maps.to_map[string, Any, string, f64](mmap,
							fn (k string, v Any) (string, f64) {
							return k, v.f64()
						})
					}
					map[string]bool {
						typ.$(field.name) = maps.to_map[string, Any, string, bool](mmap,
							fn (k string, v Any) (string, bool) {
							return k, v.bool()
						})
					}
					map[string]DateTime {
						typ.$(field.name) = maps.to_map[string, Any, string, DateTime](mmap,
							fn (k string, v Any) (string, DateTime) {
							return k, v.datetime()
						})
					}
					map[string]Date {
						typ.$(field.name) = maps.to_map[string, Any, string, Date](mmap,
							fn (k string, v Any) (string, Date) {
							return k, v.date()
						})
					}
					map[string]Time {
						typ.$(field.name) = maps.to_map[string, Any, string, Time](mmap,
							fn (k string, v Any) (string, Time) {
							return k, v.time()
						})
					}
					map[string]Any {
						typ.$(field.name) = mmap.clone()
					}
					else {}
				}
			} $else $if field.is_struct {
				mut s := typ.$(field.name)
				decode_struct(value, mut s)
				typ.$(field.name) = s
			}
		}
	}
}

// encode encodes the type `T` into a TOML string.
// If `T` has a custom `.to_toml()` method, it will be used instead of the default.
pub fn encode[T](typ T) string {
	$if T is $struct {
		$for method in T.methods {
			$if method.name == 'to_toml' {
				return typ.$method()
			}
		}
		mp := encode_struct[T](typ)
		return mp.to_toml()
	} $else {
		$compile_error('Currently only type `struct` is supported for `T` to encode as TOML')
	}
	return ''
}

fn encode_struct[T](typ T) map[string]Any {
	mut mp := map[string]Any{}
	$for field in T.fields {
		mut skip := false
		mut field_name := field.name
		for attr in field.attrs {
			if attr == 'skip' {
				skip = true
				break
			}
			if attr.starts_with('toml:') {
				field_name = attr.all_after(':').trim_space()
			}
		}
		if !skip {
			mp[field_name] = to_any(typ.$(field.name))
		}
	}
	return mp
}

fn to_any[T](value T) Any {
	$if T is $enum {
		return Any(int(value))
	} $else $if T is Date {
		return Any(value)
	} $else $if T is Time {
		return Any(value)
	} $else $if T is Null {
		return Any(value)
	} $else $if T is bool {
		return Any(value)
	} $else $if T is $float {
		return Any(value)
	} $else $if T is i64 {
		return Any(value)
	} $else $if T is int {
		return Any(value)
	} $else $if T is u64 {
		return Any(value)
	} $else $if T is DateTime {
		return Any(value)
	} $else $if T is Any {
		return value
	} $else $if T is $struct {
		$for method in T.methods {
			$if method.name == 'to_toml' {
				return Any(value.$method())
			}
		}
		return encode_struct(value)
	} $else $if T is $array {
		mut arr := []Any{cap: value.len}
		for v in value {
			arr << to_any(v)
		}
		return arr
	} $else $if T is $map {
		mut mmap := map[string]Any{}
		for key, val in value {
			mmap['${key}'] = to_any(val)
		}
		return mmap
	} $else {
		return Any('${value}')
	}
}

// DateTime is the representation of an RFC 3339 datetime string.
pub struct DateTime {
pub:
	datetime string
}

// str returns the RFC 3339 string representation of the datetime.
pub fn (dt DateTime) str() string {
	return dt.datetime
}

// Date is the representation of an RFC 3339 date-only string.
pub struct Date {
pub:
	date string
}

// str returns the RFC 3339 date-only string representation.
pub fn (d Date) str() string {
	return d.date
}

// Time is the representation of an RFC 3339 time-only string.
pub struct Time {
pub:
	time string
}

// str returns the RFC 3339 time-only string representation.
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
	ast_ := p.parse()!
	return Doc{
		ast: ast_
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
	ast_ := p.parse()!
	return Doc{
		ast: ast_
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

// decode decodes a TOML `string` into the target struct type `T`.
pub fn (d Doc) decode[T]() !T {
	$if T !is $struct {
		return error('Doc.decode: expected struct, found ${T.name}')
	}
	mut typ := T{}
	decode_struct(d.to_any(), mut typ)
	return typ
}

// to_any converts the `Doc` to toml.Any type.
pub fn (d Doc) to_any() Any {
	return ast_to_any(d.ast.table)
}

// reflect returns `T` with `T.<field>`'s value set to the
// value of any 1st level TOML key by the same name.
pub fn (d Doc) reflect[T]() T {
	return d.to_any().reflect[T]()
}

// value queries a value from the TOML document.
// `key` supports a small query syntax scheme:
// Maps can be queried in "dotted" form e.g. `a.b.c`.
// quoted keys are supported as `a."b.c"` or `a.'b.c'`.
// Arrays can be queried with `a[0].b[1].[2]`.
pub fn (d Doc) value(key string) Any {
	key_split := parse_dotted_key(key) or { return null }
	return d.value_(d.ast.table, key_split)
}

pub const null = Any(Null{})

// value_opt queries a value from the TOML document. Returns an error if the
// key is not valid or there is no value for the key.
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
		return null
	}
	mut ast_value := ast.Value(ast.Null{})
	k, index := parse_array_key(key[0])

	if k == '' {
		a := value as []ast.Value
		ast_value = a[index] or { return null }
	}

	if value is map[string]ast.Value {
		ast_value = value[k] or { return null }
		if index > -1 {
			a := ast_value as []ast.Value
			ast_value = a[index] or { return null }
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
			return null
		}
	}

	return null
	// TODO: decide this
	// panic(@MOD + '.' + @STRUCT + '.' + @FN + ' can\'t convert "$value"')
	// return Any('')
}
