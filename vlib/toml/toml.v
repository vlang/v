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
		$if field.is_embed && field.typ !is DateTime && field.typ !is Date && field.typ !is Time {
			// Embedding is a V concept, TOML has no equivalent: the fields of an
			// embedded struct live at the same level as the fields of the embedding
			// struct. A table named after the embedded field is accepted as well,
			// and takes precedence when present. Embedded `DateTime`, `Date` and
			// `Time` are TOML scalars, so they are decoded below like other fields.
			if !skip {
				mut embedded := typ.$(field.name)
				value := doc.value(field_name)
				if value is map[string]Any {
					decode_struct(value, mut embedded)
				} else {
					decode_struct(doc, mut embedded)
				}
				typ.$(field.name) = embedded
			}
		} $else {
			value := doc.value(field_name)
			// only set the field's value when value != null and !skip, else field got it's default value
			if !skip && value != null {
				$if field.typ is $option {
					// Checked before `is_enum`: an `?SomeEnum` field reports
					// `is_enum` too, and the bare enum branch would assign a
					// plain `int` into the Option.
					decode_option(mut typ.$(field.name), value)
				} $else $if field.is_enum {
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
				} $else $if field.typ is u8 {
					n, ok := to_narrow_int[u8](value)
					if ok {
						typ.$(field.name) = n
					}
				} $else $if field.typ is u16 {
					n, ok := to_narrow_int[u16](value)
					if ok {
						typ.$(field.name) = n
					}
				} $else $if field.typ is u32 {
					n, ok := to_narrow_int[u32](value)
					if ok {
						typ.$(field.name) = n
					}
				} $else $if field.typ is i8 {
					n, ok := to_narrow_int[i8](value)
					if ok {
						typ.$(field.name) = n
					}
				} $else $if field.typ is i16 {
					n, ok := to_narrow_int[i16](value)
					if ok {
						typ.$(field.name) = n
					}
				} $else $if field.typ is i32 {
					n, ok := to_narrow_int[i32](value)
					if ok {
						typ.$(field.name) = n
					}
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
					typ.$(field.name) = decode_array(typ.$(field.name), value.array())
				} $else $if field.is_map {
					typ.$(field.name) = decode_map(typ.$(field.name), value.as_map())
				} $else $if field.is_struct {
					mut s := typ.$(field.name)
					decode_struct(value, mut s)
					typ.$(field.name) = s
				}
			}
		}
	}
}

// decode_option fills `val` from `value`. It mirrors the conversions
// `decode_struct` applies to a plain field of the same type, so that `a = 5`
// fills a `?int` instead of being ignored. A payload that is itself an array or
// a map is not unwrapped: the element type of a container cannot be named from
// its container type, so `decode_array` and `decode_map` cannot be instantiated
// for it.
fn decode_option[T](mut val ?T, value Any) {
	$if T is string {
		val = value.string()
	} $else $if T is bool {
		val = value.bool()
	} $else $if T is int {
		val = value.int()
	} $else $if T is i64 {
		val = value.i64()
	} $else $if T is u64 {
		val = value.u64()
	} $else $if T is u8 {
		n, ok := to_narrow_int[u8](value)
		if ok {
			val = n
		}
	} $else $if T is u16 {
		n, ok := to_narrow_int[u16](value)
		if ok {
			val = n
		}
	} $else $if T is u32 {
		n, ok := to_narrow_int[u32](value)
		if ok {
			val = n
		}
	} $else $if T is i8 {
		n, ok := to_narrow_int[i8](value)
		if ok {
			val = n
		}
	} $else $if T is i16 {
		n, ok := to_narrow_int[i16](value)
		if ok {
			val = n
		}
	} $else $if T is i32 {
		n, ok := to_narrow_int[i32](value)
		if ok {
			val = n
		}
	} $else $if T is f32 {
		val = value.f32()
	} $else $if T is f64 {
		val = value.f64()
	} $else $if T is DateTime {
		val = value.datetime()
	} $else $if T is Date {
		val = value.date()
	} $else $if T is Time {
		val = value.time()
	} $else $if T is Any {
		val = value
	} $else $if T is $enum {
		val = unsafe { T(value.int()) }
	} $else $if T is $struct {
		mut inner := val or { T{} }
		decode_struct(value, mut inner)
		val = inner
	}
}

// to_narrow_int converts `value` to the narrow integer type `T` and reports
// whether `value` holds an integer that `T` can represent. V integer conversions
// are unchecked (`u8(300)` silently wraps to 44), so values outside the range of
// `T` are rejected instead of converted.
fn to_narrow_int[T](value Any) (T, bool) {
	n := any_to_checked_i64(value) or { return T(0), false }
	$if T is u8 {
		if n >= 0 && n <= 255 {
			return T(n), true
		}
	} $else $if T is u16 {
		if n >= 0 && n <= 65535 {
			return T(n), true
		}
	} $else $if T is u32 {
		if n >= 0 && n <= 4294967295 {
			return T(n), true
		}
	} $else $if T is i8 {
		if n >= -128 && n <= 127 {
			return T(n), true
		}
	} $else $if T is i16 {
		if n >= -32768 && n <= 32767 {
			return T(n), true
		}
	} $else $if T is i32 {
		if n >= -2147483648 && n <= 2147483647 {
			return T(n), true
		}
	}
	return T(0), false
}

// any_to_checked_i64 returns the integer held by `value`. Unlike `Any.i64()`, it
// does not turn unsupported values into 0: floats, booleans, `inf` and `-inf`
// (stored as `u64`), `nan`, tables, arrays and dates are rejected. Decimal strings
// are parsed, because older versions of `encode` wrote narrow integers as quoted
// strings (`port = "8080"`).
fn any_to_checked_i64(value Any) ?i64 {
	match value {
		i64 {
			return value
		}
		int {
			return i64(value)
		}
		string {
			if value == '' {
				return none
			}
			return strconv.parse_int(value, 10, 64) or { return none }
		}
		else {
			return none
		}
	}
}

// decode_narrow_int_array decodes `values` into a `[]T` of narrow integers,
// skipping the values that `T` cannot represent.
fn decode_narrow_int_array[T](values []Any) []T {
	mut arr := []T{cap: values.len}
	for value in values {
		n, ok := to_narrow_int[T](value)
		if ok {
			arr << n
		}
	}
	return arr
}

// enum_payload_int returns the number stored in `value` when `value` is a TOML
// integer, and none otherwise. `Any.int()` coerces booleans and floats and
// folds every other value to 0, so using it here would turn a mismatched
// element into an arbitrary enum member instead of skipping it.
fn enum_payload_int(value Any) ?int {
	return match value {
		int { int(value) }
		i64 { int(i64(value)) }
		else { none }
	}
}

// decode_enum_array decodes `values` into a `[]T` of enums. Enum values are
// stored as their number in TOML, like they are for a plain enum field. An
// element that is not an integer does not fit the element type and is skipped.
fn decode_enum_array[T](values []Any) []T {
	mut arr := []T{cap: values.len}
	for value in values {
		if n := enum_payload_int(value) {
			arr << unsafe { T(n) }
		}
	}
	return arr
}

// decode_array_element decodes the array `values` into the array `val`. The
// element type is taken from `val`, which is what makes an array of arrays
// decodable. Callers check that the source is an array, so that the scalar and
// table wrapping of `Any.array()` cannot be reached from here.
fn decode_array_element[T](mut val T, values []Any) {
	$if T is $array {
		val = decode_array(val, values)
	}
}

fn decode_array[T](current []T, values []Any) []T {
	$if T is string {
		return values.map(it.string())
	} $else $if T is bool {
		return values.map(it.bool())
	} $else $if T is int {
		return values.map(it.int())
	} $else $if T is i64 {
		return values.map(it.i64())
	} $else $if T is u64 {
		return values.map(it.u64())
	} $else $if T is u8 || T is u16 || T is u32 || T is i8 || T is i16 || T is i32 {
		return decode_narrow_int_array[T](values)
	} $else $if T is f32 {
		return values.map(it.f32())
	} $else $if T is f64 {
		return values.map(it.f64())
	} $else $if T is DateTime {
		return values.map(it.datetime())
	} $else $if T is Date {
		return values.map(it.date())
	} $else $if T is Time {
		return values.map(it.time())
	} $else $if T is Any {
		return values
	} $else $if T is $enum {
		return decode_enum_array[T](values)
	} $else $if T is $map {
		mut arr := []T{cap: values.len}
		for value in values {
			if value is map[string]Any {
				arr << decode_map(T{}, value)
			}
		}
		return arr
	} $else $if T is $array {
		mut arr := []T{cap: values.len}
		for value in values {
			// `Any.array()` wraps a scalar in a singleton array and turns a table
			// into its values, so the source shape is checked here instead.
			if value is []Any {
				mut item := T{}
				decode_array_element[T](mut item, value)
				arr << item
			}
		}
		return arr
	} $else $if T is $struct {
		mut decoded := []T{cap: values.len}
		for value in values {
			if value is map[string]Any {
				mut item := T{}
				decode_struct(value, mut item)
				decoded << item
			}
		}
		return decoded
	} $else {
		return current
	}
}

// decode_map preserves the actual key type rather than treating every map as
// a string-keyed map. Invalid keys are skipped like invalid narrow values.
fn decode_map[K, T](current map[K]T, values map[string]Any) map[K]T {
	mut decoded := map[K]T{}
	for key_string, value in values {
		key := decode_map_key[K](key_string) or { continue }
		$if T is string {
			decoded[key] = value.string()
		} $else $if T is bool {
			decoded[key] = value.bool()
		} $else $if T is int {
			decoded[key] = value.int()
		} $else $if T is i64 {
			decoded[key] = value.i64()
		} $else $if T is u64 {
			decoded[key] = value.u64()
		} $else $if T is u8 || T is u16 || T is u32 || T is i8 || T is i16 || T is i32 {
			n, ok := to_narrow_int[T](value)
			if ok { decoded[key] = n }
		} $else $if T is f32 {
			decoded[key] = value.f32()
		} $else $if T is f64 {
			decoded[key] = value.f64()
		} $else $if T is DateTime {
			decoded[key] = value.datetime()
		} $else $if T is Date {
			decoded[key] = value.date()
		} $else $if T is Time {
			decoded[key] = value.time()
		} $else $if T is Any {
			decoded[key] = value
		} $else $if T is $enum {
			// A value that is not an integer does not fit the element type, so the
			// key is left out rather than folded into an arbitrary enum member.
			if n := enum_payload_int(value) {
				decoded[key] = unsafe { T(n) }
			}
		} $else $if T is $map {
			if value is map[string]Any {
				mut item := decode_map(T{}, value)
				decoded[key] = item.move()
			}
		} $else $if T is $array {
			// See the matching branch in `decode_array`: a scalar or a table is
			// not an array element.
			if value is []Any {
				mut item := T{}
				decode_array_element[T](mut item, value)
				decoded[key] = item
			}
		} $else $if T is $struct {
			if value is map[string]Any {
				mut item := T{}
				decode_struct(value, mut item)
				decoded[key] = item
			}
		} $else {
			return current
		}
	}
	$if T is string || T is bool || T is int || T is i64 || T is u64 || T is u8 || T is u16 || T is u32 || T is i8 || T is i16 || T is i32 || T is f32 || T is f64 || T is Any || T is $enum || T is $array || T is $struct || T is $map {
		return decoded
	} $else {
		return current
	}
}

fn decode_map_key[K](text string) ?K {
	$if K is string {
		return K(text)
	} $else $if K is bool {
		if text == 'true' { return K(true) }
		if text == 'false' { return K(false) }
		return none
	} $else $if K is i128 || K is u128 {
		$compile_error('toml.decode: integer map keys wider than 64 bits are not supported')
	} $else $if K is $int {
		// Check the complete decimal spelling before parsing: strconv accepts an
		// empty string as zero, and unchecked casts can truncate large integers.
		start := if text.starts_with('-') || text.starts_with('+') { 1 } else { 0 }
		if text.len <= start { return none }
		for digit in text[start..].bytes() {
			if digit < `0` || digit > `9` { return none }
		}
		$if K is u8 || K is u16 || K is u32 || K is u64 || K is usize {
			if text.starts_with('-') { return none }
			unsigned_text := if text.starts_with('+') { text[1..] } else { text }
			n := strconv.common_parse_uint(unsigned_text, 10, int(sizeof(K) * 8), true, true) or { return none }
			return K(n)
		} $else {
			n := strconv.common_parse_int(text, 10, int(sizeof(K) * 8), true, true) or { return none }
			return K(n)
		}
	} $else {
		$compile_error('toml.decode: map keys must be strings, booleans, or integers')
	}
	return none
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

// get_option_payload unwraps an Option<T> known to be `Some`. Its signature
// exists so that V's generic inferrer picks up the inner T at the comptime call
// site, which is how `decode_option` reads a field's payload type.
fn get_option_payload[T](val ?T) T {
	return val or { T{} }
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
			$if field.is_embed {
				// Embedding is a V concept, TOML has no equivalent, so the fields of an
				// embedded struct are flattened into the encoding struct's table. Fields
				// of the embedding struct shadow same-named embedded fields.
				embedded_any := to_any(typ.$(field.name))
				if embedded_any is map[string]Any {
					for key, value in embedded_any {
						if key !in mp {
							mp[key] = value
						}
					}
				} else {
					// Scalar embeds (`DateTime`, `Date`, `Time`) and embedded types with a
					// custom `to_toml` method are kept as one value under the field name.
					mp[field_name] = embedded_any
				}
			} $else $if field.typ is $option {
				// TOML has no null, so a `none` field is left out of the table.
				opt_value := typ.$(field.name)
				if opt_value != none {
					mp[field_name] = to_any(get_option_payload(opt_value))
				}
			} $else {
				mp[field_name] = to_any(typ.$(field.name))
			}
		}
	}
	return mp
}

fn voidptr_to_toml_string[T](value T) string {
	ptr := unsafe { voidptr(&value) }
	return unsafe { '0x${ptr_str(*(&voidptr(ptr)))}' }
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
	} $else $if T is f32 {
		return Any(value)
	} $else $if T is f64 {
		return Any(value)
	} $else $if T is i64 {
		return Any(value)
	} $else $if T is int {
		return Any(value)
	} $else $if T is u64 {
		return Any(value)
	} $else $if T is u8 || T is u16 || T is u32 || T is i8 || T is i16 || T is i32 {
		// `Any` has no narrow integer variant, so these are widened to `i64`, which
		// represents all of their values.
		return Any(i64(value))
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
		if typeof(value).name == 'voidptr' {
			return Any(voidptr_to_toml_string(value))
		}
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
	return ast_to_any_(value)
}

fn ast_to_any_(value ast.Value) Any {
	match value {
		ast.Date {
			return Any(Date{value.text.clone()})
		}
		ast.Time {
			return Any(Time{value.text.clone()})
		}
		ast.DateTime {
			return Any(DateTime{value.text.clone()})
		}
		ast.Quoted {
			return Any(value.text.clone())
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
			str := value.text
			if str == 'true' {
				return Any(true)
			}
			return Any(false)
		}
		map[string]ast.Value {
			m := (value as map[string]ast.Value)
			mut am := map[string]Any{}
			for k, v in m {
				converted := ast_to_any_(v)
				am[k] = converted
			}
			return am
			// return d.get_map_value(m, key_split[1..].join('.'))
		}
		[]ast.Value {
			a := (value as []ast.Value)
			mut aa := []Any{cap: a.len}
			for val in a {
				converted := ast_to_any_(val)
				aa << converted
			}
			return aa
		}
		else {
			return null
		}
	}

	return null
	// TODO: decide this
	// panic(@MOD + '.' + @STRUCT + '.' + @FN + ' can\'t convert "${value}"')
	// return Any('')
}
