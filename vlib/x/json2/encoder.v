// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import io
import strings
import time

// Encoder encodes the an `Any` type into JSON representation.
// It provides parameters in order to change the end result.
pub struct Encoder {
	newline              u8
	newline_spaces_count int
	escape_unicode       bool = true
}

pub const default_encoder = Encoder{}

// byte array versions of the most common tokens/chars to avoid reallocations
const null_in_bytes = 'null'.bytes()

const true_in_bytes = 'true'.bytes()

const false_in_bytes = 'false'.bytes()

const zero_in_bytes = [u8(`0`)]

const comma_bytes = [u8(`,`)]

const colon_bytes = [u8(`:`)]

const space_bytes = [u8(` `)]

const unicode_escape_chars = [u8(`\\`), `u`]

const quote_bytes = [u8(`"`)]

const escaped_chars = [(r'\b').bytes(), (r'\f').bytes(), (r'\n').bytes(),
	(r'\r').bytes(), (r'\t').bytes()]

// encode_value encodes a value to the specific writer.
pub fn (e &Encoder) encode_value[T](val T, mut wr io.Writer) ! {
	e.encode_value_with_level[T](val, 1, mut wr)!
}

fn (e &Encoder) encode_newline(level int, mut wr io.Writer) ! {
	if e.newline != 0 {
		wr.write([e.newline])!
		for j := 0; j < level * e.newline_spaces_count; j++ {
			wr.write(json2.space_bytes)!
		}
	}
}

fn (e &Encoder) encode_any(val Any, level int, mut wr io.Writer) ! {
	match val {
		string {
			e.encode_string(val, mut wr)!
		}
		bool {
			if val == true {
				wr.write(json2.true_in_bytes)!
			} else {
				wr.write(json2.false_in_bytes)!
			}
		}
		i8, i16, int, i64 {
			wr.write(val.str().bytes())!
		}
		u8, u16, u32, u64 {
			wr.write(val.str().bytes())!
		}
		f32, f64 {
			$if !nofloat ? {
				str_float := val.str().bytes()
				wr.write(str_float)!
				if str_float[str_float.len - 1] == `.` {
					wr.write(json2.zero_in_bytes)!
				}
				return
			}
			wr.write(json2.zero_in_bytes)!
		}
		map[string]Any {
			wr.write([u8(`{`)])!
			mut i := 0
			for k, v in val {
				e.encode_newline(level, mut wr)!
				e.encode_string(k, mut wr)!
				wr.write(json2.colon_bytes)!
				if e.newline != 0 {
					wr.write(json2.space_bytes)!
				}
				e.encode_value_with_level(v, level + 1, mut wr)!
				if i < val.len - 1 {
					wr.write(json2.comma_bytes)!
				}
				i++
			}
			e.encode_newline(level - 1, mut wr)!
			wr.write([u8(`}`)])!
		}
		[]Any {
			wr.write([u8(`[`)])!
			for i in 0 .. val.len {
				e.encode_newline(level, mut wr)!
				e.encode_value_with_level(val[i], level + 1, mut wr)!
				if i < val.len - 1 {
					wr.write(json2.comma_bytes)!
				}
			}
			e.encode_newline(level - 1, mut wr)!
			wr.write([u8(`]`)])!
		}
		time.Time {}
		Null {
			wr.write(json2.null_in_bytes)!
		}
	}
}

fn (e &Encoder) encode_value_with_level[T](val T, level int, mut wr io.Writer) ! {
	$if T is string {
		e.encode_string(val, mut wr)!
	} $else $if T is Any {
		e.encode_any(val, level, mut wr)!
	} $else $if T is map[string]Any {
		// weird quirk but val is destructured immediately to Any
		e.encode_any(val, level, mut wr)!
	} $else $if T is []Any {
		e.encode_any(val, level, mut wr)!
	} $else $if T is Encodable {
		wr.write(val.json_str().bytes())!
	} $else $if T is $struct {
		e.encode_struct(val, level, mut wr)!
	} $else $if T is $enum {
		e.encode_any(Any(int(val)), level, mut wr)!
	} $else $if T in [Null, bool, $Float, $Int] {
		e.encode_any(val, level, mut wr)!
	} $else {
		// dump(val.str())
		return error('cannot encode value with ${typeof(val).name} type')
	}
}

fn (e &Encoder) encode_struct[U](val U, level int, mut wr io.Writer) ! {
	wr.write([u8(`{`)])!
	mut i := 0
	mut fields_len := 0
	$for field in U.fields {
		if val.$(field.name).str() != 'Option(error: none)' {
			fields_len++
		}
	}
	$for field in U.fields {
		mut ignore_field := false
		value := val.$(field.name)
		mut json_name := ''
		for attr in field.attrs {
			if attr.contains('json: ') {
				json_name = attr.replace('json: ', '')
				break
			}
		}

		$if field.is_option {
			is_none := value.str() == 'Option(error: none)'

			if !is_none {
				e.encode_newline(level, mut wr)!
				if json_name != '' {
					e.encode_string(json_name, mut wr)!
				} else {
					e.encode_string(field.name, mut wr)!
				}
				wr.write(json2.colon_bytes)!

				if e.newline != 0 {
					wr.write(json2.space_bytes)!
				}

				$if field.typ is ?string {
					e.encode_string(val.$(field.name) ?.str()#[8..-2], mut wr)!
				} $else $if field.typ is ?bool || field.typ is ?f32 || field.typ is ?f64
					|| field.typ is ?i8 || field.typ is ?i16 || field.typ is ?int
					|| field.typ is ?i64 || field.typ is ?u8 || field.typ is ?u16
					|| field.typ is ?u32 || field.typ is ?u64 {
					wr.write(val.$(field.name) ?.str()#[7..-1].bytes())!
				} $else $if field.typ is ?time.Time {
					option_value := val.$(field.name) as ?time.Time
					parsed_time := option_value as time.Time
					e.encode_string(parsed_time.format_rfc3339(), mut wr)!
				} $else $if field.is_array {
					e.encode_array(value, level + 1, mut wr)!
				} $else $if field.is_struct {
					e.encode_struct(value, level + 1, mut wr)!
				} $else $if field.is_enum {
					// FIXME - checker and cast error
					// wr.write(int(val.$(field.name)?).str().bytes())!
					return error('type ${typeof(val).name} cannot be encoded yet')
				} $else $if field.is_alias {
					match field.unaliased_typ {
						typeof[string]().idx {
							e.encode_string(value.str(), mut wr)!
						}
						typeof[bool]().idx, typeof[f32]().idx, typeof[f64]().idx, typeof[i8]().idx,
						typeof[i16]().idx, typeof[int]().idx, typeof[i64]().idx, typeof[u8]().idx,
						typeof[u16]().idx, typeof[u32]().idx, typeof[u64]().idx {
							wr.write(value.str().bytes())!
						}
						typeof[[]byte]().idx, typeof[[]int]().idx {
							// FIXME - error: could not infer generic type `U` in call to `encode_array`
							// e.encode_array(value, level, mut wr)!
						}
						else {
							// e.encode_value_with_level(value, level + 1, mut wr)!
						}
					}
				} $else {
					return error('type ${typeof(val).name} cannot be array encoded')
				}
			} else {
				ignore_field = true
			}
		} $else {
			is_none := val.$(field.name).str() == 'unknown sum type value'
			if !is_none {
				e.encode_newline(level, mut wr)!
				if json_name != '' {
					e.encode_string(json_name, mut wr)!
				} else {
					e.encode_string(field.name, mut wr)!
				}
				wr.write(json2.colon_bytes)!

				if e.newline != 0 {
					wr.write(json2.space_bytes)!
				}
			}

			$if field.typ is string {
				e.encode_string(val.$(field.name).str(), mut wr)!
			} $else $if field.typ is time.Time {
				wr.write(json2.quote_bytes)!
				wr.write(val.$(field.name).format_rfc3339().bytes())!
				wr.write(json2.quote_bytes)!
			} $else $if field.typ in [bool, $Float, $Int] {
				wr.write(val.$(field.name).str().bytes())!
			} $else $if field.is_array {
				// TODO - replace for `field.typ is $array`
				e.encode_array(value, level + 1, mut wr)!
			} $else $if field.typ is $array {
				// e.encode_array(value, level + 1, mut wr)! // FIXME - error: could not infer generic type `U` in call to `encode_array`
			} $else $if field.typ is $struct {
				e.encode_struct(value, level + 1, mut wr)!
			} $else $if field.is_enum {
				// TODO - replace for `field.typ is $enum`
				wr.write(int(val.$(field.name)).str().bytes())!
			} $else $if field.typ is $enum {
				// wr.write(int(val.$(field.name)).str().bytes())! // FIXME - error: cannot cast string to `int`, use `val.$field.name.int()` instead.
			} $else $if field.typ is $sumtype {
				// dump(val.$(field.name).str())
				// dump(is_none)
				sum_type_value := value.str()#[typeof(val.$(field.name)).name.len + 1..-1]

				is_string := sum_type_value[0] == "'"[0]

				// mut is_struct := false
				// mut is_sumtype := false
				// mut is_enum := false
				// mut is_array := false

				match sum_type_value[0] {
					`0`...`9` {
						if sum_type_value.contains_any(' /:-') {
							date_time_str := time.parse(sum_type_value)!
							wr.write(date_time_str.format_rfc3339().bytes())!
						} else {
							wr.write(sum_type_value.bytes())!
						}
					}
					`A`...`Z` {
						// SumTypes(0)
						if sum_type_value.contains('(') {
							if !sum_type_value.all_before('(').contains_any(' "\'[') {
								// is_sumtype = true
							}
						}
						// StructType{
						// StructType[int]{
						if sum_type_value.contains('{') {
							if !sum_type_value.all_before('{').contains_any(' "\'') {
								// is_struct = true
								// TODO
								// e.encode_struct_from_sumtype(value, level + 1, mut wr)!
							}
						}
					}
					`a`...`z` {
						if sum_type_value in ['true', 'false'] {
							wr.write(sum_type_value.bytes())!
						} else {
							// is_enum = true
						}
					}
					else {
						// dump('else')
					}
				}
				// dump(sum_type_value)

				// dump(is_none)
				// dump(is_string)
				// dump(is_struct)
				// dump(is_sumtype)
				// dump(is_enum)
				// dump(is_array)
				if is_string {
					e.encode_string(sum_type_value#[1..-1], mut wr)!
				}
			} $else $if field.typ is $alias {
				$if field.unaliased_typ is string {
					e.encode_string(val.$(field.name).str(), mut wr)!
				} $else $if field.unaliased_typ is time.Time {
					parsed_time := time.parse(val.$(field.name).str()) or { time.Time{} }
					e.encode_string(parsed_time.format_rfc3339(), mut wr)!
				} $else $if field.unaliased_typ in [bool, $Float, $Int] {
					wr.write(val.$(field.name).str().bytes())!
				} $else $if field.unaliased_typ is $array {
					// e.encode_array(val.$(field.name), level + 1, mut wr)! // FIXME - error: could not infer generic type `U` in call to `encode_array`
				} $else $if field.unaliased_typ is $struct {
					// e.encode_struct(val.$(field.name), level + 1, mut wr)! // FIXME - error: cannot use `BoolAlias` as `StringAlias` in argument 1 to `x.json2.Encoder.encode_struct`
					e.encode_struct(value, level + 1, mut wr)!
				} $else $if field.unaliased_typ is $enum {
					// enum_value := val.$(field.name)
					// dump(int(val.$(field.name))) // FIXME
					// dump(val.$(field.name).int()) // FIXME - error: unknown method or field: `BoolAlias.int`
					// dump(val.$(field.name).int()) // FIXME - error: cannot convert 'enum <anonymous>' to 'struct string'

					// wr.write(val.$(field.name).int().str().bytes())! // FIXME - error: unknown method or field: `BoolAlias.int`
				} $else $if field.unaliased_typ is $sumtype {
				} $else {
					return error('the alias ${typeof(val).name} cannot be encoded')
				}
			} $else {
				return error('type ${typeof(val).name} cannot be array encoded')
			}
		}

		if i < fields_len - 1 && !ignore_field {
			wr.write(json2.comma_bytes)!
		}
		if !ignore_field {
			i++
		}
	}
	e.encode_newline(level - 1, mut wr)!
	wr.write([u8(`}`)])!
}

fn (e &Encoder) encode_array[U](val []U, level int, mut wr io.Writer) ! {
	wr.write([u8(`[`)])!
	for i in 0 .. val.len {
		e.encode_newline(level, mut wr)!

		$if U is string {
			e.encode_any(val[i], level + 1, mut wr)!
		} $else $if U is bool {
			e.encode_any(bool(val[i]), level + 1, mut wr)!
		} $else $if U is f32 {
			e.encode_any(f32(val[i]), level + 1, mut wr)!
		} $else $if U is f64 {
			e.encode_any(f64(val[i]), level + 1, mut wr)!
		} $else $if U is i8 {
			e.encode_any(i8(val[i]), level + 1, mut wr)!
		} $else $if U is i16 {
			e.encode_any(i16(val[i]), level + 1, mut wr)!
		} $else $if U is int {
			e.encode_any(int(val[i]), level + 1, mut wr)!
		} $else $if U is i64 {
			e.encode_any(i64(val[i]), level + 1, mut wr)!
		} $else $if U is u8 {
			e.encode_any(u8(val[i]), level + 1, mut wr)!
		} $else $if U is byte {
			e.encode_any(u8(val[i]), level + 1, mut wr)!
		} $else $if U is u16 {
			e.encode_any(u16(val[i]), level + 1, mut wr)!
		} $else $if U is u32 {
			e.encode_any(u32(val[i]), level + 1, mut wr)!
		} $else $if U is u64 {
			e.encode_any(u64(val[i]), level + 1, mut wr)!
		} $else $if U is $array {
			// FIXME - error: could not infer generic type `U` in call to `encode_array`
			// e.encode_array(val[i], level + 1, mut wr)!
		} $else $if U is $struct {
			e.encode_struct(val[i], level + 1, mut wr)!
		} $else $if U is $sumtype {
			$if U is Any {
				e.encode_any(val[i], level + 1, mut wr)!
			} $else {
				// TODO
			}
		} $else $if U is $enum {
			e.encode_any(i64(val[i]), level + 1, mut wr)!
		} $else {
			return error('type ${typeof(val).name} cannot be array encoded')
		}
		if i < val.len - 1 {
			wr.write(json2.comma_bytes)!
		}
	}

	e.encode_newline(level - 1, mut wr)!
	wr.write([u8(`]`)])!
}

// str returns the JSON string representation of the `map[string]Any` type.
pub fn (f map[string]Any) str() string {
	return Any(f).json_str()
}

// str returns the JSON string representation of the `[]Any` type.
pub fn (f []Any) str() string {
	return Any(f).json_str()
}

// str returns the string representation of the `Any` type. Use the `json_str` method
// if you want to use the escaped str() version of the `Any` type.
pub fn (f Any) str() string {
	if f is string {
		return f
	} else {
		return f.json_str()
	}
}

// json_str returns the JSON string representation of the `Any` type.
[manualfree]
pub fn (f Any) json_str() string {
	return encode(f)
}

// prettify_json_str returns the pretty-formatted JSON string representation of the `Any` type.
[manualfree]
pub fn (f Any) prettify_json_str() string {
	mut sb := strings.new_builder(4096)
	defer {
		unsafe { sb.free() }
	}
	mut enc := Encoder{
		newline: `\n`
		newline_spaces_count: 2
	}
	enc.encode_value(f, mut sb) or {}
	return sb.str()
}

// CharLengthIterator is an iterator that generates a char
// length value of every iteration based on the given text.
// (e.g.: "t✔" => [t => 1, ✔ => 2])
struct CharLengthIterator {
	text string
mut:
	idx int
}

fn (mut iter CharLengthIterator) next() ?int {
	if iter.idx >= iter.text.len {
		return none
	}
	defer {
		iter.idx++
	}
	mut len := 1
	c := iter.text[iter.idx]
	if (c & (1 << 7)) != 0 {
		for t := u8(1 << 6); (c & t) != 0; t >>= 1 {
			len++
			iter.idx++
		}
	}
	return len
}

// TODO - Need refactor. Is so slow. The longer the string, the lower the performance.
// encode_string returns the JSON spec-compliant version of the string.
[manualfree]
fn (e &Encoder) encode_string(s string, mut wr io.Writer) ! {
	mut char_lens := CharLengthIterator{
		text: s
	}
	mut i := 0
	wr.write(json2.quote_bytes)!
	for char_len in char_lens {
		if char_len == 1 {
			chr := s[i]
			if chr in important_escapable_chars {
				for j := 0; j < important_escapable_chars.len; j++ {
					if chr == important_escapable_chars[j] {
						wr.write(json2.escaped_chars[j])!
						break
					}
				}
			} else if chr == `"` || chr == `/` || chr == `\\` {
				wr.write([u8(`\\`), chr])!
			} else if int(chr) < 0x20 {
				hex_code := chr.hex().bytes()
				wr.write(json2.unicode_escape_chars)! // \u
				wr.write(json2.zero_in_bytes)! // \u0
				wr.write(json2.zero_in_bytes)! // \u00
				wr.write(hex_code)! // \u00xxxx
			} else {
				wr.write([u8(chr)])!
			}
		} else {
			slice := s[i..i + char_len]
			hex_code := slice.utf32_code().hex().bytes()
			if !e.escape_unicode || hex_code.len < 4 {
				// unescaped non-ASCII char
				wr.write(slice.bytes())!
			} else if hex_code.len == 4 {
				// a unicode endpoint
				wr.write(json2.unicode_escape_chars)!
				wr.write(hex_code)!
			} else {
				// TODO: still figuring out what
				// to do with more than 4 chars
				wr.write(json2.space_bytes)!
			}
			unsafe {
				slice.free()
				hex_code.free()
			}
		}
		i += char_len
	}

	wr.write(json2.quote_bytes)!
}
