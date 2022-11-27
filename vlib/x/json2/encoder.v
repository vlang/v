// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import io
import strings

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
		[]int {
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
	} $else $if T is Null || T is bool || T is f32 || T is f64 || T is i8 || T is i16 || T is int
		|| T is i64 || T is u8 || T is u16 || T is u32 || T is u64 {
		e.encode_any(val, level, mut wr)!
	} $else $if T is Encodable {
		wr.write(val.json_str().bytes())!
	} $else $if T is []int {
		// wr.write(val.str)!
		e.encode_any(val, level, mut wr)!
	} $else $if T is $Struct {
		e.encode_struct(val, level, mut wr)!
	} $else $if T is $Enum {
		e.encode_any(Any(int(val)), level, mut wr)!
	} $else {
		// dump(val.str())
		return error('cannot encode value with ${typeof(val).name} type')
	}
}

fn (e &Encoder) encode_struct[U](val U, level int, mut wr io.Writer) ! {
	wr.write([u8(`{`)])!
	mut i := 0
	mut fields_len := 0
	$for _ in U.fields {
		fields_len++
	}
	$for field in U.fields {
		mut json_name := ''
		for attr in field.attrs {
			if attr.contains('json: ') {
				json_name = attr.replace('json: ', '')
				break
			}
		}
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
		if typeof(val.$(field.name)).name.contains('?') {
			if field.typ == 20 {
				if val.$(field.name).str() == 'Option(error: none)' {
					// TODO?
				} else {
					e.encode_string(val.$(field.name).str().replace("Option('", '').trim_string_right("')"), mut
						wr)!
				}
			}
		} else {
			match field.unaliased_typ {
				typeof([string]).idx {
					e.encode_string(val.$(field.name).str(), mut wr)!
				}
				typeof([int]).idx {
					wr.write(val.$(field.name).str().bytes())!
				}
				typeof([[]byte]).idx {
					//! array
					e.encode_array(val.$(field.name), level, mut wr)!
				}
				else {
					field_value := val.$(field.name)
					e.encode_value_with_level(field_value, level + 1, mut wr)!
				}
			}
		}
		if i < fields_len - 1 {
			wr.write(json2.comma_bytes)!
		}
		i++
	}
	e.encode_newline(level - 1, mut wr)!
	wr.write([u8(`}`)])!
}

fn (e &Encoder) encode_array[U](val U, level int, mut wr io.Writer) ! {
	$if U is $Array {
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
	} $else {
		return error('encoded array value is not an array')
	}
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
		newline_spaces_count: 4
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
