// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import time

// Encoder encodes the an `Any` type into JSON representation.
// It provides parameters in order to change the end result.
pub struct Encoder {
	newline              u8
	newline_spaces_count int
	escape_unicode       bool = true
}

// byte array versions of the most common tokens/chars to avoid reallocations
const null_in_bytes = 'null'

const true_in_string = 'true'

const false_in_string = 'false'

const zero_rune = `0`

const comma_rune = `,`

const colon_rune = `:`

const unicode_escape_chars = `\\u`

const quote_rune = `"`

const escaped_chars = [(r'\b'), (r'\f'), (r'\n'), (r'\r'), (r'\t')]!

const back_slash = [u8(`\\`), `\\`]!

const quote = [u8(`\\`), `"`]!

const slash = [u8(`\\`), `/`]!

const ascii_control_characters = ['\\u0000', '\\t', '\\n', '\\r', '\\u0004', '\\u0005', '\\u0006',
	'\\u0007', '\\b', '\\t', '\\n', '\\v', '\\f', '\\r', '\\u000e', '\\u000f', '\\u0010', '\\u0011',
	'\\u0012', '\\u0013', '\\u0014', '\\u0015', '\\u0016', '\\u0017', '\\u0018', '\\u0019', '\\u001a',
	'\\u001b', '\\u001c', '\\u001d', '\\u001e', '\\u001f']!

const curly_open_rune = `{`

const curly_close_rune = `}`

// vfmt off
const g_digits_lut = [
`0`,`0`, `0`,`1`, `0`,`2`, `0`,`3`, `0`,`4`, `0`,`5`, `0`,`6`, `0`,`7` ,`0`,`8`, `0`,`9`, `0`,`a`, `0`,`b`, `0`,`c`, `0`,`d`, `0`,`e`, `0`,`f`,
`1`,`0`, `1`,`1`, `1`,`2`, `1`,`3`, `1`,`4`, `1`,`5`, `1`,`6`, `1`,`7` ,`1`,`8`, `1`,`9`, `1`,`a`, `1`,`b`, `1`,`c`, `1`,`d`, `1`,`e`, `1`,`f`,
`2`,`0`, `2`,`1`, `2`,`2`, `2`,`3`, `2`,`4`, `2`,`5`, `2`,`6`, `2`,`7` ,`2`,`8`, `2`,`9`, `2`,`a`, `2`,`b`, `2`,`c`, `2`,`d`, `2`,`e`, `2`,`f`,
`3`,`0`, `3`,`1`, `3`,`2`, `3`,`3`, `3`,`4`, `3`,`5`, `3`,`6`, `3`,`7` ,`3`,`8`, `3`,`9`, `3`,`a`, `3`,`b`, `3`,`c`, `3`,`d`, `3`,`e`, `3`,`f`,
`4`,`0`, `4`,`1`, `4`,`2`, `4`,`3`, `4`,`4`, `4`,`5`, `4`,`6`, `4`,`7` ,`4`,`8`, `4`,`9`, `4`,`a`, `4`,`b`, `4`,`c`, `4`,`d`, `4`,`e`, `4`,`f`,
`5`,`0`, `5`,`1`, `5`,`2`, `5`,`3`, `5`,`4`, `5`,`5`, `5`,`6`, `5`,`7` ,`5`,`8`, `5`,`9`, `5`,`a`, `5`,`b`, `5`,`c`, `5`,`d`, `5`,`e`, `5`,`f`,
`6`,`0`, `6`,`1`, `6`,`2`, `6`,`3`, `6`,`4`, `6`,`5`, `6`,`6`, `6`,`7` ,`6`,`8`, `6`,`9`, `6`,`a`, `6`,`b`, `6`,`c`, `6`,`d`, `6`,`e`, `6`,`f`,
`7`,`0`, `7`,`1`, `7`,`2`, `7`,`3`, `7`,`4`, `7`,`5`, `7`,`6`, `7`,`7` ,`7`,`8`, `7`,`9`, `7`,`a`, `7`,`b`, `7`,`c`, `7`,`d`, `7`,`e`, `7`,`f`,
`8`,`0`, `8`,`1`, `8`,`2`, `8`,`3`, `8`,`4`, `8`,`5`, `8`,`6`, `8`,`7` ,`8`,`8`, `8`,`9`, `8`,`a`, `8`,`b`, `8`,`c`, `8`,`d`, `8`,`e`, `8`,`f`,
`9`,`0`, `9`,`1`, `9`,`2`, `9`,`3`, `9`,`4`, `9`,`5`, `9`,`6`, `9`,`7` ,`9`,`8`, `9`,`9`, `9`,`a`, `9`,`b`, `9`,`c`, `9`,`d`, `9`,`e`, `9`,`f`,
`a`,`0`, `a`,`1`, `a`,`2`, `a`,`3`, `a`,`4`, `a`,`5`, `a`,`6`, `a`,`7` ,`a`,`8`, `a`,`9`, `a`,`a`, `a`,`b`, `a`,`c`, `a`,`d`, `a`,`e`, `a`,`f`,
`b`,`0`, `b`,`1`, `b`,`2`, `b`,`3`, `b`,`4`, `b`,`5`, `b`,`6`, `b`,`7` ,`b`,`8`, `b`,`9`, `b`,`a`, `b`,`b`, `b`,`c`, `b`,`d`, `b`,`e`, `b`,`f`,
`c`,`0`, `c`,`1`, `c`,`2`, `c`,`3`, `c`,`4`, `c`,`5`, `c`,`6`, `c`,`7` ,`c`,`8`, `c`,`9`, `c`,`a`, `c`,`b`, `c`,`c`, `c`,`d`, `c`,`e`, `c`,`f`,
`d`,`0`, `d`,`1`, `d`,`2`, `d`,`3`, `d`,`4`, `d`,`5`, `d`,`6`, `d`,`7` ,`d`,`8`, `d`,`9`, `d`,`a`, `d`,`b`, `d`,`c`, `d`,`d`, `d`,`e`, `d`,`f`,
`e`,`0`, `e`,`1`, `e`,`2`, `e`,`3`, `e`,`4`, `e`,`5`, `e`,`6`, `e`,`7` ,`e`,`8`, `e`,`9`, `e`,`a`, `e`,`b`, `e`,`c`, `e`,`d`, `e`,`e`, `e`,`f`,
`f`,`0`, `f`,`1`, `f`,`2`, `f`,`3`, `f`,`4`, `f`,`5`, `f`,`6`, `f`,`7` ,`f`,`8`, `f`,`9`, `f`,`a`, `f`,`b`, `f`,`c`, `f`,`d`, `f`,`e`, `f`,`f`
]!
// vfmt on

// Pre-computed lookup tables for UTF-8 encoding
const g_ascii_lookup_table = [u8(0), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
	18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, `"`, 35, 36, 37, 38, 39, 40,
	41, 42, 43, 44, 45, 46, `/`, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
	64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
	87, 88, 89, 90, 91, `\\`, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107,
	108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126,
	127, 128]!

// const g_multibyte_utf8_lengths = [
// 	u8(0), // Unused (1-byte characters have length 1)
// 	1,
// 	2, // Length of 2-byte UTF-8 sequences
// 	3, // Length of 3-byte UTF-8 sequences
// 	4, // Length of 4-byte UTF-8 sequences
// ]!

// encode is a generic function that encodes a type into a JSON string.
pub fn encode[T](val T) string {
	$if T is $array {
		return encode_array(val)
	} $else {
		mut buf := []u8{}

		defer {
			unsafe { buf.free() }
		}
		encoder := Encoder{}

		encoder.encode_value(val, mut buf) or {
			println(err)
			encoder.encode_value[string]('null', mut buf) or {}
		}

		return buf.bytestr()
	}
}

// encode_array is a generic function that encodes a array into a JSON string.
fn encode_array[T](val []T) string {
	mut buf := []u8{}

	defer {
		unsafe { buf.free() }
	}

	encoder := Encoder{}
	encoder.encode_array(val, 1, mut buf) or {
		println(err)
		encoder.encode_value[string]('null', mut buf) or {}
	}

	return buf.bytestr()
}

// encode_pretty ...
pub fn encode_pretty[T](typed_data T) string {
	encoded := encode(typed_data)
	raw_decoded := raw_decode(encoded) or { 0 }
	return raw_decoded.prettify_json_str()
}

// encode_value encodes a value to the specific buffer.
pub fn (e &Encoder) encode_value[T](val T, mut buf []u8) ! {
	e.encode_value_with_level[T](val, 1, mut buf)!
}

fn (e &Encoder) encode_newline(level int, mut buf []u8) ! {
	if e.newline != 0 {
		buf << e.newline
		for j := 0; j < level * e.newline_spaces_count; j++ {
			buf << ` `
		}
	}
}

fn (e &Encoder) encode_map[T](value T, level int, mut buf []u8) ! {
	buf << json2.curly_open_rune
	mut idx := 0
	for k, v in value {
		e.encode_newline(level, mut buf)!
		// e.encode_string(k.str(), mut buf)!
		e.encode_string(k, mut buf)!
		buf << json2.colon_rune
		if e.newline != 0 {
			buf << ` `
		}

		// workaround to avoid `cannot convert 'struct x__json2__Any' to 'struct string'`
		$if v is $sumtype {
			$for variant_value in v.variants {
				if v is variant_value {
					e.encode_value_with_level(v, level + 1, mut buf)!
				}
			}
		} $else {
			e.encode_value_with_level(v, level + 1, mut buf)!
		}

		if idx < value.len - 1 {
			buf << json2.comma_rune
		}
		idx++
	}
	// e.encode_newline(level, mut buf)!
	e.encode_newline(level - 1, mut buf)!
	buf << json2.curly_close_rune
}

fn (e &Encoder) encode_value_with_level[T](val T, level int, mut buf []u8) ! {
	$if val is $option {
		workaround := val
		if workaround != none {
			e.encode_value_with_level(val, level, mut buf)!
		}
	} $else $if T is string {
		e.encode_string(val, mut buf)!
	} $else $if T is $sumtype {
		$for v in val.variants {
			if val is v {
				// dump(val)
				// dump(typeof(val).name)
				e.encode_value_with_level(val, level, mut buf)!
			}
		}
	} $else $if T is $alias {
		// TODO
	} $else $if T is time.Time {
		str_value := val.format_rfc3339()
		buf << json2.quote_rune
		unsafe { buf.push_many(str_value.str, str_value.len) }
		buf << json2.quote_rune
	} $else $if T is $map {
		e.encode_map(val, level, mut buf)!
	} $else $if T is $array {
		e.encode_array(val, level, mut buf)!
	} $else $if T is Encodable {
		str_value := val.json_str()
		unsafe { buf.push_many(str_value.str, str_value.len) }
	} $else $if T is Null {
		unsafe { buf.push_many(json2.null_in_bytes.str, json2.null_in_bytes.len) }
	} $else $if T is $struct {
		e.encode_struct(val, level, mut buf)!
	} $else $if T is $enum {
		str_int := int(val).str()
		unsafe { buf.push_many(str_int.str, str_int.len) }
	} $else $if T is $int || T is $float || T is bool {
		str_int := val.str()
		unsafe { buf.push_many(str_int.str, str_int.len) }
	} $else {
		return error('cannot encode value with ${typeof(val).name} type')
	}
}

fn (e &Encoder) encode_struct[U](val U, level int, mut buf []u8) ! {
	buf << json2.curly_open_rune
	mut i := 0
	mut fields_len := 0

	$for field in U.fields {
		mut @continue := false
		for attr in field.attrs {
			if attr.contains('json: ') {
				if attr.replace('json: ', '') == '-' {
					@continue = true
				}
				break
			}
		}
		if !@continue {
			$if field.is_option {
				if val.$(field.name) != none {
					fields_len++
				}
			} $else {
				fields_len++
			}
		}
	}
	$for field in U.fields {
		mut ignore_field := false

		value := val.$(field.name)

		is_nil := val.$(field.name).str() == '&nil'

		mut json_name := ''

		for attr in field.attrs {
			if attr.contains('json: ') {
				json_name = attr.replace('json: ', '')
				if json_name == '-' {
					ignore_field = true
				}
				break
			}
		}

		if !ignore_field {
			$if value is $option {
				workaround := val.$(field.name)
				if workaround != none { // smartcast
					e.encode_newline(level, mut buf)!
					if json_name != '' {
						e.encode_string(json_name, mut buf)!
					} else {
						e.encode_string(field.name, mut buf)!
					}
					buf << json2.colon_rune

					if e.newline != 0 {
						buf << ` `
					}
					e.encode_value_with_level(value, level, mut buf)!
				} else {
					ignore_field = true
				}
			} $else {
				is_none := val.$(field.name).str() == 'unknown sum type value' // assert json.encode(StructType[SumTypes]{}) == '{}'
				if !is_none && !is_nil {
					e.encode_newline(level, mut buf)!
					if json_name != '' {
						e.encode_string(json_name, mut buf)!
					} else {
						e.encode_string(field.name, mut buf)!
					}
					buf << json2.colon_rune

					if e.newline != 0 {
						buf << ` `
					}
				}

				$if field.indirections != 0 {
					if val.$(field.name) != unsafe { nil } {
						$if field.indirections == 1 {
							e.encode_value_with_level(*val.$(field.name), level + 1, mut
								buf)!
						}
						$if field.indirections == 2 {
							e.encode_value_with_level(**val.$(field.name), level + 1, mut
								buf)!
						}
						$if field.indirections == 3 {
							e.encode_value_with_level(***val.$(field.name), level + 1, mut
								buf)!
						}
					}
				} $else $if field.typ is string {
					e.encode_string(val.$(field.name).str(), mut buf)!
				} $else $if field.typ is time.Time {
					str_value := val.$(field.name).format_rfc3339()
					buf << json2.quote_rune
					unsafe { buf.push_many(str_value.str, str_value.len) }
					buf << json2.quote_rune
				} $else $if field.typ is bool {
					if value {
						unsafe { buf.push_many(json2.true_in_string.str, json2.true_in_string.len) }
					} else {
						unsafe { buf.push_many(json2.false_in_string.str, json2.false_in_string.len) }
					}
				} $else $if field.typ in [$float, $int] {
					str_value := val.$(field.name).str()
					unsafe { buf.push_many(str_value.str, str_value.len) }
				} $else $if field.is_array {
					// TODO - replace for `field.typ is $array`
					e.encode_array(value, level + 1, mut buf)!
				} $else $if field.typ is $array {
					// e.encode_array(value, level + 1, mut buf)! // FIXME - error: could not infer generic type `U` in call to `encode_array`
				} $else $if field.typ is $struct {
					e.encode_struct(value, level + 1, mut buf)!
				} $else $if field.is_map {
					e.encode_map(value, level + 1, mut buf)!
				} $else $if field.is_enum {
					// TODO - replace for `field.typ is $enum`
					// str_value := int(val.$(field.name)).str()
					// unsafe { buf.push_many(str_value.str, str_value.len) }
					e.encode_value_with_level(val.$(field.name), level + 1, mut buf)!
				} $else $if field.typ is $enum {
				} $else $if field.typ is $sumtype {
					field_value := val.$(field.name)
					if field_value.str() != 'unknown sum type value' {
						$for v in field_value.variants {
							if field_value is v {
								e.encode_value_with_level(field_value, level, mut buf)!
							}
						}
					}
				} $else $if field.typ is $alias {
					$if field.unaliased_typ is string {
						e.encode_string(val.$(field.name).str(), mut buf)!
					} $else $if field.unaliased_typ is time.Time {
						parsed_time := time.parse(val.$(field.name).str()) or { time.Time{} }
						e.encode_string(parsed_time.format_rfc3339(), mut buf)!
					} $else $if field.unaliased_typ is bool {
						if val.$(field.name).str() == json2.true_in_string {
							unsafe { buf.push_many(json2.true_in_string.str, json2.true_in_string.len) }
						} else {
							unsafe { buf.push_many(json2.false_in_string.str, json2.false_in_string.len) }
						}
					} $else $if field.unaliased_typ in [$float, $int] {
						str_value := val.$(field.name).str()
						unsafe { buf.push_many(str_value.str, str_value.len) }
					} $else $if field.unaliased_typ is $array {
						// TODO
					} $else $if field.unaliased_typ is $struct {
						e.encode_struct(value, level + 1, mut buf)!
					} $else $if field.unaliased_typ is $enum {
						// TODO
					} $else $if field.unaliased_typ is $sumtype {
						// TODO
					} $else {
						return error('the alias ${typeof(val).name} cannot be encoded')
					}
				} $else {
					return error('type ${typeof(val).name} cannot be array encoded')
				}
			}
		}

		if i < fields_len - 1 && !ignore_field {
			if !is_nil {
				buf << json2.comma_rune
			}
		}
		if !ignore_field {
			i++
		}
	}
	e.encode_newline(level - 1, mut buf)!
	buf << json2.curly_close_rune
	// b.measure('encode_struct')
}

fn (e &Encoder) encode_array[U](val []U, level int, mut buf []u8) ! {
	buf << `[`
	for i in 0 .. val.len {
		e.encode_newline(level, mut buf)!

		$if U is string || U is bool || U is $int || U is $float {
			e.encode_value_with_level(val[i], level + 1, mut buf)!
		} $else $if U is $array {
			e.encode_array(val[i], level + 1, mut buf)!
		} $else $if U is $struct {
			e.encode_struct(val[i], level + 1, mut buf)!
		} $else $if U is $sumtype {
			e.encode_value_with_level(val[i], level + 1, mut buf)!
		} $else $if U is $enum {
			// TODO test
			e.encode_value_with_level(val[i], level + 1, mut buf)!
		} $else {
			return error('type ${typeof(val).name} cannot be array encoded')
		}
		if i < val.len - 1 {
			buf << json2.comma_rune
		}
	}

	e.encode_newline(level - 1, mut buf)!
	buf << `]`
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
@[manualfree]
pub fn (f Any) json_str() string {
	return encode(f)
}

// prettify_json_str returns the pretty-formatted JSON string representation of the `Any` type.
@[manualfree]
pub fn (f Any) prettify_json_str() string {
	mut buf := []u8{}
	defer {
		unsafe { buf.free() }
	}
	mut enc := Encoder{
		newline: `\n`
		newline_spaces_count: 2
	}
	enc.encode_value(f, mut buf) or {}
	return buf.bytestr()
}

// TODO - Need refactor. Is so slow. The longer the string, the lower the performance.
// encode_string returns the JSON spec-compliant version of the string.
@[direct_array_access]
fn (e &Encoder) encode_string(s string, mut buf []u8) ! {
	buf << json2.quote_rune

	if !e.escape_unicode {
		unsafe {
			buf.push_many(s.str, s.len)
			buf << json2.quote_rune
		}
		return
	}

	for idx, current_byte in s {
		mut utf8_len := ((0xe5000000 >> ((current_byte >> 3) & 0x1e)) & 3) + 1

		if utf8_len == 1 {
			if current_byte < 32 {
				// ASCII Control Characters
				unsafe {
					buf.push_many(json2.ascii_control_characters[current_byte].str, json2.ascii_control_characters[current_byte].len)
				}
				continue
			} else if current_byte >= 32 && current_byte < 128 {
				// ASCII especial characters
				if current_byte == `\\` {
					unsafe { buf.push_many(&json2.back_slash[0], json2.back_slash.len) }
					continue
				} else if current_byte == `"` {
					unsafe { buf.push_many(&json2.quote[0], json2.quote.len) }
					continue
				} else if current_byte == `/` {
					unsafe { buf.push_many(&json2.slash[0], json2.slash.len) }
					continue
				}

				// ASCII no Control Characters or especial characters
				buf << json2.g_ascii_lookup_table[current_byte]
				continue
			}
			continue
		} else if utf8_len == 2 {
			// João, Schilddrüsenerkrankungen...
			unsafe { buf.push_many(s.str + idx, utf8_len) }

			continue
		} else if utf8_len == 3 {
			// ✔, ひらがな ...
		} else if utf8_len == 4 {
			// Emojis ranges
			// 	(0x1F300, 0x1F5FF),  # Miscellaneous Symbols and Pictographs
			// 	(0x1F600, 0x1F64F),  # Emoticons
			// 	(0x1F680, 0x1F6FF),  # Transport and Map Symbols

			unsafe { buf.push_many(s.str + idx, utf8_len) }

			continue
		}

		// Handle multi-byte characters byte-by-byte
		mut codepoint := u32(current_byte & ((1 << (7 - utf8_len)) - 1))
		for j in 1 .. utf8_len {
			if idx + j >= s.len {
				// Incomplete UTF-8 sequence, handle error or return error
				continue
			}

			mut b := s[idx + j]
			if (b & 0xC0) != 0x80 {
				// Invalid continuation byte, handle error or return error
				continue // assert json.encode('te✔st') == r'"te\u2714st"'
			}

			codepoint = u32((codepoint << 6) | (b & 0x3F))
		}

		new_chars := [u8(`\\`), `u`, hex_digit((codepoint >> 12) & 0xF),
			hex_digit((codepoint >> 8) & 0xF), hex_digit((codepoint >> 4) & 0xF),
			hex_digit(codepoint & 0xF)]
		unsafe { buf.push_many(&u8(new_chars.data), new_chars.len) }
	}

	buf << json2.quote_rune
}

fn hex_digit(n int) u8 {
	if n < 10 {
		return `0` + n
	}
	return `a` + (n - 10)
}
