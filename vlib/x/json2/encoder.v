// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import time
import math
import strconv

// Encoder encodes the an `Any` type into JSON representation.
// It provides parameters in order to change the end result.
pub struct Encoder {
pub:
	newline              u8
	newline_spaces_count int
	escape_unicode       bool = true
}

// byte array versions of the most common tokens/chars to avoid reallocations
const null_in_string = 'null'

const true_in_string = 'true'

const false_in_string = 'false'

const empty_array = [u8(`[`), `]`]!

const comma_rune = `,`

const colon_rune = `:`

const quote_rune = `"`

const back_slash = [u8(`\\`), `\\`]!

const quote = [u8(`\\`), `"`]!

const slash = [u8(`\\`), `/`]!

const null_unicode = [u8(`\\`), `u`, `0`, `0`, `0`, `0`]!

const ascii_control_characters = ['\\u0000', '\\t', '\\n', '\\r', '\\u0004', '\\u0005', '\\u0006',
	'\\u0007', '\\b', '\\t', '\\n', '\\u000b', '\\f', '\\r', '\\u000e', '\\u000f', '\\u0010',
	'\\u0011', '\\u0012', '\\u0013', '\\u0014', '\\u0015', '\\u0016', '\\u0017', '\\u0018', '\\u0019',
	'\\u001a', '\\u001b', '\\u001c', '\\u001d', '\\u001e', '\\u001f']!

const curly_open_rune = `{`

const curly_close_rune = `}`

const ascii_especial_characters = [u8(`\\`), `"`, `/`]!

// encode is a generic function that encodes a type into a JSON string.
@[manualfree]
pub fn encode[T](val T) string {
	$if T is $array {
		return encode_array(val)
	} $else {
		mut count := Count{0}
		count.count_chars(val)

		mut buf := []u8{cap: count.total}

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
@[manualfree]
fn encode_array[T](val []T) string {
	if val.len == 0 {
		return '[]'
	}

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
	raw_decoded := decode[Any](encoded) or { 0 }
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
	buf << curly_open_rune
	mut idx := 0
	for k, v in value {
		e.encode_newline(level, mut buf)!
		// e.encode_string(k.str(), mut buf)!
		e.encode_string(k, mut buf)!
		buf << colon_rune
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
			buf << comma_rune
		}
		idx++
	}

	e.encode_newline(level - 1, mut buf)!
	buf << curly_close_rune
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
				e.encode_value_with_level(val, level, mut buf)!
			}
		}
	} $else $if T is $alias {
		// TODO
	} $else $if T is time.Time {
		str_value := val.format_rfc3339()
		buf << quote_rune
		unsafe { buf.push_many(str_value.str, str_value.len) }
		buf << quote_rune
	} $else $if T is $map {
		e.encode_map(val, level, mut buf)!
	} $else $if T is $array {
		e.encode_array(val, level, mut buf)!
	} $else $if T is Encodable {
		str_value := val.json_str()
		unsafe { buf.push_many(str_value.str, str_value.len) }
	} $else $if T is Null {
		unsafe { buf.push_many(null_in_string.str, null_in_string.len) }
	} $else $if T is $struct {
		e.encode_struct(val, level, mut buf)!
	} $else $if T is $enum {
		str_int := int(val).str()
		unsafe { buf.push_many(str_int.str, str_int.len) }
	} $else $if T is $int || T is bool {
		str_int := val.str()
		unsafe { buf.push_many(str_int.str, str_int.len) }
	} $else $if T is $float {
		str_float := encode_number(val)
		unsafe { buf.push_many(str_float.str, str_float.len) }
	} $else {
		return error('cannot encode value with ${typeof(val).name} type')
	}
}

fn (e &Encoder) encode_struct[U](val U, level int, mut buf []u8) ! {
	buf << curly_open_rune
	mut i := 0
	mut fields_len := 0

	$for field in U.fields {
		mut @continue := false
		for attr in field.attrs {
			if attr.contains('skip') {
				@continue = true
			}
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
		mut is_nil := false
		$if value is $option {
			if field.indirections > 0 {
				is_nil = value == none
			}
		} $else $if field.indirections > 0 {
			is_nil = value == unsafe { nil }
		}
		mut json_name := ''

		for attr in field.attrs {
			if attr.contains('skip') {
				ignore_field = true
			}
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
					buf << colon_rune

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
					buf << colon_rune

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
					buf << quote_rune
					unsafe { buf.push_many(str_value.str, str_value.len) }
					buf << quote_rune
				} $else $if field.typ is bool {
					if value {
						unsafe { buf.push_many(true_in_string.str, true_in_string.len) }
					} else {
						unsafe { buf.push_many(false_in_string.str, false_in_string.len) }
					}
				} $else $if field.typ is $int {
					str_value := val.$(field.name).str()
					unsafe { buf.push_many(str_value.str, str_value.len) }
				} $else $if field.typ is $float {
					str_value := encode_number(val.$(field.name))
					unsafe { buf.push_many(str_value.str, str_value.len) }
				} $else $if field.is_array {
					// TODO: replace for `field.typ is $array`
					e.encode_array(value, level + 1, mut buf)!
				} $else $if field.typ is $array {
					// e.encode_array(value, level + 1, mut buf)! // FIXME: error: could not infer generic type `U` in call to `encode_array`
				} $else $if field.typ is $struct {
					e.encode_struct(value, level + 1, mut buf)!
				} $else $if field.is_map {
					e.encode_map(value, level + 1, mut buf)!
				} $else $if field.is_enum {
					// TODO: replace for `field.typ is $enum`
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
						if val.$(field.name) {
							unsafe { buf.push_many(true_in_string.str, true_in_string.len) }
						} else {
							unsafe { buf.push_many(false_in_string.str, false_in_string.len) }
						}
					} $else $if field.unaliased_typ is $int {
						str_value := val.$(field.name).str()
						unsafe { buf.push_many(str_value.str, str_value.len) }
					} $else $if field.unaliased_typ is $float {
						str_value := encode_number(val)
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
				buf << comma_rune
			}
		}
		if !ignore_field {
			i++
		}
	}
	e.encode_newline(level - 1, mut buf)!
	buf << curly_close_rune
	// b.measure('encode_struct')
}

fn (e &Encoder) encode_array[U](val []U, level int, mut buf []u8) ! {
	if val.len == 0 {
		unsafe { buf.push_many(&empty_array[0], empty_array.len) }
		return
	}
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
			// TODO: test
			e.encode_value_with_level(val[i], level + 1, mut buf)!
		} $else {
			return error('type ${typeof(val).name} cannot be array encoded')
		}
		if i < val.len - 1 {
			buf << comma_rune
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

// str returns the string representation of the `Any` type. Use the `json_str` method.
// If you want to use the escaped str() version of the `Any` type.
pub fn (f Any) str() string {
	if f is string {
		return f
	} else {
		return f.json_str()
	}
}

// json_str returns the JSON string representation of the `Any` type.
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
		newline:              `\n`
		newline_spaces_count: 2
	}
	enc.encode_value(f, mut buf) or {}
	return buf.bytestr()
}

// TODO: Need refactor. Is so slow. The longer the string, the lower the performance.
// encode_string returns the JSON spec-compliant version of the string.
@[direct_array_access]
fn (e &Encoder) encode_string(s string, mut buf []u8) ! {
	if s == '' {
		empty := [u8(quote_rune), quote_rune]!
		unsafe { buf.push_many(&empty[0], 2) }
		return
	}
	mut last_no_buffer_expansible_char_position_candidate := 0
	buf << quote_rune

	if !e.escape_unicode {
		unsafe {
			buf.push_many(s.str, s.len)
			buf << quote_rune
		}
		return
	}

	for idx := 0; idx < s.len; idx++ {
		current_byte := s[idx]

		mut current_utf8_len := ((0xe5000000 >> ((current_byte >> 3) & 0x1e)) & 3) + 1

		current_value_cause_buffer_expansion :=
			(current_utf8_len == 1 && ((current_byte < 32 || current_byte > 127)
			|| current_byte in ascii_especial_characters)) || current_utf8_len == 3

		if !current_value_cause_buffer_expansion {
			// while it is not the last one
			if idx < s.len - 1 {
				if s.len > (idx + current_utf8_len) {
					if current_utf8_len == 2 || current_utf8_len == 4 {
						// runes like: ã, ü, etc.
						// Emojis ranges
						// 	(0x1F300, 0x1F5FF),  # Miscellaneous Symbols and Pictographs
						// 	(0x1F600, 0x1F64F),  # Emoticons
						// 	(0x1F680, 0x1F6FF),  # Transport and Map Symbols
						idx += current_utf8_len - 1
						continue
					}
				} else {
					unsafe {
						buf.push_many(s.str + last_no_buffer_expansible_char_position_candidate,
							s.len - last_no_buffer_expansible_char_position_candidate)
					}
					break
				}
			} else if idx == s.len - 1 {
				unsafe {
					buf.push_many(s.str + last_no_buffer_expansible_char_position_candidate,
						s.len - last_no_buffer_expansible_char_position_candidate)
				}
			}
		} else {
			if idx > 0 {
				length := idx - last_no_buffer_expansible_char_position_candidate
				unsafe {
					buf.push_many(s.str + last_no_buffer_expansible_char_position_candidate,
						length)
				}
				last_no_buffer_expansible_char_position_candidate = idx + 1
			}
		}

		if current_utf8_len == 1 {
			if current_byte < 32 {
				// ASCII Control Characters
				unsafe {
					buf.push_many(ascii_control_characters[current_byte].str, ascii_control_characters[current_byte].len)
				}
				last_no_buffer_expansible_char_position_candidate = idx + 1
			} else if current_byte >= 32 && current_byte < 128 {
				// ASCII especial characters
				if current_byte == `\\` {
					unsafe { buf.push_many(&back_slash[0], back_slash.len) }
					last_no_buffer_expansible_char_position_candidate = idx + 1
					continue
				} else if current_byte == `"` {
					unsafe { buf.push_many(&quote[0], quote.len) }
					last_no_buffer_expansible_char_position_candidate = idx + 1
					continue
				} else if current_byte == `/` {
					unsafe { buf.push_many(&slash[0], slash.len) }
					last_no_buffer_expansible_char_position_candidate = idx + 1
					continue
				}
			}
			continue
		} else if current_utf8_len == 3 {
			// runes like: ✔, ひらがな ...

			// Handle multi-byte characters byte-by-byte
			mut codepoint := u32(current_byte & ((1 << (7 - current_utf8_len)) - 1))
			for j in 1 .. current_utf8_len {
				if idx + j >= s.len {
					// Incomplete UTF-8 sequence, TODO handle error
					idx++
					continue
				}

				mut b := s[idx + j]
				if (b & 0xC0) != 0x80 {
					// Invalid continuation byte, TODO handle error
					idx++
					continue
				}

				codepoint = u32((codepoint << 6) | (b & 0x3F))
			}
			// runes like: ✔, ひらがな ...
			unsafe { buf.push_many(&null_unicode[0], null_unicode.len) }
			buf[buf.len - 1] = hex_digit(codepoint & 0xF)
			buf[buf.len - 2] = hex_digit((codepoint >> 4) & 0xF)
			buf[buf.len - 3] = hex_digit((codepoint >> 8) & 0xF)
			buf[buf.len - 4] = hex_digit((codepoint >> 12) & 0xF)
			idx += current_utf8_len - 1
			last_no_buffer_expansible_char_position_candidate = idx + 1
		}
	}

	buf << quote_rune
}

fn hex_digit(n u32) u8 {
	if n < 10 {
		return `0` + n
	}
	return `a` + (n - 10)
}

fn encode_number(value f64) string {
	if math.is_nan(value) || math.is_inf(value, 0) {
		return 'null'
	} else if value == f64(int(value)) {
		return int(value).str()
	} else {
		// TODO:cjson Try 15 decimal places of precision to avoid nonsignificant nonzero digits
		// If not, print with 17 decimal places of precision
		// strconv.f64_to_str_l try max 18 digits instead.
		return strconv.f64_to_str_l(value)
	}
}
