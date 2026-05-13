// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json

import math
import strconv
import time

#flag -I @VEXEROOT/thirdparty/cJSON
#flag @VEXEROOT/thirdparty/cJSON/cJSON.o
#include "cJSON.h"
#define js_get(object, key) cJSON_GetObjectItemCaseSensitive((object), (key))

// As cJSON use `libm`, we need to link it.
$if windows {
	$if tinyc {
		#flag @VEXEROOT/thirdparty/tcc/lib/openlibm.o
	}
} $else {
	#flag -lm
}

pub struct C.cJSON {
	next        &C.cJSON
	prev        &C.cJSON
	child       &C.cJSON
	type        int
	valuestring &char
	valueint    int
	valuedouble f64
}

fn C.cJSON_IsTrue(&C.cJSON) bool
fn C.cJSON_IsFalse(&C.cJSON) bool
fn C.cJSON_IsNull(&C.cJSON) bool
fn C.cJSON_IsNumber(&C.cJSON) bool
fn C.cJSON_IsString(&C.cJSON) bool
fn C.cJSON_IsObject(&C.cJSON) bool
fn C.cJSON_IsArray(&C.cJSON) bool

fn C.cJSON_CreateNumber(f64) &C.cJSON
fn C.cJSON_CreateRaw(&char) &C.cJSON

fn C.cJSON_CreateBool(bool) &C.cJSON

fn C.cJSON_CreateString(&char) &C.cJSON

fn C.cJSON_CreateRaw(&char) &C.cJSON

fn C.cJSON_Parse(&char) &C.cJSON

fn C.cJSON_PrintUnformatted(&C.cJSON) &char

fn C.cJSON_Print(&C.cJSON) &char

fn C.cJSON_free(voidptr)
fn C.malloc(usize) voidptr
fn C.memcpy(voidptr, voidptr, usize) voidptr

// decode tries to decode the provided JSON string, into a V structure.
// If it can not do that, it returns an error describing the reason for
// the parsing failure.
pub fn decode(typ voidptr, s string) !voidptr {
	// compiler implementation
	return 0
}

// encode serialises the provided V value as a JSON string, optimised for shortness.
pub fn encode(x voidptr) string {
	// compiler implementation
	return ''
}

// encode_pretty serialises the provided V value as a JSON string, in a formatted way, optimised for viewing by humans.
pub fn encode_pretty(x voidptr) string {
	// compiler implementation
	return ''
}

@[markused]
fn decode_int(root &C.cJSON) int {
	if isnil(root) {
		return 0
	}
	return root.valueint
}

@[markused]
fn decode_i8(root &C.cJSON) i8 {
	if isnil(root) {
		return i8(0)
	}
	return i8(root.valueint)
}

@[markused]
fn decode_i16(root &C.cJSON) i16 {
	if isnil(root) {
		return i16(0)
	}
	return i16(root.valueint)
}

@[markused]
fn decode_i64(root &C.cJSON) i64 {
	if isnil(root) {
		return i64(0)
	}
	if value := decode_exact_i64(root) {
		return value
	}
	return i64(root.valuedouble) // i64 is double in C
}

// TODO: remove when `byte` is removed
@[markused]
fn decode_byte(root &C.cJSON) u8 {
	return decode_u8(root)
}

@[markused]
fn decode_u8(root &C.cJSON) u8 {
	if isnil(root) {
		return u8(0)
	}
	return u8(root.valueint)
}

@[markused]
fn decode_u16(root &C.cJSON) u16 {
	if isnil(root) {
		return u16(0)
	}
	return u16(root.valueint)
}

@[markused]
fn decode_u32(root &C.cJSON) u32 {
	if isnil(root) {
		return u32(0)
	}
	return u32(root.valueint)
}

@[markused]
fn decode_u64(root &C.cJSON) u64 {
	if isnil(root) {
		return u64(0)
	}
	if value := decode_exact_u64(root) {
		return value
	}
	return u64(root.valuedouble)
}

@[markused]
fn decode_f32(root &C.cJSON) f32 {
	if isnil(root) {
		return f32(0)
	}
	return f32(root.valuedouble)
}

@[markused]
fn decode_f64(root &C.cJSON) f64 {
	if isnil(root) {
		return f64(0)
	}
	return root.valuedouble
}

@[markused]
fn decode_rune(root &C.cJSON) rune {
	if isnil(root) {
		return rune(0)
	}
	if isnil(root.valuestring) || !C.cJSON_IsString(root) {
		return rune(0)
	}

	// TODO: Parse as runes, bypassing string casting...?
	return unsafe { tos_clone(&u8(root.valuestring)).runes().first() }
}

@[markused]
fn decode_string(root &C.cJSON) string {
	if isnil(root) {
		return ''
	}
	if !isnil(root.valuestring) && C.cJSON_IsString(root) {
		return unsafe { tos_clone(&u8(root.valuestring)) } // , _strlen(root.valuestring))
	}
	// Object/array values can be stringified JSON payloads (e.g. `{}` from JSON.stringify()).
	if C.cJSON_IsObject(root) || C.cJSON_IsArray(root) {
		return json_print(root)
	}
	return ''
}

@[markused]
fn decode_bool(root &C.cJSON) bool {
	if isnil(root) {
		return false
	}
	return C.cJSON_IsTrue(root)
}

@[markused]
fn decode_time(root &C.cJSON) !time.Time {
	if isnil(root) || C.cJSON_IsNull(root) {
		return time.Time{}
	}
	mut decoded_time := time.Time{}
	if C.cJSON_IsString(root) {
		decoded_time.from_json_string(decode_string(root))!
		return decoded_time
	}
	if C.cJSON_IsNumber(root) {
		decoded_time.from_json_number(json_print(root))!
		return decoded_time
	}
	return error('expected time.Time to decode from a JSON string or number, got: ${json_print(root)}')
}

// ///////////////////

@[markused]
fn encode_int(val int) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

@[markused]
fn encode_i8(val i8) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

@[markused]
fn encode_i16(val i16) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

@[markused]
fn encode_i64(val i64) &C.cJSON {
	lit := val.str()
	return C.cJSON_CreateRaw(&char(lit.str))
}

// TODO: remove when `byte` is removed
@[markused]
fn encode_byte(root u8) &C.cJSON {
	return encode_u8(root)
}

@[markused]
fn encode_u8(val u8) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

@[markused]
fn encode_u16(val u16) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

@[markused]
fn encode_u32(val u32) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

@[markused]
fn encode_u64(val u64) &C.cJSON {
	lit := val.str()
	return C.cJSON_CreateRaw(&char(lit.str))
}

@[markused]
fn encode_f32(val f32) &C.cJSON {
	return C.cJSON_CreateRaw(&char(json_float_to_raw_string(val).str))
}

@[markused]
fn encode_f64(val f64) &C.cJSON {
	return C.cJSON_CreateRaw(&char(json_float_to_raw_string(val).str))
}

@[markused]
fn encode_bool(val bool) &C.cJSON {
	return C.cJSON_CreateBool(val)
}

@[markused]
fn encode_rune(val rune) &C.cJSON {
	return C.cJSON_CreateRaw(&char(json_ascii_string(val.str()).str))
}

@[markused]
fn encode_string(val string) &C.cJSON {
	return C.cJSON_CreateRaw(&char(json_ascii_string(val).str))
}

// json_ascii_string returns a quoted JSON string with non-ASCII runes escaped as `\uXXXX`.
fn json_ascii_string(val string) string {
	mut output := []u8{cap: val.len + 2}
	output << `"`
	for character in val.runes() {
		match character {
			`"` {
				output << `\\`
				output << `"`
			}
			`\\` {
				output << `\\`
				output << `\\`
			}
			`\b` {
				output << `\\`
				output << `b`
			}
			`\f` {
				output << `\\`
				output << `f`
			}
			`\n` {
				output << `\\`
				output << `n`
			}
			`\r` {
				output << `\\`
				output << `r`
			}
			`\t` {
				output << `\\`
				output << `t`
			}
			else {
				if character < 0x20 || character > 0x7f {
					if character <= 0xffff {
						output << `\\`
						output << `u`
						hex_string := '${u32(character):04x}'
						unsafe { output.push_many(hex_string.str, 4) }
					} else {
						unicode_point_low := u32(character) - 0x10000
						surrogate_pair := '\\u${0xD800 + ((unicode_point_low >> 10) & 0x3FF):04X}\\u${
							0xDC00 + (unicode_point_low & 0x3FF):04x}'
						unsafe { output.push_many(surrogate_pair.str, surrogate_pair.len) }
					}
				} else {
					output << u8(character)
				}
			}
		}
	}
	output << `"`
	return output.bytestr()
}

// json_float_to_raw_string uses V's float formatter so json.encode keeps exact float round-trips.
fn json_float_to_raw_string[T](val T) string {
	if val == 0 {
		return '0'
	}
	if math.is_nan(f64(val)) || math.is_inf(f64(val), 0) {
		return 'null'
	}
	mut raw := val.str()
	if raw.len > 2 && raw[raw.len - 2] == `.` && raw[raw.len - 1] == `0` {
		raw = raw[..raw.len - 2]
	}
	return raw
}

fn decode_exact_i64(root &C.cJSON) ?i64 {
	if isnil(root) || isnil(root.valuestring) {
		return none
	}
	lit := unsafe { tos_clone(&u8(root.valuestring)) }
	value := strconv.parse_int(lit, 10, 64) or { return none }
	return value
}

fn decode_exact_u64(root &C.cJSON) ?u64 {
	if isnil(root) || isnil(root.valuestring) {
		return none
	}
	lit := unsafe { tos_clone(&u8(root.valuestring)) }
	value := strconv.parse_uint(lit, 10, 64) or { return none }
	return value
}

fn clone_cstring(s string) &char {
	buf := C.malloc(usize(s.len + 1))
	if buf == unsafe { nil } {
		return unsafe { nil }
	}
	unsafe {
		if s.len > 0 {
			C.memcpy(buf, s.str, usize(s.len))
		}
		(&u8(buf))[s.len] = 0
	}
	return &char(buf)
}

@[inline]
fn is_json_whitespace(ch u8) bool {
	return ch == ` ` || ch == `\n` || ch == `\r` || ch == `\t`
}

fn skip_json_whitespace(src string, idx int) int {
	mut pos := idx
	for pos < src.len && is_json_whitespace(src[pos]) {
		pos++
	}
	return pos
}

fn skip_json_string(src string, idx int) int {
	mut pos := idx
	if pos >= src.len || src[pos] != `"` {
		return -1
	}
	pos++
	for pos < src.len {
		match src[pos] {
			`"` {
				return pos + 1
			}
			`\\` {
				pos++
				if pos >= src.len {
					return -1
				}
				if src[pos] == `u` {
					pos += 5
				} else {
					pos++
				}
			}
			else {
				pos++
			}
		}
	}
	return -1
}

fn skip_json_literal(src string, lit string, idx int) int {
	if idx + lit.len > src.len || src[idx..idx + lit.len] != lit {
		return -1
	}
	return idx + lit.len
}

fn skip_json_number(src string, idx int) (string, int) {
	mut pos := idx
	start := pos
	if pos < src.len && src[pos] == `-` {
		pos++
	}
	if pos >= src.len {
		return '', -1
	}
	if src[pos] == `0` {
		pos++
	} else {
		if !src[pos].is_digit() {
			return '', -1
		}
		for pos < src.len && src[pos].is_digit() {
			pos++
		}
	}
	if pos < src.len && src[pos] == `.` {
		pos++
		if pos >= src.len || !src[pos].is_digit() {
			return '', -1
		}
		for pos < src.len && src[pos].is_digit() {
			pos++
		}
	}
	if pos < src.len && (src[pos] == `e` || src[pos] == `E`) {
		pos++
		if pos < src.len && (src[pos] == `+` || src[pos] == `-`) {
			pos++
		}
		if pos >= src.len || !src[pos].is_digit() {
			return '', -1
		}
		for pos < src.len && src[pos].is_digit() {
			pos++
		}
	}
	return src[start..pos], pos
}

fn annotate_json_number_literal(root &C.cJSON, lit string) {
	if lit.len == 0 || !isnil(root.valuestring) {
		return
	}
	unsafe {
		root.valuestring = clone_cstring(lit)
	}
}

fn annotate_json_value(root &C.cJSON, src string, idx int) int {
	if isnil(root) {
		return -1
	}
	mut pos := skip_json_whitespace(src, idx)
	if C.cJSON_IsObject(root) {
		if pos >= src.len || src[pos] != `{` {
			return -1
		}
		pos++
		pos = skip_json_whitespace(src, pos)
		mut child := root.child
		if isnil(child) {
			if pos >= src.len || src[pos] != `}` {
				return -1
			}
			return pos + 1
		}
		for !isnil(child) {
			pos = skip_json_string(src, pos)
			if pos == -1 {
				return -1
			}
			pos = skip_json_whitespace(src, pos)
			if pos >= src.len || src[pos] != `:` {
				return -1
			}
			pos++
			pos = annotate_json_value(child, src, pos)
			if pos == -1 {
				return -1
			}
			pos = skip_json_whitespace(src, pos)
			if !isnil(child.next) {
				if pos >= src.len || src[pos] != `,` {
					return -1
				}
				pos++
			}
			child = child.next
		}
		if pos >= src.len || src[pos] != `}` {
			return -1
		}
		return pos + 1
	}
	if C.cJSON_IsArray(root) {
		if pos >= src.len || src[pos] != `[` {
			return -1
		}
		pos++
		pos = skip_json_whitespace(src, pos)
		mut child := root.child
		if isnil(child) {
			if pos >= src.len || src[pos] != `]` {
				return -1
			}
			return pos + 1
		}
		for !isnil(child) {
			pos = annotate_json_value(child, src, pos)
			if pos == -1 {
				return -1
			}
			pos = skip_json_whitespace(src, pos)
			if !isnil(child.next) {
				if pos >= src.len || src[pos] != `,` {
					return -1
				}
				pos++
			}
			child = child.next
		}
		if pos >= src.len || src[pos] != `]` {
			return -1
		}
		return pos + 1
	}
	if C.cJSON_IsString(root) {
		return skip_json_string(src, pos)
	}
	if C.cJSON_IsNumber(root) {
		lit, next_pos := skip_json_number(src, pos)
		if next_pos == -1 {
			return -1
		}
		annotate_json_number_literal(root, lit)
		return next_pos
	}
	if C.cJSON_IsTrue(root) {
		return skip_json_literal(src, 'true', pos)
	}
	if C.cJSON_IsFalse(root) {
		return skip_json_literal(src, 'false', pos)
	}
	if C.cJSON_IsNull(root) {
		return skip_json_literal(src, 'null', pos)
	}
	return -1
}

fn annotate_json_number_literals(root &C.cJSON, src string) {
	_ := annotate_json_value(root, src, 0)
}

// ///////////////////////
// user := decode_User(json_parse(js_string_var))
@[markused]
fn json_parse(s string) &C.cJSON {
	root := C.cJSON_Parse(&char(s.str))
	if !isnil(root) {
		annotate_json_number_literals(root, s)
	}
	return root
}

// json_string := json_print(encode_User(user))
@[markused]
fn json_print(data &C.cJSON) string {
	s := C.cJSON_PrintUnformatted(data)
	if s == unsafe { nil } {
		return ''
	}
	r := unsafe { tos_clone(&u8(s)) }
	C.cJSON_free(s)
	return r
}

@[markused]
fn json_print_pretty(data &C.cJSON) string {
	s := C.cJSON_Print(data)
	if s == unsafe { nil } {
		return ''
	}
	r := unsafe { tos_clone(&u8(s)) }
	C.cJSON_free(s)
	return r
}

// /  cjson wrappers
// fn json_array_for_each(val, root &C.cJSON) {
// #cJSON_ArrayForEach (val ,root)
// }
