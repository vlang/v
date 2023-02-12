// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json

#flag -I @VEXEROOT/thirdparty/cJSON
#flag @VEXEROOT/thirdparty/cJSON/cJSON.o
#include "cJSON.h"
#define js_get(object, key) cJSON_GetObjectItemCaseSensitive((object), (key))

pub const used = 1

pub struct C.cJSON {
	valueint    int
	valuedouble f64
	valuestring &char
}

fn C.cJSON_IsTrue(&C.cJSON) bool

fn C.cJSON_CreateNumber(f64) &C.cJSON

fn C.cJSON_CreateBool(bool) &C.cJSON

fn C.cJSON_CreateString(&char) &C.cJSON

fn C.cJSON_Parse(&char) &C.cJSON

fn C.cJSON_PrintUnformatted(&C.cJSON) &char

fn C.cJSON_Print(&C.cJSON) &char

fn C.cJSON_free(voidptr)

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

[markused]
fn decode_int(root &C.cJSON) int {
	if isnil(root) {
		return 0
	}
	return root.valueint
}

[markused]
fn decode_i8(root &C.cJSON) i8 {
	if isnil(root) {
		return i8(0)
	}
	return i8(root.valueint)
}

[markused]
fn decode_i16(root &C.cJSON) i16 {
	if isnil(root) {
		return i16(0)
	}
	return i16(root.valueint)
}

[markused]
fn decode_i64(root &C.cJSON) i64 {
	if isnil(root) {
		return i64(0)
	}
	return i64(root.valuedouble) // i64 is double in C
}

// TODO: remove when `byte` is removed
[markused]
fn decode_byte(root &C.cJSON) byte {
	return byte(decode_u8(root))
}

[markused]
fn decode_u8(root &C.cJSON) u8 {
	if isnil(root) {
		return u8(0)
	}
	return u8(root.valueint)
}

[markused]
fn decode_u16(root &C.cJSON) u16 {
	if isnil(root) {
		return u16(0)
	}
	return u16(root.valueint)
}

[markused]
fn decode_u32(root &C.cJSON) u32 {
	if isnil(root) {
		return u32(0)
	}
	return u32(root.valueint)
}

[markused]
fn decode_u64(root &C.cJSON) u64 {
	if isnil(root) {
		return u64(0)
	}
	return u64(root.valuedouble)
}

[markused]
fn decode_f32(root &C.cJSON) f32 {
	if isnil(root) {
		return f32(0)
	}
	return f32(root.valuedouble)
}

[markused]
fn decode_f64(root &C.cJSON) f64 {
	if isnil(root) {
		return f64(0)
	}
	return root.valuedouble
}

[markused]
fn decode_rune(root &C.cJSON) rune {
	if isnil(root) {
		return rune(0)
	}
	if isnil(root.valuestring) {
		return rune(0)
	}

	// TODO: Parse as runes, bypassing string casting...?
	return unsafe { tos_clone(&u8(root.valuestring)).runes().first() }
}

[markused]
fn decode_string(root &C.cJSON) string {
	if isnil(root) {
		return ''
	}
	if isnil(root.valuestring) {
		return ''
	}
	return unsafe { tos_clone(&u8(root.valuestring)) } // , _strlen(root.valuestring))
}

[markused]
fn decode_bool(root &C.cJSON) bool {
	if isnil(root) {
		return false
	}
	return C.cJSON_IsTrue(root)
}

// ///////////////////

[markused]
fn encode_int(val int) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

[markused]
fn encode_i8(val i8) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

[markused]
fn encode_i16(val i16) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

[markused]
fn encode_i64(val i64) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

// TODO: remove when `byte` is removed
[markused]
fn encode_byte(root byte) &C.cJSON {
	return encode_u8(u8(root))
}

[markused]
fn encode_u8(val u8) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

[markused]
fn encode_u16(val u16) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

[markused]
fn encode_u32(val u32) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

[markused]
fn encode_u64(val u64) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

[markused]
fn encode_f32(val f32) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

[markused]
fn encode_f64(val f64) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

[markused]
fn encode_bool(val bool) &C.cJSON {
	return C.cJSON_CreateBool(val)
}

[markused]
fn encode_rune(val rune) &C.cJSON {
	return C.cJSON_CreateString(&char(val.str().str))
}

[markused]
fn encode_string(val string) &C.cJSON {
	return C.cJSON_CreateString(&char(val.str))
}

// ///////////////////////
// user := decode_User(json_parse(js_string_var))
[markused]
fn json_parse(s string) &C.cJSON {
	return C.cJSON_Parse(&char(s.str))
}

// json_string := json_print(encode_User(user))
[markused]
fn json_print(json &C.cJSON) string {
	s := C.cJSON_PrintUnformatted(json)
	r := unsafe { tos_clone(&u8(s)) }
	C.cJSON_free(s)
	return r
}

fn json_print_pretty(json &C.cJSON) string {
	s := C.cJSON_Print(json)
	r := unsafe { tos_clone(&u8(s)) }
	C.cJSON_free(s)
	return r
}

// /  cjson wrappers
// fn json_array_for_each(val, root &C.cJSON) {
// #cJSON_ArrayForEach (val ,root)
// }
