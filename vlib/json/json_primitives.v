// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json

#flag -I @VROOT/thirdparty/cJSON
#flag @VROOT/thirdparty/cJSON/cJSON.o
#include "cJSON.h"
#define js_get(object, key) cJSON_GetObjectItemCaseSensitive((object), (key))
struct C.cJSON {
	valueint    int
	valuedouble f32
	valuestring byteptr
}

pub fn decode(typ voidptr, s string) ?voidptr {
	// compiler implementation
	return 0
}

pub fn encode(x voidptr) string {
	// compiler implementation
	return ''
}

fn decode_int(root &C.cJSON) int {
	if isnil(root) {
		return 0
	}
	return root.valueint
}

fn decode_i8(root &C.cJSON) i8 {
	if isnil(root) {
		return i8(0)
	}
	return i8(root.valueint)
}

fn decode_i16(root &C.cJSON) i16 {
	if isnil(root) {
		return i16(0)
	}
	return i16(root.valueint)
}

fn decode_i64(root &C.cJSON) i64 {
	if isnil(root) {
		return i64(0)
	}
	return i64(root.valuedouble) // i64 is double in C
}

fn decode_byte(root &C.cJSON) byte {
	if isnil(root) {
		return byte(0)
	}
	return byte(root.valueint)
}

fn decode_u16(root &C.cJSON) u16 {
	if isnil(root) {
		return u16(0)
	}
	return u16(root.valueint)
}

fn decode_u32(root &C.cJSON) u32 {
	if isnil(root) {
		return u32(0)
	}
	return u32(root.valueint)
}

fn decode_u64(root &C.cJSON) u64 {
	if isnil(root) {
		return u64(0)
	}
	return u64(root.valueint)
}

fn decode_f32(root &C.cJSON) f32 {
	if isnil(root) {
		return f32(0)
	}
	return root.valuedouble
}

fn decode_f64(root &C.cJSON) f64 {
	if isnil(root) {
		return f64(0)
	}
	return f64(root.valuedouble)
}

fn decode_string(root &C.cJSON) string {
	if isnil(root) {
		return ''
	}
	if isnil(root.valuestring) {
		return ''
	}
	// println('decode string valuestring="$root.valuestring"')
	// return tos(root.valuestring, _strlen(root.valuestring))
	return tos_clone(root.valuestring) // , _strlen(root.valuestring))
}

fn C.cJSON_IsTrue() bool


fn C.cJSON_CreateNumber() &C.cJSON


fn C.cJSON_CreateBool() &C.cJSON


fn C.cJSON_CreateString() &C.cJSON


fn C.cJSON_Parse() &C.cJSON


fn C.cJSON_PrintUnformatted() byteptr


fn decode_bool(root &C.cJSON) bool {
	if isnil(root) {
		return false
	}
	return C.cJSON_IsTrue(root)
}

// ///////////////////
fn encode_int(val int) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn encode_i8(val i8) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn encode_i16(val i16) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn encode_i64(val i64) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn encode_byte(val byte) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn encode_u16(val u16) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn encode_u32(val u32) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn encode_u64(val u64) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn encode_f32(val f32) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn encode_f64(val f64) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn encode_bool(val bool) &C.cJSON {
	return C.cJSON_CreateBool(val)
}

fn encode_string(val string) &C.cJSON {
	return C.cJSON_CreateString(val.str)
}
// ///////////////////////
// user := decode_User(json_parse(js_string_var))
fn json_parse(s string) &C.cJSON {
	return C.cJSON_Parse(s.str)
}

// json_string := json_print(encode_User(user))
fn json_print(json &C.cJSON) string {
	s := C.cJSON_PrintUnformatted(json)
	return unsafe { tos(s, C.strlen(s)) }
}

// /  cjson wrappers
// fn json_array_for_each(val, root &C.cJSON) {
// #cJSON_ArrayForEach (val ,root)
// }
