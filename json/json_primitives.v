// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module json

// TODO: windows support
#flag linux -I$HOME/code/v/thirdparty/cJSON
#flag darwin -I$HOME/code/v/thirdparty/cJSON

// #include "cJSON.c"
#include "cJSON.h"
struct C.cJSON {
	valueint    int
	valuedouble float
	valuestring byteptr
}

fn jsdecode_int(root *C.cJSON) int {
	if isnil(root) {
		return 0
	}
	return root.valueint
}

//TODO: Refactor with generics when it will be avaible

fn jsdecode_i8(root *C.cJSON) i8 {
	if isnil(root) {
		return i8(0)
	}
	return i8(root.valueint)
}

fn jsdecode_i16(root *C.cJSON) i16 {
	if isnil(root) {
		return i16(0)
	}
	return i16(root.valueint)
}

fn jsdecode_i32(root *C.cJSON) i32 {
	if isnil(root) {
		return i32(0)
	}
	return i32(root.valueint)
}

fn jsdecode_i64(root *C.cJSON) i64 {
	if isnil(root) {
		return i64(0)
	}
	return i64(root.valuedouble) //i64 is double in C
}

fn jsdecode_float(root *C.cJSON) float {
	if isnil(root) {
		return 0
	}
	return root.valuedouble
}

fn jsdecode_f32(root *C.cJSON) f32 {
	if isnil(root) {
		return f32(0)
	}
	return f32(root.valuedouble)
}

fn jsdecode_f64(root *C.cJSON) f64 {
	if isnil(root) {
		return f64(0)
	}
	return f64(root.valuedouble)
}


fn jsdecode_string(root *C.cJSON) string {
	if isnil(root) {
		return ''
	}
	if isnil(root.valuestring) {
		return ''
	}
	// println('jsdecode string valuestring="$root.valuestring"')
	// return tos(root.valuestring, _strlen(root.valuestring))
	return tos_clone(root.valuestring)// , _strlen(root.valuestring))
}

fn jsdecode_bool(root *C.cJSON) bool {
	if isnil(root) {
		return false
	}
	return C.cJSON_IsTrue(root)
}

// ///////////////////
//TODO: Refactor with Generics when it will be available
fn jsencode_int(val int) *C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn jsencode_i8(val i8) *C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn jsencode_i16(val i16) *C.cJSON {
	return C.cJSON_CreateNumber(val)
}
fn jsencode_i32(val i32) *C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn jsencode_i64(val i64) *C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn jsencode_float(val float) *C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn jsencode_f32(val f32) *C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn jsencode_f64(val f64) *C.cJSON {
	return C.cJSON_CreateNumber(val)
}

fn jsencode_bool(val bool) *C.cJSON {
	return C.cJSON_CreateBool(val)
}

fn jsencode_string(val string) *C.cJSON {
	clone := val.clone()
	return C.cJSON_CreateString(clone.str)
	// return C.cJSON_CreateString2(val.str, val.len)
}

// ///////////////////////
// user := decode_User(json_parse(js_string_var))
fn json_parse(s string) *C.cJSON {
	return C.cJSON_Parse(s.str)
}

// json_string := json_print(encode_User(user))
fn json_print(json *C.cJSON) string {
	s := C.cJSON_PrintUnformatted(json)
	return tos(s, _strlen(s))
}

// /  cjson wrappers
// fn json_array_for_each(val, root *C.cJSON) {
// #cJSON_ArrayForEach (val ,root)
// }
