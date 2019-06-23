// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module json

// TODO: windows support
#flag -Dlinux -I$HOME/code/v/thirdparty/cJSON
#flag -Dmac -I$HOME/code/v/thirdparty/cJSON

// #include "cJSON.c"
#include "cJSON.h"
struct C.cJSON {
	valueint    int
	valuestring byteptr
}

fn jsdecode_int(root *C.cJSON) int {
	if isnil(root) {
		return 0
	}
	return root.valueint
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
fn jsencode_int(val int) *C.cJSON {
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
