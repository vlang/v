module main

import json

struct UseJson {
	x int
}

fn suppress_json_warning() {
	json.encode(UseJson{})
}

// struct C.cJSON {}
fn C.cJSON_CreateObject() &C.cJSON

fn C.cJSON_CreateArray() &C.cJSON

// fn C.cJSON_CreateBool(bool) &C.cJSON
fn C.cJSON_CreateTrue() &C.cJSON

fn C.cJSON_CreateFalse() &C.cJSON

fn C.cJSON_CreateNull() &C.cJSON

// fn C.cJSON_CreateNumber() &C.cJSON
// fn C.cJSON_CreateString() &C.cJSON
fn C.cJSON_CreateRaw(&u8) &C.cJSON

fn C.cJSON_IsInvalid(voidptr) bool

fn C.cJSON_IsFalse(voidptr) bool

// fn C.cJSON_IsTrue(voidptr) bool
fn C.cJSON_IsBool(voidptr) bool

fn C.cJSON_IsNull(voidptr) bool

fn C.cJSON_IsNumber(voidptr) bool

fn C.cJSON_IsString(voidptr) bool

fn C.cJSON_IsArray(voidptr) bool

fn C.cJSON_IsObject(voidptr) bool

fn C.cJSON_IsRaw(voidptr) bool

fn C.cJSON_AddItemToObject(voidptr, &u8, voidptr)

fn C.cJSON_AddItemToArray(voidptr, voidptr)

fn C.cJSON_Delete(voidptr)

fn C.cJSON_Print(voidptr) &u8

[inline]
fn create_object() &C.cJSON {
	return C.cJSON_CreateObject()
}

[inline]
fn create_array() &C.cJSON {
	return C.cJSON_CreateArray()
}

[inline]
fn create_string(val string) &C.cJSON {
	return C.cJSON_CreateString(val.str)
}

[inline]
fn create_number(val f64) &C.cJSON {
	return C.cJSON_CreateNumber(val)
}

[inline]
fn create_bool(val bool) &C.cJSON {
	return C.cJSON_CreateBool(val)
}

[inline]
fn create_true() &C.cJSON {
	return C.cJSON_CreateTrue()
}

[inline]
fn create_false() &C.cJSON {
	return C.cJSON_CreateFalse()
}

[inline]
fn create_null() &C.cJSON {
	return C.cJSON_CreateNull()
}

[inline]
fn delete(b voidptr) {
	C.cJSON_Delete(b)
}

[inline]
fn add_item_to_object(obj &C.cJSON, key string, item &C.cJSON) {
	C.cJSON_AddItemToObject(obj, key.str, item)
}

[inline]
fn add_item_to_array(obj &C.cJSON, item &C.cJSON) {
	C.cJSON_AddItemToArray(obj, item)
}

fn json_print(json &C.cJSON) string {
	s := C.cJSON_Print(json)
	return unsafe { tos3(s) }
}
