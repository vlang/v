// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module cjson

// The `json.cjson` module provides more manual and low level access to the cJSON library.
// For example, using it, it is possible to build a tree in memory, that represents a JSON object,
// that has NULL leaves for example, which is currently not convenient/easy to do with just the
// high level `json.encode(value)` API that V has.

#flag -I @VEXEROOT/thirdparty/cJSON
#flag @VEXEROOT/thirdparty/cJSON/cJSON.o
#include "cJSON.h"

[typedef]
pub struct C.cJSON {
pub:
	next  &C.cJSON // next/prev allow you to walk array/object chains. Alternatively, use GetArraySize/GetArrayItem/GetObjectItem
	prev  &C.cJSON
	child &C.cJSON // An array or object item will have a child pointer pointing to a chain of the items in the array/object
	//
	@type int // The type of the item, as above
	//
	valueint    int   // writing to valueint is DEPRECATED, use cJSON_SetNumberValue instead
	valuedouble f64   // The item's number, if type==cJSON_Number
	valuestring &char // The item's string, if type==cJSON_String  and type == cJSON_Raw
	// @string &char // The item's name string, if this item is the child of, or is in the list of subitems of an object
	// TODO: `@string &char` from above does not work. It should be fixed, at least inside `struct C.`.
}

pub const used = 1

pub type Node = C.cJSON

fn C.cJSON_Version() &char

fn C.cJSON_Parse(const_value &char) &C.cJSON

fn C.cJSON_CreateObject() &C.cJSON

fn C.cJSON_CreateArray() &C.cJSON

fn C.cJSON_CreateBool(bool) &C.cJSON

fn C.cJSON_CreateTrue() &C.cJSON

fn C.cJSON_CreateFalse() &C.cJSON

fn C.cJSON_CreateNull() &C.cJSON

fn C.cJSON_CreateNumber(f64) &C.cJSON

fn C.cJSON_CreateString(const_s &char) &C.cJSON

fn C.cJSON_CreateRaw(const_s &char) &C.cJSON

fn C.cJSON_IsInvalid(object &C.cJSON) bool

fn C.cJSON_IsFalse(object &C.cJSON) bool

fn C.cJSON_IsTrue(object &C.cJSON) bool

fn C.cJSON_IsBool(object &C.cJSON) bool

fn C.cJSON_IsNull(object &C.cJSON) bool

fn C.cJSON_IsNumber(object &C.cJSON) bool

fn C.cJSON_IsString(object &C.cJSON) bool

fn C.cJSON_IsArray(object &C.cJSON) bool

fn C.cJSON_IsObject(object &C.cJSON) bool

fn C.cJSON_IsRaw(object &C.cJSON) bool

fn C.cJSON_AddItemToObject(object &C.cJSON, const_key &char, item &C.cJSON)

fn C.cJSON_AddItemToArray(object &C.cJSON, item &C.cJSON)

fn C.cJSON_Delete(object &C.cJSON)

fn C.cJSON_Print(object &C.cJSON) &char

fn C.cJSON_PrintUnformatted(object &C.cJSON) &char

//

// version returns the version of cJSON as a string
pub fn version() string {
	return unsafe { tos3(&char(C.cJSON_Version())) }
}

//

// create_object creates a new JSON object/map item. Use .add_item_to_object(key, value) calls, to add other items to it later.
pub fn create_object() &Node {
	return C.cJSON_CreateObject()
}

// create_array creates a new JSON array item. Use .add_item_to_array(value) calls, to add items to it later.
pub fn create_array() &Node {
	return C.cJSON_CreateArray()
}

// create_string creates a new JSON string item.
pub fn create_string(val string) &Node {
	return C.cJSON_CreateString(&char(val.str))
}

// create_raw creates a new JSON RAW string item.
pub fn create_raw(const_val string) &Node {
	return C.cJSON_CreateRaw(&char(const_val.str))
}

// create_number creates a new JSON number item.
pub fn create_number(val f64) &Node {
	return C.cJSON_CreateNumber(val)
}

// create_bool creates a new JSON boolean item.
pub fn create_bool(val bool) &Node {
	return C.cJSON_CreateBool(val)
}

// create_true creates a new JSON boolean item, with value `true`.
pub fn create_true() &Node {
	return C.cJSON_CreateTrue()
}

// create_false creates a new JSON boolean item, with value `false`.
pub fn create_false() &Node {
	return C.cJSON_CreateFalse()
}

// create_null creates a new JSON NULL item, with the value `null`. It symbolises a missing value for a given key in an object.
pub fn create_null() &Node {
	return C.cJSON_CreateNull()
}

//

// delete removes the given node from memory.
// NB: DO NOT USE that node, after you have called `unsafe { delete(node) }` !
[unsafe]
pub fn delete(node &Node) {
	C.cJSON_Delete(node)
}

//

// add_item_to_array adds the given item to the object, under the given `key`.
pub fn (mut obj Node) add_item_to_object(key string, item &Node) {
	C.cJSON_AddItemToObject(obj, &char(key.str), item)
}

// add_item_to_array append the given item to the object.
pub fn (mut obj Node) add_item_to_array(item &Node) {
	C.cJSON_AddItemToArray(obj, item)
}

//

// print serialises the node to a string, formatting its structure, so the resulting string is more prettier/human readable.
pub fn (mut obj Node) print() string {
	mut s := C.cJSON_Print(obj)
	return unsafe { tos3(s) }
}

// print serialises the node to a string, without formatting its structure, so the resulting string is shorter/cheaper to transmit.
pub fn (mut obj Node) print_unformatted() string {
	mut s := C.cJSON_PrintUnformatted(obj)
	return unsafe { tos3(s) }
}

// str returns the unformatted serialisation to string of the given Node.
pub fn (mut obj Node) str() string {
	return obj.print_unformatted()
}
