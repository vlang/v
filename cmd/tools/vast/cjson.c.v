module main

// vast builds its JSON output with the cJSON C library directly.
#flag -I @VEXEROOT/thirdparty/cJSON
#flag @VEXEROOT/thirdparty/cJSON/cJSON.o
#include "cJSON.h"

// cJSON uses `libm`.
$if windows {
	$if tinyc {
		#flag @VEXEROOT/thirdparty/tcc/lib/openlibm.o
	}
} $else {
	#flag -lm
}

@[typedef]
struct C.cJSON {}

type Node = C.cJSON

fn C.cJSON_CreateObject() &C.cJSON

fn C.cJSON_CreateArray() &C.cJSON

fn C.cJSON_CreateTrue() &C.cJSON

fn C.cJSON_CreateFalse() &C.cJSON

fn C.cJSON_CreateNumber(f64) &C.cJSON

fn C.cJSON_CreateString(const_s &char) &C.cJSON

fn C.cJSON_AddItemToObject(object &C.cJSON, const_key &char, item &C.cJSON)

fn C.cJSON_AddItemToArray(object &C.cJSON, item &C.cJSON)

fn C.cJSON_Print(object &C.cJSON) &char

@[inline]
fn create_object() &Node {
	return C.cJSON_CreateObject()
}

@[inline]
fn create_array() &Node {
	return C.cJSON_CreateArray()
}

@[inline]
fn create_string(val string) &Node {
	return C.cJSON_CreateString(&char(val.str))
}

@[inline]
fn create_number(val f64) &Node {
	return C.cJSON_CreateNumber(val)
}

@[inline]
fn create_true() &Node {
	return C.cJSON_CreateTrue()
}

@[inline]
fn create_false() &Node {
	return C.cJSON_CreateFalse()
}

@[inline]
fn add_item_to_object(mut obj Node, key string, item &Node) {
	C.cJSON_AddItemToObject(obj, &char(key.str), item)
}

@[inline]
fn add_item_to_array(mut obj Node, item &Node) {
	C.cJSON_AddItemToArray(obj, item)
}

fn json_print(mut obj Node) string {
	return unsafe { tos3(C.cJSON_Print(obj)) }
}
