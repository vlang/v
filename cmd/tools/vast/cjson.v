module main

import json.cjson

type Node = C.cJSON

fn as_n(p &cjson.Node) &Node {
	return unsafe { &Node(p) }
}

fn as_c(p &Node) &cjson.Node {
	return unsafe { &cjson.Node(p) }
}

@[inline]
fn create_object() &Node {
	return as_n(cjson.create_object())
}

@[inline]
fn create_array() &Node {
	return as_n(cjson.create_array())
}

@[inline]
fn create_string(val string) &Node {
	return as_n(cjson.create_string(val))
}

@[inline]
fn create_number(val f64) &Node {
	return as_n(cjson.create_number(val))
}

@[inline]
fn create_bool(val bool) &Node {
	return as_n(cjson.create_bool(val))
}

@[inline]
fn create_true() &Node {
	return as_n(cjson.create_true())
}

@[inline]
fn create_false() &Node {
	return as_n(cjson.create_false())
}

@[inline]
fn create_null() &Node {
	return as_n(cjson.create_null())
}

@[inline]
fn delete(b voidptr) {
	unsafe { cjson.delete(b) }
}

@[inline]
fn add_item_to_object(mut obj Node, key string, item &Node) {
	mut o := unsafe { &cjson.Node(obj) }
	o.add_item_to_object(key, item)
}

@[inline]
fn add_item_to_array(mut obj Node, item &Node) {
	mut o := as_c(obj)
	o.add_item_to_array(item)
}

fn json_print(mut obj Node) string {
	mut o := as_c(obj)
	return o.print()
}
