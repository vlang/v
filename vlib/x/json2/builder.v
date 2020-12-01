// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

// Inserts a string into the map.
pub fn (mut obj map[string]Any) insert_str(key string, val string) {
	mut fi := Any{}
	fi = val
	obj[key] = fi
}

// Inserts an int into the map.
pub fn (mut obj map[string]Any) insert_int(key string, val int) {
	obj[key] = Any(val)
}

// Inserts a float into the map.
pub fn (mut obj map[string]Any) insert_f(key string, val f64) {
	obj[key] = Any(val)
}

// Inserts a null into the map.
pub fn (mut obj map[string]Any) insert_null(key string) {
	obj[key] = Any(Null{})
}

// Inserts a bool into the map.
pub fn (mut obj map[string]Any) insert_bool(key string, val bool) {
	obj[key] = Any(val)
}

// Inserts a map into the map.
pub fn (mut obj map[string]Any) insert_map(key string, val map[string]Any) {
	obj[key] = Any(val)
}

// Inserts an array into the map.
pub fn (mut obj map[string]Any) insert_arr(key string, val []Any) {
	obj[key] = Any(val)
}

// Inserts a string into the array.
pub fn (mut arr []Any) insert_str(val string) {
	mut fi := Any{}
	fi = val
	arr << fi
}

// Inserts an int into the array.
pub fn (mut arr []Any) insert_int(val int) {
	arr << Any(val)
}

// Inserts a float into the array.
pub fn (mut arr []Any) insert_f(val f64) {
	arr << Any(val)
}

// Inserts a null into the array.
pub fn (mut arr []Any) insert_null() {
	arr << Any(Null{})
}

// Inserts a bool into the array.
pub fn (mut arr []Any) insert_bool(val bool) {
	arr << Any(val)
}

// Inserts a map into the array.
pub fn (mut arr []Any) insert_map(val map[string]Any) {
	arr << Any(val)
}

// Inserts an array into the array.
pub fn (mut arr []Any) insert_arr(val []Any) {
	arr << Any(val)
}
