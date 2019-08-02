// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module urllib

struct ValueStruct {
pub:
mut:
	data []string
}

// using this instead of just ValueStruct
// because of unknown map initializer bug
type Value ValueStruct

struct Values {
pub:
mut:
	data map[string]Value
	size int
}

pub fn new_values() Values {
	return Values{
		data: map[string]Value{}
	}
}

pub fn (v &Value) all() []string {
	return v.data
}

// Get gets the first value associated with the given key.
// If there are no values associated with the key, get returns
// a empty string.
pub fn (v Values) get(key string) string {
	if v.data.size == 0 {
		return ''
	}
	vs := v.data[key]
	if vs.data.len == 0 {
		return ''
	}
	return vs.data[0]
}

// Get gets the all the values associated with the given key.
// If there are no values associated with the key, get returns
// a empty []string.
pub fn (v Values) get_all(key string) []string {
	if v.data.size == 0 {
		return []string
	}
	vs := v.data[key]
	if vs.data.len == 0 {
		return []string
	}
	return vs.data
}

// Set sets the key to value. It replaces any existing
// values.
pub fn (v mut Values) set(key, value string) {
	mut a := v.data[key]
	a.data = [value]
	v.data[key] = a
	v.size = v.data.size
}

// Add adds the value to key. It appends to any existing
// values associated with key.
pub fn (v mut Values) add(key, value string) {
	mut a := v.data[key]
	if a.data.len == 0 {
		a.data = []string
	}
	a.data << value
	v.data[key] = a
	v.size = v.data.size
}

// Del deletes the values associated with key.
pub fn (v mut Values) del(key string) {
	v.data.delete(key)
	v.size = v.data.size
}

pub fn (v Values) iter() map[string]Value {
	return v.data
}
