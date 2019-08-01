// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module url

struct ValueStruct {
pub:
mut:
	data []string
}

type Value ValueStruct

struct Values {
pub:
mut:
	data map[string]Value
	size int
}

fn new_values() Values {
	return Values{
		data: map[string]Value{}
	}
}

pub fn (v Value) all() []string {
	return v.data
}

// Get gets the first value associated with the given key.
// If there are no values associated with the key, Get returns
// the empty string. To access multiple values, use the map
// directly.
fn (v Values) get(key string) string {
	if v.data.size == 0 {
		return ''
	}
	vs := v.data[key]
	if vs.data.len == 0 {
		return ''
	}
	return vs.data[0]
}

// Set sets the key to value. It replaces any existing
// values.
fn (v Values) set(key, value string) {
	v.data[key].data = [value]
	v.size = v.data.size
}

// Add adds the value to key. It appends to any existing
// values associated with key.
fn (v mut Values) add(key, value string) {
	mut a := v.data[key]
	a.data << value
}

// Del deletes the values associated with key.
fn (v mut Values) del(key string) {
	v.data.delete(key)
	v.size = v.data.size
}

pub fn (v Values) iter() map[string]Value {
	return v.data
}