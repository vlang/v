// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module urllib

struct QueryValue {
pub mut:
	key   string
	value string
}

struct Values {
pub mut:
	data []QueryValue
	len  int
}

// new_values returns a new Values struct for creating
// urlencoded query string parameters. it can also be to
// post form data with application/x-www-form-urlencoded.
// values.encode() will return the encoded data
pub fn new_values() Values {
	return Values{
		data: []QueryValue{}
	}
}

// get gets the first value associated with the given key.
// If there are no values associated with the key, get returns
// a empty string.
pub fn (v &Values) get(key string) string {
	if v.data.len == 0 {
		return ''
	}
	for qvalue in v.data {
		if qvalue.key == key {
			return qvalue.value
		}
	}
	return ''
}

// get_all gets the all the values associated with the given key.
// If there are no values associated with the key, get returns
// a empty []string.
pub fn (v &Values) get_all(key string) []string {
	if v.data.len == 0 {
		return []
	}
	mut values := []string{}
	for qvalue in v.data {
		if qvalue.key == key {
			values << qvalue.value
		}
	}
	return values
}

// set sets the key to value. It replaces any existing
// values, or create a new bucket with the new key if it is missed.
pub fn (mut v Values) set(key string, value string) {
	// A query string can contains several
	// duplicate, so we need to make sure that we
	// cover all the edge case.
	mut found := false
	for mut qvalue in v.data {
		if qvalue.key == key {
			found = true
			qvalue.value = value
		}
	}

	if !found {
		v.add(key, value)
	}
}

// add adds the value to key. It appends to any existing
// values associated with key.
pub fn (mut v Values) add(key string, value string) {
	v.data << QueryValue{
		key: key
		value: value
	}
	v.len = v.data.len
}

// del deletes the values associated with key.
pub fn (mut v Values) del(key string) {
	for idx, qvalue in v.data {
		if qvalue.key == key {
			v.data.delete(idx)
		}
	}
	v.len = v.data.len
}

// return the list of values in the query string
pub fn (v Values) values() []string {
	mut values := []string{}
	for qvalue in v.data {
		if qvalue.value != '' {
			values << qvalue.value
		}
	}
	return values
}

// return a map <key []value> of the query string
pub fn (v Values) to_map() map[string][]string {
	mut result := map[string][]string{}

	for qvalue in v.data {
		if qvalue.key in result {
			result[qvalue.key] << qvalue.value
		} else {
			result[qvalue.key] = [qvalue.value]
		}
	}

	return result
}
