// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import json2
import os
import strconv

// SaveStore is a small string-keyed settings/save container. Values are
// stored as strings so the whole store round-trips through JSON without
// losing typed accessors for the common primitives.
pub struct SaveStore {
pub mut:
	values map[string]string
}

// new_save_store returns an empty store.
pub fn new_save_store() SaveStore {
	return SaveStore{ values: map[string]string{} }
}

// set_string stores a string value.
pub fn (mut s SaveStore) set_string(key string, value string) {
	s.values[key] = value
}

// get_string returns a value or an empty string when the key is absent.
pub fn (s &SaveStore) get_string(key string) string {
	return s.values[key] or { '' }
}

// has reports whether a key exists.
pub fn (s &SaveStore) has(key string) bool {
	return key in s.values
}

// remove deletes a key.
pub fn (mut s SaveStore) remove(key string) {
	s.values.delete(key)
}

// set_int stores an integer as text.
pub fn (mut s SaveStore) set_int(key string, value int) {
	s.values[key] = value.str()
}

// get_int reads an integer, returning fallback when missing or malformed.
pub fn (s &SaveStore) get_int(key string, fallback int) int {
	raw := s.get_string(key)
	if raw == '' {
		return fallback
	}
	return strconv.atoi(raw) or { fallback }
}

// set_float stores a float as text.
pub fn (mut s SaveStore) set_float(key string, value f64) {
	s.values[key] = value.str()
}

// get_float reads a float, returning fallback when missing or malformed.
pub fn (s &SaveStore) get_float(key string, fallback f64) f64 {
	raw := s.get_string(key)
	if raw == '' {
		return fallback
	}
	return strconv.atof64(raw) or { fallback }
}

// set_bool stores a boolean as "true" or "false".
pub fn (mut s SaveStore) set_bool(key string, value bool) {
	s.values[key] = value.str()
}

// get_bool reads a boolean, returning fallback when missing or malformed.
pub fn (s &SaveStore) get_bool(key string, fallback bool) bool {
	raw := s.get_string(key)
	if raw == 'true' {
		return true
	}
	if raw == 'false' {
		return false
	}
	return fallback
}

// set_json stores any encodable value under key.
pub fn (mut s SaveStore) set_json[T](key string, value T) {
	s.values[key] = json2.encode(value)
}

// get_json decodes a value previously stored with set_json.
pub fn (s &SaveStore) get_json[T](key string) !T {
	return json2.decode[T](s.get_string(key))
}

// to_json serializes the whole store.
pub fn (s &SaveStore) to_json() string {
	return json2.encode(s.values)
}

// save_store_from_json parses a store produced by to_json.
pub fn save_store_from_json(text string) !SaveStore {
	return SaveStore{ values: json2.decode[map[string]string](text)! }
}

// save writes the store to path as JSON.
pub fn (s &SaveStore) save(path string) ! {
	os.write_file(path, s.to_json())!
}

// load_save_store reads a store from a JSON file.
pub fn load_save_store(path string) !SaveStore {
	return save_store_from_json(os.read_file(path)!)
}
