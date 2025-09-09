// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

// encode_pretty ...
@[deprecated: 'use `encode(..., prettify: true)` instead']
@[deprecated_after: '2025-10-30']
pub fn encode_pretty[T](typed_data T) string {
	return encode(typed_data, prettify: true)
}

// str returns the JSON string representation of the `map[string]Any` type.
pub fn (f map[string]Any) str() string {
	return Any(f).json_str()
}

// str returns the JSON string representation of the `[]Any` type.
pub fn (f []Any) str() string {
	return Any(f).json_str()
}

// str returns the string representation of the `Any` type. Use the `json_str` method.
// If you want to use the escaped str() version of the `Any` type.
pub fn (f Any) str() string {
	if f is string {
		return f
	} else {
		return f.json_str()
	}
}

// json_str returns the JSON string representation of the `Any` type.
pub fn (f Any) json_str() string {
	return encode(f)
}

// prettify_json_str returns the pretty-formatted JSON string representation of the `Any` type.
@[deprecated: 'use `encode(Any(...), prettify: true)` instead']
@[deprecated_after: '2025-10-30']
pub fn (f Any) prettify_json_str() string {
	return encode(f, prettify: true)
}
