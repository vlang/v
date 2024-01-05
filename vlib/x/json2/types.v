module json2

import time

// `Any` is a sum type that lists the possible types to be decoded and used.

// @[deprecated: 'use `Any` module']
// @[deprecated_after: '2024-03-18']
pub type Any = []Any
	| bool
	| f32
	| f64
	| i16
	| i64
	| i8
	| int
	| map[string]Any
	| string
	| time.Time
	| u16
	| u32
	| u64
	| u8

// Decodable is an interface, that allows custom implementations for decoding structs from JSON encoded values
pub interface Decodable {
	from_json(f Any)
}

// Decodable is an interface, that allows custom implementations for encoding structs to their string based JSON representations
pub interface Encodable {
	json_str() string
}

// ValueKind enumerates the kinds of possible values of the Any sumtype.
pub enum ValueKind {
	unknown
	array
	object
	string_
	number
}

// str returns the string representation of the specific ValueKind
pub fn (k ValueKind) str() string {
	return match k {
		.unknown { 'unknown' }
		.array { 'array' }
		.object { 'object' }
		.string_ { 'string' }
		.number { 'number' }
	}
}
