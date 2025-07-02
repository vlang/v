module json2

import time

// Any is a sum type that lists the possible types to be decoded and used.
// `Any` priority order for numbers: floats -> signed integers -> unsigned integers
// `Any` priority order for strings: string -> time.Time
pub type Any = []Any
	| bool
	| f64
	| f32
	| i64
	| int
	| i32
	| i16
	| i8
	| map[string]Any
	| string
	| time.Time
	| u64
	| u32
	| u16
	| u8
	| Null

// Decodable is an interface, that allows custom implementations for decoding structs from JSON encoded values.
pub interface Decodable {
	from_json(f Any)
}

// Encodable is an interface, that allows custom implementations for encoding structs to their string based JSON representations.
pub interface Encodable {
	json_str() string
}

// Null is a simple representation of the `null` value in JSON.
pub struct Null {
	is_null bool = true
}

// null is an instance of the Null type, to ease comparisons with it.
pub const null = Null{}

// ValueKind enumerates the kinds of possible values of the Any sumtype.
pub enum ValueKind {
	unknown
	array
	object
	string_
	number
}

// str returns the string representation of the specific ValueKind.
pub fn (k ValueKind) str() string {
	return match k {
		.unknown { 'unknown' }
		.array { 'array' }
		.object { 'object' }
		.string_ { 'string' }
		.number { 'number' }
	}
}
