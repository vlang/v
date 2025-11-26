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

// Null is a simple representation of the `null` value in JSON.
pub struct Null {}

// null is an instance of the Null type, to ease comparisons with it.
pub const null = Null{}

// from_json_null implements a custom decoder for json2
pub fn (mut n Null) from_json_null() {}

// to_json implements a custom encoder for json2
pub fn (n Null) to_json() string {
	return 'null'
}

// ValueKind enumerates the kinds of possible values of the Any sumtype.
enum ValueKind {
	array
	object
	string
	number
	boolean
	null
}
