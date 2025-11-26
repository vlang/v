module json2

// implements encoding json, this is not validated so implementations must be correct
pub interface JsonEncoder {
	// to_json returns a string containing an objects json representation
	to_json() string
}

// Encodable is an interface, that allows custom implementations for encoding structs to their string based JSON representations.

@[deprecated: 'use `to_json` to implement `JsonEncoder` instead']
@[deprecated_after: '2025-10-30']
pub interface Encodable {
	json_str() string
}

// implements decoding json strings, e.g. "hello, \u2164!"
pub interface StringDecoder {
mut:
	// called with raw string (minus apostrophes) e.g. 'hello, \u2164!'
	from_json_string(raw_string string) !
}

// implements decoding json numbers, e.g. -1.234e23
pub interface NumberDecoder {
mut:
	// called with raw string of number e.g. '-1.234e23'
	from_json_number(raw_number string) !
}

// implements decoding json true/false
pub interface BooleanDecoder {
mut:
	// called with converted bool
	// already checked so no error needed
	from_json_boolean(boolean_value bool)
}

// implements decoding json null
pub interface NullDecoder {
mut:
	// only has one value
	// already checked so no error needed
	from_json_null()
}

// Implement once generic interfaces are more stable

// // implements decoding json arrays, e.g. ["hi", "bye", 0.9, true]
// // elements are already decoded
// pub interface ArrayDecoder[T] {
// mut:
// 	// called for every element in array e.g. 'hi', 'bye', '0.9', 'true'
// 	from_json_value(raw_value T)!
// }

// // implements decoding json object, e.g. {"name": "foo", "name": "bar", "age": 99}
// // this allows for duplicate/ordered keys to be decoded
// // elements are already decoded
// pub interface ObjectDecoder[T] {
// mut:
// 	// called for every key-value pair in object (minus apostrophes for keys) e.g. ('name': 'foo'), ('name': 'bar'), ('age': '99')
// 	from_json_pair(raw_key string, raw_value T)!
// }
