module decoder2

import x.json2

// decode decodes a JSON string into a specified type.
@[deprecated: '`decode` has been moved to `x.json2`, use `decode` from `x.json2` instead']
@[deprecated_after: '2025-10-12']
pub fn decode[T](val string) !T {
	return json2.decode[T](val)!
}

// decode_array decodes a JSON string into a specified type. This is the same as decode.
@[deprecated: '`decode_array` has been moved to `x.json2`, use `decode` from `x.json2` instead']
@[deprecated_after: '2025-03-18']
pub fn decode_array[T](src string) !T {
	return json2.decode[T](src)!
}
