module decoder2

import json2

// decode_array decodes a JSON string into a specified type. This is the same as decode.
@[deprecated: '`decode_array` has been moved to `json2`, use `decode` from `json2` instead']
@[deprecated_after: '2025-03-18']
pub fn decode_array[T](src string) !T {
	return json2.decode[T](src)!
}
