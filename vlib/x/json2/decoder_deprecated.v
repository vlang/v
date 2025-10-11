module json2

// decode_array is a generic function that decodes a JSON string into the array target type.
@[deprecated: 'use `decode` instead']
@[deprecated_after: '2025-03-18']
pub fn decode_array[T](src string) !T {
	return decode[T](src)
}

@[deprecated: 'use `decode[json2.Any]` instead']
@[deprecated_after: '2025-10-10']
pub fn raw_decode(src string) !Any {
	return decode[Any](src)
}
