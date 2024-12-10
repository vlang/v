module decoder2

// decode_array is a generic function that decodes a JSON string into the array target type.
@[deprecated: 'use `decode` instead']
@[deprecated_after: '2025-03-18']
pub fn decode_array[T](src string) !T {
	return decode[T](src)
}
