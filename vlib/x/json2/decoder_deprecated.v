module json2

// Decodes a JSON string into an `Any` type. Returns an option.
@[deprecated(msg: 'use `decode[json2.Any]` instead', after: '2025-10-10')]
pub fn raw_decode(src string) !Any {
	return decode[Any](src)
}
