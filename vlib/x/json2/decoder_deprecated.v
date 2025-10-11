module json2

@[deprecated: 'use `decode[json2.Any]` instead']
@[deprecated_after: '2025-10-10']
pub fn raw_decode(src string) !Any {
	return decode[Any](src)
}
