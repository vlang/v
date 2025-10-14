module decoder2

import x.json2

@[deprecated: '`decode` has been moved to `x.json2`, use `decode` from `x.json2` instead']
@[deprecated_after: '2025-10-12']
fn decode[T](val string) !T {
	return json2.decode[T](val)
}

@[deprecated: '`decode` has been moved to `x.json2`, use `decode` from `x.json2` instead']
@[deprecated_after: '2025-03-18']
fn decode_array[T](src string) !T {
	return json2.decode[T](src)
}
