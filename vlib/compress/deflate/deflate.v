module deflate

import compress as compr

// pack compresses an array of bytes using deflate and returns the result in a new array
// Example: compressed := deflate.compress(b)!
pub fn pack(data []u8) ![]u8 {
	return compr.pack(data, 0)
}

// unpack decompresses an array of bytes using deflate and returns the result in a new array
// Example: decompressed := deflate.decompress(b)!
pub fn unpack(data []u8) ![]u8 {
	return compr.unpack(data, 0)
}

[deprecated: 'use pack() instead']
[deprecated_after: '2023-10-31']
pub fn compress(data []u8) ![]u8 {
	return pack(data)
}

[deprecated: 'use unpack() instead']
[deprecated_after: '2023-10-31']
pub fn decompress(data []u8) ![]u8 {
	return unpack(data)
}
