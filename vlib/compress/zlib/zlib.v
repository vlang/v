module zlib

import compress as compr

// pack compresses an array of bytes using zlib and returns the result in a new array
// Example: compressed := zlib.compress(b)!
pub fn pack(data []u8) ![]u8 {
	// flags = TDEFL_WRITE_ZLIB_HEADER (0x01000)
	return compr.pack(data, 0x01000)
}

// unpack decompresses an array of bytes using zlib and returns the result in a new array
// Example: decompressed := zlib.decompress(b)!
pub fn unpack(data []u8) ![]u8 {
	// flags = TINFL_FLAG_PARSE_ZLIB_HEADER (0x1)
	return compr.unpack(data, 0x1)
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
