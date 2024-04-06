module deflate

import compress as compr

// compresses an array of bytes using deflate and returns the compressed bytes in a new array
// Example: compressed := deflate.compress(b)!
pub fn compress(data []u8) ![]u8 {
	return compr.compress(data, 0)
}

// decompresses an array of bytes using deflate and returns the decompressed bytes in a new array
// Example: decompressed := deflate.decompress(b)!
pub fn decompress(data []u8) ![]u8 {
	return compr.decompress(data, 0)
}
