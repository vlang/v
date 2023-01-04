module deflate

import compress

// compresses an array of bytes using deflate and returns the compressed bytes in a new array
// Example: compressed := deflate.compress(b)?
pub fn compress(data []u8) ![]u8 {
	return compress.compress(data, 0)
}

// decompresses an array of bytes using deflate and returns the decompressed bytes in a new array
// Example: decompressed := deflate.decompress(b)?
pub fn decompress(data []u8) ![]u8 {
	return compress.decompress(data, 0)
}
