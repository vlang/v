module deflate

import compress as compr

// compresses an array of bytes using deflate and returns the compressed bytes in a new array
// Example: b := 'abcabc'.repeat(100).bytes(); compressed := deflate.compress(b)!; dump(compressed); assert compressed.len == 163
pub fn compress(data []u8) ![]u8 {
	return compr.compress(data, 0)
}

// decompresses an array of bytes using deflate and returns the decompressed bytes in a new array
// Example: b := 'abcabc'.repeat(100).bytes(); compressed := deflate.compress(b)!; decompressed := deflate.decompress(compressed)!; assert b == decompressed
pub fn decompress(data []u8) ![]u8 {
	return compr.decompress(data, 0)
}
