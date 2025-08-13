module zlib

import compress as compr

// compresses an array of bytes using zlib and returns the compressed bytes in a new array
// Example: b := 'abcdefgh'.repeat(1000).bytes(); cmpr := zlib.compress(b)!; assert cmpr.len < b.len; dc := zlib.decompress(cmpr)!; assert b == dc
pub fn compress(data []u8) ![]u8 {
	// flags = TDEFL_WRITE_ZLIB_HEADER (0x01000)
	return compr.compress(data, 0x01000)
}

// decompresses an array of bytes using zlib and returns the decompressed bytes in a new array
// Example: b := 'abcdefgh'.repeat(1000).bytes(); cmpr := zlib.compress(b)!; assert cmpr.len < b.len; dc := zlib.decompress(cmpr)!; assert b == dc
pub fn decompress(data []u8) ![]u8 {
	// flags = TINFL_FLAG_PARSE_ZLIB_HEADER (0x1)
	return compr.decompress(data, 0x1)
}
