module zlib

import compress

// compresses an array of bytes using zlib and returns the compressed bytes in a new array
// Example: compressed := zlib.compress(b)?
pub fn compress(data []u8) ![]u8 {
	// flags = TDEFL_WRITE_ZLIB_HEADER (0x01000)
	return compress.compress(data, 0x01000)
}

// decompresses an array of bytes using zlib and returns the decompressed bytes in a new array
// Example: decompressed := zlib.decompress(b)?
pub fn decompress(data []u8) ![]u8 {
	// flags = TINFL_FLAG_PARSE_ZLIB_HEADER (0x1)
	return compress.decompress(data, 0x1)
}
