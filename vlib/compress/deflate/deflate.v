module deflate

import compress

// compresses an array of bytes using gzip and returns the compressed bytes in a new array
// Example: compressed := gzip.compress(b)?
pub fn compress(data []u8) ?[]u8 {
	return compress.compress(data, 0)
}

// decompresses an array of bytes using zlib and returns the decompressed bytes in a new array
// Example: decompressed := zlib.decompress(b)?
[manualfree]
pub fn decompress(data []u8) ?[]u8 {
	return compress.decompress(data, 0)
}
