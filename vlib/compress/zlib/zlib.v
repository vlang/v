module zlib

import compress.deflate

// compress compresses data using the zlib container format.
pub fn compress(data []u8) ![]u8 {
	return deflate.compress_zlib(data)
}

// decompress decompresses zlib-compressed data.
pub fn decompress(data []u8) ![]u8 {
	return deflate.decompress_zlib(data)
}
