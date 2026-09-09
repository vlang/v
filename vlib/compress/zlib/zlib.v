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

// decompress_with_callback decompresses a zlib stream (RFC 1950) using a callback for chunked delivery.
// The callback receives chunks of decompressed data and should return the chunk length to continue, or 0 to abort.
// Returns the total decompressed length.
pub fn decompress_with_callback(data []u8, cb deflate.ChunkCallback, userdata voidptr) !int {
	deflate.validate_zlib_header(data)!
	return deflate.decompress_with_callback(data, cb, userdata)
}
