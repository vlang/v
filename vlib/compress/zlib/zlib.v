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

// decompress_with_callback decompresses the given zlib `data` and calls `cb` with each chunk of
// decompressed bytes. A chunk is usually 32 KB or less. The chunk data received by `cb` should be
// cloned if it needs to be stored for later use.
// The callback should return the chunk length to continue, or 0 to abort early.
// Returns the total number of decompressed bytes delivered to the callback.
pub fn decompress_with_callback(data []u8, cb deflate.ChunkCallback, userdata voidptr) !int {
	return deflate.decompress_with_callback(data, cb, userdata)
}
