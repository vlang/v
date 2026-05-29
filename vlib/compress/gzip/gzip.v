// [rfc1952](https://datatracker.ietf.org/doc/html/rfc1952) compliant
// gzip compression/decompression

module gzip

import compress.deflate

// CompressParams set compression_level for compression:
// 0: Huffman only;
// 1: Huffman+LZ (fastest/crap compression);
// 128: default_max_probes;
// 4095: Huffman+LZ (slowest/best compression)
@[params]
pub struct CompressParams {
pub:
	compression_level int = 128 // 0~4095
}

// compress compresses an array of bytes using gzip and returns the compressed bytes in a new array
// Note: compression_level 0~4095 (currently unused, reserved for future optimization)
pub fn compress(data []u8, params CompressParams) ![]u8 {
	if params.compression_level !in 0..4096 {
		return error('compression level should in [0,4095]')
	}
	// Delegate to deflate.compress_gzip() which implements RFC 1952
	return deflate.compress_gzip(data)
}

// DecompressParams controls gzip decompression behaviour.
// All verification is now performed by the deflate backend; this struct
// is kept for API compatibility.
@[params]
pub struct DecompressParams {}

// validate validates the gzip header of data and returns its parsed details if valid.
// Note: only retained for API compatibility, all validation is now performed by the deflate backend.
// The returned header details are not used by the decompressor.
pub fn validate(data []u8, _ DecompressParams) !deflate.GzipHeader {
	return deflate.validate_gzip_header(data)!
}

// decompress decompresses a gzip stream and returns the decompressed bytes in a new array.
pub fn decompress(data []u8, params DecompressParams) ![]u8 {
	return deflate.decompress_gzip(data)
}

// decompress_with_callback decompresses a gzip stream (RFC 1952) using a callback for chunked delivery. The callback
// receives chunks of decompressed data and should return the chunk length to continue, or 0 to abort.
// Returns the total decompressed length.
pub fn decompress_with_callback(data []u8, cb deflate.ChunkCallback, userdata voidptr) !int {
	deflate.validate_gzip_header(data)!
	return deflate.decompress_with_callback(data, cb, userdata)
}
