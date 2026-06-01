// [rfc1952](https://datatracker.ietf.org/doc/html/rfc1952) compliant
// gzip compression/decompression

module gzip

import compress.deflate

// CompressFlags
@[deprecated: 'never used']
@[deprecated_after: '2026-12-31']
@[flag]
pub enum CompressFlags {
	// The low 12 bits will be overwritten by `compression_level`
	compression_level_overwrite_flag01
	compression_level_overwrite_flag02
	compression_level_overwrite_flag03
	compression_level_overwrite_flag04
	compression_level_overwrite_flag05
	compression_level_overwrite_flag06
	compression_level_overwrite_flag07
	compression_level_overwrite_flag08
	compression_level_overwrite_flag09
	compression_level_overwrite_flag10
	compression_level_overwrite_flag11
	compression_level_overwrite_flag12

	// If set, the compressor outputs a zlib header before the deflate data, and the Adler-32 of the source data at the end.
	// Otherwise, you'll get raw deflate data.
	write_zlib_header //= 0x01000
	// Always compute the adler-32 of the input data (even when not writing zlib headers).
	compute_adler32 //= 0x02000
	// Set to use faster greedy parsing, instead of more efficient lazy parsing.
	greedy_parsing_flag //= 0x04000
	// Enable to decrease the compressor's initialization time to the minimum, but the output may vary from run to run given the same input (depending on the contents of memory).
	nondeterministic_parsing_flag //= 0x08000
	// Only look for RLE matches (matches with a distance of 1)
	rle_matches //= 0x10000
	// Discards matches <= 5 chars if enabled.
	filter_matches //= 0x20000
	// Disable usage of optimized Huffman tables.
	force_all_static_blocks //= 0x40000
	// Only use raw (uncompressed) deflate blocks.
	force_all_raw_blocks //= 0x80000
}

// CompressParams set compression_level for compression:
@[deprecated: 'never used']
@[deprecated_after: '2026-12-31']
@[params]
pub struct CompressParams {
pub:
	compression_level int
	flags             CompressFlags
}

// compress compresses an array of bytes using gzip and returns the compressed bytes in a new array.
// TODO: remove the CompressParams argument after the deprecation period.
pub fn compress(data []u8, _ CompressParams) ![]u8 {
	// Delegate to deflate.compress_gzip() which implements RFC 1952
	return deflate.compress_gzip(data)
}

// DecompressParams controls gzip decompression behaviour.
// All verification is now performed by the deflate backend; this struct is kept for API compatibility.
// TODO: remove after the deprecation period.
@[deprecated: 'never used']
@[deprecated_after: '2026-12-31']
@[params]
pub struct DecompressParams {}

// validate validates the gzip header of data and returns its parsed details if valid.
// Note: only retained for API compatibility, all validation is now performed by the deflate backend.
// The returned header details are not used by the decompressor.
// TODO: remove after the deprecation period.
@[deprecated: 'never used']
@[deprecated_after: '2026-12-31']
pub fn validate(data []u8, _ DecompressParams) !deflate.GzipHeader {
	return deflate.validate_gzip_header(data)!
}

// decompress decompresses a gzip stream and returns the decompressed bytes in a new array.
// TODO: remove the DecompressParams argument after the deprecation period.
pub fn decompress(data []u8, _ DecompressParams) ![]u8 {
	return deflate.decompress_gzip(data)
}

// decompress_with_callback decompresses a gzip stream (RFC 1952) using a callback for chunked delivery.
// The callback receives chunks of decompressed data and should return the chunk length to continue, or 0 to abort.
// Returns the total decompressed length.
pub fn decompress_with_callback(data []u8, cb deflate.ChunkCallback, userdata voidptr) !int {
	deflate.validate_gzip_header(data)!
	return deflate.decompress_with_callback(data, cb, userdata)
}
