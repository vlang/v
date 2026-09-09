// [rfc1952](https://datatracker.ietf.org/doc/html/rfc1952) compliant
// gzip compression/decompression

module gzip

import compress.deflate

// CompressFlags
@[deprecated: 'never used']
@[deprecated_after: '2026-07-31']
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
	write_zlib_header
	compute_adler32
	greedy_parsing_flag
	nondeterministic_parsing_flag
	rle_matches
	filter_matches
	force_all_static_blocks
	force_all_raw_blocks
}

// CompressParams set compression_level for compression:
@[deprecated: 'never used']
@[deprecated_after: '2026-07-31']
@[params]
pub struct CompressParams {
pub:
	compression_level int
	flags             CompressFlags
}

// compress compresses an array of bytes using gzip and returns the compressed bytes in a new array.
pub fn compress(data []u8) ![]u8 {
	// Delegate to deflate.compress_gzip() which implements RFC 1952
	return deflate.compress_gzip(data)
}

// DecompressFlags
// N.B.: only retained for API compatibility.
@[deprecated: 'never used']
@[deprecated_after: '2026-07-31']
@[flag]
pub enum DecompressFlags {
	parse_zlib_header
	has_more_input
	using_non_wrapping_output_buf
	compute_adler32
}

// DecompressParams controls gzip decompression behavior.
// N.B.: only retained for API compatibility.
@[deprecated: 'never used']
@[deprecated_after: '2026-07-31']
@[params]
pub struct DecompressParams {
pub:
	verify_header_checksum bool = true
	verify_length          bool = true
	verify_checksum        bool = true
	flags                  DecompressFlags
}

// validate validates the gzip header of data and returns its parsed details if valid.
// N.B.: only retained for API compatibility, all validation is now performed by the deflate backend.
// The returned header details are not used by the decompressor.
// TODO: remove after the deprecation period.
@[deprecated: 'never used']
@[deprecated_after: '2026-07-31']
pub fn validate(data []u8, _ DecompressParams) !deflate.GzipHeader {
	return deflate.validate_gzip_header(data)!
}

// decompress decompresses a gzip stream and returns the decompressed bytes in a new array.
pub fn decompress(data []u8) ![]u8 {
	return deflate.decompress_gzip(data)
}

// decompress_with_callback decompresses a gzip stream (RFC 1952) using a callback for chunked delivery.
// The callback receives chunks of decompressed data and should return the chunk length to continue, or 0 to abort.
// Returns the total decompressed length.
pub fn decompress_with_callback(data []u8, cb deflate.ChunkCallback, userdata voidptr) !int {
	deflate.validate_gzip_header(data)!
	return deflate.decompress_with_callback(data, cb, userdata)
}
