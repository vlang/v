module lz

const lz4_profile = MatchProfile{
	window:      65535
	min_match:   4
	max_match:   130
	max_literal: 128
}

// compress_lz4 compresses data using a pure-V LZ4-like stream.
pub fn compress_lz4(data []u8) ![]u8 {
	return compress_with_profile(data, lz4_profile, .lz4)
}

// decompress_lz4 decompresses data produced by compress_lz4.
pub fn decompress_lz4(data []u8) ![]u8 {
	return decompress_with_profile(data, lz4_profile, .lz4)
}
