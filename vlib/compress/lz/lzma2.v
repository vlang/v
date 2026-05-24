module lz

const lzma2_profile = MatchProfile{
	window:      65535
	min_match:   3
	max_match:   130
	max_literal: 128
}

// compress_lzma2 compresses data using a pure-V LZMA2-like stream.
pub fn compress_lzma2(data []u8) ![]u8 {
	return compress_with_profile(data, lzma2_profile, .lzma2)
}

// decompress_lzma2 decompresses data produced by compress_lzma2.
pub fn decompress_lzma2(data []u8) ![]u8 {
	return decompress_with_profile(data, lzma2_profile, .lzma2)
}
