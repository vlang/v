module lz

const lzma_profile = MatchProfile{
	window:      32768
	min_match:   3
	max_match:   130
	max_literal: 128
}

// compress_lzma compresses data using a pure-V LZMA-like stream.
pub fn compress_lzma(data []u8) ![]u8 {
	return compress_with_profile(data, lzma_profile, .lzma)
}

// decompress_lzma decompresses data produced by compress_lzma.
pub fn decompress_lzma(data []u8) ![]u8 {
	return decompress_with_profile(data, lzma_profile, .lzma)
}
