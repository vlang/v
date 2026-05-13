module lz

const lzss_profile = MatchProfile{
	window:      4096
	min_match:   3
	max_match:   130
	max_literal: 128
}

// compress_lzss compresses data using a pure-V LZSS style stream.
pub fn compress_lzss(data []u8) ![]u8 {
	return compress_with_profile(data, lzss_profile, .lzss)
}

// decompress_lzss decompresses data produced by compress_lzss.
pub fn decompress_lzss(data []u8) ![]u8 {
	return decompress_with_profile(data, lzss_profile, .lzss)
}
