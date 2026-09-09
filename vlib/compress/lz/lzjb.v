module lz

const lzjb_profile = MatchProfile{
	window:      1024
	min_match:   3
	max_match:   66
	max_literal: 128
}

// compress_lzjb compresses data using a pure-V LZJB-like stream.
pub fn compress_lzjb(data []u8) ![]u8 {
	return compress_with_profile(data, lzjb_profile, .lzjb)
}

// decompress_lzjb decompresses data produced by compress_lzjb.
pub fn decompress_lzjb(data []u8) ![]u8 {
	return decompress_with_profile(data, lzjb_profile, .lzjb)
}
