module lz

const lz77_profile = MatchProfile{
	window:      4096
	min_match:   3
	max_match:   130
	max_literal: 128
}

// compress_lz77 compresses data using a pure-V LZ77 style stream.
pub fn compress_lz77(data []u8) ![]u8 {
	return compress_with_profile(data, lz77_profile, .lz77)
}

// decompress_lz77 decompresses data produced by compress_lz77.
pub fn decompress_lz77(data []u8) ![]u8 {
	return decompress_with_profile(data, lz77_profile, .lz77)
}
