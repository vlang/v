module lz

// Format identifies which LZ-family codec variant to use.
pub enum Format {
	lz77
	lz78
	lzw
	lz4
	lzss
	lzma
	lzjb
}

// format_from_string parses a case-insensitive format name.
pub fn format_from_string(name string) !Format {
	key := name.to_lower()
	return match key {
		'lz77' { .lz77 }
		'lz78' { .lz78 }
		'lzw' { .lzw }
		'lz4' { .lz4 }
		'lzss' { .lzss }
		'lzma' { .lzma }
		'lzjb' { .lzjb }
		else { return error('unknown lz format: ${name}') }
	}
}

// compress compresses data with the selected LZ format.
pub fn compress(data []u8, format Format) ![]u8 {
	return match format {
		.lz77 { compress_lz77(data) }
		.lz78 { compress_lz78(data) }
		.lzw { compress_lzw(data) }
		.lz4 { compress_lz4(data) }
		.lzss { compress_lzss(data) }
		.lzma { compress_lzma(data) }
		.lzjb { compress_lzjb(data) }
	}
}

// decompress decompresses data with the selected LZ format.
pub fn decompress(data []u8, format Format) ![]u8 {
	return match format {
		.lz77 { decompress_lz77(data) }
		.lz78 { decompress_lz78(data) }
		.lzw { decompress_lzw(data) }
		.lz4 { decompress_lz4(data) }
		.lzss { decompress_lzss(data) }
		.lzma { decompress_lzma(data) }
		.lzjb { decompress_lzjb(data) }
	}
}
