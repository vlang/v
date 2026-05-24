module lz

const sample_data = ('The quick brown fox jumps over the lazy dog. '.repeat(12) +
	'aaaaaaaaabbbbbbbbbcccccccccdddddddddeeeeeeeee').bytes()

fn test_roundtrip_all_formats() {
	formats := [Format.lz77, .lz78, .lzw, .lz4, .lzss, .lzma, .lzma2, .lzjb]
	for format in formats {
		compressed := compress(sample_data, format)!
		decompressed := decompress(compressed, format)!
		assert decompressed == sample_data
	}
}

fn test_format_specific_api_roundtrip() {
	lz77_data := compress_lz77(sample_data)!
	assert decompress_lz77(lz77_data)! == sample_data

	lz78_data := compress_lz78(sample_data)!
	assert decompress_lz78(lz78_data)! == sample_data

	lzw_data := compress_lzw(sample_data)!
	assert decompress_lzw(lzw_data)! == sample_data

	lz4_data := compress_lz4(sample_data)!
	assert decompress_lz4(lz4_data)! == sample_data

	lzss_data := compress_lzss(sample_data)!
	assert decompress_lzss(lzss_data)! == sample_data

	lzma_data := compress_lzma(sample_data)!
	assert decompress_lzma(lzma_data)! == sample_data

	lzma2_data := compress_lzma2(sample_data)!
	assert decompress_lzma2(lzma2_data)! == sample_data

	lzjb_data := compress_lzjb(sample_data)!
	assert decompress_lzjb(lzjb_data)! == sample_data
}

fn test_mismatched_format_fails() {
	compressed := compress(sample_data, .lz77)!
	decompress(compressed, .lz4) or {
		assert err.msg().contains('format mismatch')
		return
	}
	assert false
}

fn test_decoded_length_too_large_fails() {
	mut corrupt := []u8{}
	corrupt << stream_magic
	corrupt << u8(Format.lz77)
	encode_uvarint(mut corrupt, u64(1) << 31)

	decompress_lz77(corrupt) or {
		assert err.msg().contains('decoded length too large')
		return
	}
	assert false
}

fn test_match_offset_too_large_fails() {
	mut corrupt := []u8{}
	corrupt << stream_magic
	corrupt << u8(Format.lz77)
	encode_uvarint(mut corrupt, u64(4))
	corrupt << u8(0)
	corrupt << `A`
	corrupt << u8(0x80)
	encode_uvarint(mut corrupt, u64(1) << 63)

	decompress_lz77(corrupt) or {
		assert err.msg().contains('bad match offset')
		return
	}
	assert false
}

fn test_high_entropy_roundtrip_large_window_formats() {
	mut data := []u8{len: 128 * 1024}
	mut state := u32(0x9e3779b9)
	for i in 0 .. data.len {
		state = state * 1664525 + 1013904223
		data[i] = u8(state >> 24)
	}

	for format in [Format.lz4, .lzma, .lzma2] {
		compressed := compress(data, format)!
		decompressed := decompress(compressed, format)!
		assert decompressed == data
	}
}
