module brotli

import hash.huffman

struct WindowBitsCase {
	bits  u32
	nbits int
	want  int
}

struct MetaBlockLengthCase {
	nibbles int
	value   int
}

struct ImplicitDistanceCopyCase {
	cmd_sym    int
	insert_len int
	copy_len   int
}

struct RoundtripCase {
	data []u8
}

fn find_implicit_distance_copy_case(min_insert int) !ImplicitDistanceCopyCase {
	for sym in 0 .. 704 {
		cmd := cmd_lut(sym)
		insert_min := cmd.insert_len_offset
		insert_max := cmd.insert_len_offset + (1 << cmd.insert_len_extra_bits) - 1
		copy_min := if cmd.copy_len_offset > 0 {
			cmd.copy_len_offset
		} else if cmd.copy_len_extra_bits > 0 {
			1
		} else {
			0
		}
		if copy_min <= 0 {
			continue
		}
		if insert_max < min_insert {
			continue
		}
		insert_len := if min_insert > insert_min { min_insert } else { insert_min }
		return ImplicitDistanceCopyCase{
			cmd_sym:    sym
			insert_len: insert_len
			copy_len:   copy_min
		}
	}
	return error('brotli: no implicit-distance command symbol for copy case')
}

fn write_metadata_skip_block(mut w BitWriter, skip_len int, is_last bool) {
	w.write_bit(u8(if is_last { 1 } else { 0 }))
	if is_last {
		w.write_bit(0) // ISLASTEMPTY
	}
	w.write_bits(3, 2) // MNIBBLES = 0 => metadata block
	w.write_bit(0) // reserved
	if skip_len == 0 {
		w.write_bits(0, 2) // MSKIPBYTES = 0
		w.align_byte()
		return
	}
	assert skip_len - 1 <= 0xffffff
	mskip_bytes := if skip_len - 1 <= 0xff {
		1
	} else if skip_len - 1 <= 0xffff {
		2
	} else {
		3
	}
	w.write_bits(u32(mskip_bytes), 2)
	w.write_bits(u32(skip_len - 1), mskip_bytes * 8)
}

fn test_trivial_roundtrip_very_large_uncompressed_block() {
	data := []u8{len: (1 << 24) + 7, init: u8((index * 23 + 9) & 0xff)}
	compressed := compress(data)!
	decompressed := decompress(compressed)!
	assert decompressed == data
}

fn test_trivial_roundtrip_stress_sizes() {
	for size in [
		0,
		1,
		2,
		3,
		7,
		15,
		31,
		63,
		64,
		65,
		127,
		255,
		256,
		257,
		1023,
		1024,
		1025,
		trivial_block_size - 1,
		trivial_block_size,
		trivial_block_size + 1,
		(1 << 20) - 1,
		1 << 20,
		(1 << 20) + 1,
	] {
		data := []u8{len: size, init: u8((index * 37 + 5) & 0xff)}
		compressed := compress(data)!
		decompressed := decompress(compressed)!
		assert decompressed == data
	}
}

fn test_compress_options() {
	data := 'option-aware Brotli payload '.repeat(32).bytes()
	for window_size in [16, 17, 22, 24] {
		for quality in [0, 5, 11] {
			compressed := compress(data, quality: quality, window_size: window_size)!
			assert decompress(compressed)! == data
		}
	}
	commented := compress(data, comment: 'test comment'.bytes(), large_window: 25)!
	assert decompress(commented, comment: 'test comment'.bytes())! == data
	assert decompress(compress(data, large_window: 25)!)! == data
	dictionary := 'custom dictionary payload'.bytes()
	dictionary_compressed := compress(dictionary, dictionary: dictionary)!
	assert dictionary_compressed.len < dictionary.len
	assert decompress(dictionary_compressed, dictionary: dictionary)! == dictionary
	static_word_len := 20
	static_word_offset := brotli_dictionary_offsets[static_word_len - 4]
	static_word := brotli_dictionary_data[static_word_offset..static_word_offset + static_word_len]
	static_dictionary := transform_dictionary_word(static_word, 0)!
	static_compressed := compress(static_dictionary)!
	assert decompress(static_compressed)! == static_dictionary
	assert static_compressed.len < static_dictionary.len
	if _ := compress(data, quality: -1) {
		assert false
	}
	if _ := compress(data, quality: 12) {
		assert false
	}
	if _ := compress(data, window_size: 15) {
		assert false
	}
	if _ := compress(data, window_size: 25) {
		assert false
	}
	if _ := compress(data, comment: []u8{len: 81}) {
		assert false
	}
	if _ := compress(data, large_window: 9) {
		assert false
	}
	if _ := compress(data, large_window: 31) {
		assert false
	}
}

fn test_prefix_decoder_uses_flat_table() {
	decoder := build_prefix_decoder([1, 1])!
	assert decoder.flat.len == 2
	mut writer := BitWriter{}
	writer.write_bit(1)
	mut reader := BitReader{
		data: writer.finish()
	}
	assert reader.read_prefix_symbol(decoder)! == 1
}

fn test_quality_and_window_select_periodic_strategy() {
	pattern := []u8{len: 120, init: u8((index * 37 + index / 7) & 63)}
	data := pattern.repeat(4)
	low_effort := compress(data, quality: 1, window_size: 16)!
	high_effort := compress(data, quality: 11, window_size: 24)!
	small_window := compress(data, quality: 11, window_size: 16)!
	assert decompress(low_effort)! == data
	assert decompress(high_effort)! == data
	assert decompress(small_window)! == data
	assert high_effort.len < low_effort.len
	assert high_effort.len <= small_window.len
}

fn test_cost_based_lz_roundtrip() {
	mut data := []u8{}
	for index in 0 .. 4096 {
		data << u8((index * 29 + index / 11) & 255)
	}
	data << 'cost-based Brotli parser '.repeat(128).bytes()
	compressed := compress(data, quality: 8, window_size: 16)!
	assert decompress(compressed)! == data
	assert compressed.len < data.len
}

fn test_concatenated_decompression() {
	first := compress('first stream'.bytes())!
	second := compress('second stream'.bytes())!
	mut combined := first.clone()
	combined << second
	assert decompress(combined, concatenated: true)! == 'first streamsecond stream'.bytes()
	if _ := decompress(combined) {
		assert false
	}
}

fn test_quality_and_window_dispatch_roundtrip() {
	data := ('quality and window dispatch '.repeat(64)).bytes()
	for window_size in [16, 22, 24] {
		for quality in [0, 5, 11] {
			compressed := compress(data, quality: quality, window_size: window_size)!
			mut reader := BitReader{ data: compressed }
			assert decode_window_bits(mut reader)! == window_size
			assert decompress(compressed)! == data
		}
	}
}

fn test_static_and_custom_dictionary_matches() {
	static_word_len := 20
	static_word_offset := brotli_dictionary_offsets[static_word_len - 4]
	static_word := brotli_dictionary_data[static_word_offset..static_word_offset + static_word_len]
	static_data := transform_dictionary_word(static_word, 0)!
	static_compressed := compress(static_data, quality: 5, window_size: 16)!
	assert static_compressed.len < static_data.len
	assert decompress(static_compressed)! == static_data

	dictionary := 'custom dictionary match '.repeat(8).bytes()
	custom_data := 'custom dictionary match '.repeat(2).bytes()
	custom_compressed := compress(custom_data, dictionary: dictionary, quality: 5, window_size: 16)!
	assert custom_compressed.len < custom_data.len
	assert decompress(custom_compressed, dictionary: dictionary)! == custom_data
}

fn test_dictionary_transform_lengths_match_output() {
	for word_len in [4, 8, 20, 24] {
		offset := brotli_dictionary_offsets[word_len - 4]
		word := brotli_dictionary_data[offset..offset + word_len]
		for transform_idx in 0 .. brotli_transforms.len / 3 {
			transformed := transform_dictionary_word(word, transform_idx) or { continue }
			assert dictionary_transform_output_len(word_len, transform_idx)! == transformed.len
		}
	}
}

fn test_trivial_roundtrip_exact_block_boundaries() {
	for size in [trivial_block_size, 1 << 20, 1 << 24] {
		data := []u8{len: size, init: u8((index * 13 + 7) & 0xff)}
		compressed := compress(data)!
		decompressed := decompress(compressed)!
		assert decompressed == data
	}
}

fn test_decompress_rejects_compressed_meta_block() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	w.write_bit(1) // ISLAST
	w.write_bit(0) // not last-empty
	w.write_bits(0, 2) // MNIBBLES = 4
	write_meta_block_length(mut w, 1, 4)
	if _ := decompress(w.finish()) {
		assert false
	}
}

fn test_decompress_rejects_large_window_stream() {
	mut w := BitWriter{}
	w.write_bit(1) // not WBITS=16
	w.write_bits(0, 3) // n == 0
	w.write_bits(1, 3) // n2 == 1 => large window
	if _ := decompress(w.finish()) {
		assert false
	}
}

fn test_decompress_rejects_non_trivial_streams() {
	if _ := decompress([u8(0)]) {
		assert false
	}
}

fn test_last_empty_meta_block_rejects_non_zero_padding() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	w.write_bit(1) // ISLAST
	w.write_bit(1) // ISLASTEMPTY
	w.write_bytes([u8(1)])
	if _ := decompress(w.finish()) {
		assert false
	}
}

fn test_last_empty_meta_block_rejects_extra_zero_byte() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	w.write_bit(1) // ISLAST
	w.write_bit(1) // ISLASTEMPTY
	w.write_bytes([u8(0)])
	if _ := decompress(w.finish()) {
		assert false
	}
}

fn test_minimal_compressed_meta_block_roundtrip() {
	data := []u8{len: 8, init: 0}
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	mut recent_distances := [16, 15, 11, 4]
	pattern := [u8(0)]
	lit_symbols := collect_small_alphabet_symbols(pattern, 256)
	cmd_sym := if data.len == pattern.len {
		find_literal_only_cmd_symbol_for_insert_len(pattern.len)!
	} else {
		find_copy_cmd_symbol(pattern.len, data.len - pattern.len)!
	}
	write_periodic_pattern_compressed_meta_block_with_symbols_and_cmd(mut w, pattern, data.len, true,
		lit_symbols, cmd_sym, mut recent_distances)!
	out := decompress(w.finish()) or {
		panic(err.msg())
	}
	assert out == data
}

fn test_implicit_distance_copy_compressed_meta_block_roundtrip() {
	implicit := find_implicit_distance_copy_case(0) or { panic(err.msg()) }
	explicit_sym := find_copy_cmd_symbol(1, 2) or { panic(err.msg()) }
	explicit_cmd := cmd_lut(explicit_sym)
	implicit_cmd := cmd_lut(implicit.cmd_sym)
	mlen := 1 + 2 + implicit.insert_len + implicit.copy_len
	data := []u8{len: mlen, init: u8(0x2a)}
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	w.write_bit(1) // ISLAST
	w.write_bit(0) // not last-empty
	mnibbles := meta_block_nibbles(data.len)
	w.write_bits(u32(match mnibbles {
		4 { 0 }
		5 { 1 }
		6 { 2 }
		else { 3 }
	}), 2)
	write_meta_block_length(mut w, data.len, mnibbles)
	write_varlen_uint8(mut w, 0) // 1 literal block type
	write_varlen_uint8(mut w, 0) // 1 insert-copy block type
	write_varlen_uint8(mut w, 0) // 1 distance block type
	w.write_bits(0, 2) // NPOSTFIX = 0
	w.write_bits(0, 4) // NDIRECT = 0
	w.write_bits(0, 2) // literal context mode
	write_varlen_uint8(mut w, 0) // 1 literal tree
	write_varlen_uint8(mut w, 0) // 1 distance tree
	write_simple_prefix_tree(mut w, 256, 0x2a)
	mut cmd_symbols := [explicit_sym, implicit.cmd_sym]
	sort_small(mut cmd_symbols)
	write_simple_prefix_tree_symbols(mut w, 704, cmd_symbols)!
	write_simple_prefix_tree(mut w, 64, 16)
	cmd_lengths := simple_prefix_lengths(704, cmd_symbols)!
	cmd_enc := huffman.build(
		lengths:   cmd_lengths
		max_bits:  max_length(cmd_lengths)
		bit_order: .lsb_first
	)!
	w.write_bits(cmd_enc.codes[explicit_sym], cmd_lengths[explicit_sym])
	w.write_bits(u32(1 - explicit_cmd.insert_len_offset), explicit_cmd.insert_len_extra_bits)
	w.write_bits(u32(2 - explicit_cmd.copy_len_offset), explicit_cmd.copy_len_extra_bits)
	w.write_bit(0) // distance symbol 16 extra bit
	w.write_bits(cmd_enc.codes[implicit.cmd_sym], cmd_lengths[implicit.cmd_sym])
	w.write_bits(u32(implicit.insert_len - implicit_cmd.insert_len_offset),
		implicit_cmd.insert_len_extra_bits)
	w.write_bits(u32(implicit.copy_len - implicit_cmd.copy_len_offset),
		implicit_cmd.copy_len_extra_bits)
	out := decompress(w.finish()) or { panic(err.msg()) }
	assert out == data
}

fn test_implicit_distance_copy_across_compressed_metablocks() {
	implicit := find_implicit_distance_copy_case(0) or { panic(err.msg()) }
	second_len := implicit.insert_len + implicit.copy_len
	mut w := BitWriter{}
	mut recent_distances := [16, 15, 11, 4]
	w.write_bit(0) // WBITS = 16
	pattern := [u8(0x5a)]
	lit_symbols := collect_small_alphabet_symbols(pattern, 256)
	cmd_sym := find_copy_cmd_symbol(pattern.len, 3 - pattern.len) or { panic(err.msg()) }
	write_periodic_pattern_compressed_meta_block_with_symbols_and_cmd(mut w, pattern, 3, false,
		lit_symbols, cmd_sym, mut recent_distances) or { panic(err.msg()) }
	w.write_bit(1) // ISLAST
	w.write_bit(0) // not last-empty
	mnibbles := meta_block_nibbles(second_len)
	w.write_bits(u32(match mnibbles {
		4 { 0 }
		5 { 1 }
		6 { 2 }
		else { 3 }
	}), 2)
	write_meta_block_length(mut w, second_len, mnibbles)
	write_varlen_uint8(mut w, 0) // 1 literal block type
	write_varlen_uint8(mut w, 0) // 1 insert-copy block type
	write_varlen_uint8(mut w, 0) // 1 distance block type
	w.write_bits(0, 2) // NPOSTFIX = 0
	w.write_bits(0, 4) // NDIRECT = 0
	w.write_bits(0, 2) // literal context mode
	write_varlen_uint8(mut w, 0) // 1 literal tree
	write_varlen_uint8(mut w, 0) // 1 distance tree
	write_simple_prefix_tree(mut w, 256, 0x5a)
	write_simple_prefix_tree(mut w, 704, implicit.cmd_sym)
	write_simple_prefix_tree(mut w, 64, 0) // unused for distance_code == 0
	cmd := cmd_lut(implicit.cmd_sym)
	w.write_bits(u32(implicit.insert_len - cmd.insert_len_offset), cmd.insert_len_extra_bits)
	w.write_bits(u32(implicit.copy_len - cmd.copy_len_offset), cmd.copy_len_extra_bits)
	want := []u8{len: 3 + second_len, init: u8(0x5a)}
	out := decompress(w.finish()) or { panic(err.msg()) }
	assert out == want
}

fn test_ndirect_distance_code_roundtrip() {
	data := []u8{len: 8, init: u8(0x5a)}
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	w.write_bit(1) // ISLAST
	w.write_bit(0) // not last-empty
	w.write_bits(0, 2) // MNIBBLES = 4
	write_meta_block_length(mut w, data.len, 4)
	write_varlen_uint8(mut w, 0) // 1 literal block type
	write_varlen_uint8(mut w, 0) // 1 insert-copy block type
	write_varlen_uint8(mut w, 0) // 1 distance block type
	w.write_bits(0, 2) // NPOSTFIX = 0
	w.write_bits(1, 4) // NDIRECT = 1
	w.write_bits(0, 2) // literal context mode
	write_varlen_uint8(mut w, 0) // 1 literal tree
	write_varlen_uint8(mut w, 0) // 1 distance tree
	write_simple_prefix_tree(mut w, 256, 0x5a)
	cmd_sym := find_copy_cmd_symbol(1, 7)!
	write_simple_prefix_tree(mut w, 704, cmd_sym)
	write_simple_prefix_tree(mut w, 65, 16) // direct distance code => distance 1
	cmd := cmd_lut(cmd_sym)
	w.write_bits(u32(1 - cmd.insert_len_offset), cmd.insert_len_extra_bits)
	w.write_bits(u32(7 - cmd.copy_len_offset), cmd.copy_len_extra_bits)
	assert decompress(w.finish())! == data
}

fn test_nonzero_literal_context_mode_roundtrip() {
	data := []u8{len: 8, init: u8(0x5a)}
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	w.write_bit(1) // ISLAST
	w.write_bit(0) // not last-empty
	w.write_bits(0, 2) // MNIBBLES = 4
	write_meta_block_length(mut w, data.len, 4)
	write_varlen_uint8(mut w, 0) // 1 literal block type
	write_varlen_uint8(mut w, 0) // 1 insert-copy block type
	write_varlen_uint8(mut w, 0) // 1 distance block type
	w.write_bits(0, 2) // NPOSTFIX = 0
	w.write_bits(0, 4) // NDIRECT = 0
	w.write_bits(3, 2) // literal context mode (nonzero)
	write_varlen_uint8(mut w, 0) // 1 literal tree
	write_varlen_uint8(mut w, 0) // 1 distance tree
	write_simple_prefix_tree(mut w, 256, 0x5a)
	cmd_sym := find_copy_cmd_symbol(1, 7)!
	write_simple_prefix_tree(mut w, 704, cmd_sym)
	write_simple_prefix_tree(mut w, 64, 16) // explicit distance code => distance 1
	cmd := cmd_lut(cmd_sym)
	w.write_bits(u32(1 - cmd.insert_len_offset), cmd.insert_len_extra_bits)
	w.write_bits(u32(7 - cmd.copy_len_offset), cmd.copy_len_extra_bits)
	w.write_bit(0) // distance symbol 16 extra bit
	assert decompress(w.finish())! == data
}

fn test_utf8_literal_context_lead_byte_boundaries() {
	assert compute_literal_context_id(1, 32, 2) == 0
	assert compute_literal_context_id(1, 33, 2) == 1
	assert compute_literal_context_id(1, 48, 2) == 2
	assert compute_literal_context_id(1, 97, 2) == 3
	assert compute_literal_context_id(1, 192, 2) == 0
	assert compute_literal_context_id(1, 223, 2) == 0
	assert compute_literal_context_id(1, 224, 2) == 2
}

fn test_short_distance_symbol_roundtrip() {
	data := []u8{len: 8, init: u8(0x5a)}
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	w.write_bit(1) // ISLAST
	w.write_bit(0) // not last-empty
	w.write_bits(0, 2) // MNIBBLES = 4
	write_meta_block_length(mut w, data.len, 4)
	write_varlen_uint8(mut w, 0) // 1 literal block type
	write_varlen_uint8(mut w, 0) // 1 insert-copy block type
	write_varlen_uint8(mut w, 0) // 1 distance block type
	w.write_bits(0, 2) // NPOSTFIX = 0
	w.write_bits(0, 4) // NDIRECT = 0
	w.write_bits(0, 2) // literal context mode
	write_varlen_uint8(mut w, 0) // 1 literal tree
	write_varlen_uint8(mut w, 0) // 1 distance tree
	write_simple_prefix_tree(mut w, 256, 0x5a)
	cmd_sym := find_copy_cmd_symbol(5, 3)!
	write_simple_prefix_tree(mut w, 704, cmd_sym)
	write_simple_prefix_tree(mut w, 64, 5) // short code: last distance + 1 => 5
	cmd := cmd_lut(cmd_sym)
	w.write_bits(u32(5 - cmd.insert_len_offset), cmd.insert_len_extra_bits)
	w.write_bits(u32(3 - cmd.copy_len_offset), cmd.copy_len_extra_bits)
	assert decompress(w.finish())! == data
}

fn test_repeated_byte_compressed_meta_block_roundtrip() {
	data := []u8{len: 97, init: u8(0x5a)}
	mut w := BitWriter{}
	mut recent_distances := [16, 15, 11, 4]
	w.write_bit(0) // WBITS = 16
	pattern := [u8(0x5a)]
	lit_symbols := collect_small_alphabet_symbols(pattern, 256)
	cmd_sym := find_copy_cmd_symbol(pattern.len, data.len - pattern.len)!
	write_periodic_pattern_compressed_meta_block_with_symbols_and_cmd(mut w, pattern, data.len, true,
		lit_symbols, cmd_sym, mut recent_distances)!
	assert decompress(w.finish())! == data
}

fn test_repeated_byte_compressed_meta_block_single_byte_roundtrip() {
	data := [u8(0x5a)]
	mut w := BitWriter{}
	mut recent_distances := [16, 15, 11, 4]
	w.write_bit(0) // WBITS = 16
	pattern := [u8(0x5a)]
	lit_symbols := collect_small_alphabet_symbols(pattern, 256)
	cmd_sym := find_literal_only_cmd_symbol_for_insert_len(pattern.len)!
	write_periodic_pattern_compressed_meta_block_with_symbols_and_cmd(mut w, pattern, data.len, true,
		lit_symbols, cmd_sym, mut recent_distances)!
	assert decompress(w.finish())! == data
}

fn test_decompress_rejects_invalid_copy_distance() {
	mut w := BitWriter{}
	mut recent_distances := [16, 15, 11, 4]
	w.write_bit(0) // WBITS = 16
	pattern := [u8(0x5a)]
	lit_symbols := collect_small_alphabet_symbols(pattern, 256)
	cmd_sym := find_copy_cmd_symbol(pattern.len, 12 - pattern.len)!
	write_periodic_pattern_compressed_meta_block_with_symbols_and_cmd(mut w, pattern, 12, true,
		lit_symbols, cmd_sym, mut recent_distances)!
	mut bad := w.finish().clone()
	if bad.len > 0 {
		bad[bad.len - 1] = 0xff
	}
	if _ := decompress(bad) {
		assert false
	}
}

fn test_small_alphabet_compressed_meta_block_roundtrip() {
	data := 'ABCDABACABADABCDABACABAD'.bytes()
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	write_small_alphabet_compressed_meta_block(mut w, data,
		[int(`A`), int(`B`), int(`C`), int(`D`)], true)!
	assert decompress(w.finish())! == data
}

fn test_two_byte_pattern_compressed_meta_block_roundtrip() {
	data := 'AB'.repeat(256).bytes()
	mut w := BitWriter{}
	mut recent_distances := [16, 15, 11, 4]
	w.write_bit(0) // WBITS = 16
	pattern := [u8(`A`), u8(`B`)]
	lit_symbols := collect_small_alphabet_symbols(pattern, 256)
	cmd_sym := find_copy_cmd_symbol(pattern.len, data.len - pattern.len)!
	write_periodic_pattern_compressed_meta_block_with_symbols_and_cmd(mut w, pattern, data.len, true,
		lit_symbols, cmd_sym, mut recent_distances)!
	assert decompress(w.finish())! == data
}

fn test_three_byte_pattern_compressed_meta_block_roundtrip() {
	data := 'ABC'.repeat(256).bytes()
	mut w := BitWriter{}
	mut recent_distances := [16, 15, 11, 4]
	w.write_bit(0) // WBITS = 16
	pattern := [u8(`A`), u8(`B`), u8(`C`)]
	lit_symbols := collect_small_alphabet_symbols(pattern, 256)
	cmd_sym := find_copy_cmd_symbol(pattern.len, data.len - pattern.len)!
	write_periodic_pattern_compressed_meta_block_with_symbols_and_cmd(mut w, pattern, data.len, true,
		lit_symbols, cmd_sym, mut recent_distances)!
	assert decompress(w.finish())! == data
}

fn test_four_byte_pattern_compressed_meta_block_roundtrip() {
	data := 'ABCD'.repeat(256).bytes()
	mut w := BitWriter{}
	mut recent_distances := [16, 15, 11, 4]
	w.write_bit(0) // WBITS = 16
	pattern := [u8(`A`), u8(`B`), u8(`C`), u8(`D`)]
	lit_symbols := collect_small_alphabet_symbols(pattern, 256)
	cmd_sym := find_copy_cmd_symbol(pattern.len, data.len - pattern.len)!
	write_periodic_pattern_compressed_meta_block_with_symbols_and_cmd(mut w, pattern, data.len, true,
		lit_symbols, cmd_sym, mut recent_distances)!
	assert decompress(w.finish())! == data
}

fn test_trivial_roundtrip_chunked() {
	data := []u8{len: trivial_block_size + 17, init: u8((index * 31) & 0xff)}
	compressed := compress(data)!
	decompressed := decompress(compressed)!
	assert decompressed == data
}

fn test_quality_five_random_block_boundary_roundtrip() {
	mut state := u32(0x13579bdf)
	mut data := []u8{len: 65537}
	for index in 0 .. data.len {
		state = state * 1664525 + 1013904223
		data[index] = u8(state >> 24)
	}
	compressed := compress(data, quality: 5)!
	assert decompress(compressed)! == data
}

fn test_trivial_roundtrip_cases() {
	cases := [
		RoundtripCase{ data: []u8{} },
		RoundtripCase{ data: 'Hello Brotli! '.repeat(2000).bytes() },
		RoundtripCase{ data: []u8{len: 4096, init: u8(index * 31 + (index >> 2))} },
	]
	for c in cases {
		compressed := compress(c.data)!
		assert decompress(compressed)! == c.data
	}
}

fn test_trivial_roundtrip_empty_stream() {
	compressed := compress([]u8{})!
	assert compressed == [u8(6)]
	assert decompress(compressed)! == []u8{}
}

fn test_trivial_roundtrip_large_uncompressed_block() {
	data := []u8{len: (1 << 20) + 1, init: u8((index * 17 + 3) & 0xff)}
	compressed := compress(data)!
	decompressed := decompress(compressed)!
	assert decompressed == data
}

fn test_trivial_roundtrip_repeated_byte_prefers_compressed() {
	data := []u8{len: 16384, init: u8(0x7f)}
	compressed := compress(data)!
	assert compressed.len < 128
	assert decompress(compressed)! == data
}

fn test_trivial_roundtrip_two_byte_pattern_prefers_compressed() {
	data := 'AB'.repeat(4096).bytes()
	compressed := compress(data)!
	mut uncompressed := BitWriter{}
	uncompressed.write_bit(0) // WBITS = 16
	write_uncompressed_meta_block(mut uncompressed, data, true)
	assert compressed.len < uncompressed.finish().len
	assert decompress(compressed)! == data
}

fn test_trivial_roundtrip_three_byte_pattern_prefers_compressed() {
	data := 'ABC'.repeat(3072).bytes()
	compressed := compress(data)!
	mut uncompressed := BitWriter{}
	uncompressed.write_bit(0) // WBITS = 16
	write_uncompressed_meta_block(mut uncompressed, data, true)
	assert compressed.len < uncompressed.finish().len
	assert decompress(compressed)! == data
}

fn test_trivial_roundtrip_four_byte_pattern_prefers_compressed() {
	data := 'ABCD'.repeat(3072).bytes()
	compressed := compress(data)!
	mut uncompressed := BitWriter{}
	uncompressed.write_bit(0) // WBITS = 16
	write_uncompressed_meta_block(mut uncompressed, data, true)
	assert compressed.len < uncompressed.finish().len
	assert decompress(compressed)! == data
}

fn test_trivial_roundtrip_two_periodic_runs() {
	first := 'V Brotli interop payload: the quick brown fox jumps over the lazy dog.\n'
	second := '0123456789 abcdefghijklmnopqrstuvwxyz\n'
	data := (first.repeat(256) + second.repeat(128)).bytes()
	compressed := compress(data)!
	assert compressed.len < data.len
	assert decompress(compressed)! == data
}

fn test_trivial_roundtrip_two_small_alphabet_runs() {
	first := 'AB'.repeat(16)
	second := 'CD'.repeat(16)
	data := (first.repeat(64) + second.repeat(64)).bytes()
	compressed := compress(data)!
	assert compressed.len < data.len
	assert decompress(compressed)! == data
}

fn test_trivial_roundtrip_five_byte_periodic_prefers_compressed() {
	data := 'ABCDA'.repeat(2048).bytes()
	compressed := compress(data)!
	mut uncompressed := BitWriter{}
	uncompressed.write_bit(0) // WBITS = 16
	write_uncompressed_meta_block(mut uncompressed, data, true)
	assert compressed.len < uncompressed.finish().len
	assert decompress(compressed)! == data
}

fn test_trivial_roundtrip_small_alphabet_prefers_compressed() {
	data := 'ABCD'.repeat(4096).bytes()
	compressed := compress(data)!
	mut uncompressed := BitWriter{}
	uncompressed.write_bit(0) // WBITS = 16
	write_uncompressed_meta_block(mut uncompressed, data, true)
	assert compressed.len < uncompressed.finish().len
	assert decompress(compressed)! == data
}

fn test_small_alphabet_strategy_selected_when_smaller() {
	mut data := []u8{len: 128}
	for index in 0 .. data.len {
		data[index] = u8((index * 13 + index / 5) & 3)
	}
	params := CompressParams{ quality: 0, window_size: 16 }
	general := compress_general(data, params, 16)!
	candidate := specialized_compress_candidate(data, params, 16)!
	assert candidate.len > 0
	compressed := compress(data, params)!
	assert compressed.len == if candidate.len < general.len { candidate.len } else { general.len }
	assert decompress(compressed)! == data
}

fn test_decompress_rejects_invalid_meta_block_length_nibble() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	w.write_bit(1) // ISLAST
	w.write_bit(0) // not last-empty
	w.write_bits(1, 2) // MNIBBLES = 5
	w.write_bits(0x2345, 16)
	w.write_bits(0, 4) // invalid final nibble for 5 nibbles
	if _ := decompress(w.finish()) {
		assert false
	}
}

fn test_decompress_rejects_truncated_meta_block_length() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	w.write_bit(1) // ISLAST
	w.write_bit(0) // not last-empty
	w.write_bits(2, 2) // MNIBBLES = 6
	w.write_bits(0x1234, 16) // truncated length bits
	if _ := decompress(w.finish()) {
		assert false
	}
}

fn test_decompress_rejects_invalid_stream() {
	if _ := decompress('not brotli'.bytes()) {
		assert false
	}
}

fn test_prefix_decoder_roundtrip() {
	lengths := [1, 2, 2]
	dec := build_prefix_decoder(lengths)!
	enc := huffman.build(lengths: lengths, max_bits: 2, bit_order: .lsb_first)!
	mut w := BitWriter{}
	w.write_bits(enc.codes[0], lengths[0])
	w.write_bits(enc.codes[1], lengths[1])
	w.write_bits(enc.codes[2], lengths[2])
	data := w.finish()
	mut br := BitReader{
		data: data
	}
	assert br.read_prefix_symbol(dec)! == 0
	assert br.read_prefix_symbol(dec)! == 1
	assert br.read_prefix_symbol(dec)! == 2
}

fn test_bit_io_crosses_byte_boundaries() {
	values := [u32(1), 0x5a, 0x101, 0x5, 0xabcd, 0x12]
	widths := [1, 7, 9, 3, 16, 5]
	mut w := BitWriter{}
	for index, value in values {
		w.write_bits(value, widths[index])
	}
	mut r := BitReader{ data: w.finish() }
	for index, value in values {
		assert r.read_bits(widths[index])! == value
	}
	assert r.pos == 5
	assert r.bit_pos == 1
}

fn test_huffman_empty_and_single_symbol_trees() {
	assert compute_huffman_lengths([0, 0, 0], 15)! == [0, 0, 0]
	assert compute_huffman_lengths([0, 4, 0], 15)! == [0, -1, 0]
	if _ := build_prefix_decoder([0, 0, 0]) {
		assert false
	}
	decoder := build_prefix_decoder([0, 1, 0])!
	mut w := BitWriter{}
	w.write_bit(0)
	mut r := BitReader{ data: w.finish() }
	assert r.read_prefix_symbol(decoder)! == 1
}

fn test_lz_matches_respect_window_limit() {
	marker := 'window-limit-marker'.bytes()
	mut data := []u8{len: 65540, init: u8((index * 37 + 11) & 255)}
	for index, value in marker {
		data[index] = value
		data[65520 + index] = value
	}
	commands := parse_lz_commands(data, 16, 5, [16, 15, 11, 4])
	mut found_match := false
	for command in commands {
		assert command.distance <= lz_max_distance(16)
		if command.copy_len > 0 {
			found_match = true
		}
	}
	assert found_match
	assert decompress(compress(data, quality: 5, window_size: 16)!)! == data
}

fn test_lz_matches_use_large_window_distance() {
	marker := 'large-window-distance-marker'.bytes()
	mut data := []u8{len: 90000 + marker.len, init: u8((index * 37 + 11) & 255)}
	for index, value in marker {
		data[index] = value
		data[90000 + index] = value
	}
	commands := parse_lz_commands(data, 22, 5, [16, 15, 11, 4])
	mut found_long_match := false
	for command in commands {
		if command.copy_len >= marker.len && command.distance >= 90000 {
			found_long_match = true
		}
	}
	assert found_long_match
	assert decompress(compress(data, quality: 5, window_size: 22)!)! == data
}

fn test_stable_fixture_size_and_roundtrip() {
	assert compress([]u8{})! == [u8(6)]
	data := 'ABCD'.repeat(4096).bytes()
	compressed := compress(data, quality: 5, window_size: 16)!
	assert compressed.len < 128
	assert decompress(compressed)! == data
}

fn test_json_like_quality_five_size_and_roundtrip() {
	mut data := []u8{}
	for index in 0 .. 2048 {
		data << '{"id":${index},"name":"item-${index % 31}","active":${index % 2 == 0}}\n'.bytes()
	}
	compressed := compress(data, quality: 5, window_size: 22)!
	assert compressed.len < 4500
	assert decompress(compressed)! == data
}

fn test_short_input_compares_uncompressed_candidate() {
	data := 'tiny Brotli input'.bytes()
	compressed := compress(data, quality: 5, window_size: 22)!
	mut writer := new_compressor_writer(data.len, 22, false, []u8{})
	write_uncompressed_meta_block(mut writer, data, true)
	uncompressed := writer.finish()
	assert compressed.len == uncompressed.len
	assert decompress(compressed)! == data
}

fn test_simple_prefix_code_three_symbols() {
	mut w := BitWriter{}
	w.write_bits(1, 2)
	w.write_bits(2, 2)
	w.write_bits(5, 3)
	w.write_bits(2, 3)
	w.write_bits(7, 3)
	mut br := BitReader{
		data: w.finish()
	}
	lengths := decode_simple_prefix_code(mut br, 8)!
	assert lengths[5] == 1
	assert lengths[2] == 2
	assert lengths[7] == 2
}

fn test_simple_prefix_code_four_symbols_tree_select() {
	mut w := BitWriter{}
	w.write_bits(1, 2)
	w.write_bits(3, 2)
	w.write_bits(6, 3)
	w.write_bits(2, 3)
	w.write_bits(1, 3)
	w.write_bits(4, 3)
	w.write_bit(1)
	mut br := BitReader{
		data: w.finish()
	}
	lengths := decode_simple_prefix_code(mut br, 8)!
	assert lengths[6] == 1
	assert lengths[2] == 2
	assert lengths[1] == 3
	assert lengths[4] == 3
}

fn test_window_bits_patterns() {
	cases := [
		WindowBitsCase{
			bits:  0b0
			nbits: 1
			want:  16
		},
		WindowBitsCase{
			bits:  0b0100001
			nbits: 7
			want:  10
		},
		WindowBitsCase{
			bits:  0b0110001
			nbits: 7
			want:  11
		},
		WindowBitsCase{
			bits:  0b1000001
			nbits: 7
			want:  12
		},
		WindowBitsCase{
			bits:  0b1010001
			nbits: 7
			want:  13
		},
		WindowBitsCase{
			bits:  0b1100001
			nbits: 7
			want:  14
		},
		WindowBitsCase{
			bits:  0b1110001
			nbits: 7
			want:  15
		},
		WindowBitsCase{
			bits:  0b0000001
			nbits: 7
			want:  17
		},
		WindowBitsCase{
			bits:  0b0011
			nbits: 4
			want:  18
		},
		WindowBitsCase{
			bits:  0b0101
			nbits: 4
			want:  19
		},
		WindowBitsCase{
			bits:  0b0111
			nbits: 4
			want:  20
		},
		WindowBitsCase{
			bits:  0b1001
			nbits: 4
			want:  21
		},
		WindowBitsCase{
			bits:  0b1011
			nbits: 4
			want:  22
		},
		WindowBitsCase{
			bits:  0b1101
			nbits: 4
			want:  23
		},
		WindowBitsCase{
			bits:  0b1111
			nbits: 4
			want:  24
		},
	]
	for c in cases {
		mut w := BitWriter{}
		w.write_bits(c.bits, c.nbits)
		mut br := BitReader{
			data: w.finish()
		}
		assert decode_window_bits(mut br)! == c.want
	}
}

fn test_varlen_u8_roundtrip() {
	for value in 0 .. 256 {
		mut w := BitWriter{}
		write_varlen_uint8(mut w, value)
		mut br := BitReader{
			data: w.finish()
		}
		assert br.read_varlen_uint8()! == value
	}
}

fn test_metadata_block_is_skipped() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	write_metadata_skip_block(mut w, 1, false)
	w.write_bytes([u8(0xaa)])
	w.write_bit(1) // ISLAST
	w.write_bit(1) // ISLASTEMPTY
	assert decompress(w.finish())! == []u8{}
}

fn test_metadata_block_zero_skip_is_skipped() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	write_metadata_skip_block(mut w, 0, false)
	w.write_bit(1) // ISLAST
	w.write_bit(1) // ISLASTEMPTY
	assert decompress(w.finish())! == []u8{}
}

fn test_zero_skip_then_data_roundtrip() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	write_metadata_skip_block(mut w, 0, false)
	write_uncompressed_meta_block(mut w, 'zero skip'.bytes(), true)
	assert decompress(w.finish())! == 'zero skip'.bytes()
}

fn test_metadata_block_large_skip_is_skipped() {
	skip_len := 300
	skip := []u8{len: skip_len, init: u8((index * 19 + 11) & 0xff)}
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	write_metadata_skip_block(mut w, skip_len, false)
	w.write_bytes(skip)
	w.write_bit(1) // ISLAST
	w.write_bit(1) // ISLASTEMPTY
	assert decompress(w.finish())! == []u8{}
}

fn test_metadata_block_then_data_roundtrip() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	write_metadata_skip_block(mut w, 1, false)
	w.write_bytes([u8(0xaa)])
	write_uncompressed_meta_block(mut w, 'after meta'.bytes(), true)
	assert decompress(w.finish())! == 'after meta'.bytes()
}

fn test_metadata_block_rejects_reserved_bit() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	w.write_bit(1) // ISLAST
	w.write_bit(0) // not last-empty
	w.write_bits(3, 2) // MNIBBLES = 0 => metadata block
	w.write_bit(1) // reserved (invalid)
	w.write_bits(0, 2) // MSKIPBYTES = 0
	if _ := decompress(w.finish()) {
		assert false
	}
}

fn test_metadata_block_rejects_truncated_skip_payload() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	write_metadata_skip_block(mut w, 2, true)
	w.write_bytes([u8(0xaa)]) // truncated: need 2 bytes
	if _ := decompress(w.finish()) {
		assert false
	}
}

fn test_metadata_block_rejects_huge_skip_payload() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	w.write_bit(1) // ISLAST
	w.write_bit(0) // not last-empty
	w.write_bits(3, 2) // MNIBBLES = 0 => metadata block
	w.write_bit(0) // reserved
	w.write_bits(3, 2) // MSKIPBYTES = 3
	w.write_bits(0xff_ff_ff, 24) // MSKIPLEN - 1
	if _ := decompress(w.finish()) {
		assert false
	}
}

fn test_last_metadata_block_is_skipped() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	write_metadata_skip_block(mut w, 1, true)
	w.write_bytes([u8(0xaa)])
	assert decompress(w.finish())! == []u8{}
}

fn test_last_metadata_block_rejects_non_zero_padding() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	write_metadata_skip_block(mut w, 1, true)
	w.write_bytes([u8(0xaa)])
	w.write_bytes([u8(1)])
	if _ := decompress(w.finish()) {
		assert false
	}
}

fn test_last_uncompressed_meta_block_roundtrip() {
	data := 'final block'.bytes()
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	write_uncompressed_meta_block(mut w, data, true)
	assert decompress(w.finish())! == data
}

fn test_last_uncompressed_meta_block_rejects_non_zero_padding() {
	data := 'final block'.bytes()
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	write_uncompressed_meta_block(mut w, data, true)
	w.write_bytes([u8(1)])
	if _ := decompress(w.finish()) {
		assert false
	}
}

fn test_last_uncompressed_meta_block_rejects_truncated_payload() {
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	w.write_bit(1) // ISLAST
	w.write_bit(0) // not last-empty
	w.write_bits(0, 2) // MNIBBLES = 4
	write_meta_block_length(mut w, 5, 4) // mlen = 5
	w.write_bit(1) // ISUNCOMPRESSED
	w.write_bytes([u8(1), 2, 3]) // truncated
	if _ := decompress(w.finish()) {
		assert false
	}
}

fn test_multi_block_uncompressed_roundtrip() {
	blocks := [
		'hello '.bytes(),
		'world'.bytes(),
	]
	mut w := BitWriter{}
	w.write_bit(0) // WBITS = 16
	write_uncompressed_meta_block(mut w, blocks[0], false)
	write_uncompressed_meta_block(mut w, blocks[1], true)
	mut want := []u8{}
	want << blocks[0]
	want << blocks[1]
	assert decompress(w.finish())! == want
}

fn test_meta_block_length_roundtrip() {
	cases := [
		MetaBlockLengthCase{
			nibbles: 4
			value:   0x1234
		},
		MetaBlockLengthCase{
			nibbles: 5
			value:   0x54321
		},
		MetaBlockLengthCase{
			nibbles: 6
			value:   0x654321
		},
	]
	for c in cases {
		mut w := BitWriter{}
		for i in 0 .. c.nibbles {
			w.write_bits(u32((c.value >> (i * 4)) & 0xf), 4)
		}
		mut br := BitReader{
			data: w.finish()
		}
		assert read_meta_block_length(mut br, c.nibbles)! == c.value + 1
	}
}

fn test_meta_block_length_rejects_trailing_zero_nibble() {
	mut w := BitWriter{}
	w.write_bits(0x2345, 16)
	w.write_bits(0, 4)
	mut br := BitReader{
		data: w.finish()
	}
	if _ := read_meta_block_length(mut br, 5) {
		assert false
	}
}

fn test_meta_block_length_writer_roundtrip() {
	for c in [
		MetaBlockLengthCase{
			nibbles: 4
			value:   0x1234
		},
		MetaBlockLengthCase{
			nibbles: 5
			value:   0x54321
		},
		MetaBlockLengthCase{
			nibbles: 6
			value:   0x654321
		},
	] {
		mut w := BitWriter{}
		write_meta_block_length(mut w, c.value + 1, c.nibbles)
		mut br := BitReader{
			data: w.finish()
		}
		assert read_meta_block_length(mut br, c.nibbles)! == c.value + 1
	}
}
