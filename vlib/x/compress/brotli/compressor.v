module brotli

import compress.brotli as native_brotli

fn new_compressor_writer(data_len int, window_bits int, large_window bool, comment []u8) BitWriter {
	mut w := BitWriter{
		out: []u8{cap: data_len + (data_len / trivial_block_size + 2) * 5}
	}
	write_window_bits(mut w, window_bits, large_window)
	if comment.len > 0 {
		write_metadata_block(mut w, comment)
	}
	return w
}

fn compress_uncompressed(data []u8, params CompressParams, window_bits int) ![]u8 {
	mut w := new_compressor_writer(data.len, window_bits, params.large_window != 0, params.comment)
	mut i := 0
	for i < data.len {
		remaining := data.len - i
		mut chunk_len := remaining
		mnibbles := meta_block_nibbles(remaining)
		max_len := max_meta_block_len_for_nibbles(mnibbles)
		if chunk_len > max_len {
			chunk_len = max_len
		}
		if data.len <= 1 << 20 && chunk_len > trivial_block_size {
			chunk_len = trivial_block_size
		}
		is_last := i + chunk_len == data.len
		chunk := data[i..i + chunk_len]
		write_uncompressed_meta_block(mut w, chunk, is_last)
		i += chunk_len
	}
	return w.finish()
}

fn compress_general(data []u8, params CompressParams, window_bits int) ![]u8 {
	if params.large_window != 0 {
		return compress_uncompressed(data, params, window_bits)
	}
	mut w := new_compressor_writer(data.len, window_bits, false, params.comment)
	mut i := 0
	mut recent_distances := [16, 15, 11, 4]
	mut history := []u8{}
	max_history := lz_max_distance(params.window_size)
	for i < data.len {
		remaining := data.len - i
		mut chunk_len := remaining
		mnibbles := meta_block_nibbles(remaining)
		max_len := max_meta_block_len_for_nibbles(mnibbles)
		if chunk_len > max_len {
			chunk_len = max_len
		}
		is_last := i + chunk_len == data.len
		chunk := data[i..i + chunk_len]
		write_lz_compressed_meta_block_with_history(mut w, chunk, is_last, params.window_size,
			params.quality, mut recent_distances, history)!
		history << chunk
		if history.len > max_history {
			history = history[history.len - max_history..].clone()
		}
		i += chunk_len
	}
	return w.finish()
}

fn has_specialized_periodic_run(data []u8, period_limit int) bool {
	if data.len > 0x8000 {
		return false
	}
	mut i := 0
	for i < data.len {
		periodic_run := find_periodic_run(data, i, period_limit)
		if periodic_run.period > 0 && periodic_run.length >= periodic_run.period * 2
			&& periodic_run.length >= 64 && periodic_run.period >= 32 {
			return true
		}
		i++
	}
	return false
}

fn specialized_compress_candidate(data []u8, params CompressParams, window_bits int) ![]u8 {
	mut best := []u8{}
	if params.large_window == 0 && data.len >= 20 {
		if dictionary_match := find_static_dictionary_match(data) {
			mut w := new_compressor_writer(data.len, window_bits, false, params.comment)
			write_dictionary_copy_meta_block_with_length(mut w, data, dictionary_match.word_len,
				dictionary_match.address + 1)!
			candidate := w.finish()
			if best.len == 0 || candidate.len < best.len {
				best = candidate.clone()
			}
		}
	}

	period_limit := periodic_search_limit(params.quality, params.window_size)
	if params.quality >= 4 && params.large_window == 0 && data.len <= 0x8000 {
		periodic_pattern := find_periodic_pattern(data)
		periodic_symbols := if periodic_pattern.len > 0 {
			collect_small_alphabet_symbols(periodic_pattern, 4)
		} else {
			[]int{}
		}
		if periodic_symbols.len > 0 {
			mut w := new_compressor_writer(data.len, window_bits, false, params.comment)
			mut recent_distances := [16, 15, 11, 4]
			cmd_sym := if data.len == periodic_pattern.len {
				find_literal_only_cmd_symbol_for_insert_len(periodic_pattern.len)!
			} else {
				find_smallest_copy_cmd_symbol(periodic_pattern.len, data.len - periodic_pattern.len)!
			}
			write_periodic_pattern_compressed_meta_block_with_symbols_and_cmd(mut w, periodic_pattern,
				data.len, true, periodic_symbols, cmd_sym, mut recent_distances)!
			candidate := w.finish()
			if best.len == 0 || candidate.len < best.len {
				best = candidate.clone()
			}
		}
	}

	if params.large_window == 0 && data.len <= 0x10000 {
		first_run := find_periodic_run(data, 0, period_limit)
		if first_run.period > 0 && first_run.period >= 32 && first_run.length < data.len {
			second_run := find_periodic_run(data, first_run.length, period_limit)
			if second_run.period > 0 && second_run.period >= 32
				&& first_run.length + second_run.length == data.len {
				first_pattern := data[..first_run.period]
				second_pattern := data[first_run.length..first_run.length + second_run.period]
				pair_symbols := collect_small_alphabet_symbols_pair(first_pattern, second_pattern, 64)
				if pair_symbols.len > 0 {
					mut w := new_compressor_writer(data.len, window_bits, false, params.comment)
					write_two_periodic_runs_with_symbols(mut w, first_pattern, first_run.length,
						second_pattern, second_run.length, pair_symbols)!
					candidate := w.finish()
					if best.len == 0 || candidate.len < best.len {
						best = candidate.clone()
					}
				}
			}
		}
	}

	if params.large_window == 0 && data.len >= 32 && data.len <= 0x10000 {
		symbols := collect_small_alphabet_symbols(data, 4)
		if symbols.len > 1 {
			mut w := new_compressor_writer(data.len, window_bits, false, params.comment)
			if _ := write_small_alphabet_compressed_meta_block(mut w, data, symbols, true) {
				candidate := w.finish()
				if best.len == 0 || candidate.len < best.len {
					best = candidate.clone()
				}
			}
		}
	}

	if params.large_window == 0 && params.quality < 8 && data.len <= 0x10000
		&& has_specialized_periodic_run(data, period_limit) {
		mut i := 0
		mut w := new_compressor_writer(data.len, window_bits, false, params.comment)
		mut recent_distances := [16, 15, 11, 4]
		for i < data.len {
			periodic_run := find_periodic_run(data, i, period_limit)
			if periodic_run.period > 0 && periodic_run.length >= periodic_run.period * 2
				&& periodic_run.length >= 64 && periodic_run.period >= 32 {
				if cmd_sym := find_smallest_copy_cmd_symbol(periodic_run.period,
					periodic_run.length - periodic_run.period) {
					run_symbols := collect_small_alphabet_symbols(data[i..i + periodic_run.period], 64)
					if run_symbols.len > 0 {
						is_last := i + periodic_run.length == data.len
						write_periodic_pattern_compressed_meta_block_with_symbols_and_cmd(mut w,
							data[i..i + periodic_run.period], periodic_run.length, is_last,
							run_symbols, cmd_sym, mut recent_distances)!
						i += periodic_run.length
						continue
					}
				}
			}
			remaining := data.len - i
			mut chunk_len := remaining
			mnibbles := meta_block_nibbles(remaining)
			max_len := max_meta_block_len_for_nibbles(mnibbles)
			if chunk_len > max_len {
				chunk_len = max_len
			}
			is_last := i + chunk_len == data.len
			chunk := data[i..i + chunk_len]
			if params.quality >= 4 {
				write_lz_compressed_meta_block(mut w, chunk, is_last, params.window_size, params.quality,
					mut recent_distances)!
			} else {
				write_uncompressed_meta_block(mut w, chunk, is_last)
			}
			i += chunk_len
		}
		candidate := w.finish()
		if best.len == 0 || candidate.len < best.len {
			best = candidate.clone()
		}
	}
	return best
}

// compress encodes data as a Brotli stream using the requested quality and window.
// Large inputs without metadata, custom dictionaries, or large windows may use
// the system encoder when available; other inputs use the V encoder.
pub fn compress(data []u8, params CompressParams) ![]u8 {
	if params.quality < 0 || params.quality > 11 {
		return error('brotli: quality must be between 0 and 11')
	}
	if params.window_size < 16 || params.window_size > 24 {
		return error('brotli: window_size must be between 16 and 24')
	}
	if params.large_window != 0 && (params.large_window < 10 || params.large_window > 30) {
		return error('brotli: large_window must be between 10 and 30')
	}
	if params.comment.len > 80 {
		return error('brotli: comment must be at most 80 bytes')
	}
	if data.len > 4096 && params.large_window == 0 && params.comment.len == 0 && params.dictionary.len == 0 {
		if compressed := native_brotli.compress(data,
			quality: params.quality
			lgwin:   params.window_size
		) {
			return compressed
		}
	}
	window_bits := if params.large_window != 0 { params.large_window } else { params.window_size }
	if data.len == 0 {
		if params.comment.len == 0 {
			return [u8(6)]
		}
		mut empty_writer := BitWriter{}
		write_window_bits(mut empty_writer, window_bits, params.large_window != 0)
		write_metadata_block(mut empty_writer, params.comment)
		empty_writer.write_bit(1)
		empty_writer.write_bit(1)
		return empty_writer.finish()
	}
	if data.len < 20 && params.large_window == 0 && params.comment.len == 0 && params.dictionary.len == 0
		&& !has_short_lz_match(data) {
		mut short_writer := new_compressor_writer(data.len, window_bits, false, params.comment)
		write_uncompressed_meta_block(mut short_writer, data, true)
		return short_writer.finish()
	}
	mut w := new_compressor_writer(data.len, window_bits, params.large_window != 0, params.comment)
	if params.dictionary.len >= data.len
		&& params.dictionary[params.dictionary.len - data.len..] == data {
		write_dictionary_copy_meta_block_with_length(mut w, data, data.len, data.len)!
		return w.finish()
	}
	general := compress_general(data, params, window_bits)!
	mut best := general.clone()
	if params.quality < 4 {
		uncompressed := compress_uncompressed(data, params, window_bits)!
		if uncompressed.len < best.len {
			best = uncompressed.clone()
		}
	}
	if params.quality >= 4 && params.large_window == 0 && data.len <= 256 {
		mut uncompressed_writer := new_compressor_writer(data.len, window_bits, false, params.comment)
		write_uncompressed_meta_block(mut uncompressed_writer, data, true)
		uncompressed := uncompressed_writer.finish()
		if uncompressed.len < best.len {
			best = uncompressed.clone()
		}
	}
	candidate := specialized_compress_candidate(data, params, window_bits)!
	if candidate.len > 0 && candidate.len < best.len {
		return candidate
	}
	return best
}

fn decode_stream(data []u8, dictionary []u8) !DecodedStream {
	if data.len == 1 && data[0] == 6 {
		return DecodedStream{ data: []u8{}, consumed: 1 }
	}
	if data.len == 0 {
		return error('brotli: empty input')
	}
	mut br := BitReader{
		data: data
	}
	window_bits := decode_window_bits(mut br)!
	mut metadata := []u8{}
	window_size := 1 << window_bits
	mut out := []u8{}
	mut recent_distances := [16, 15, 11, 4]
	mut distance_index := [0]
	for {
		is_last := br.read_bit()! == 1
		if is_last {
			if br.read_bit()! == 1 {
				if !br.padding_bits_are_zero() {
					return error('brotli: non-zero padding after last empty meta-block')
				}
				return DecodedStream{
					data:     out
					metadata: metadata
					consumed: br.pos + if br.bit_pos == 0 {
						0
					} else {
						1
					}
				}
			}
		}
		mnibbles_code := int(br.read_bits(2)!)
		mnibbles := match mnibbles_code {
			3 { 0 }
			0 { 4 }
			1 { 5 }
			2 { 6 }
			else { return error('brotli: invalid meta-block nibble code') }
		}

		if mnibbles == 0 {
			metadata << decode_empty_meta_block(mut br)!
			if is_last {
				if !br.padding_bits_are_zero() {
					return error('brotli: non-zero padding after last metadata meta-block')
				}
				return DecodedStream{
					data:     out
					metadata: metadata
					consumed: br.pos + if br.bit_pos == 0 {
						0
					} else {
						1
					}
				}
			}
			continue
		}
		mlen := read_meta_block_length(mut br, mnibbles)!
		is_uncompressed := !is_last && br.read_bit()! == 1
		if is_uncompressed {
			out = decode_uncompressed_meta_block(mut br, mut out, mlen)!
			if is_last {
				if !br.padding_bits_are_zero() {
					return error('brotli: non-zero padding after last uncompressed meta-block')
				}
				return DecodedStream{
					data:     out
					metadata: metadata
					consumed: br.pos + if br.bit_pos == 0 {
						0
					} else {
						1
					}
				}
			}
			continue
		}
		out = decode_compressed_meta_block(mut br, mut out, mlen, window_size, dictionary,
			mut recent_distances, mut distance_index)!
		if is_last {
			if !br.padding_bits_are_zero() {
				return error('brotli: non-zero padding after last compressed meta-block')
			}
			return DecodedStream{
				data:     out
				metadata: metadata
				consumed: br.pos + if br.bit_pos == 0 { 0 } else { 1 }
			}
		}
	}
	return error('brotli: truncated stream')
}
