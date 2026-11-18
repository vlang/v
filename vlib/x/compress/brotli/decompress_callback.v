module brotli

// DecompressCallback receives decoded chunks and returns the chunk length to
// continue, or 0 to stop early. It matches compress.brotli.DecompressCallback
// and compress.deflate.ChunkCallback so callbacks written for those entry
// points can be reused here.
pub type DecompressCallback = fn (chunk []u8, userdata voidptr) int

const callback_chunk_size = 32 * 1024

struct CbSink {
	cb       DecompressCallback @[required]
	userdata voidptr
mut:
	delivered        int
	delivered_offset int
}

fn (mut s CbSink) deliver_full(out []u8) bool {
	for out.len - s.delivered_offset >= callback_chunk_size {
		chunk := out[s.delivered_offset..s.delivered_offset + callback_chunk_size]
		if s.cb(chunk, s.userdata) != chunk.len {
			return false
		}
		s.delivered += chunk.len
		s.delivered_offset += chunk.len
	}
	return true
}

fn (mut s CbSink) deliver_rest(out []u8) bool {
	if s.delivered_offset >= out.len {
		return true
	}
	chunk := out[s.delivered_offset..]
	if s.cb(chunk, s.userdata) != chunk.len {
		return false
	}
	s.delivered += chunk.len
	s.delivered_offset += chunk.len
	return true
}

// decompress_with_callback decodes `data` with default DecompressParams and
// delivers the output in 32 KiB chunks, mirroring the native
// compress.brotli entry point. Returning a short count from `cb` stops
// decoding before the remaining input is validated and returns the bytes
// delivered so far without an error, so callers can enforce output limits
// the same way vlib/net/http does. The chunk slice is only valid for the
// duration of the callback; copy its contents if they must outlive the call.
// Callers that need concatenated streams, metadata comments, or custom
// dictionaries should use decompress() directly.
pub fn decompress_with_callback(data []u8, cb DecompressCallback, userdata voidptr) !int {
	if data.len == 0 {
		return error('brotli: empty input')
	}
	if data.len == 1 && data[0] == 6 {
		return 0
	}
	mut br := BitReader{
		data: data
	}
	window_bits := decode_window_bits(mut br)!
	window_size := 1 << window_bits
	mut out := []u8{}
	defer {
		unsafe { out.free() }
	}
	mut metadata := []u8{}
	defer {
		unsafe { metadata.free() }
	}
	mut recent_distances := [16, 15, 11, 4]
	defer {
		unsafe {
			recent_distances.free()
		}
	}
	mut distance_index := [0]
	defer {
		unsafe {
			distance_index.free()
		}
	}
	mut sink := CbSink{
		cb:       cb
		userdata: userdata
	}
	for {
		is_last := br.read_bit()! == 1
		if is_last {
			if br.read_bit()! == 1 {
				if !br.padding_bits_are_zero() {
					return error('brotli: non-zero padding after last empty meta-block')
				}
				consumed := br.pos + if br.bit_pos == 0 { 0 } else { 1 }
				if consumed != data.len {
					// Deliver what was decoded before reporting trailing data,
					// mirroring the native entry point.
					if !sink.deliver_rest(out) {
						return sink.delivered
					}
					return error('brotli: trailing data after stream')
				}
				if !sink.deliver_rest(out) {
					return sink.delivered
				}
				return sink.delivered
			}
		}
		mnibbles_code := int(br.read_bits(2)!)!
		mnibbles := match mnibbles_code {
			3 { 0 }
			0 { 4 }
			1 { 5 }
			2 { 6 }
			else { return error('brotli: invalid meta-block nibble code') }
		}
		if mnibbles == 0 {
			empty_block := decode_empty_meta_block(mut br)!
			defer {
				unsafe {
					empty_block.free()
				}
			}
			metadata << empty_block
			if is_last {
				if !br.padding_bits_are_zero() {
					return error('brotli: non-zero padding after last metadata meta-block')
				}
				consumed := br.pos + if br.bit_pos == 0 { 0 } else { 1 }
				if consumed != data.len {
					if !sink.deliver_rest(out) {
						return sink.delivered
					}
					return error('brotli: trailing data after stream')
				}
				if !sink.deliver_rest(out) {
					return sink.delivered
				}
				return sink.delivered
			}
			continue
		}
		mlen := read_meta_block_length(mut br, mnibbles)!
		is_uncompressed := !is_last && br.read_bit()! == 1
		if is_uncompressed {
			cont := decode_uncompressed_meta_block_streaming(mut br, mut out, mlen, mut sink)!
			if !cont {
				return sink.delivered
			}
			if is_last {
				if !br.padding_bits_are_zero() {
					return error('brotli: non-zero padding after last uncompressed meta-block')
				}
				consumed := br.pos + if br.bit_pos == 0 { 0 } else { 1 }
				if consumed != data.len {
					if !sink.deliver_rest(out) {
						return sink.delivered
					}
					return error('brotli: trailing data after stream')
				}
				if !sink.deliver_rest(out) {
					return sink.delivered
				}
				return sink.delivered
			}
			continue
		}
		cont := decode_compressed_meta_block_streaming(mut br, mut out, mlen, window_size,
			[], mut recent_distances, mut distance_index, mut sink)!
		if !cont {
			return sink.delivered
		}
		if is_last {
			if !br.padding_bits_are_zero() {
				return error('brotli: non-zero padding after last compressed meta-block')
			}
			consumed := br.pos + if br.bit_pos == 0 { 0 } else { 1 }
			if consumed != data.len {
				if !sink.deliver_rest(out) {
					return sink.delivered
				}
				return error('brotli: trailing data after stream')
			}
			if !sink.deliver_rest(out) {
				return sink.delivered
			}
			return sink.delivered
		}
	}
	return error('brotli: truncated stream')
}

fn decode_uncompressed_meta_block_streaming(mut r BitReader, mut out []u8, mlen int, mut sink CbSink) !bool {
	if !r.padding_bits_are_zero() {
		return error('brotli: non-zero padding before uncompressed block')
	}
	r.align_byte()
	if r.remaining_bytes() < mlen {
		return error('brotli: truncated uncompressed meta-block')
	}
	mut remaining := mlen
	for remaining > 0 {
		take := if remaining > callback_chunk_size { callback_chunk_size } else { remaining }
		out << r.data[r.pos..r.pos + take]
		r.pos += take
		remaining -= take
		if !sink.deliver_full(out) {
			return false
		}
	}
	return true
}

// free_prefix_decoders releases decoder tables built while reading one
// meta-block header. Every table is freshly built per block, so freeing
// each element once is safe; empty slots from an early error return are
// skipped, which is what makes one deferred call cover every exit.
fn free_prefix_decoders(mut decs []PrefixDecoder) {
	for i in 0 .. decs.len {
		if decs[i].entries.len > 0 {
			decs[i].entries.free()
		}
		if decs[i].flat.len > 0 {
			unsafe {
				decs[i].flat.free()
			}
		}
	}
}

fn decode_compressed_meta_block_streaming(mut r BitReader, mut out []u8, mlen int, window_size int,
	dictionary []u8, mut recent_distances []int, mut distance_index []int, mut sink CbSink) !bool {
	start_len := out.len
	mut nbltypes := [0, 0, 0]
	mut block_type_decs := []PrefixDecoder{len: 3}
	mut block_count_decs := []PrefixDecoder{len: 3}
	mut block_remaining := [0, 0, 0]
	mut literal_context_modes := []int{}
	mut literal_context_map := []int{}
	mut distance_context_map := []int{}
	mut literal_trees := []PrefixDecoder{}
	mut insert_copy_trees := []PrefixDecoder{}
	mut distance_trees := []PrefixDecoder{}
	mut prev_block := [1, 1, 1]
	mut curr_block := [0, 0, 0]
	// Every table above is function-local: the trees and maps are freshly
	// built while reading this block's header and die with it, so one
	// deferred free covers every exit, including an early error or an
	// early stop. Empty slots from an aborted header read are skipped by
	// the length guards.
	defer {
		unsafe {
			nbltypes.free()
			block_remaining.free()
			literal_context_modes.free()
			literal_context_map.free()
			distance_context_map.free()
			prev_block.free()
			curr_block.free()
		}
		free_prefix_decoders(mut block_type_decs)
		free_prefix_decoders(mut block_count_decs)
		free_prefix_decoders(mut literal_trees)
		free_prefix_decoders(mut insert_copy_trees)
		free_prefix_decoders(mut distance_trees)
	}
	for i in 0 .. 3 {
		nbltypes[i] = read_trees_count(mut r)!
		if nbltypes[i] >= 2 {
			block_type_decs[i] = read_prefix_tree(mut r, nbltypes[i] + 2)!
			block_count_decs[i] = read_prefix_tree(mut r, 26)!
			block_remaining[i] = read_block_count(mut r, block_count_decs[i])!
		} else {
			block_remaining[i] = mlen
		}
	}
	nbltypes_l := nbltypes[0]
	nbltypes_i := nbltypes[1]
	nbltypes_d := nbltypes[2]
	npostfix := int(r.read_bits(2)!)!
	ndirect_field := int(r.read_bits(4)!)!
	ndirect := int(u32(ndirect_field) << u32(npostfix))
	for _ in 0 .. nbltypes_l {
		literal_context_modes << int(r.read_bits(2)!)!
	}
	ntrees_l := read_trees_count(mut r)!
	if ntrees_l >= 2 {
		literal_context_map = read_context_map(mut r, 64 * nbltypes_l, ntrees_l)!
	} else {
		literal_context_map = []int{len: 64 * nbltypes_l}
		for i in 0 .. 64 * nbltypes_l {
			literal_context_map[i] = 0
		}
	}
	ntrees_d := read_trees_count(mut r)!
	if ntrees_d >= 2 {
		distance_context_map = read_context_map(mut r, 4 * nbltypes_d, ntrees_d)!
	} else {
		distance_context_map = []int{len: 4 * nbltypes_d}
		for i in 0 .. 4 * nbltypes_d {
			distance_context_map[i] = 0
		}
	}
	for _ in 0 .. ntrees_l {
		literal_trees << read_prefix_tree(mut r, 256)!
	}
	for _ in 0 .. nbltypes_i {
		insert_copy_trees << read_prefix_tree(mut r, 704)!
	}
	distance_alphabet_size := 16 + ndirect + (48 << npostfix)
	for _ in 0 .. ntrees_d {
		distance_trees << read_prefix_tree(mut r, distance_alphabet_size)!
	}
	mut produced := 0
	mut command_count := 0
	mut p1 := u8(0)
	mut p2 := u8(0)
	if out.len >= 1 {
		p1 = out[out.len - 1]
	}
	if out.len >= 2 {
		p2 = out[out.len - 2]
	}
	for produced < mlen {
		if block_remaining[1] == 0 && nbltypes_i >= 2 {
			b := r.read_prefix_symbol(block_type_decs[1])!
			mut new_type := 0
			if b == 0 {
				new_type = prev_block[1]
			} else if b == 1 {
				new_type = (curr_block[1] + 1) % nbltypes_i
			} else {
				new_type = b - 2
			}
			prev_block[1] = curr_block[1]
			curr_block[1] = new_type
			block_remaining[1] = read_block_count(mut r, block_count_decs[1])!
		}
		cmd_tree := insert_copy_trees[curr_block[1]]
		sym := r.read_prefix_symbol(cmd_tree) or {
			return error('brotli: command decode at command ${command_count}, produced ${produced}: ${err.msg()}')
		}
		command_count++
		cmd := cmd_lut(sym)
		insert_len := cmd.insert_len_offset + int(r.read_bits(cmd.insert_len_extra_bits)!)!
		copy_len := cmd.copy_len_offset + int(r.read_bits(cmd.copy_len_extra_bits)!)!
		if insert_len == 0 && copy_len == 0 {
			return error('brotli: invalid zero-progress command in compressed meta-block')
		}
		if produced + insert_len > mlen {
			return error('brotli: compressed insert length exceeds metablock')
		}
		if nbltypes_i >= 2 {
			block_remaining[1]--
		}
		if produced == mlen {
			break
		}
		for _ in 0 .. insert_len {
			if block_remaining[0] == 0 && nbltypes_l >= 2 {
				b := r.read_prefix_symbol(block_type_decs[0])!
				mut new_type := 0
				if b == 0 {
					new_type = prev_block[0]
				} else if b == 1 {
					new_type = (curr_block[0] + 1) % nbltypes_l
				} else {
					new_type = b - 2
				}
				prev_block[0] = curr_block[0]
				curr_block[0] = new_type
				block_remaining[0] = read_block_count(mut r, block_count_decs[0])!
			}
			mut lit_tree := PrefixDecoder{}
			mut context_mode := -1
			mut context_id := -1
			mut context_tree := -1
			if ntrees_l == 1 {
				lit_tree = literal_trees[0]
			} else {
				context_mode = literal_context_modes[curr_block[0]]
				context_id = compute_literal_context_id(p1, p2, context_mode)
				context_tree = literal_context_map[64 * curr_block[0] + context_id]
				if context_tree < 0 || context_tree >= ntrees_l {
					return error('brotli: context map index out of range')
				}
				lit_tree = literal_trees[context_tree]
			}
			lit := r.read_prefix_symbol(lit_tree) or {
				return error('brotli: literal decode at command ${command_count}, produced ${produced}: ${err.msg()}')
			}
			out << u8(lit)
			p2 = p1
			p1 = u8(lit)
			produced++
			if nbltypes_l >= 2 {
				block_remaining[0]--
			}
			if out.len - sink.delivered_offset >= callback_chunk_size {
				if !sink.deliver_full(out) {
					return false
				}
			}
		}
		if produced == mlen {
			continue
		}
		if copy_len == 0 {
			continue
		}
		if cmd.distance_code == -1 && block_remaining[2] == 0 && nbltypes_d >= 2 {
			b := r.read_prefix_symbol(block_type_decs[2])!
			mut new_type := 0
			if b == 0 {
				new_type = prev_block[2]
			} else if b == 1 {
				new_type = (curr_block[2] + 1) % nbltypes_d
			} else {
				new_type = b - 2
			}
			prev_block[2] = curr_block[2]
			curr_block[2] = new_type
			block_remaining[2] = read_block_count(mut r, block_count_decs[2])!
		}
		mut distance := 0
		mut distance_index_adjusted := false
		if cmd.distance_code == -1 {
			mut dist_tree := PrefixDecoder{}
			if ntrees_d == 1 {
				dist_tree = distance_trees[0]
			} else {
				cidd := cmd.context
				map_idx := distance_context_map[4 * curr_block[2] + cidd]
				if map_idx < 0 || map_idx >= ntrees_d {
					return error('brotli: distance context map index out of range')
				}
				dist_tree = distance_trees[map_idx]
			}
			dist_sym := r.read_prefix_symbol(dist_tree) or {
				return error('brotli: distance decode at command ${command_count}, produced ${produced}: ${err.msg()}')
			}
			if dist_sym < 16 {
				distance = resolve_distance_short_symbol(dist_sym, recent_distances, distance_index[0])!
				if dist_sym == 0 {
					distance_index[0]--
					distance_index_adjusted = true
				}
			} else {
				dextra_bits := distance_symbol_extra_bits(dist_sym, npostfix, ndirect)!
				dextra := int(r.read_bits(dextra_bits)!)!
				distance = distance_from_code(dist_sym, dextra, npostfix, ndirect)!
			}
			if nbltypes_d >= 2 {
				block_remaining[2]--
			}
		} else if cmd.distance_code >= 0 {
			distance_index[0]--
			distance_index_adjusted = cmd.distance_code == 0
			distance = recent_distances[distance_index[0] & 3]
		}
		max_history := out.len + dictionary.len
		max_distance := if max_history < window_size - 16 { max_history } else { window_size - 16 }
		custom_dictionary_copy := dictionary.len > 0 && distance > out.len && distance <= max_history
		if copy_len > mlen - produced && (distance <= max_distance || custom_dictionary_copy) {
			return error('brotli: compressed copy length exceeds metablock at ${produced}, command ${command_count}, insert ${insert_len}, copy ${copy_len}, sym ${sym}, mlen ${mlen}, distance index ${distance_index[0]}, recent ${recent_distances}')
		}
		if dictionary.len > 0 && distance > out.len {
			source := dictionary.len + out.len - distance
			if source >= 0 && source < dictionary.len {
				for _ in 0 .. copy_len {
					current_source := dictionary.len + out.len - distance
					b := if current_source < dictionary.len {
						dictionary[current_source]
					} else {
						out[current_source - dictionary.len]
					}
					out << b
					p2 = p1
					p1 = b
					produced++
					if out.len - sink.delivered_offset >= callback_chunk_size {
						if !sink.deliver_full(out) {
							return false
						}
					}
				}
				recent_distances[distance_index[0] & 3] = distance
				distance_index[0]++
				continue
			}
		}
		if distance > max_distance {
			if distance - max_distance - 1 < 1 << 20 {
				transformed_len := decode_static_dictionary_word(distance, max_distance, copy_len,
					mlen - produced, mut out)!
				produced += transformed_len
				if distance_index_adjusted {
					distance_index[0]++
				}
				p2 = if out.len >= 2 { out[out.len - 2] } else { 0 }
				p1 = if out.len > 0 { out[out.len - 1] } else { 0 }
				if !sink.deliver_full(out) {
					return false
				}
				continue
			}
			return error('brotli: invalid copy distance ${distance} at ${out.len}, command ${command_count}, distance index ${distance_index[0]}, recent ${recent_distances}')
		}
		if distance <= 0 || distance > out.len {
			return error('brotli: invalid copy distance ${distance} at ${out.len}')
		}
		for _ in 0 .. copy_len {
			b := out[out.len - distance]
			out << b
			p2 = p1
			p1 = b
			produced++
			if out.len - sink.delivered_offset >= callback_chunk_size {
				if !sink.deliver_full(out) {
					return false
				}
			}
		}
		recent_distances[distance_index[0] & 3] = distance
		distance_index[0]++
	}
	if produced != mlen {
		return error('brotli: truncated compressed meta-block')
	}
	if out.len - start_len != mlen {
		return error('brotli: compressed meta-block length mismatch')
	}
	return true
}
