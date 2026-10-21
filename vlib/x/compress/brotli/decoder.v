module brotli

// decompress reverses compress() and decodes the RFC 7932 trivial stream
// layout. Empty input is invalid; an encoded empty stream decodes to an empty array.
pub fn decompress(data []u8, params DecompressParams) ![]u8 {
	if data.len == 0 {
		return error('brotli: empty input')
	}
	mut offset := 0
	mut out := []u8{}
	mut metadata := []u8{}
	for offset < data.len {
		stream := decode_stream(data[offset..], params.dictionary)!
		out << stream.data
		metadata << stream.metadata
		offset += stream.consumed
		if !params.concatenated {
			break
		}
	}
	if params.comment.len > 0 && metadata != params.comment {
		return error('brotli: comment does not match stream metadata')
	}
	if offset != data.len {
		return error('brotli: trailing data after stream')
	}
	return out
}

fn decode_window_bits(mut r BitReader) !int {
	if r.read_bit()! == 0 {
		return 16
	}
	n := r.read_bits(3)!
	if n != 0 {
		return int((17 + n) & 63)
	}
	n2 := r.read_bits(3)!
	if n2 == 1 {
		if r.read_bit()! != 0 {
			return error('brotli: invalid large window marker')
		}
		window_size := int(r.read_bits(6)!)
		if window_size < 10 || window_size > 30 {
			return error('brotli: invalid large window size')
		}
		return window_size
	}
	if n2 != 0 {
		return int((8 + n2) & 63)
	}
	return 17
}

fn decode_empty_meta_block(mut r BitReader) ![]u8 {
	if r.read_bit()! != 0 {
		return error('brotli: invalid reserved bit in empty meta-block')
	}
	mskip_bytes := int(r.read_bits(2)!)
	mskip_len := if mskip_bytes == 0 { 0 } else { int(r.read_bits(mskip_bytes * 8)!) + 1 }
	r.align_byte()
	if r.remaining_bytes() < mskip_len {
		return error('brotli: truncated metadata block')
	}
	metadata := r.data[r.pos..r.pos + mskip_len].clone()
	r.pos += mskip_len
	return metadata
}

fn decode_uncompressed_meta_block(mut r BitReader, mut out []u8, mlen int) ![]u8 {
	r.align_byte()
	if r.remaining_bytes() < mlen {
		return error('brotli: truncated uncompressed meta-block')
	}
	out << r.data[r.pos..r.pos + mlen]
	r.pos += mlen
	return out
}

fn decode_compressed_meta_block(mut r BitReader, mut out []u8, mlen int, window_size int,
	dictionary []u8, mut recent_distances []int, mut distance_index []int) ![]u8 {
	start_len := out.len

	// Each category emits its count, type tree, length tree, and initial length
	// before the next category begins.
	mut nbltypes := [0, 0, 0]
	mut block_type_decs := []PrefixDecoder{len: 3}
	mut block_count_decs := []PrefixDecoder{len: 3}
	mut block_remaining := [0, 0, 0]
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

	// Distance parameters
	npostfix := int(r.read_bits(2)!)
	ndirect_field := int(r.read_bits(4)!)
	ndirect := int(u32(ndirect_field) << u32(npostfix))

	// literal context modes (2 bits per literal block type)
	mut literal_context_modes := []int{}
	for _ in 0 .. nbltypes_l {
		literal_context_modes << int(r.read_bits(2)!)
	}

	ntrees_l := read_trees_count(mut r)!

	// Read literal context map (size = 64 * NBLTYPESL) when multiple literal trees
	mut literal_context_map := []int{}
	if ntrees_l >= 2 {
		literal_context_map = read_context_map(mut r, 64 * nbltypes_l, ntrees_l)!
	} else {
		literal_context_map = []int{len: 64 * nbltypes_l}
		for i in 0 .. 64 * nbltypes_l {
			literal_context_map[i] = 0
		}
	}

	ntrees_d := read_trees_count(mut r)!
	// Read distance context map (size = 4 * NBLTYPESD) when multiple distance trees
	mut distance_context_map := []int{}
	if ntrees_d >= 2 {
		distance_context_map = read_context_map(mut r, 4 * nbltypes_d, ntrees_d)!
	} else {
		distance_context_map = []int{len: 4 * nbltypes_d}
		for i in 0 .. 4 * nbltypes_d {
			distance_context_map[i] = 0
		}
	}

	// Read prefix codes: NTREESL literals, NBLTYPESI insert-and-copy, NTREESD distances
	mut literal_trees := []PrefixDecoder{}
	for _ in 0 .. ntrees_l {
		literal_trees << read_prefix_tree(mut r, 256)!
	}
	mut insert_copy_trees := []PrefixDecoder{}
	for _ in 0 .. nbltypes_i {
		insert_copy_trees << read_prefix_tree(mut r, 704)!
	}
	distance_alphabet_size := 16 + ndirect + (48 << npostfix)
	mut distance_trees := []PrefixDecoder{}
	for _ in 0 .. ntrees_d {
		distance_trees << read_prefix_tree(mut r, distance_alphabet_size)!
	}

	// Block switching state: prev=1, curr=0 per RFC
	mut prev_block := [1, 1, 1]
	mut curr_block := [0, 0, 0]

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
		// Ensure an insert-and-copy block is active
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
		insert_len := cmd.insert_len_offset + int(r.read_bits(cmd.insert_len_extra_bits)!)
		copy_len := cmd.copy_len_offset + int(r.read_bits(cmd.copy_len_extra_bits)!)
		if insert_len == 0 && copy_len == 0 {
			return error('brotli: invalid zero-progress command in compressed meta-block')
		}
		if produced + insert_len > mlen {
			return error('brotli: compressed insert length exceeds metablock')
		}
		if nbltypes_i >= 2 {
			block_remaining[1]--
		}
		// The final command may end with inserted literals and omit its copy.
		if produced == mlen {
			break
		}

		// Literals
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
			// select literal tree based on context map and last two bytes
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
			// update last two bytes
			p2 = p1
			p1 = u8(lit)
			produced++
			if nbltypes_l >= 2 {
				block_remaining[0]--
			}
		}

		if produced == mlen {
			continue
		}
		if produced + copy_len > mlen {
			return error('brotli: compressed copy length exceeds metablock at ${produced}, command ${command_count}, insert ${insert_len}, copy ${copy_len}, sym ${sym}, mlen ${mlen}, distance index ${distance_index[0]}, recent ${recent_distances}')
		}
		if copy_len == 0 {
			continue
		}

		// Distance block switch (only if next element is a distance symbol)
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
			// select distance tree based on copy length context
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
				dextra := int(r.read_bits(dextra_bits)!)
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
				}
				recent_distances[distance_index[0] & 3] = distance
				distance_index[0]++
				continue
			}
		}
		if distance > max_distance {
			if distance - max_distance - 1 < 1 << 20 {
				transformed_len := decode_static_dictionary_word(distance, max_distance, copy_len, mut out)!
				produced += transformed_len
				if distance_index_adjusted {
					distance_index[0]++
				}
				p2 = if out.len >= 2 { out[out.len - 2] } else { 0 }
				p1 = out[out.len - 1]
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
			// update last-two context bytes
			p2 = p1
			p1 = b
			produced++
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
	return out
}
