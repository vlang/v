module brotli

fn resolve_distance_short_symbol(dist_sym int, recent_distances []int, distance_index int) !int {
	if recent_distances.len != 4 {
		return error('brotli: invalid distance ring state')
	}
	if dist_sym < 0 || dist_sym >= 16 {
		return error('brotli: invalid short distance symbol')
	}
	mut ring_offset := 3
	mut delta := 0
	if dist_sym <= 3 {
		ring_offset = 3 - dist_sym
	} else if dist_sym < 10 {
		delta = ((dist_sym - 4) / 2 + 1) * (if dist_sym & 1 == 0 { -1 } else { 1 })
	} else {
		ring_offset = 2
		delta = ((dist_sym - 10) / 2 + 1) * (if dist_sym & 1 == 0 { -1 } else { 1 })
	}
	distance := recent_distances[(distance_index + ring_offset) & 3] + delta
	if distance <= 0 {
		return error('brotli: invalid non-positive short distance')
	}
	return distance
}

// Lookup tables for UTF8/signed literal context computation (RFC 7932).
const lut0 = [
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	4,
	4,
	0,
	0,
	4,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	8,
	12,
	16,
	12,
	12,
	20,
	12,
	16,
	24,
	28,
	12,
	12,
	32,
	12,
	36,
	12,
	44,
	44,
	44,
	44,
	44,
	44,
	44,
	44,
	44,
	44,
	32,
	32,
	24,
	40,
	28,
	12,
	12,
	48,
	52,
	52,
	52,
	48,
	52,
	52,
	52,
	48,
	52,
	52,
	52,
	52,
	52,
	48,
	52,
	52,
	52,
	52,
	52,
	48,
	52,
	52,
	52,
	52,
	52,
	24,
	12,
	28,
	12,
	12,
	12,
	56,
	60,
	60,
	60,
	56,
	60,
	60,
	60,
	56,
	60,
	60,
	60,
	60,
	60,
	56,
	60,
	60,
	60,
	60,
	60,
	56,
	60,
	60,
	60,
	60,
	60,
	24,
	12,
	28,
	12,
	0,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	0,
	1,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
	2,
	3,
]

const lut1 = [
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	1,
	1,
	1,
	1,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
]

const lut2 = [
	0,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	1,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	2,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	3,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	4,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	5,
	6,
	6,
	6,
	6,
	6,
	6,
	6,
	6,
	6,
	6,
	6,
	6,
	6,
	6,
	6,
	7,
]

fn utf8_second_context(value u8) int {
	if value <= 32 {
		return 0
	}
	if value <= 47 || value >= 58 && value <= 64 || value >= 91 && value <= 96
		|| value >= 123 && value <= 126 {
		return 1
	}
	if value <= 57 || value >= 65 && value <= 90 {
		return 2
	}
	if value <= 122 {
		return 3
	}
	if value <= 191 {
		return 0
	}
	if value <= 223 {
		return 0
	}
	return 2
}

fn signed_second_context(value u8) int {
	if value == 0 {
		return 0
	}
	if value <= 15 {
		return 1
	}
	if value <= 63 {
		return 2
	}
	if value <= 127 {
		return 3
	}
	if value <= 191 {
		return 4
	}
	if value <= 239 {
		return 5
	}
	return if value == 255 { 7 } else { 6 }
}

fn compute_literal_context_id(p1 u8, p2 u8, mode int) int {
	match mode {
		0 { return int(p1 & 0x3f) }
		1 { return int(p1 >> 2) }
		2 { return int(lut0[int(p1)]) | utf8_second_context(p2) }
		3 { return int((u32(lut2[int(p1)]) << u32(3)) | u32(signed_second_context(p2))) }
		else { return 0 }
	}
}

// read_context_map decodes a context map of given size. The context map's
// alphabet has NTREES + RLEMAX symbols as specified in RFC 7932 Section 7.3.
fn read_context_map(mut r BitReader, size int, ntrees int) ![]int {
	// RLEMAX: 0 encoded as single 0 bit, else 1 followed by 4 bits giving 1..16
	first := r.read_bit()!
	mut rlemax := 0
	if first == 0 {
		rlemax = 0
	} else {
		v := int(r.read_bits(4)!)
		rlemax = v + 1
	}
	alphabet_size := ntrees + rlemax
	// Read prefix code for context map alphabet
	dec := read_prefix_tree(mut r, alphabet_size)!
	mut vals := []int{}
	for vals.len < size {
		sym := r.read_prefix_symbol(dec)!
		if sym < 0 || sym >= alphabet_size {
			return error('brotli: invalid context map symbol')
		}
		if sym == 0 {
			vals << 0
			continue
		}
		if sym <= rlemax {
			bits := sym
			mut extra := 0
			if bits > 0 {
				extra = int(r.read_bits(bits)!)
			}
			count := (1 << bits) + extra
			for _ in 0 .. count {
				if vals.len >= size {
					return error('brotli: context map run exceeds size')
				}
				vals << 0
			}
			continue
		}
		// value symbol: maps to (sym - rlemax)
		vals << (sym - rlemax)
	}
	// IMTF bit: inverse move-to-front transform when set
	imtf := r.read_bit()!
	if imtf == 1 {
		// inverse move-to-front: values are positions, map to actual values
		mut mtf := []int{len: 256, init: index}
		mut out := []int{}
		for pos in vals {
			if pos < 0 || pos >= mtf.len {
				return error('brotli: invalid mtf index in context map')
			}
			val := mtf[pos]
			out << val
			// move to front
			for j := pos; j > 0; j-- {
				mtf[j] = mtf[j - 1]
			}
			mtf[0] = val
		}
		return out
	}
	return vals
}

fn distance_symbol_extra_bits(dcode int, npostfix int, ndirect int) !int {
	if dcode < 0 {
		return error('brotli: invalid distance code')
	}
	if dcode < 16 + ndirect {
		return 0
	}
	return 1 + ((dcode - ndirect - 16) >> (npostfix + 1))
}

const block_count_extra_bits = [2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 7, 8, 9,
	10, 11, 12, 13, 24]

const block_count_bases = [1, 5, 9, 13, 17, 25, 33, 41, 49, 65, 81, 97, 113, 145, 177, 209, 241,
	305, 369, 497, 753, 1265, 2289, 4337, 8433, 16625]

// read_block_count decodes a block count using the block count prefix decoder
fn read_block_count(mut r BitReader, dec PrefixDecoder) !int {
	sym := r.read_prefix_symbol(dec)!
	if sym < 0 || sym >= block_count_extra_bits.len {
		return error('brotli: invalid block count code')
	}
	bits := block_count_extra_bits[sym]
	mut extra := 0
	if bits > 0 {
		extra = int(r.read_bits(bits)!)
	}
	return block_count_bases[sym] + extra
}

// BlockCountCode represents the symbol and extra bits for a block count value
struct BlockCountCode {
	symbol     int
	extra      int
	extra_bits int
}

// block_count_symbol_for_value selects the block count symbol and extra bits for a given
// block count value using the RFC 7932 block count mapping.
fn block_count_symbol_for_value(count int) !BlockCountCode {
	for i in 0 .. block_count_extra_bits.len {
		bits := block_count_extra_bits[i]
		base := block_count_bases[i]
		mut max := base
		if bits > 0 {
			max = base + ((1 << bits) - 1)
		}
		if count >= base && count <= max {
			return BlockCountCode{ symbol: i, extra: count - base, extra_bits: bits }
		}
	}
	return error('brotli: invalid block count value')
}

// encode_block_type_symbol computes the block type symbol for a block-switch command
// given the previous and current block types and the desired new block type.
fn encode_block_type_symbol(prev int, curr int, new_type int, nbltypes int) int {
	if new_type == prev {
		return 0
	}
	if new_type == (curr + 1) % nbltypes {
		return 1
	}
	return new_type + 2
}

// write_symbol_using_codes writes a symbol using a canonical code table (codes + lengths).
fn write_symbol_using_codes(mut w BitWriter, codes []u32, lengths []int, sym int) {
	if sym < 0 || sym >= lengths.len {
		// out-of-range: nothing to write
		return
	}
	// Treat non-positive lengths as zero-length (no bits emitted).
	if lengths[sym] <= 0 {
		return
	}
	w.write_bits(codes[sym], lengths[sym])
}

// write_block_count_using_enc writes a block count using an existing block-count prefix
// code represented by codes/lengths. It selects the symbol and writes any extra bits.
fn write_block_count_using_enc(mut w BitWriter, codes []u32, lengths []int, count int) ! {
	bc := block_count_symbol_for_value(count)!
	write_symbol_using_codes(mut w, codes, lengths, bc.symbol)
	if bc.extra_bits > 0 {
		w.write_bits(u32(bc.extra), bc.extra_bits)
	}
}

fn distance_from_code(dcode int, dextra int, npostfix int, ndirect int) !int {
	if dcode < 16 + ndirect {
		return dcode - 15
	}
	postfix_mask := (1 << npostfix) - 1
	ndistbits := distance_symbol_extra_bits(dcode, npostfix, ndirect)!
	hcode := (dcode - ndirect - 16) >> npostfix
	lcode := (dcode - ndirect - 16) & postfix_mask
	offset := int(u32(2 + (hcode & 1)) << u32(ndistbits)) - 4
	return int(u32(offset + dextra) << u32(npostfix)) + lcode + ndirect + 1
}

fn encode_distance_code(distance int, npostfix int, ndirect int) !DistanceCode {
	if distance <= 0 {
		return error('brotli: invalid non-positive distance')
	}
	for dcode in 16 + ndirect .. 64 {
		nbits := distance_symbol_extra_bits(dcode, npostfix, ndirect)!
		min_dist := distance_from_code(dcode, 0, npostfix, ndirect)!
		max_dist := distance_from_code(dcode, (1 << nbits) - 1, npostfix, ndirect)!
		if distance < min_dist || distance > max_dist {
			continue
		}
		return DistanceCode{
			symbol:      dcode
			extra:       distance - min_dist
			extra_bits:  nbits
			should_push: true
		}
	}
	return error('brotli: distance out of supported range')
}

fn push_recent_distance(mut recent_distances []int, distance int) {
	if recent_distances.len != 4 {
		return
	}
	recent_distances[0] = recent_distances[1]
	recent_distances[1] = recent_distances[2]
	recent_distances[2] = recent_distances[3]
	recent_distances[3] = distance
}

// RFC 7932 dictionary transforms use byte operations, rather than Unicode casing.
fn brotli_uppercase_first(mut out []u8) int {
	if out.len == 0 {
		return 0
	}
	if out[0] < 0xc0 {
		if out[0] >= `a` && out[0] <= `z` {
			out[0] ^= 32
		}
		return 1
	}
	if out[0] < 0xe0 {
		if out.len >= 2 {
			out[1] ^= 32
		}
		return 2
	}
	if out.len >= 3 {
		out[2] ^= 5
	}
	return 3
}

fn brotli_uppercase_all(mut out []u8) {
	mut i := 0
	for i < out.len {
		i += brotli_uppercase_first(mut out[i..])
	}
}

fn transform_dictionary_word(word []u8, transform_idx int) ![]u8 {
	if transform_idx < 0 || transform_idx >= brotli_transforms.len / 3 {
		return error('brotli: invalid dictionary transform')
	}
	prefix_id := brotli_transforms[transform_idx * 3]
	transform_type := brotli_transforms[transform_idx * 3 + 1]
	suffix_id := brotli_transforms[transform_idx * 3 + 2]
	prefix_pos := brotli_prefix_suffix_map[prefix_id]
	suffix_pos := brotli_prefix_suffix_map[suffix_id]
	prefix_len := brotli_prefix_suffix[prefix_pos]
	suffix_len := brotli_prefix_suffix[suffix_pos]
	mut transformed := []u8{cap: int(prefix_len + suffix_len) + word.len}
	for i in 0 .. int(prefix_len) {
		transformed << u8(brotli_prefix_suffix[prefix_pos + 1 + i])
	}
	mut body := word.clone()
	if transform_type <= 9 {
		kept_len := if int(transform_type) > body.len { 0 } else { body.len - int(transform_type) }
		body = body[..kept_len]
	} else if transform_type >= 12 && transform_type <= 20 {
		skip := int(transform_type) - 11
		omitted_len := if skip > body.len { body.len } else { skip }
		body = body[omitted_len..]
	}
	transformed << body
	if transform_type == 10 {
		brotli_uppercase_first(mut transformed[prefix_len..])
	} else if transform_type == 11 {
		brotli_uppercase_all(mut transformed[prefix_len..])
	}
	for i in 0 .. int(suffix_len) {
		transformed << u8(brotli_prefix_suffix[suffix_pos + 1 + i])
	}
	return transformed
}

fn decode_static_dictionary_word(distance int, max_distance int, copy_len int, max_output_len int,
	mut out []u8) !int {
	if copy_len < 4 || copy_len > 24 {
		return error('brotli: invalid dictionary word length ${copy_len} at distance ${distance}')
	}
	address := distance - max_distance - 1
	bits := brotli_dictionary_bits[copy_len - 4]
	word_idx := address & ((1 << bits) - 1)
	transform_idx := address >> bits
	offset := brotli_dictionary_offsets[copy_len - 4] + word_idx * copy_len
	word := brotli_dictionary_data[offset..offset + copy_len]
	transformed := transform_dictionary_word(word, transform_idx)!
	if transformed.len > max_output_len {
		return error('brotli: transformed dictionary word exceeds metablock')
	}
	out << transformed
	return transformed.len
}

fn encode_distance_code_with_recent(distance int, recent_distances []int, npostfix int, ndirect int) !DistanceCode {
	if recent_distances.len != 4 {
		return error('brotli: invalid distance ring state')
	}
	last := recent_distances[3]
	second_last := recent_distances[2]
	symbol := recent_distance_symbol(distance, last, second_last, recent_distances[1], recent_distances[0])
	if symbol >= 0 {
		return DistanceCode{
			symbol:      symbol
			extra:       0
			extra_bits:  0
			should_push: symbol != 0
		}
	}
	return encode_distance_code(distance, npostfix, ndirect)!
}

fn recent_distance_symbol(distance int, last int, second_last int, third int, first int) int {
	if distance <= 0 {
		return -1
	}
	if distance == last {
		return 0
	}
	if distance == second_last {
		return 1
	}
	if distance == third {
		return 2
	}
	if distance == first {
		return 3
	}
	if distance == last - 1 {
		return 4
	}
	if distance == last + 1 {
		return 5
	}
	if distance == last - 2 {
		return 6
	}
	if distance == last + 2 {
		return 7
	}
	if distance == last - 3 {
		return 8
	}
	if distance == last + 3 {
		return 9
	}
	if distance == second_last - 1 {
		return 10
	}
	if distance == second_last + 1 {
		return 11
	}
	if distance == second_last - 2 {
		return 12
	}
	if distance == second_last + 2 {
		return 13
	}
	if distance == second_last - 3 {
		return 14
	}
	if distance == second_last + 3 {
		return 15
	}
	return -1
}

const cmd_insert_length_extra_bits = [u8(0), 0, 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 7,
	8, 9, 10, 12, 14, 24]

const cmd_copy_length_extra_bits = [u8(0), 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6,
	7, 8, 9, 10, 24]

const cmd_insert_offsets = [u16(0), 1, 2, 3, 4, 5, 6, 8, 10, 14, 18, 26, 34, 50, 66, 98, 130, 194,
	322, 578, 1090, 2114, 6210, 22594]

const cmd_copy_offsets = [u16(2), 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 18, 22, 30, 38, 54, 70, 102,
	134, 198, 326, 582, 1094, 2118]

const cmd_cell_pos = [0, 1, 0, 1, 8, 9, 2, 16, 10, 17, 18]

fn cmd_lut(symbol int) CmdLutElement {
	cell_pos := cmd_cell_pos[symbol >> 6]
	insert_code := (cell_pos & 0x18) | ((symbol >> 3) & 7)
	copy_code := ((cell_pos << 3) & 0x18) | (symbol & 7)
	copy_offset := int(cmd_copy_offsets[copy_code])
	return CmdLutElement{
		insert_len_extra_bits: int(cmd_insert_length_extra_bits[insert_code])
		copy_len_extra_bits:   int(cmd_copy_length_extra_bits[copy_code])
		distance_code:         if symbol >> 6 >= 2 { -1 } else { 0 }
		context:               if copy_offset > 4 { 3 } else { copy_offset - 2 }
		insert_len_offset:     int(cmd_insert_offsets[insert_code])
		copy_len_offset:       copy_offset
	}
}

fn bit_width(v int) int {
	mut n := 0
	mut x := v
	for x > 0 {
		n++
		x >>= 1
	}
	return if n == 0 { 1 } else { n }
}

fn sort_small(mut values []int) {
	for i in 1 .. values.len {
		key := values[i]
		mut j := i
		for j > 0 && values[j - 1] > key {
			values[j] = values[j - 1]
			j--
		}
		values[j] = key
	}
}

fn decode_simple_prefix_code(mut r BitReader, alphabet_size int) ![]int {
	if alphabet_size <= 0 {
		return error('brotli: invalid alphabet size')
	}
	if r.read_bits(2)! != 1 {
		return error('brotli: expected simple prefix code')
	}
	nsym := int(r.read_bits(2)!) + 1
	alphabet_bits := bit_width(alphabet_size - 1)
	mut symbols := []int{len: nsym}
	mut seen := []bool{len: alphabet_size}
	for i in 0 .. nsym {
		sym := int(r.read_bits(alphabet_bits)!)
		if sym >= alphabet_size {
			return error('brotli: simple prefix symbol out of range')
		}
		if seen[sym] {
			return error('brotli: duplicate simple prefix symbol')
		}
		seen[sym] = true
		symbols[i] = sym
	}
	mut lengths := []int{len: alphabet_size}
	match nsym {
		1 {
			// Keep single-symbol trees distinguishable from "all absent symbols".
			lengths[symbols[0]] = -1
		}
		2 {
			for sym in symbols {
				lengths[sym] = 1
			}
		}
		3 {
			lengths[symbols[0]] = 1
			lengths[symbols[1]] = 2
			lengths[symbols[2]] = 2
		}
		4 {
			tree_select := r.read_bit()!
			if tree_select == 0 {
				for sym in symbols {
					lengths[sym] = 2
				}
			} else {
				lengths[symbols[0]] = 1
				lengths[symbols[1]] = 2
				lengths[symbols[2]] = 3
				lengths[symbols[3]] = 3
			}
		}
		else {
			return error('brotli: invalid simple prefix symbol count')
		}
	}

	return lengths
}

const code_length_code_bits = [2, 2, 2, 3, 2, 2, 2, 4, 2, 2, 2, 3, 2, 2, 2, 4]
const code_length_code_values = [0, 4, 3, 2, 0, 4, 3, 1, 0, 4, 3, 2, 0, 4, 3, 5]

fn read_code_length_value(mut r BitReader) !int {
	first := int(r.read_bits(2)!)
	match first {
		0 { return 0 }
		1 { return 4 }
		2 { return 3 }
		3 {
			third := int(r.read_bit()!)
			if third == 0 {
				return 2
			}
			fourth := int(r.read_bit()!)
			return if fourth == 0 { 1 } else { 5 }
		}
		else { return error('brotli: invalid code-length code') }
	}
}

fn decode_prefix_lengths_rfc(mut r BitReader, alphabet_size int) ![]int {
	if alphabet_size <= 0 {
		return error('brotli: invalid alphabet size')
	}
	tag := int(r.read_bits(2)!)
	if tag == 1 {
		nsym := int(r.read_bits(2)!) + 1
		alphabet_bits := bit_width(alphabet_size - 1)
		mut symbols := []int{len: nsym}
		mut seen := []bool{len: alphabet_size}
		for i in 0 .. nsym {
			sym := int(r.read_bits(alphabet_bits)!)
			if sym >= alphabet_size || seen[sym] {
				return error('brotli: invalid simple prefix symbol')
			}
			seen[sym] = true
			symbols[i] = sym
		}
		mut lengths := []int{len: alphabet_size}
		match nsym {
			1 { lengths[symbols[0]] = -1 }
			2 {
				for sym in symbols {
					lengths[sym] = 1
				}
			}
			3 {
				lengths[symbols[0]] = 1
				lengths[symbols[1]] = 2
				lengths[symbols[2]] = 2
			}
			4 {
				if r.read_bit()! == 0 {
					for sym in symbols {
						lengths[sym] = 2
					}
				} else {
					lengths[symbols[0]] = 1
					lengths[symbols[1]] = 2
					lengths[symbols[2]] = 3
					lengths[symbols[3]] = 3
				}
			}
			else { return error('brotli: invalid simple prefix symbol count') }
		}
		return lengths
	}
	if tag != 0 && tag != 2 && tag != 3 {
		return error('brotli: invalid complex prefix HSKIP')
	}
	hskip := tag
	mut code_lengths := []int{len: 18}
	mut start := 0
	mut space := 32
	mut num_codes := 0
	if hskip == 2 {
		start = 2
	}
	if hskip == 3 {
		start = 3
	}
	for i := start; i < code_length_code_order.len; i++ {
		value := read_code_length_value(mut r)!
		code_lengths[code_length_code_order[i]] = value
		if value > 0 {
			space -= 32 >> value
			num_codes++
			if space <= 0 {
				break
			}
		}
	}
	if num_codes != 1 && space != 0 {
		return error('brotli: invalid code-length prefix tree')
	}
	mut nonzero := 0
	mut only := -1
	for i, l in code_lengths {
		if l > 0 {
			nonzero++
			only = i
		}
	}
	if nonzero == 0 {
		return error('brotli: empty code-length prefix tree')
	}
	mut lengths := []int{len: alphabet_size}
	if nonzero == 1 {
		code_lengths[only] = -1
	}
	token_dec := build_prefix_decoder_with_order(code_lengths, .lsb_first) or {
		return err
	}
	mut out := 0
	mut previous := 8
	mut repeat := 0
	mut repeat_value := -1
	mut target_space := 32768
	for out < alphabet_size && target_space > 0 {
		sym := r.read_prefix_symbol(token_dec)!
		match sym {
			0...15 {
				repeat = 0
				lengths[out] = sym
				if sym != 0 {
					previous = sym
					target_space -= 32768 >> sym
				}
				out++
			}
			16, 17 {
				value := if sym == 16 { previous } else { 0 }
				bits := if sym == 16 { 2 } else { 3 }
				if repeat_value != value {
					repeat = 0
					repeat_value = value
				}
				old_repeat := repeat
				if repeat > 0 {
					repeat = (repeat - 2) << bits
				}
				repeat += int(r.read_bits(bits)!) + 3
				repeat_count := repeat - old_repeat
				if out + repeat_count > alphabet_size {
					return error('brotli: code-length repeat overflow')
				}
				for _ in 0 .. repeat_count {
					lengths[out] = value
					out++
				}
				if value > 0 {
					target_space -= repeat_count * (32768 >> value)
				}
			}
			else { return error('brotli: invalid code-length symbol') }
		}
	}
	if target_space != 0 {
		return error('brotli: incomplete prefix tree')
	}
	return lengths
}

fn read_prefix_tree(mut r BitReader, alphabet_size int) !PrefixDecoder {
	lengths := decode_prefix_lengths_rfc(mut r, alphabet_size)!
	return build_prefix_decoder(lengths) or {
		return err
	}
}

fn read_trees_count(mut r BitReader) !int {
	return r.read_varlen_uint8()! + 1
}
