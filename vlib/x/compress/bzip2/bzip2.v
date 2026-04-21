module bzip2

const bzip2_block_magic = u64(0x314159265359)
const bzip2_eos_magic = u64(0x177245385090)
const bzip2_runa = 0
const bzip2_runb = 1
const bzip2_max_groups = 6
const bzip2_max_alpha = 258
const bzip2_group_size = 50
const bzip2_max_code_len = 20
const bzip2_max_selectors = 18002
// vfmt off
const bzip2_crc32_table = [
	u32(0x00000000), 0x04c11db7, 0x09823b6e, 0x0d4326d9, 0x130476dc, 0x17c56b6b, 0x1a864db2, 0x1e475005,
	0x2608edb8, 0x22c9f00f, 0x2f8ad6d6, 0x2b4bcb61, 0x350c9b64, 0x31cd86d3, 0x3c8ea00a, 0x384fbdbd,
	0x4c11db70, 0x48d0c6c7, 0x4593e01e, 0x4152fda9, 0x5f15adac, 0x5bd4b01b, 0x569796c2, 0x52568b75,
	0x6a1936c8, 0x6ed82b7f, 0x639b0da6, 0x675a1011, 0x791d4014, 0x7ddc5da3, 0x709f7b7a, 0x745e66cd,
	0x9823b6e0, 0x9ce2ab57, 0x91a18d8e, 0x95609039, 0x8b27c03c, 0x8fe6dd8b, 0x82a5fb52, 0x8664e6e5,
	0xbe2b5b58, 0xbaea46ef, 0xb7a96036, 0xb3687d81, 0xad2f2d84, 0xa9ee3033, 0xa4ad16ea, 0xa06c0b5d,
	0xd4326d90, 0xd0f37027, 0xddb056fe, 0xd9714b49, 0xc7361b4c, 0xc3f706fb, 0xceb42022, 0xca753d95,
	0xf23a8028, 0xf6fb9d9f, 0xfbb8bb46, 0xff79a6f1, 0xe13ef6f4, 0xe5ffeb43, 0xe8bccd9a, 0xec7dd02d,
	0x34867077, 0x30476dc0, 0x3d044b19, 0x39c556ae, 0x278206ab, 0x23431b1c, 0x2e003dc5, 0x2ac12072,
	0x128e9dcf, 0x164f8078, 0x1b0ca6a1, 0x1fcdbb16, 0x018aeb13, 0x054bf6a4, 0x0808d07d, 0x0cc9cdca,
	0x7897ab07, 0x7c56b6b0, 0x71159069, 0x75d48dde, 0x6b93dddb, 0x6f52c06c, 0x6211e6b5, 0x66d0fb02,
	0x5e9f46bf, 0x5a5e5b08, 0x571d7dd1, 0x53dc6066, 0x4d9b3063, 0x495a2dd4, 0x44190b0d, 0x40d816ba,
	0xaca5c697, 0xa864db20, 0xa527fdf9, 0xa1e6e04e, 0xbfa1b04b, 0xbb60adfc, 0xb6238b25, 0xb2e29692,
	0x8aad2b2f, 0x8e6c3698, 0x832f1041, 0x87ee0df6, 0x99a95df3, 0x9d684044, 0x902b669d, 0x94ea7b2a,
	0xe0b41de7, 0xe4750050, 0xe9362689, 0xedf73b3e, 0xf3b06b3b, 0xf771768c, 0xfa325055, 0xfef34de2,
	0xc6bcf05f, 0xc27dede8, 0xcf3ecb31, 0xcbffd686, 0xd5b88683, 0xd1799b34, 0xdc3abded, 0xd8fba05a,
	0x690ce0ee, 0x6dcdfd59, 0x608edb80, 0x644fc637, 0x7a089632, 0x7ec98b85, 0x738aad5c, 0x774bb0eb,
	0x4f040d56, 0x4bc510e1, 0x46863638, 0x42472b8f, 0x5c007b8a, 0x58c1663d, 0x558240e4, 0x51435d53,
	0x251d3b9e, 0x21dc2629, 0x2c9f00f0, 0x285e1d47, 0x36194d42, 0x32d850f5, 0x3f9b762c, 0x3b5a6b9b,
	0x0315d626, 0x07d4cb91, 0x0a97ed48, 0x0e56f0ff, 0x1011a0fa, 0x14d0bd4d, 0x19939b94, 0x1d528623,
	0xf12f560e, 0xf5ee4bb9, 0xf8ad6d60, 0xfc6c70d7, 0xe22b20d2, 0xe6ea3d65, 0xeba91bbc, 0xef68060b,
	0xd727bbb6, 0xd3e6a601, 0xdea580d8, 0xda649d6f, 0xc423cd6a, 0xc0e2d0dd, 0xcda1f604, 0xc960ebb3,
	0xbd3e8d7e, 0xb9ff90c9, 0xb4bcb610, 0xb07daba7, 0xae3afba2, 0xaafbe615, 0xa7b8c0cc, 0xa379dd7b,
	0x9b3660c6, 0x9ff77d71, 0x92b45ba8, 0x9675461f, 0x8832161a, 0x8cf30bad, 0x81b02d74, 0x857130c3,
	0x5d8a9099, 0x594b8d2e, 0x5408abf7, 0x50c9b640, 0x4e8ee645, 0x4a4ffbf2, 0x470cdd2b, 0x43cdc09c,
	0x7b827d21, 0x7f436096, 0x7200464f, 0x76c15bf8, 0x68860bfd, 0x6c47164a, 0x61043093, 0x65c52d24,
	0x119b4be9, 0x155a565e, 0x18197087, 0x1cd86d30, 0x029f3d35, 0x065e2082, 0x0b1d065b, 0x0fdc1bec,
	0x3793a651, 0x3352bbe6, 0x3e119d3f, 0x3ad08088, 0x2497d08d, 0x2056cd3a, 0x2d15ebe3, 0x29d4f654,
	0xc5a92679, 0xc1683bce, 0xcc2b1d17, 0xc8ea00a0, 0xd6ad50a5, 0xd26c4d12, 0xdf2f6bcb, 0xdbee767c,
	0xe3a1cbc1, 0xe760d676, 0xea23f0af, 0xeee2ed18, 0xf0a5bd1d, 0xf464a0aa, 0xf9278673, 0xfde69bc4,
	0x89b8fd09, 0x8d79e0be, 0x803ac667, 0x84fbdbd0, 0x9abc8bd5, 0x9e7d9662, 0x933eb0bb, 0x97ffad0c,
	0xafb010b1, 0xab710d06, 0xa6322bdf, 0xa2f33668, 0xbcb4666d, 0xb8757bda, 0xb5365d03, 0xb1f740b4
]!
// vfmt on

@[params]
pub struct CompressParams {
pub:
	block_size int = 9 // Valid range is 1..9 (100k to 900k block size).
}

@[params]
pub struct DecompressParams {
pub:
	verify_crc bool = true
}

// compress compresses `src` into a bzip2 byte stream.
pub fn compress(src []u8, params CompressParams) ![]u8 {
	if params.block_size < 1 || params.block_size > 9 {
		return error('bzip2: block_size must be in 1..9')
	}
	mut w := BitWriter{
		out: []u8{}
	}

	w.write_byte(`B`)
	w.write_byte(`Z`)
	w.write_byte(`h`)
	w.write_byte(u8(`0`) + u8(params.block_size))

	max_block := params.block_size * 100000
	mut stream_crc := u32(0)
	if src.len > 0 {
		mut pos := 0
		for pos < src.len {
			end := if pos + max_block < src.len { pos + max_block } else { src.len }
			chunk := src[pos..end]
			block := encode_block(chunk)!
			w.write_bits(48, bzip2_block_magic)
			w.write_bits(32, u64(block.block_crc))
			w.write_bit(0) // randomized flag; unsupported legacy mode
			w.write_bits(24, u64(block.orig_ptr))
			write_in_use_map(mut w, block.in_use)
			w.write_bits(3, u64(block.n_groups))
			w.write_bits(15, u64(block.selectors.len))
			for sel in block.selector_mtf {
				for _ in 0 .. sel {
					w.write_bit(1)
				}
				w.write_bit(0)
			}
			for g in 0 .. block.n_groups {
				write_code_lengths(mut w, block.code_lengths[g], block.alpha_size)
			}
			mut grp_idx := 0
			mut grp_pos := 0
			for sym in block.symbols {
				if grp_pos == 0 {
					grp_idx = block.selectors[grp_idx]
				}
				code := block.codes[grp_idx][sym]
				len := block.code_lengths[grp_idx][sym]
				w.write_bits(len, u64(code))
				grp_pos++
				if grp_pos == bzip2_group_size {
					grp_pos = 0
					grp_idx++
				}
			}
			stream_crc = rotate_left_1(stream_crc) ^ block.block_crc
			pos = end
		}
	}
	w.write_bits(48, bzip2_eos_magic)
	w.write_bits(32, u64(stream_crc))
	return w.finish()
}

// decompress decompresses a bzip2 byte stream.
pub fn decompress(src []u8, params DecompressParams) ![]u8 {
	mut r := BitReader{
		data: src
	}
	if r.read_byte()! != `B` || r.read_byte()! != `Z` || r.read_byte()! != `h` {
		return error('bzip2: invalid header')
	}
	lvl := r.read_byte()!
	if lvl < `1` || lvl > `9` {
		return error('bzip2: invalid block size marker')
	}
	block_limit := int(lvl - `0`) * 100000
	mut out := []u8{}
	mut stream_crc := u32(0)
	for {
		magic := r.read_bits(48)!
		if magic == bzip2_eos_magic {
			stored_stream_crc := u32(r.read_bits(32)!)
			if params.verify_crc && stored_stream_crc != stream_crc {
				return error('bzip2: stream crc mismatch')
			}
			if !r.is_aligned_to_byte_zero_padding() {
				return error('bzip2: trailing non-zero bits')
			}
			return out
		}
		if magic != bzip2_block_magic {
			return error('bzip2: invalid block magic')
		}
		block_crc := u32(r.read_bits(32)!)
		randomized := r.read_bit()!
		if randomized != 0 {
			return error('bzip2: randomized blocks are not supported')
		}
		orig_ptr := int(r.read_bits(24)!)
		in_use := read_in_use_map(mut r)!
		mut seq_to_unseq := []u8{}
		for i, used in in_use {
			if used {
				seq_to_unseq << u8(i)
			}
		}
		n_in_use := seq_to_unseq.len
		alpha_size := n_in_use + 2
		if alpha_size < 2 || alpha_size > bzip2_max_alpha {
			return error('bzip2: invalid alphabet size')
		}
		n_groups := int(r.read_bits(3)!)
		if n_groups < 2 || n_groups > bzip2_max_groups {
			return error('bzip2: invalid huffman group count')
		}
		n_selectors := int(r.read_bits(15)!)
		if n_selectors < 1 || n_selectors > bzip2_max_selectors {
			return error('bzip2: invalid selector count')
		}
		mut selectors_mtf := []int{len: n_selectors}
		for i in 0 .. n_selectors {
			mut c := 0
			for {
				b := r.read_bit()!
				if b == 0 {
					break
				}
				c++
				if c >= n_groups {
					return error('bzip2: invalid selector mtf value')
				}
			}
			selectors_mtf[i] = c
		}
		selectors := decode_selector_mtf(selectors_mtf, n_groups)!

		mut code_lengths := [][]int{len: n_groups, init: []int{len: alpha_size}}
		for g in 0 .. n_groups {
			mut curr := int(r.read_bits(5)!)
			if curr < 1 || curr > bzip2_max_code_len {
				return error('bzip2: invalid initial huffman code length')
			}
			for i in 0 .. alpha_size {
				for {
					flag := r.read_bit()!
					if flag == 0 {
						break
					}
					up_down := r.read_bit()!
					if up_down == 0 {
						curr++
					} else {
						curr--
					}
					if curr < 1 || curr > bzip2_max_code_len {
						return error('bzip2: invalid huffman code length delta')
					}
				}
				code_lengths[g][i] = curr
			}
		}

		mut tables := []DecodeTable{len: n_groups}
		for g in 0 .. n_groups {
			tables[g] = build_decode_table(code_lengths[g], alpha_size)!
		}

		eob := n_in_use + 1
		mut selector_index := 0
		mut group_pos := 0
		mut mtf := []int{len: n_in_use}
		for i in 0 .. n_in_use {
			mtf[i] = i
		}
		mut decoded_syms := []int{}
		mut next_sym := 0
		for {
			if group_pos == 0 {
				if selector_index >= selectors.len {
					return error('bzip2: selectors exhausted')
				}
			}
			grp := selectors[selector_index]
			next_sym = tables[grp].decode(mut r)!
			group_pos++
			if group_pos == bzip2_group_size {
				group_pos = 0
				selector_index++
			}
			if next_sym == eob {
				break
			}
			if next_sym == bzip2_runa || next_sym == bzip2_runb {
				mut s := i64(-1)
				mut n := i64(1)
				remaining := block_limit - decoded_syms.len
				if remaining < 1 {
					return error('bzip2: block output exceeds declared block size')
				}
				for {
					if next_sym == bzip2_runa {
						s += n
					} else {
						s += n * 2
					}
					if s + 1 > i64(remaining) {
						return error('bzip2: block output exceeds declared block size')
					}
					n *= 2
					if group_pos == 0 {
						if selector_index >= selectors.len {
							return error('bzip2: selectors exhausted in run')
						}
					}
					grp2 := selectors[selector_index]
					next_sym = tables[grp2].decode(mut r)!
					group_pos++
					if group_pos == bzip2_group_size {
						group_pos = 0
						selector_index++
					}
					if next_sym != bzip2_runa && next_sym != bzip2_runb {
						break
					}
				}
				run_len := int(s + 1)
				if run_len < 1 {
					return error('bzip2: invalid run length')
				}
				if mtf.len == 0 {
					return error('bzip2: invalid run with empty mtf table')
				}
				ensure_block_output_limit(decoded_syms.len, run_len, block_limit)!
				for _ in 0 .. run_len {
					decoded_syms << mtf[0]
				}
				if next_sym == eob {
					break
				}
			}
			if next_sym == eob {
				break
			}
			if next_sym < 2 || next_sym > eob {
				return error('bzip2: invalid symbol value')
			}
			pos := next_sym - 1
			if pos >= mtf.len {
				return error('bzip2: mtf index out of range')
			}
			sym := mtf[pos]
			move_to_front_int(mut mtf, pos)
			ensure_block_output_limit(decoded_syms.len, 1, block_limit)!
			decoded_syms << sym
		}

		mut bwt_bytes := []u8{cap: decoded_syms.len}
		for s in decoded_syms {
			if s < 0 || s >= seq_to_unseq.len {
				return error('bzip2: decoded symbol is out of sequence map range')
			}
			bwt_bytes << seq_to_unseq[s]
		}

		rle1 := inverse_bwt(bwt_bytes, orig_ptr)!
		plain := rle1_decode(rle1)!
		if params.verify_crc {
			calc := bzip2_crc32(plain)
			if calc != block_crc {
				return error('bzip2: block crc mismatch')
			}
		}
		stream_crc = rotate_left_1(stream_crc) ^ block_crc
		out << plain
	}
	return error('bzip2: unexpected end of stream')
}

struct EncodedBlock {
	block_crc    u32
	orig_ptr     int
	in_use       []bool
	alpha_size   int
	n_groups     int
	selectors    []int
	selector_mtf []int
	symbols      []int
	code_lengths [][]int
	codes        [][]u32
}

fn encode_block(src []u8) !EncodedBlock {
	block_crc := bzip2_crc32(src)
	rle1 := rle1_encode(src)
	if rle1.len == 0 {
		return error('bzip2: internal error, empty block after rle1')
	}
	bwt, orig_ptr := bwt_transform(rle1)
	mut in_use := []bool{len: 256}
	for b in bwt {
		in_use[int(b)] = true
	}
	mut seq_to_unseq := []u8{}
	for i, used in in_use {
		if used {
			seq_to_unseq << u8(i)
		}
	}
	mut unseq_to_seq := []int{len: 256, init: -1}
	for i, b in seq_to_unseq {
		unseq_to_seq[int(b)] = i
	}
	mtf_symbols, alpha_size := mtf_rle2_encode(bwt, unseq_to_seq, seq_to_unseq.len)
	if mtf_symbols.len == 0 {
		return error('bzip2: internal error, empty symbol stream')
	}
	n_groups := select_group_count(mtf_symbols.len)
	n_selectors := selector_count_from_symbol_count(mtf_symbols.len)!
	mut selectors := []int{len: n_selectors, init: 0}
	selector_mtf := encode_selector_mtf(selectors, n_groups)
	freq := symbol_freq(mtf_symbols, alpha_size)
	lens := make_huffman_lengths(freq, bzip2_max_code_len)
	codes := build_huffman_codes(lens)
	mut code_lengths := [][]int{len: n_groups}
	mut code_words := [][]u32{len: n_groups}
	for i in 0 .. n_groups {
		code_lengths[i] = lens.clone()
		code_words[i] = codes.clone()
	}
	return EncodedBlock{
		block_crc:    block_crc
		orig_ptr:     orig_ptr
		in_use:       in_use
		alpha_size:   alpha_size
		n_groups:     n_groups
		selectors:    selectors
		selector_mtf: selector_mtf
		symbols:      mtf_symbols
		code_lengths: code_lengths
		codes:        code_words
	}
}

fn select_group_count(n_syms int) int {
	if n_syms < 200 {
		return 2
	}
	if n_syms < 600 {
		return 3
	}
	if n_syms < 1200 {
		return 4
	}
	if n_syms < 2400 {
		return 5
	}
	return 6
}

fn selector_count_from_symbol_count(n_symbols int) !int {
	if n_symbols < 1 {
		return error('bzip2: invalid selector count')
	}
	n_selectors := (n_symbols + bzip2_group_size - 1) / bzip2_group_size
	if n_selectors < 1 || n_selectors > bzip2_max_selectors {
		return error('bzip2: invalid selector count')
	}
	return n_selectors
}

fn ensure_block_output_limit(decoded_len int, add_len int, block_limit int) ! {
	if decoded_len < 0 || add_len < 0 || block_limit < 0 {
		return error('bzip2: invalid block output state')
	}
	if decoded_len > block_limit || add_len > block_limit - decoded_len {
		return error('bzip2: block output exceeds declared block size')
	}
}

fn symbol_freq(symbols []int, alpha_size int) []int {
	mut freq := []int{len: alpha_size}
	for s in symbols {
		if s >= 0 && s < alpha_size {
			freq[s]++
		}
	}
	for i in 0 .. alpha_size {
		if freq[i] == 0 {
			freq[i] = 1
		}
	}
	return freq
}

fn write_in_use_map(mut w BitWriter, in_use []bool) {
	mut group_used := []bool{len: 16}
	for i in 0 .. 16 {
		for j in 0 .. 16 {
			if in_use[i * 16 + j] {
				group_used[i] = true
				break
			}
		}
	}
	for g in group_used {
		w.write_bit(if g { 1 } else { 0 })
	}
	for i in 0 .. 16 {
		if !group_used[i] {
			continue
		}
		for j in 0 .. 16 {
			w.write_bit(if in_use[i * 16 + j] { 1 } else { 0 })
		}
	}
}

fn read_in_use_map(mut r BitReader) ![]bool {
	mut group_used := []bool{len: 16}
	for i in 0 .. 16 {
		group_used[i] = r.read_bit()! == 1
	}
	mut in_use := []bool{len: 256}
	for i in 0 .. 16 {
		if !group_used[i] {
			continue
		}
		for j in 0 .. 16 {
			in_use[i * 16 + j] = r.read_bit()! == 1
		}
	}
	return in_use
}

fn write_code_lengths(mut w BitWriter, lengths []int, alpha_size int) {
	mut curr := lengths[0]
	w.write_bits(5, u64(curr))
	for i in 0 .. alpha_size {
		target := lengths[i]
		for curr < target {
			w.write_bit(1)
			w.write_bit(0)
			curr++
		}
		for curr > target {
			w.write_bit(1)
			w.write_bit(1)
			curr--
		}
		w.write_bit(0)
	}
}

fn decode_selector_mtf(vals []int, n_groups int) ![]int {
	mut pos := []int{len: n_groups}
	for i in 0 .. n_groups {
		pos[i] = i
	}
	mut out := []int{len: vals.len}
	for i, v in vals {
		if v < 0 || v >= n_groups {
			return error('bzip2: selector mtf value out of range')
		}
		sel := pos[v]
		for j := v; j > 0; j-- {
			pos[j] = pos[j - 1]
		}
		pos[0] = sel
		out[i] = sel
	}
	return out
}

fn encode_selector_mtf(selectors []int, n_groups int) []int {
	mut pos := []int{len: n_groups}
	for i in 0 .. n_groups {
		pos[i] = i
	}
	mut out := []int{len: selectors.len}
	for i, sel in selectors {
		mut idx := 0
		for idx < pos.len && pos[idx] != sel {
			idx++
		}
		if idx >= pos.len {
			out[i] = 0
			continue
		}
		out[i] = idx
		for j := idx; j > 0; j-- {
			pos[j] = pos[j - 1]
		}
		pos[0] = sel
	}
	return out
}

fn make_huffman_lengths(freq []int, max_len int) []int {
	mut scaled := freq.clone()
	for {
		lens := build_huffman_lengths_unbounded(scaled)
		mut max_seen := 0
		for l in lens {
			if l > max_seen {
				max_seen = l
			}
		}
		if max_seen <= max_len {
			return lens
		}
		for i in 0 .. scaled.len {
			scaled[i] = (scaled[i] >> 1) + 1
		}
	}
	return []int{}
}

fn build_huffman_lengths_unbounded(freq []int) []int {
	n := freq.len
	if n == 0 {
		return []int{}
	}
	mut weight := []int{cap: 2 * n}
	mut parent := []int{cap: 2 * n}
	for i in 0 .. n {
		w := if freq[i] > 0 { freq[i] } else { 1 }
		weight << w
		parent << -1
	}
	mut active := []int{len: n}
	for i in 0 .. n {
		active[i] = i
	}
	for active.len > 1 {
		mut m1 := 0
		mut m2 := 1
		if weight[active[m2]] < weight[active[m1]] {
			t := m1
			m1 = m2
			m2 = t
		}
		for i in 2 .. active.len {
			idx := active[i]
			if weight[idx] < weight[active[m1]] {
				m2 = m1
				m1 = i
			} else if weight[idx] < weight[active[m2]] {
				m2 = i
			}
		}
		a := active[m1]
		b := active[m2]
		new_idx := weight.len
		weight << (weight[a] + weight[b])
		parent << -1
		parent[a] = new_idx
		parent[b] = new_idx
		if m1 > m2 {
			active.delete(m1)
			active.delete(m2)
		} else {
			active.delete(m2)
			active.delete(m1)
		}
		active << new_idx
	}
	mut lengths := []int{len: n}
	for i in 0 .. n {
		mut l := 0
		mut p := parent[i]
		for p != -1 {
			l++
			p = parent[p]
		}
		if l == 0 {
			l = 1
		}
		lengths[i] = l
	}
	return lengths
}

fn build_huffman_codes(lengths []int) []u32 {
	mut max_len := 0
	for l in lengths {
		if l > max_len {
			max_len = l
		}
	}
	mut bl_count := []int{len: max_len + 1}
	for l in lengths {
		if l > 0 {
			bl_count[l]++
		}
	}
	mut next_code := []u32{len: max_len + 1}
	mut code := u32(0)
	for bits in 1 .. max_len + 1 {
		code = (code + u32(bl_count[bits - 1])) << 1
		next_code[bits] = code
	}
	mut out := []u32{len: lengths.len}
	for i, l in lengths {
		if l == 0 {
			continue
		}
		out[i] = next_code[l]
		next_code[l]++
	}
	return out
}

struct DecodeTable {
	min_len int
	max_len int
	base    []int
	limit   []int
	perm    []int
}

fn build_decode_table(lengths []int, alpha_size int) !DecodeTable {
	mut min_len := 999
	mut max_len := 0
	for i in 0 .. alpha_size {
		l := lengths[i]
		if l < 1 || l > bzip2_max_code_len {
			return error('bzip2: invalid huffman code length')
		}
		if l < min_len {
			min_len = l
		}
		if l > max_len {
			max_len = l
		}
	}
	mut perm := []int{}
	for l in min_len .. max_len + 1 {
		for i in 0 .. alpha_size {
			if lengths[i] == l {
				perm << i
			}
		}
	}
	mut base := []int{len: max_len + 2}
	mut limit := []int{len: max_len + 1}
	for i in 0 .. alpha_size {
		base[lengths[i] + 1]++
	}
	for i in 1 .. base.len {
		base[i] += base[i - 1]
	}
	mut vec := 0
	for l in min_len .. max_len + 1 {
		vec += base[l + 1] - base[l]
		limit[l] = vec - 1
		vec <<= 1
	}
	for l in min_len + 1 .. max_len + 1 {
		base[l] = ((limit[l - 1] + 1) * 2) - base[l]
	}
	return DecodeTable{
		min_len: min_len
		max_len: max_len
		base:    base
		limit:   limit
		perm:    perm
	}
}

fn (t DecodeTable) decode(mut r BitReader) !int {
	mut zn := t.min_len
	mut zvec := int(r.read_bits(zn)!)
	for zn <= t.max_len && zvec > t.limit[zn] {
		zn++
		bit := int(r.read_bits(1)!)
		zvec = (zvec * 2) | bit
	}
	if zn > t.max_len {
		return error('bzip2: invalid huffman code')
	}
	idx := zvec - t.base[zn]
	if idx < 0 || idx >= t.perm.len {
		return error('bzip2: invalid huffman decode index')
	}
	return t.perm[idx]
}

fn mtf_rle2_encode(data []u8, unseq_to_seq []int, n_in_use int) ([]int, int) {
	mut mtf := []int{len: n_in_use}
	for i in 0 .. n_in_use {
		mtf[i] = i
	}
	eob := n_in_use + 1
	mut out := []int{}
	mut zpend := 0
	for b in data {
		seq := unseq_to_seq[int(b)]
		mut pos := 0
		for pos < mtf.len && mtf[pos] != seq {
			pos++
		}
		if pos == 0 {
			zpend++
			continue
		}
		if zpend > 0 {
			emit_run_a_b(mut out, zpend)
			zpend = 0
		}
		out << (pos + 1)
		move_to_front_int(mut mtf, pos)
	}
	if zpend > 0 {
		emit_run_a_b(mut out, zpend)
	}
	out << eob
	return out, n_in_use + 2
}

fn emit_run_a_b(mut out []int, run_len int) {
	mut z := run_len - 1
	for {
		if (z & 1) == 0 {
			out << bzip2_runa
		} else {
			out << bzip2_runb
		}
		if z < 2 {
			break
		}
		z = (z - 2) >> 1
	}
}

fn move_to_front_int(mut arr []int, pos int) {
	if pos <= 0 {
		return
	}
	tmp := arr[pos]
	for i := pos; i > 0; i-- {
		arr[i] = arr[i - 1]
	}
	arr[0] = tmp
}

fn rle1_encode(src []u8) []u8 {
	if src.len == 0 {
		return []u8{}
	}
	mut out := []u8{cap: src.len + (src.len / 4) + 8}
	mut i := 0
	for i < src.len {
		b := src[i]
		mut run := 1
		for i + run < src.len && src[i + run] == b && run < max_i32 {
			run++
		}
		mut rem := run
		for rem > 0 {
			if rem <= 3 {
				for _ in 0 .. rem {
					out << b
				}
				rem = 0
			} else {
				chunk := if rem > 259 { 259 } else { rem }
				for _ in 0 .. 4 {
					out << b
				}
				out << u8(chunk - 4)
				rem -= chunk
			}
		}
		i += run
	}
	return out
}

fn rle1_decode(src []u8) ![]u8 {
	mut out := []u8{cap: src.len}
	mut i := 0
	for i < src.len {
		if i + 4 < src.len && src[i] == src[i + 1] && src[i] == src[i + 2] && src[i] == src[i + 3] {
			run := int(src[i + 4]) + 4
			for _ in 0 .. run {
				out << src[i]
			}
			i += 5
		} else {
			out << src[i]
			i++
		}
	}
	return out
}

fn bwt_transform(data []u8) ([]u8, int) {
	n := data.len
	if n == 0 {
		return []u8{}, 0
	}
	mut sa := cyclic_suffix_array(data)
	mut out := []u8{len: n}
	mut orig_ptr := 0
	for i, s in sa {
		if s == 0 {
			orig_ptr = i
			out[i] = data[n - 1]
		} else {
			out[i] = data[s - 1]
		}
	}
	return out, orig_ptr
}

fn inverse_bwt(last_col []u8, orig_ptr int) ![]u8 {
	n := last_col.len
	if n == 0 {
		return []u8{}
	}
	if orig_ptr < 0 || orig_ptr >= n {
		return error('bzip2: invalid bwt origin pointer')
	}
	mut count := []int{len: 256}
	for b in last_col {
		count[int(b)]++
	}
	mut tots := []int{len: 256}
	mut sum := 0
	for i in 0 .. 256 {
		tots[i] = sum
		sum += count[i]
	}
	mut tt := []int{len: n}
	for i, b in last_col {
		idx := int(b)
		tt[tots[idx]] = i
		tots[idx]++
	}
	mut out := []u8{len: n}
	mut tpos := orig_ptr
	for i in 0 .. n {
		tpos = tt[tpos]
		out[i] = last_col[tpos]
	}
	return out
}

fn cyclic_suffix_array(data []u8) []int {
	n := data.len
	if n <= 1 {
		if n == 1 {
			return [0]
		}
		return []int{}
	}
	mut sa := []int{len: n}
	mut rank := []int{len: n}
	for i in 0 .. n {
		sa[i] = i
		rank[i] = int(data[i])
	}
	mut tmp_sa := []int{len: n}
	mut new_rank := []int{len: n}
	mut k := 1
	for k < n {
		radix_sort_cyclic(mut sa, mut tmp_sa, rank, k)
		new_rank[sa[0]] = 0
		mut classes := 1
		for i in 1 .. n {
			a := sa[i - 1]
			b := sa[i]
			if rank[a] != rank[b] || rank[(a + k) % n] != rank[(b + k) % n] {
				classes++
			}
			new_rank[b] = classes - 1
		}
		rank = new_rank.clone()
		if classes == n {
			break
		}
		k <<= 1
	}
	return sa
}

fn radix_sort_cyclic(mut sa []int, mut tmp []int, rank []int, k int) {
	n := sa.len
	m := if n > 256 { n } else { 256 }
	mut count := []int{len: m}
	for i in 0 .. n {
		key := rank[(sa[i] + k) % n]
		count[key]++
	}
	mut pos := []int{len: m}
	mut sum := 0
	for i in 0 .. m {
		pos[i] = sum
		sum += count[i]
	}
	for i in 0 .. n {
		v := sa[i]
		key := rank[(v + k) % n]
		tmp[pos[key]] = v
		pos[key]++
	}
	count = []int{len: m}
	for i in 0 .. n {
		key := rank[tmp[i]]
		count[key]++
	}
	sum = 0
	for i in 0 .. m {
		pos[i] = sum
		sum += count[i]
	}
	for i in 0 .. n {
		v := tmp[i]
		key := rank[v]
		sa[pos[key]] = v
		pos[key]++
	}
}

struct BitWriter {
mut:
	out       []u8
	bitbuf    u64
	bit_count int
}

fn (mut w BitWriter) write_byte(b u8) {
	w.out << b
}

fn (mut w BitWriter) write_bit(bit int) {
	w.write_bits(1, u64(bit & 1))
}

fn (mut w BitWriter) write_bits(n int, value u64) {
	if n <= 0 {
		return
	}
	for i := n - 1; i >= 0; i-- {
		b := (value >> u32(i)) & 1
		w.bitbuf = (w.bitbuf << 1) | b
		w.bit_count++
		if w.bit_count == 8 {
			w.out << u8(w.bitbuf & 0xff)
			w.bitbuf = 0
			w.bit_count = 0
		}
	}
}

fn (mut w BitWriter) finish() ![]u8 {
	if w.bit_count > 0 {
		w.bitbuf <<= u32(8 - w.bit_count)
		w.out << u8(w.bitbuf & 0xff)
		w.bitbuf = 0
		w.bit_count = 0
	}
	return w.out
}

struct BitReader {
	data []u8
mut:
	byte_pos int
	bit_pos  int
}

fn (mut r BitReader) read_byte() !u8 {
	if r.bit_pos != 0 {
		return error('bzip2: attempted byte read on non-byte boundary')
	}
	if r.byte_pos >= r.data.len {
		return error('bzip2: unexpected end of input')
	}
	b := r.data[r.byte_pos]
	r.byte_pos++
	return b
}

fn (mut r BitReader) read_bit() !int {
	return int(r.read_bits(1)!)
}

fn (mut r BitReader) read_bits(n int) !u64 {
	if n < 0 || n > 56 {
		return error('bzip2: invalid bit width')
	}
	mut out := u64(0)
	for _ in 0 .. n {
		if r.byte_pos >= r.data.len {
			return error('bzip2: unexpected end of input')
		}
		b := r.data[r.byte_pos]
		bit := (b >> u8(7 - r.bit_pos)) & u8(1)
		out = (out << 1) | u64(bit)
		r.bit_pos++
		if r.bit_pos == 8 {
			r.bit_pos = 0
			r.byte_pos++
		}
	}
	return out
}

fn (r &BitReader) is_aligned_to_byte_zero_padding() bool {
	if r.bit_pos == 0 {
		return r.byte_pos == r.data.len
	}
	if r.byte_pos >= r.data.len {
		return false
	}
	mask := u8((1 << u8(8 - r.bit_pos)) - 1)
	if (r.data[r.byte_pos] & mask) != 0 {
		return false
	}
	for i in r.byte_pos + 1 .. r.data.len {
		if r.data[i] != 0 {
			return false
		}
	}
	return true
}

fn rotate_left_1(v u32) u32 {
	return (v << 1) | (v >> 31)
}

fn bzip2_crc32(data []u8) u32 {
	mut crc := u32(0xffffffff)
	for b in data {
		table_idx := int(((crc >> 24) ^ u32(b)) & u32(0xff))
		crc = (crc << 8) ^ bzip2_crc32_table[table_idx]
	}
	return ~crc
}
