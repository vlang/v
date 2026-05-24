module deflate

const deflate_hash_bits = 15
const deflate_hash_size = 1 << deflate_hash_bits
const deflate_max_chain = 64
const deflate_min_match = 3
const deflate_max_match = 258
const deflate_window = 32768

// fixed_litlen_encode returns (reversed_codes, code_lengths) for fixed Huffman lit/len.
fn fixed_litlen_encode() ([]u32, []int) {
	lens := fixed_litlen_lengths()
	mut max_bits := 0
	for l in lens {
		if l > max_bits {
			max_bits = l
		}
	}
	mut bl_count := []int{len: max_bits + 1}
	for l in lens {
		if l > 0 {
			bl_count[l]++
		}
	}
	mut next_code := []u32{len: max_bits + 1}
	mut c := u32(0)
	for bits in 1 .. max_bits + 1 {
		c = (c + u32(bl_count[bits - 1])) << 1
		next_code[bits] = c
	}
	mut codes := []u32{len: 288}
	for sym in 0 .. 288 {
		l := lens[sym]
		if l == 0 {
			continue
		}
		codes[sym] = bit_reverse(next_code[l], l)
		next_code[l]++
	}
	return codes, lens
}

// fixed_dist_encode returns (reversed_codes, code_lengths) for fixed Huffman distance.
fn fixed_dist_encode() ([]u32, []int) {
	mut codes := []u32{len: 30}
	for i in 0 .. 30 {
		codes[i] = bit_reverse(u32(i), 5)
	}
	return codes, []int{len: 30, init: 5}
}

fn length_code_info(length int) (int, int, int) {
	for i := length_bases.len - 1; i >= 0; i-- {
		if length >= length_bases[i] {
			return i, length - length_bases[i], length_extra_bits[i]
		}
	}
	return 0, 0, 0
}

fn dist_code_info(distance int) (int, int, int) {
	for i := dist_bases.len - 1; i >= 0; i-- {
		if distance >= dist_bases[i] {
			return i, distance - dist_bases[i], dist_extra_bits[i]
		}
	}
	return 0, 0, 0
}

fn hash3(data []u8, pos int) int {
	v := u32(data[pos]) | (u32(data[pos + 1]) << 8) | (u32(data[pos + 2]) << 16)
	return int((v * u32(2654435761)) >> u32(32 - deflate_hash_bits))
}

@[direct_array_access]
fn find_lz_match(data []u8, pos int, last []int, prev []int) (int, int) {
	if pos + deflate_min_match > data.len {
		return 0, 0
	}
	max_len := if pos + deflate_max_match < data.len { deflate_max_match } else { data.len - pos }
	mut best_len := 0
	mut best_off := 0
	mut i := last[hash3(data, pos)]
	mut chain := 0
	for i >= 0 && chain < deflate_max_chain {
		off := pos - i
		if off > deflate_window {
			break
		}
		mut l := 0
		for l < max_len && data[i + l] == data[pos + l] {
			l++
		}
		if l > best_len {
			best_len = l
			best_off = off
			if best_len == max_len {
				break
			}
		}
		i = prev[i]
		chain++
	}
	return best_off, best_len
}

struct BitWriter {
mut:
	buf   []u8
	bits  u32
	nbits int
}

@[direct_array_access; inline]
fn (mut w BitWriter) write_bits(value u32, nbits int) {
	if nbits == 0 {
		return
	}
	w.bits |= value << w.nbits
	w.nbits += nbits
	for w.nbits >= 8 {
		w.buf << u8(w.bits & 0xff)
		w.bits >>= 8
		w.nbits -= 8
	}
}

fn (mut w BitWriter) flush() {
	if w.nbits > 0 {
		w.buf << u8(w.bits & 0xff)
		w.bits = 0
		w.nbits = 0
	}
}

// deflate_compress_fixed compresses data to RFC 1951 DEFLATE using fixed Huffman codes.
@[direct_array_access]
fn deflate_compress_fixed(data []u8) []u8 {
	ll_codes, ll_lens := fixed_litlen_encode()
	d_codes, d_lens := fixed_dist_encode()
	mut w := BitWriter{}
	// BFINAL=1, BTYPE=01 (fixed Huffman)
	w.write_bits(1, 1)
	w.write_bits(1, 2)
	if data.len == 0 {
		w.write_bits(ll_codes[256], ll_lens[256])
		w.flush()
		return w.buf
	}
	mut last := []int{len: deflate_hash_size, init: -1}
	mut prev := []int{len: data.len, init: -1}
	mut pos := 0
	for pos < data.len {
		off, match_len := find_lz_match(data, pos, last, prev)
		if match_len >= deflate_min_match {
			li, lext, lext_bits := length_code_info(match_len)
			sym := 257 + li
			w.write_bits(ll_codes[sym], ll_lens[sym])
			w.write_bits(u32(lext), lext_bits)
			di, dext, dext_bits := dist_code_info(off)
			w.write_bits(d_codes[di], d_lens[di])
			w.write_bits(u32(dext), dext_bits)
			for i in pos .. pos + match_len {
				if i + deflate_min_match < data.len {
					h := hash3(data, i)
					prev[i] = last[h]
					last[h] = i
				}
			}
			pos += match_len
		} else {
			w.write_bits(ll_codes[int(data[pos])], ll_lens[int(data[pos])])
			if pos + deflate_min_match < data.len {
				h := hash3(data, pos)
				prev[pos] = last[h]
				last[h] = pos
			}
			pos++
		}
	}
	w.write_bits(ll_codes[256], ll_lens[256])
	w.flush()
	return w.buf
}
