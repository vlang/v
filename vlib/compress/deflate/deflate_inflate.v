module deflate

import hash.huffman

// vfmt off
// RFC 1951 length/distance decode tables
const length_bases = [3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59,
	67, 83, 99, 115, 131, 163, 195, 227, 258]
const length_extra_bits = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4,
	4, 5, 5, 5, 5, 0]
const dist_bases = [1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769,
	1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577]
const dist_extra_bits = [0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10,
	10, 11, 11, 12, 12, 13, 13]
// code-length alphabet order (RFC 1951)
const cl_order = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]
// vmt on

// fixed_litlen_lengths returns code lengths for the fixed Huffman lit/len tree (RFC 1951 §3.2.6).
fn fixed_litlen_lengths() []int {
	mut lens := []int{len: 288}
	for i in 0 .. 144 {
		lens[i] = 8
	}
	for i in 144 .. 256 {
		lens[i] = 9
	}
	for i in 256 .. 280 {
		lens[i] = 7
	}
	for i in 280 .. 288 {
		lens[i] = 8
	}
	return lens
}

fn fixed_dist_lengths() []int {
	return []int{len: 32, init: 5}
}

// HuffTree is a MSB-first Huffman lookup table for DEFLATE decoding.
// Indexed by the next max_bits bits read LSB-first from the stream.
struct HuffTree {
	table    []u32 // entry: (symbol << 5) | code_length; 0xFFFF_FFFF = invalid
	max_bits int
}

// max_code_bits is the longest code a DEFLATE stream can describe (RFC 1951 §3.2.7).
const max_code_bits = 15

// CodeSet names the code that a set of code lengths describes. The three differ in
// which incomplete sets build_huff_tree() accepts.
enum CodeSet {
	code_length // the code of the code length alphabet, in a dynamic block header
	litlen      // the literal/length code
	distance    // the distance code
}

// build_huff_tree builds the decode table for a set of code lengths (each 0..15).
// Like zlib, it rejects a set that does not use the whole code space, with two
// exceptions: a literal/length or distance set with a single code of one bit, and
// a distance set with no code at all (a block of literals only, RFC 1951 §3.2.7).
fn build_huff_tree(lengths []int, set CodeSet) !HuffTree {
	mut max_bits := 0
	mut used := 0 // code space the lengths take, in units of 2^-max_code_bits
	for l in lengths {
		if l > max_bits {
			max_bits = l
		}
		if l > 0 {
			used += 1 << (max_code_bits - l)
		}
	}
	if used < (1 << max_code_bits) {
		single_code := max_bits == 1 && set != .code_length
		no_code := max_bits == 0 && set == .distance
		if !single_code && !no_code {
			name := match set {
				.code_length { 'code length' }
				.litlen { 'literal/length' }
				.distance { 'distance' }
			}
			return error('inflate: incomplete ${name} code')
		}
	}
	if max_bits == 0 {
		// No distance code: the only entry is invalid, so that a length symbol,
		// which needs a distance, is an error and not a code of zero bits.
		return HuffTree{
			table:    [huffman.flat_invalid_entry]
			max_bits: 0
		}
	}
	// Build the flat LSB-first lookup table the decode loop expects directly
	// from the lengths via the shared canonical builder: entry = (symbol << 5) |
	// length, 0xFFFF_FFFF for invalid. flat_table() allocates no intermediate
	// codes array, so this matches the hand-rolled loop's cost. Over-subscribed
	// lengths now surface as an error instead of a silently corrupt table.
	table := huffman.flat_table(lengths: lengths, max_bits: max_bits, bit_order: .lsb_first)!
	return HuffTree{
		table:    table
		max_bits: max_bits
	}
}

const fixed_litlen_lens = fixed_litlen_lengths()
const fixed_dist_lens = fixed_dist_lengths()
const fixed_ll_tree = build_fixed_huff_tree(fixed_litlen_lens, .litlen)
const fixed_d_tree = build_fixed_huff_tree(fixed_dist_lens, .distance)

fn build_fixed_huff_tree(lengths []int, set CodeSet) HuffTree {
	return build_huff_tree(lengths, set) or {
		panic('deflate: failed to build fixed ${set} Huffman tree: ${err}')
	}
}

fn (mut t HuffTree) free() {
	unsafe { t.table.free() }
}

struct BitReader {
	buf []u8
mut:
	pos   int
	bits  u32
	nbits int
}

@[direct_array_access; inline]
fn (mut r BitReader) read_bits(n int) !u32 {
	for r.nbits < n {
		if r.pos >= r.buf.len {
			return error('inflate: unexpected end of stream')
		}
		r.bits |= u32(r.buf[r.pos]) << r.nbits
		r.pos++
		r.nbits += 8
	}
	val := r.bits & ((u32(1) << n) - 1)
	r.bits >>= u32(n)
	r.nbits -= n
	return val
}

@[inline]
fn (mut r BitReader) align_byte() {
	r.bits = 0
	r.nbits = 0
}

@[direct_array_access; inline]
fn (mut r BitReader) read_byte_raw() !u8 {
	if r.pos >= r.buf.len {
		return error('inflate: unexpected end of stream')
	}
	b := r.buf[r.pos]
	r.pos++
	return b
}

@[direct_array_access; inline]
fn (mut r BitReader) huff_decode(t HuffTree) !u32 {
	for r.nbits < t.max_bits {
		if r.pos >= r.buf.len {
			break
		}
		r.bits |= u32(r.buf[r.pos]) << r.nbits
		r.pos++
		r.nbits += 8
	}
	idx := int(r.bits & ((u32(1) << t.max_bits) - 1))
	entry := t.table[idx]
	len_ := int(entry & 0x1f)
	// A symbol always takes at least one bit: every loop that decodes symbols
	// relies on that to end, whatever table it is given.
	if entry == 0xffff_ffff || len_ == 0 {
		return error('inflate: invalid Huffman code')
	}
	if r.nbits < len_ {
		return error('inflate: unexpected end of stream')
	}
	sym := entry >> 5
	r.bits >>= u32(len_)
	r.nbits -= len_
	return sym
}

struct InflateResult {
	decoded  []u8
	consumed int
}

struct InflateStreamResult {
	decoded   []u8
	consumed  int
	delivered int
	aborted   bool
}

struct InflateStreamState {
mut:
	delivered int
}

const inflate_callback_chunk_size = 32768

@[direct_array_access; inline]
fn flush_stream_chunks(out []u8, cb ChunkCallback, userdata voidptr, mut state InflateStreamState) bool {
	for out.len - state.delivered >= inflate_callback_chunk_size {
		end := state.delivered + inflate_callback_chunk_size
		chunk := out[state.delivered..end]
		if cb(chunk, userdata) != chunk.len {
			return false
		}
		state.delivered = end
	}
	return true
}

// inflate_with_consumed decompresses raw RFC 1951 DEFLATE data and reports
// how many input bytes were consumed by the DEFLATE bitstream.
fn inflate_with_consumed(data []u8) !InflateResult {
	mut r := BitReader{
		buf: data
	}
	mut out := []u8{}
	for {
		bfinal := r.read_bits(1)!
		btype := r.read_bits(2)!
		match btype {
			0 {
				r.align_byte()
				len_ := int(r.read_byte_raw()!) | (int(u32(r.read_byte_raw()!) << 8))
				nlen := int(r.read_byte_raw()!) | (int(u32(r.read_byte_raw()!) << 8))
				if len_ & 0xffff != (~nlen) & 0xffff {
					return error('inflate: bad stored block length')
				}
				for _ in 0 .. len_ {
					out << r.read_byte_raw()!
				}
			}
			1 {
				inflate_block(mut r, mut out, fixed_ll_tree, fixed_d_tree)!
			}
			2 {
				inflate_dynamic_block(mut r, mut out)!
			}
			else {
				return error('inflate: reserved block type')
			}
		}

		if bfinal == 1 {
			break
		}
	}
	consumed := r.pos - (r.nbits >> 3)
	return InflateResult{
		decoded:  out
		consumed: consumed
	}
}

// inflate decompresses raw RFC 1951 DEFLATE data.
fn inflate(data []u8) ![]u8 {
	res := inflate_with_consumed(data)!
	return res.decoded
}

// inflate_with_callback decompresses raw RFC 1951 DEFLATE data and streams output through `cb`.
// It still tracks consumed input bytes for container-level validation.
fn inflate_with_callback(data []u8, cb ChunkCallback, userdata voidptr) !InflateStreamResult {
	mut r := BitReader{
		buf: data
	}
	mut out := []u8{}
	mut state := InflateStreamState{}
	mut aborted := false
	for {
		bfinal := r.read_bits(1)!
		btype := r.read_bits(2)!
		match btype {
			0 {
				r.align_byte()
				len_ := int(r.read_byte_raw()!) | (int(u32(r.read_byte_raw()!) << 8))
				nlen := int(r.read_byte_raw()!) | (int(u32(r.read_byte_raw()!) << 8))
				if len_ & 0xffff != (~nlen) & 0xffff {
					return error('inflate: bad stored block length')
				}
				for _ in 0 .. len_ {
					out << r.read_byte_raw()!
					if !flush_stream_chunks(out, cb, userdata, mut state) {
						aborted = true
						break
					}
				}
			}
			1 {
				if !inflate_block_stream(mut r, mut out, fixed_ll_tree, fixed_d_tree, cb, userdata, mut
					state)! {
					aborted = true
				}
			}
			2 {
				if !inflate_dynamic_block_stream(mut r, mut out, cb, userdata, mut state)! {
					aborted = true
				}
			}
			else {
				return error('inflate: reserved block type')
			}
		}

		if aborted || bfinal == 1 {
			break
		}
	}
	if !aborted && out.len > state.delivered {
		chunk := unsafe { out[state.delivered..] }
		if cb(chunk, userdata) != chunk.len {
			aborted = true
		} else {
			state.delivered = out.len
		}
	}
	consumed := r.pos - (r.nbits >> 3)
	return InflateStreamResult{
		decoded:   out
		consumed:  consumed
		delivered: state.delivered
		aborted:   aborted
	}
}

@[direct_array_access]
fn inflate_dynamic_block(mut r BitReader, mut out []u8) ! {
	hlit := int(r.read_bits(5)!) + 257
	hdist := int(r.read_bits(5)!) + 1
	hclen := int(r.read_bits(4)!) + 4
	mut cl_lens := []int{len: 19}
	defer {
		unsafe { cl_lens.free() }
	}
	for i in 0 .. hclen {
		cl_lens[cl_order[i]] = int(r.read_bits(3)!)
	}
	mut cl_tree := build_huff_tree(cl_lens, .code_length)!
	defer {
		cl_tree.free()
	}
	mut all_lens := read_dynamic_lengths(mut r, cl_tree, hlit, hdist)!
	defer {
		unsafe { all_lens.free() }
	}
	mut ll_tree := build_huff_tree(all_lens[..hlit], .litlen)!
	defer {
		ll_tree.free()
	}
	mut d_tree := build_huff_tree(all_lens[hlit..], .distance)!
	defer {
		d_tree.free()
	}
	inflate_block(mut r, mut out, ll_tree, d_tree)!
}

@[direct_array_access]
fn inflate_dynamic_block_stream(mut r BitReader, mut out []u8, cb ChunkCallback, userdata voidptr, mut state InflateStreamState) !bool {
	hlit := int(r.read_bits(5)!) + 257
	hdist := int(r.read_bits(5)!) + 1
	hclen := int(r.read_bits(4)!) + 4
	mut cl_lens := []int{len: 19}
	defer {
		unsafe { cl_lens.free() }
	}
	for i in 0 .. hclen {
		cl_lens[cl_order[i]] = int(r.read_bits(3)!)
	}
	mut cl_tree := build_huff_tree(cl_lens, .code_length)!
	defer {
		cl_tree.free()
	}
	mut all_lens := read_dynamic_lengths(mut r, cl_tree, hlit, hdist)!
	defer {
		unsafe { all_lens.free() }
	}
	mut ll_tree := build_huff_tree(all_lens[..hlit], .litlen)!
	defer {
		ll_tree.free()
	}
	mut d_tree := build_huff_tree(all_lens[hlit..], .distance)!
	defer {
		d_tree.free()
	}
	return inflate_block_stream(mut r, mut out, ll_tree, d_tree, cb, userdata, mut state)!
}

// read_dynamic_lengths reads the hlit literal/length and hdist distance code lengths
// of a dynamic block header, and checks what does not depend on the codes they form.
fn read_dynamic_lengths(mut r BitReader, cl_tree HuffTree, hlit int, hdist int) ![]int {
	// The alphabets have 286 and 30 symbols; the 5-bit counts can say 288 and 32.
	if hlit > 286 || hdist > 30 {
		return error('inflate: too many literal/length or distance codes')
	}
	mut all_lens := []int{cap: hlit + hdist}
	mut keep_all_lens := false
	defer {
		if !keep_all_lens {
			unsafe { all_lens.free() }
		}
	}
	for all_lens.len < hlit + hdist {
		sym := r.huff_decode(cl_tree)!
		if sym <= 15 {
			all_lens << int(sym)
		} else if sym == 16 {
			if all_lens.len == 0 {
				return error('inflate: repeat with empty history')
			}
			rep := int(r.read_bits(2)!) + 3
			last := all_lens[all_lens.len - 1]
			for _ in 0 .. rep {
				all_lens << last
			}
		} else if sym == 17 {
			rep := int(r.read_bits(3)!) + 3
			for _ in 0 .. rep {
				all_lens << 0
			}
		} else if sym == 18 {
			rep := int(r.read_bits(7)!) + 11
			for _ in 0 .. rep {
				all_lens << 0
			}
		} else {
			return error('inflate: bad code length symbol')
		}
	}
	// A repeat must end at the last length: what runs past it would otherwise
	// become extra distance code lengths.
	if all_lens.len > hlit + hdist {
		return error('inflate: code length repeat past the last length')
	}
	// Without a code for the end-of-block symbol the block could never end.
	if all_lens[256] == 0 {
		return error('inflate: missing end-of-block code')
	}
	keep_all_lens = true
	return all_lens
}

@[direct_array_access]
fn inflate_block(mut r BitReader, mut out []u8, ll HuffTree, dist HuffTree) ! {
	for {
		sym := r.huff_decode(ll)!
		if sym == 256 {
			break
		}
		if sym < 256 {
			out << u8(sym)
		} else {
			li := int(sym) - 257
			if li < 0 || li >= length_bases.len {
				return error('inflate: invalid length symbol ${sym}')
			}
			length := length_bases[li] + int(r.read_bits(length_extra_bits[li])!)
			dsym := r.huff_decode(dist)!
			di := int(dsym)
			if di >= dist_bases.len {
				return error('inflate: invalid distance symbol ${dsym}')
			}
			distance := dist_bases[di] + int(r.read_bits(dist_extra_bits[di])!)
			if distance > out.len {
				return error('inflate: distance past output start')
			}
			base := out.len - distance
			for i in 0 .. length {
				out << out[base + i]
			}
		}
	}
}

@[direct_array_access]
fn inflate_block_stream(mut r BitReader, mut out []u8, ll HuffTree, dist HuffTree, cb ChunkCallback, userdata voidptr, mut state InflateStreamState) !bool {
	for {
		sym := r.huff_decode(ll)!
		if sym == 256 {
			break
		}
		if sym < 256 {
			out << u8(sym)
			if !flush_stream_chunks(out, cb, userdata, mut state) {
				return false
			}
		} else {
			li := int(sym) - 257
			if li < 0 || li >= length_bases.len {
				return error('inflate: invalid length symbol ${sym}')
			}
			length := length_bases[li] + int(r.read_bits(length_extra_bits[li])!)
			dsym := r.huff_decode(dist)!
			di := int(dsym)
			if di >= dist_bases.len {
				return error('inflate: invalid distance symbol ${dsym}')
			}
			distance := dist_bases[di] + int(r.read_bits(dist_extra_bits[di])!)
			if distance > out.len {
				return error('inflate: distance past output start')
			}
			base := out.len - distance
			for i in 0 .. length {
				out << out[base + i]
				if !flush_stream_chunks(out, cb, userdata, mut state) {
					return false
				}
			}
		}
	}
	return true
}
