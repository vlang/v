// Copyright (c) 2024 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
module chacha20

import math.bits
import encoding.binary

// max_64bit_counter is a 64-bit maximum internal counter of original ChaCha20 variant.
const max_64bit_counter = max_u64
// max_32bit_counter is a 32-bit maximum internal counter of standard IETF ChaCha20 variant.
const max_32bit_counter = u64(max_u32)

// default chacha20 quarter round number
const default_qround_nr = 10

// ChaCha20 stream with internal counter
@[noinit]
struct Stream {
mut:
	// The mode (variant) of this ChaCha20 stream
	// Standard IETF variant or original (from DJ Bernstein) variant, set on creation.
	mode CipherMode = .standard
	// Flag that tells whether this stream was an extended XChaCha20 standard variant.
	// only make sense when mode == .standard
	extended bool
	// underlying stream's key
	key [8]u32
	// underlying stream's nonce with internal counter
	nonce [4]u32

	// counter-independent precomputed values
	precomp bool
	// vfmt off
	p1  u32 p5  u32 p9  u32 p13 u32
	p2  u32 p6  u32 p10 u32 p14 u32
	p3  u32 p7  u32 p11 u32 p15 u32
	// vfmt on
}

// new_stream creates a new chacha20 stream. The supported nonce size is 8, 12 or 24 bytes.
@[direct_array_access; inline]
fn new_stream(key []u8, nonce []u8) !Stream {
	if key.len != key_size {
		return error('Bad key size provided')
	}
	// setup for default value
	mut mode := CipherMode.standard
	mut extended := false

	// Based on the nonce.len supplied, it determines the variant (mode) and extended form of
	// the new chacha20 stream intended to create.
	match nonce.len {
		nonce_size {}
		x_nonce_size {
			extended = true
		}
		orig_nonce_size {
			mode = .original
		}
		else {
			return error('new_stream: unsupported nonce size')
		}
	}
	// if this an extended chacha20 construct, derives a new key and nonce
	new_key, new_nonce := if extended {
		xkey, xnonce := derive_xchacha20_key_nonce(key, nonce)!
		xkey, xnonce
	} else {
		// otherwise, use provided key and nonce
		key, nonce
	}
	// Build a new stream and setup the key
	mut b := Stream{
		mode:     mode
		extended: extended
	}
	// store the key
	b.key[0] = binary.little_endian_u32(new_key[0..4])
	b.key[1] = binary.little_endian_u32(new_key[4..8])
	b.key[2] = binary.little_endian_u32(new_key[8..12])
	b.key[3] = binary.little_endian_u32(new_key[12..16])
	b.key[4] = binary.little_endian_u32(new_key[16..20])
	b.key[5] = binary.little_endian_u32(new_key[20..24])
	b.key[6] = binary.little_endian_u32(new_key[24..28])
	b.key[7] = binary.little_endian_u32(new_key[28..32])

	// store the nonce
	if b.mode == .standard {
		// in standard IETF variant, first nonce was used as internal counter
		b.nonce[0] = 0
		b.nonce[1] = binary.little_endian_u32(new_nonce[0..4])
		b.nonce[2] = binary.little_endian_u32(new_nonce[4..8])
		b.nonce[3] = binary.little_endian_u32(new_nonce[8..12])
	} else {
		// in the original variant, two's of first counter servers as 64-bit counter value
		b.nonce[0] = 0
		b.nonce[1] = 0

		b.nonce[2] = binary.little_endian_u32(new_nonce[0..4])
		b.nonce[3] = binary.little_endian_u32(new_nonce[4..8])
	}
	return b
}

// reset resets internal stream
@[unsafe]
fn (mut s Stream) reset() {
	s.mode = .standard
	s.extended = false
	unsafe {
		_ := vmemset(&s.key, 0, 32)
		_ := vmemset(&s.nonce, 0, 16)
	}
	s.precomp = false
	s.p1, s.p5, s.p9, s.p13 = u32(0), u32(0), u32(0), u32(0)
	s.p2, s.p6, s.p10, s.p14 = u32(0), u32(0), u32(0), u32(0)
	s.p3, s.p7, s.p11, s.p15 = u32(0), u32(0), u32(0), u32(0)
}

// new_curr_state creates a new State from current stream
@[direct_array_access]
fn (s Stream) new_curr_state() State {
	// initializes ChaCha20 state
	//      0:cccccccc   1:cccccccc   2:cccccccc   3:cccccccc
	//      4:kkkkkkkk   5:kkkkkkkk   6:kkkkkkkk   7:kkkkkkkk
	//      8:kkkkkkkk   9:kkkkkkkk  10:kkkkkkkk  11:kkkkkkkk
	//     12:bbbbbbbb  13:nnnnnnnn  14:nnnnnnnn  15:nnnnnnnn
	//
	// where c=constant k=key b=blockcounter n=nonce
	mut state := State{}
	// load chacha20 constant into state
	state[0] = cc0
	state[1] = cc1
	state[2] = cc2
	state[3] = cc3
	// load key into state
	for i, k in s.key {
		state[i + 4] = k
	}
	// load nonce into state
	for j, v in s.nonce {
		state[j + 12] = v
	}
	return state
}

// keystream_full process with full size of src being processed
@[direct_array_access]
fn (mut s Stream) keystream_full(mut dst []u8, src []u8) {
	// number of block to be processed
	nr_blocks := src.len / block_size
	// check for counter overflow
	if s.check_ctr(u64(nr_blocks)) {
		panic('chacha20: internal counter overflow')
	}
	// process for full block_size-d msg
	for i := 0; i < nr_blocks; i++ {
		block := unsafe { src[i * block_size..(i + 1) * block_size] }
		// process with block_size keystream
		s.keystream_with_blocksize(mut dst[i * block_size..(i + 1) * block_size], block)
	}

	// process for remaining partial block
	if src.len % block_size != 0 {
		last_block := unsafe { src[nr_blocks * block_size..] }
		// pad to align with block_size
		mut last_bytes := []u8{len: block_size}
		_ := copy(mut last_bytes, last_block)

		// process the padded last block
		s.keystream_with_blocksize(mut last_bytes, last_bytes)
		_ := copy(mut dst[nr_blocks * block_size..], last_bytes)
	}
}

// keystream_with_blocksize produces stream from src bytes that aligns with block_size,
// serialized in little-endian form and stored into dst buffer.
@[direct_array_access]
fn (mut s Stream) keystream_with_blocksize(mut dst []u8, src []u8) {
	// ChaCha20 keystream generator was relatively easy to understand.
	// Its contains steps:
	// - loads current ChaCha20 into temporary state, used for later.
	// - performs quarter_round function on this state and returns some new state.
	// - adding back the new state with the old state.
	// - performs xor-ing between src bytes (loaded as little endian number) with result from previous step.
	// - serializes, in little endian form, this xor-ed state into destination buffer.
	//
	// Makes sure its works for size of multiple of block_size
	if dst.len != src.len || dst.len % block_size != 0 {
		panic('chacha20: internal error: wrong dst and/or src length')
	}

	// load state from current stream
	st := s.new_curr_state()
	// clone the state
	mut st_c := clone_state(st)

	// cache counter-independent precomputed values
	if s.mode == .standard {
		// first column round
		// precomputes three first column rounds that do not depend on counter
		if !s.precomp {
			mut pcr1 := Quartet{st[1], st[5], st[9], st[13]}
			mut pcr2 := Quartet{st[2], st[6], st[10], st[14]}
			mut pcr3 := Quartet{st[3], st[7], st[11], st[15]}

			qround_on_quartet(mut pcr1)
			qround_on_quartet(mut pcr2)
			qround_on_quartet(mut pcr3)

			s.p1 = pcr1.e0
			s.p5 = pcr1.e1
			s.p9 = pcr1.e2
			s.p13 = pcr1.e3

			s.p2 = pcr2.e0
			s.p6 = pcr2.e1
			s.p10 = pcr2.e2
			s.p14 = pcr2.e3

			s.p3 = pcr3.e0
			s.p7 = pcr3.e1
			s.p11 = pcr3.e2
			s.p15 = pcr3.e3

			s.precomp = true
		}
	}

	mut idx := 0
	mut src_len := src.len
	for src_len >= block_size {
		if s.mode == .standard {
			// remaining first column round
			mut fcr := Quartet{st[0], st[4], st[8], st[12]}
			qround_on_quartet(mut fcr)

			// First diagonal round.
			qround_on_state_with_quartet(mut st_c, fcr.e0, s.p5, s.p10, s.p15, 0, 5, 10,
				15)
			qround_on_state_with_quartet(mut st_c, s.p1, s.p6, s.p11, fcr.e3, 1, 6, 11,
				12)
			qround_on_state_with_quartet(mut st_c, s.p2, s.p7, fcr.e2, s.p13, 2, 7, 8,
				13)
			qround_on_state_with_quartet(mut st_c, s.p3, fcr.e1, s.p9, s.p14, 3, 4, 9,
				14)
		}
		// For standard variant, the first column-round was already precomputed,
		// For original variant, its use full quarter round number.
		n := if s.mode == .standard { 9 } else { default_qround_nr }
		// perform chacha20 quarter round n-times
		for i := 0; i < n; i++ {
			// Column-round
			//  0 |  1 |  2 |  3
			//  4 |  5 |  6 |  7
			//  8 |  9 | 10 | 11
			// 12 | 13 | 14 | 15
			qround_on_state(mut st_c, 0, 4, 8, 12) // 0
			qround_on_state(mut st_c, 1, 5, 9, 13) // 1
			qround_on_state(mut st_c, 2, 6, 10, 14) // 2
			qround_on_state(mut st_c, 3, 7, 11, 15) // 3

			// Diagonal round.
			//   0 \  1 \  2 \  3
			//   5 \  6 \  7 \  4
			//  10 \ 11 \  8 \  9
			//  15 \ 12 \ 13 \ 14
			qround_on_state(mut st_c, 0, 5, 10, 15)
			qround_on_state(mut st_c, 1, 6, 11, 12)
			qround_on_state(mut st_c, 2, 7, 8, 13)
			qround_on_state(mut st_c, 3, 4, 9, 14)
		}

		// add back keystream result to initial state, xor-ing with the src and stores into dst
		for i := 0; i < 16; i++ {
			src_block := unsafe { src[idx + (i * 4)..idx + (i + 1) * 4] }
			binary.little_endian_put_u32(mut dst[idx + (i * 4)..idx + (i + 1) * 4], binary.little_endian_u32(src_block) ^ (
				st_c[i] + st[i]))
		}

		// increases Stream's internal counter
		s.inc_ctr()

		// updates index
		idx += block_size
		src_len -= block_size
	}
}

// Handling of Stream's internal counter
//

// ctr returns a current Stream's counter as u64 value.
@[direct_array_access; inline]
fn (b Stream) ctr() u64 {
	match b.mode {
		// In the original mode, counter was 64-bit size
		// stored on b.nonce[0], and b.nonce[1]
		.original {
			return u64(b.nonce[1]) << 32 | u64(b.nonce[0])
		}
		.standard {
			// in standard mode, counter was 32-bit value, stored on b.nonce[0]
			return u64(b.nonce[0])
		}
	}
}

// set_ctr sets Stream's counter
@[direct_array_access; inline]
fn (mut b Stream) set_ctr(ctr u64) {
	match b.mode {
		.original {
			b.nonce[0] = u32(ctr)
			b.nonce[1] = u32(ctr >> 32)
		}
		.standard {
			// check for ctr value that may exceed the counter limit
			if ctr > max_32bit_counter {
				panic('set_ctr: counter value exceed the limit ')
			}
			b.nonce[0] = u32(ctr)
		}
	}
}

// inc_ctr increases internal counter by one.
@[inline]
fn (mut b Stream) inc_ctr() {
	mut curr_ctr := b.ctr()
	curr_ctr += 1

	b.set_ctr(curr_ctr)
}

// check_ctr checks for counter overflow when added by value.
// It returns true on counter overflow.
@[inline]
fn (b Stream) check_ctr(value u64) bool {
	ctr := b.ctr()
	sum := ctr + value
	max := b.max_ctr()
	if sum < ctr || sum < value || sum > max {
		return true
	}
	return false
}

// max_ctr returns maximum counter value of this stream variant
@[inline]
fn (b Stream) max_ctr() u64 {
	match b.mode {
		.original { return max_64bit_counter }
		.standard { return max_32bit_counter }
	}
}

// State represents the running 64-bytes of chacha20 stream,
type State = [16]u32

@[direct_array_access; inline; unsafe]
fn reset_state(mut s State) {
	unsafe {
		_ := vmemset(&s, 0, 64)
	}
}

@[direct_array_access; inline]
fn clone_state(s State) State {
	mut sc := State{}
	for i, v in s {
		sc[i] = v
	}
	return sc
}

// qround_on_state_with_quartet run qround_on_state by previously set up state values in offset
// (a,b,c,d) with values from quartet (q0, q1, q2, q3)
@[direct_array_access]
fn qround_on_state_with_quartet(mut s State, q0 u32, q1 u32, q2 u32, q3 u32, a int, b int, c int, d int) {
	s[a] = q0
	s[b] = q1
	s[c] = q2
	s[d] = q3
	qround_on_state(mut s, a, b, c, d)
}

// qround_on_state performs chacha20 quarter round on states with quartet index a, b, c, d.
@[direct_array_access]
fn qround_on_state(mut s State, a int, b int, c int, d int) {
	// a += b;  d ^= a;  d <<<= 16;
	s[a] += s[b]
	s[d] ^= s[a]
	s[d] = bits.rotate_left_32(s[d], 16)

	// c += d;  b ^= c;  b <<<= 12;
	s[c] += s[d]
	s[b] ^= s[c]
	s[b] = bits.rotate_left_32(s[b], 12)

	// a += b;  d ^= a;  d <<<=  8;
	s[a] += s[b]
	s[d] ^= s[a]
	s[d] = bits.rotate_left_32(s[d], 8)

	// c += d;  b ^= c;  b <<<=  7;
	s[c] += s[d]
	s[b] ^= s[c]
	s[b] = bits.rotate_left_32(s[b], 7)
}

// quartet of u32 values.
struct Quartet {
mut:
	e0 u32
	e1 u32
	e2 u32
	e3 u32
}

// qround_on_quartet runs chacha20 quarter round run on Quartet q.
fn qround_on_quartet(mut q Quartet) {
	// a += b;  d ^= a;  d <<<= 16;
	q.e0 += q.e1
	q.e3 ^= q.e0
	q.e3 = bits.rotate_left_32(q.e3, 16)

	// c += d;  b ^= c;  b <<<= 12;
	q.e2 += q.e3
	q.e1 ^= q.e2
	q.e1 = bits.rotate_left_32(q.e1, 12)

	// a += b;  d ^= a;  d <<<  8;
	q.e0 += q.e1
	q.e3 ^= q.e0
	q.e3 = bits.rotate_left_32(q.e3, 8)

	// c += d;  b ^= c;  b <<<=  7;
	q.e2 += q.e3
	q.e1 ^= q.e2
	q.e1 = bits.rotate_left_32(q.e1, 7)
}
