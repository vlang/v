// Copyright (c) 2023 Kim Shrier. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// streaming shake-128/256 xof per FIPS 202
// https://nvlpubs.nist.gov/nistpubs/fips/nist.fips.202.pdf

module sha3

@[noinit]
pub struct Shake {
	rate int // bytes per permutation (168 for shake-128, 136 for shake-256)
mut:
	s State
	// buf holds the partial input block while absorbing and the current output block while
	// squeezing. It is a fixed array, so a copy of a Shake does not share it with the original.
	buf       [200]u8
	pos       int // bytes of input in buf, or bytes of buf already read
	finalized bool
}

// new_shake128 returns a new Shake instance for SHAKE-128 extended output function.
pub fn new_shake128() &Shake {
	return &Shake{
		rate: xof_rate_128
	}
}

// new_shake256 returns a new Shake instance for SHAKE-256 extended output function.
pub fn new_shake256() &Shake {
	return &Shake{
		rate: xof_rate_256
	}
}

// write absorbs more data into the sponge state.
// Panics if called after `read`.
@[direct_array_access]
pub fn (mut s Shake) write(data []u8) {
	if s.finalized {
		panic('sha3: write after read on Shake')
	}
	mut off := 0
	if s.pos > 0 {
		take := int_min(s.rate - s.pos, data.len)
		for i in 0 .. take {
			s.buf[s.pos + i] = data[i]
		}
		s.pos += take
		off = take
		if s.pos < s.rate {
			return
		}
		s.s.xor_block(mut s.buf, s.rate)
		s.s.keccak_p_1600_24()
		s.pos = 0
	}
	for data.len - off >= s.rate {
		s.s.xor_bytes(data[off..off + s.rate], s.rate)
		s.s.keccak_p_1600_24()
		off += s.rate
	}
	for i in off .. data.len {
		s.buf[i - off] = data[i]
	}
	s.pos = data.len - off
}

@[direct_array_access]
fn (mut s Shake) finalize() {
	if s.finalized {
		return
	}
	s.finalized = true

	// pad10*1 with xof domain separator 0x1f (FIPS 202 sec B.2)
	for i in s.pos .. s.rate {
		s.buf[i] = 0
	}
	s.buf[s.pos] ^= 0x1f
	s.buf[s.rate - 1] ^= 0x80
	s.s.xor_block(mut s.buf, s.rate)
	s.s.keccak_p_1600_24()
	s.load_block()
}

// load_block copies the next `rate` bytes of output from the state into buf.
fn (mut s Shake) load_block() {
	s.s.store_block(mut s.buf, s.rate)
	s.pos = 0
}

// read squeezes `out_len` bytes from the sponge state.
// Finalizes the sponge on first call; further calls to `write` will panic.
@[direct_array_access]
pub fn (mut s Shake) read(out_len int) []u8 {
	if !s.finalized {
		s.finalize()
	}
	if out_len <= 0 {
		return []u8{}
	}
	mut out := []u8{len: out_len}
	mut off := 0
	for off < out_len {
		if s.pos == s.rate {
			s.s.keccak_p_1600_24()
			s.load_block()
		}
		take := int_min(out_len - off, s.rate - s.pos)
		for i in 0 .. take {
			out[off + i] = s.buf[s.pos + i]
		}
		s.pos += take
		off += take
	}
	return out
}

// reset clears the sponge state, allowing the Shake instance to be reused.
pub fn (mut s Shake) reset() {
	s.s = State{}
	s.buf = [200]u8{}
	s.pos = 0
	s.finalized = false
}
