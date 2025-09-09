// Copyright ©2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Common helpers used by Ascon-Hash256, Ascon-XOF128 and Ascon-CXOF128
module ascon

import encoding.binary

// The Digest is an internal structure used by Ascon-hashing variants.
@[noinit]
struct Digest {
	State
mut:
	// buffer for storing unprocessed data for streaming-way
	buf []u8 = []u8{len: block_size}
	// length of leftover unprocessed bytes on the digest buffer
	length int

	// internal flag
	done bool
}

// finish finalizes state by writing last block in the Digest internal buffer.
// and consequently updates Digest state.
@[direct_array_access; inline]
fn (mut d Digest) finish() {
	if d.length >= d.buf.len {
		panic('Digest.finish: internal error')
	}
	// Process for the last block stored on the internal buffer
	d.State.e0 ^= pad(d.length)
	d.State.e0 ^= load_bytes(d.buf[..d.length], d.length)

	// Permutation step was done in squeezing-phase
	// ascon_pnr(mut d.State, 12)

	// zeroing Digest buffer
	d.length = 0
	unsafe { d.buf.reset() }

	// After finishing phase, we can't write data anymore into state
	d.done = true
}

// absorb absorbs message msg_ into Digest state.
@[direct_array_access]
fn (mut d Digest) absorb(msg_ []u8) int {
	// nothing to absorb, just return
	if msg_.len == 0 {
		return 0
	}
	// Absorbing messages into Digest state working in streaming-way.
	// Its continuesly updates internal state until you call `.finish` or `.free` it.
	// Firstly, it would checking unprocessed bytes on internal buffer, and append it
	// with bytes from messasge to align with block_size.
	// And then absorb this buffered message into state.
	// The process continues until the last partial block thats maybe below the block_size.
	// If its happens, it will be stored on the Digest internal buffer for later processing.
	mut msg := msg_.clone()
	unsafe {
		// Check if internal buffer has previous unprocessed bytes.
		// If its on there, try to empty the buffer.
		if d.length > 0 {
			// There are bytes in the d.buf, append it with bytes taken from msg
			if d.length + msg.len >= block_size {
				n := copy(mut d.buf[d.length..], msg)
				msg = msg[n..]
				d.length += n
				// If this d.buf length has reached block_size bytes, absorb it.
				if d.length == block_size {
					d.State.e0 ^= binary.little_endian_u64(d.buf)
					ascon_pnr(mut d.State, 12)
					// reset the internal buffer
					d.length = 0
					d.buf.reset()
				}
			} else {
				// Otherwise, still fit to buffer, but nof fully fills the d.buf
				// just stores into buffer without processing
				n := copy(mut d.buf[d.length..], msg)
				msg = msg[n..]
				d.length += n
			}
		}
		// process for full block
		for msg.len >= block_size {
			d.State.e0 ^= binary.little_endian_u64(msg[0..block_size])
			msg = msg[block_size..]
			ascon_pnr(mut d.State, 12)
		}
		// If there are partial block, just stored into buffer.
		if msg.len > 0 {
			n := copy(mut d.buf[d.length..], msg)
			msg = msg[n..]
			d.length += n
		}
		return msg_.len - msg.len
	}
}

// squeeze squeezes the state and calculates checksum output for the current state.
// It accepts destination buffer with desired buffer length you want to output.
@[direct_array_access; inline]
fn (mut d Digest) squeeze(mut dst []u8) int {
	// nothing to store, just return unchanged
	if dst.len == 0 {
		return 0
	}
	// check
	if dst.len > max_hash_size {
		panic('Digest.squeeze: invalid dst.len')
	}
	// The squeezing phase begins after msg is absorbed with an
	// permutation 𝐴𝑠𝑐𝑜𝑛-𝑝[12] to the state:
	ascon_pnr(mut d.State, 12)

	mut pos := 0
	mut clen := dst.len
	// process for full block size
	for clen >= block_size {
		binary.little_endian_put_u64(mut dst[pos..pos + 8], d.State.e0)
		ascon_pnr(mut d.State, 12)
		pos += block_size
		clen -= block_size
	}
	// final output, the resulting hash is the concatenation of hash blocks
	store_bytes(mut dst[pos..], d.State.e0, clen)
	pos += clen

	// for make sure, assert it here
	assert pos == dst.len

	return pos
}
