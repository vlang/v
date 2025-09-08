// Copyright ©2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// This file implement hashing routines based on Ascon-Hash256 schema defined in NIST SP 800-232 standard,
// Ascon-Hash256 hashing produces 256-bits output.
module ascon

// block_size is the size (rate) of Ascon-Hash256, Ascon-XOF128 and Ascon-CXOF128 operates on.
const block_size = 8

// hash256_size is the length of the Ascon-Hash256 checksum output, in bytes
const hash256_size = 32

// hash256_initial_state is a precomputed value for Ascon-Hash256 state.
//
// The 320-bit internal state of Ascon-Hash256 is initialized with the
// concatenation of the 64-bit 𝐼𝑉 = 0x0000080100cc0002 and 256 zeroes, followed
// by the 𝐴𝑠𝑐𝑜𝑛-𝑝[12] permutation as S ← 𝐴𝑠𝑐𝑜𝑛-𝑝[12](𝐼𝑉 ∥0256).
//
// s.e0 = 0x0000080100cc0002
// s.e1 = 0
// s.e2 = 0
// s.e3 = 0
// s.e4 = 0
// ascon_pnr(mut s, 12)
//
// Above step can be replaced with precomputed value to reduce runtime computations.
// See the detail on the NIST SP 800-232 standard on Sec A.3. Precomputation
//	𝑆0 ← 0x9b1e5494e934d681
//	𝑆1 ← 0x4bc3a01e333751d2
//	𝑆2 ← 0xae65396c6b34b81a
// 	𝑆3 ← 0x3c7fd4a4d56a4db3
//	𝑆4 ← 0x1a5c464906c5976d
//
const hash256_initial_state = State{
	e0: u64(0x9b1e5494e934d681)
	e1: 0x4bc3a01e333751d2
	e2: 0xae65396c6b34b81a
	e3: 0x3c7fd4a4d56a4db3
	e4: 0x1a5c464906c5976d
}

// sum256 creates an Ascon-Hash256 checksum for bytes on msg and produces a 256-bit hash.
@[direct_array_access]
pub fn sum256(msg []u8) []u8 {
	mut h := new_hash256()
	_ := h.write(msg) or { panic(err) }
	h.Digest.finish()
	mut dst := []u8{len: hash256_size}
	n := h.Digest.squeeze(mut dst)
	return dst[..n]
}

// Hash256 is an opaque provides an implementation of Ascon-Hash256 from NIST.SP.800-232 standard.
// Its implements `hash.Hash` interface.
@[noinit]
pub struct Hash256 {
	Digest
}

// new_hash256 creates a new Ascon-Hash256 instance.
@[direct_array_access]
pub fn new_hash256() &Hash256 {
	return &Hash256{
		Digest: Digest{
			State: hash256_initial_state
		}
	}
}

// size returns an underlying size of Hash256 checksum, ie, 32-bytes
pub fn (h &Hash256) size() int {
	return hash256_size
}

// block_size returns an underlying Hash256 block size operates on, ie, 8-bytes
pub fn (h &Hash256) block_size() int {
	return block_size
}

// reset reinits internal Hash256 state into default initialized state.
@[direct_array_access]
pub fn (mut h Hash256) reset() {
	h.Digest.State = hash256_initial_state
	unsafe { h.Digest.buf.reset() }
	h.Digest.length = 0
	h.Digest.done = false
}

// free releases out the resources taken by the `h`. Dont use x after .free call.
@[unsafe]
pub fn (mut h Hash256) free() {
	$if prealloc {
		return
	}
	unsafe {
		h.Digest.buf.free()
	}
	// Mark it as unusable
	h.Digest.done = true
}

// write writes out the content of message and updates internal Hash256 state.
@[direct_array_access]
pub fn (mut h Hash256) write(msg []u8) !int {
	if h.Digest.done {
		panic('Digest: writing after done ')
	}
	return h.absorb(msg)
}

// clone returns the clone of the current Hash256
@[direct_array_access]
fn (h &Hash256) clone() &Hash256 {
	digest := Digest{
		State:  h.Digest.State
		buf:    h.Digest.buf.clone()
		length: h.Digest.length
		done:   h.Digest.done
	}
	return &Hash256{digest}
}

// sum returns an Ascon-Hash256 checksum of the bytes in data.
@[direct_array_access]
pub fn (mut h Hash256) sum(data []u8) []u8 {
	// working on the clone of the h, so we can keep writing
	mut h0 := h.clone()
	_ := h0.write(data) or { panic(err) }
	h0.Digest.finish()
	mut dst := []u8{len: hash256_size}
	n := h0.Digest.squeeze(mut dst)
	h0.reset()
	return dst[..n]
}
