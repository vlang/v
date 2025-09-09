// Copyright ©2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// This file implements Ascon-XOF128, an Ascon-based Extendable-output Function (XOF)
// and their variant, Ascon-CXOF128 from NIST.SP.800-232 standard.
// Ascon-XOF128 is similar to Ascon-Hash256 where Ascon-XOF128 accepts an additional input
// which specifies the desired output length in bits.
module ascon

import encoding.binary

// max_hash_size is a maximum size of Ascon-XOF128 (and Ascon-CXOF128) checksum output.
// Its very rare where checksum output bigger than 512-bits, So, we limiting it to prevent unconditional thing.
// This limitation was only occurs on this module, wee can change it later.
const max_hash_size = 4096 // in bytes

// default_xof_size is the size of Ascon-XOF128 (and Ascon-CXOF128) checksum that has 512-bits length.
const default_xof_size = 64

// xof128_initial_state is a precomputed value for Ascon-XOF128 state.
// See the comment on `hash256_initial_state` about the values
const xof128_initial_state = State{
	e0: 0xda82ce768d9447eb
	e1: 0xcc7ce6c75f1ef969
	e2: 0xe7508fd780085631
	e3: 0x0ee0ea53416b58cc
	e4: 0xe0547524db6f0bde
}

// xof128 creates an Ascon-XOF128 checksum of msg with specified desired size of output.
@[direct_array_access]
pub fn xof128(msg []u8, size int) ![]u8 {
	mut x := new_xof128(size)
	_ := x.write(msg)!
	x.Digest.finish()
	mut out := []u8{len: size}
	n := x.Digest.squeeze(mut out)
	x.reset()
	return out[..n]
}

// xof128_64 creates a 64-bytes of Ascon-XOF128 checksum of msg.
@[direct_array_access]
pub fn xof128_64(msg []u8) ![]u8 {
	return xof128(msg, default_xof_size)!
}

// Xof128 is an opaque provides an implementation of Ascon-XOF128 from NIST.SP.800-232 standard.
// Its implements `hash.Hash` interface with checksum size stored on instance creation.
@[noinit]
pub struct Xof128 {
	Digest
mut:
	// The size of Xof128 cheksum, when you dont specify it
	size int = default_xof_size
}

// new_xof128 creates a new Ascon-XOF128 instance with provided size parameter.
@[direct_array_access]
pub fn new_xof128(size int) &Xof128 {
	if size < 1 || size > max_hash_size {
		panic('new_xof128: invalid size')
	}
	return &Xof128{
		Digest: Digest{
			State: xof128_initial_state
		}
		size:   size
	}
}

// size returns an underlying size of Xof128 checksum in fixed-sized manner.
// Internally, its return underlying size stored on current Xof128 instance.
pub fn (x &Xof128) size() int {
	return x.size
}

// block_size returns an underlying Xof128 block size operates on, ie, 8-bytes
pub fn (x &Xof128) block_size() int {
	return block_size
}

// clone returns a clone of x on the current state.
fn (x &Xof128) clone() &Xof128 {
	return &Xof128{
		Digest: x.Digest
		size:   x.size
	}
}

// write writes out the content of message and updates internal Xof128 state.
@[direct_array_access]
pub fn (mut x Xof128) write(msg []u8) !int {
	if x.Digest.done {
		panic('Digest: writing after done ')
	}
	return x.Digest.absorb(msg)
}

// sum returns an Ascon-XOF128 checksum of the bytes in msg.
// Its produces `x.size` of checksum bytes. If you want to more
// extendible output, use `.read` call instead.
@[direct_array_access]
pub fn (mut x Xof128) sum(msg []u8) []u8 {
	// working on the clone of the h, so we can keep writing
	mut x0 := x.clone()
	_ := x0.write(msg) or { panic(err) }
	x0.Digest.finish()
	mut dst := []u8{len: x.size}
	x0.Digest.squeeze(mut dst)
	x0.reset()
	return dst
}

// read tries to read `dst.len` bytes of hash output from current Xof128 state and stored into dst.
// Note: 1 ≤ dst.len ≤ max_hash_size.
@[direct_array_access]
pub fn (mut x Xof128) read(mut dst []u8) !int {
	// Left unchanged
	if dst.len == 0 {
		return 0
	}
	// Check output size
	if dst.len > max_hash_size {
		panic('Xof128.read: invalid size')
	}
	mut x0 := x.clone()
	x0.Digest.finish()
	n := x0.Digest.squeeze(mut dst)
	x0.reset()
	return n
}

// reset resets internal Xof128 state into default initialized state.
@[direct_array_access]
pub fn (mut x Xof128) reset() {
	// we dont reset the x.size
	unsafe { x.Digest.buf.reset() }
	x.Digest.length = 0
	x.Digest.done = false
	x.Digest.State = xof128_initial_state
}

// free releases out the resources taken by the `x`. Dont use x after .free call.
@[unsafe]
pub fn (mut x Xof128) free() {
	$if prealloc {
		return
	}
	unsafe {
		x.Digest.buf.free()
	}
	// Mark it as unusable
	x.Digest.done = true
}

// Ascon-CXOF128
//
// Ascon-CXOF128 is customized variant of Ascon-XOF128 that extends its
// functionality by incorporating a user-defined customization string.
// In addition to the message 𝑀 and output length 𝐿, Ascon-CXOF128 takes
// the customization string 𝑍 as input.

// The length of the customization string shall be at most 2048 bits (i.e., 256 bytes)
const max_cxof128_cstring = 256

// cxof128_initial_state is a precomputed value for Ascon-CXOF128 state.
// See the comment on `hash256_initial_state` about the values
const cxof128_initial_state = State{
	e0: 0x675527c2a0e8de03
	e1: 0x43d12d7dc0377bbc
	e2: 0xe9901dec426e81b5
	e3: 0x2ab14907720780b6
	e4: 0x8f3f1d02d432bc46
}

// cxof128 creates an Ascon-CXOF128 checksum of msg with supplied size and custom string cs.
@[direct_array_access]
pub fn cxof128(msg []u8, size int, cs []u8) ![]u8 {
	mut cx := new_cxof128(size, cs)!
	_ := cx.write(msg)!
	cx.Digest.finish()
	mut out := []u8{len: size}
	n := cx.Digest.squeeze(mut out)
	cx.reset()
	return out[..n]
}

// cxof128_64 creates a 64-bytes of Ascon-CXOF128 checksum of msg with supplied custom string in cs.
@[direct_array_access]
pub fn cxof128_64(msg []u8, cs []u8) ![]u8 {
	return cxof128(msg, default_xof_size, cs)!
}

// CXof128 is an opaque provides an implementation of Ascon-CXOF128 from NIST.SP.800-232 standard.
// Its implements `hash.Hash` interface with checksum-size stored on instance creation.
@[noinit]
pub struct CXof128 {
	Digest
mut:
	// Customization string
	cs []u8
	// checksum size, for fixed-output
	size int = default_xof_size
}

// new_cxof128 creates a new Ascon-CXOF128 instanace with cheksum size
// was set to size and custom string in cs. It returns error on fails.
@[direct_array_access]
pub fn new_cxof128(size int, cs []u8) !&CXof128 {
	if cs.len > max_cxof128_cstring {
		return error('CXof128: custom string length exceed limit')
	}
	if size < 1 || size > max_hash_size {
		return error('CXof128: invalid size')
	}
	// Initialize CXof128 state with precomputed-value and absorb the customization string
	mut s := cxof128_initial_state
	cxof128_absorb_custom_string(mut s, cs)

	return &CXof128{
		Digest: Digest{
			State: s
		}
		cs:     cs
		size:   size
	}
}

// size returns an underlying size of CXof128 checksum in fixed-sized manner.
// Internally, its return underlying size stored on current CXof128 instance.
pub fn (x &CXof128) size() int {
	return x.size
}

// block_size returns an underlying CXof128 block size operates on, ie, 8-bytes
pub fn (x &CXof128) block_size() int {
	return block_size
}

// write writes out the content of message and updates internal CXof128 state.
@[direct_array_access]
pub fn (mut x CXof128) write(msg []u8) !int {
	if x.Digest.done {
		panic('CXof128: writing after done ')
	}
	return x.Digest.absorb(msg)
}

// sum returns an Ascon-CXOF128 checksum of the bytes in msg.
// Its produces `x.size` of checksum bytes. If you want to more
// extendible output, use `.read` call instead.
@[direct_array_access]
pub fn (mut x CXof128) sum(msg []u8) []u8 {
	// working on the clone of the h, so we can keep writing
	mut x0 := x.clone()
	_ := x0.write(msg) or { panic(err) }
	x0.Digest.finish()
	mut dst := []u8{len: x.size}
	x0.Digest.squeeze(mut dst)
	x0.reset()
	return dst
}

// read tries to read `dst.len` bytes of hash output from current CXof128 state and stored into dst.
@[direct_array_access]
pub fn (mut x CXof128) read(mut dst []u8) !int {
	// Left unchanged, nothing space to store checksum
	if dst.len == 0 {
		return 0
	}
	if dst.len > max_hash_size {
		panic('CXof128.read: invalid size')
	}
	mut x0 := x.clone()
	x0.Digest.finish()
	n := x0.Digest.squeeze(mut dst)
	x0.reset()
	return n
}

// clone returns a clone of x on the current state.
fn (x &CXof128) clone() &CXof128 {
	return &CXof128{
		Digest: x.Digest
		size:   x.size
		cs:     x.cs
	}
}

// reset resets internal CXof128 state into default initialized state.
@[direct_array_access]
pub fn (mut x CXof128) reset() {
	// we dont reset the x.size and custom string
	unsafe { x.Digest.buf.reset() }
	x.Digest.length = 0
	x.Digest.done = false
	x.Digest.State = cxof128_initial_state
	// reabsorbs custom string
	cxof128_absorb_custom_string(mut x.Digest.State, x.cs)
}

// free releases out the resources taken by the `x`. Dont use x after .free call.
@[unsafe]
pub fn (mut x CXof128) free() {
	$if prealloc {
		return
	}
	unsafe {
		x.Digest.buf.free()
	}
	// Mark it as unusable
	x.Digest.done = true
}

// cxof128_absorb_custom_string performs absorbing phase of custom string in cs for Ascon-CXOF128.
@[direct_array_access; inline]
fn cxof128_absorb_custom_string(mut s State, cs []u8) {
	// absorb Z0, the length of the customization string (in bits) encoded as a u64
	s.e0 ^= u64(cs.len) << 3
	ascon_pnr(mut s, 12)

	// absorb the customization string
	mut zlen := cs.len
	mut zidx := 0
	for zlen >= block_size {
		block := unsafe { cs[zidx..zidx + block_size] }
		s.e0 ^= binary.little_endian_u64(block)
		ascon_pnr(mut s, 12)

		// updates a index
		zlen -= block_size
		zidx += block_size
	}
	// absorb final customization string
	last_block := unsafe { cs[zidx..] }
	s.e0 ^= load_bytes(last_block, last_block.len)
	s.e0 ^= pad(last_block.len)
	ascon_pnr(mut s, 12)
}
