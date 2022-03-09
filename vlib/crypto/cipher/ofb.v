// The source code refers to the go standard library, which will be combined with AES in the future.
//
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// OFB (Output Feedback) Mode.
// See NIST SP 800-38A, pp 13-15
module cipher

import crypto.internal.subtle

struct Ofb {
mut:
	b        Block
	next     []byte
	out      []byte
	out_used int
}

// new_ofb returns a Ofb that encrypts or decrypts using the block cipher b
// in output feedback mode. The initialization vector iv's length must be equal
// to b's block size.
pub fn new_ofb(b Block, iv []byte) Ofb {
	block_size := b.block_size
	if iv.len != block_size {
		panic('cipher.new_ofb: IV length must be equal block size')
	}
	mut x := Ofb{
		b: b
		out: []byte{len: b.block_size}
		next: []byte{len: b.block_size}
		out_used: block_size
	}
	copy(mut x.next, iv)
	return x
}

pub fn (mut x Ofb) xor_key_stream(mut dst_ []byte, src_ []byte) {
	unsafe {
		mut dst := *dst_
		mut src := src_
		if dst.len < src.len {
			panic('crypto.cipher.xor_key_stream: output smaller than input')
		}

		if subtle.inexact_overlap(dst[..src.len], src) {
			panic('crypto.cipher.xor_key_stream: invalid buffer overlap')
		}

		for src.len > 0 {
			if x.out_used == x.out.len {
				x.b.encrypt(mut x.out, x.next)
				x.out_used = 0
			}

			copy(mut x.next, x.out)

			n := xor_bytes(mut dst, src, x.out)
			dst = dst[n..]
			src = src[n..]
			x.out_used += n
		}
	}
}
