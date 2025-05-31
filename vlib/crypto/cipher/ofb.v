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
	next     []u8
	out      []u8
	out_used int
}

// new_ofb returns a Ofb that encrypts or decrypts using the block cipher b
// in output feedback mode. The initialization vector iv's length must be equal
// to b's block size.
pub fn new_ofb(b Block, iv []u8) Ofb {
	block_size := b.block_size
	if iv.len != block_size {
		panic('cipher.new_ofb: IV length must be equal block size')
	}
	mut x := Ofb{
		b:        b
		out:      []u8{len: b.block_size}
		next:     []u8{len: b.block_size}
		out_used: block_size
	}
	copy(mut x.next, iv)
	return x
}

// xor_key_stream xors each byte in the given slice with a byte from the key stream.
pub fn (mut x Ofb) xor_key_stream(mut dst []u8, src []u8) {
	unsafe {
		mut local_dst := *dst
		mut local_src := src
		if local_dst.len < local_src.len {
			panic('crypto.cipher.xor_key_stream: output smaller than input')
		}

		if subtle.inexact_overlap(local_dst[..local_src.len], local_src) {
			panic('crypto.cipher.xor_key_stream: invalid buffer overlap')
		}

		for local_src.len > 0 {
			if x.out_used == x.out.len {
				x.b.encrypt(mut x.out, x.next)
				x.out_used = 0
			}

			copy(mut x.next, x.out)

			n := xor_bytes(mut local_dst, local_src, x.out)
			local_dst = local_dst[n..]
			local_src = local_src[n..]
			x.out_used += n
		}
	}
}
