// The source code refers to the go standard library, which will be combined with AES in the future.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Counter (CTR) mode.
//
// CTR converts a block cipher into a stream cipher by
// repeatedly encrypting an incrementing counter and
// xoring the resulting stream of data with the input.
//
// See NIST SP 800-38A, pp 13-15
module cipher

import crypto.internal.subtle

struct Ctr {
mut:
	b        Block
	next     []byte
	out      []byte
	out_used int
}

// new_ctr returns a Ctr which encrypts/decrypts using the given Block in
// counter mode. The length of iv must be the same as the Block's block size.
pub fn new_ctr(b Block, iv []byte) Ctr {
	block_size := b.block_size
	if iv.len != block_size {
		panic('cipher.new_cfb: IV length must be equal block size')
	}
	return Ctr{
		b: b
		out: []byte{len: b.block_size}
		next: iv.clone()
		out_used: block_size
	}
}

pub fn (x &Ctr) xor_key_stream(mut dst_ []byte, src_ []byte) {
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

			n := xor_bytes(mut dst, src, x.out[x.out_used..])

			// increment counter
			for i := x.next.len - 1; i >= 0; i-- {
				x.next[i]++
				if x.next[i] != 0 {
					break
				}
			}

			dst = dst[n..]
			src = src[n..]
			x.out_used += n
		}
	}
}
