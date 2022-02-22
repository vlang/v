// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// OFB (Output Feedback) Mode.
module cipher

import crypto.internal.subtle

struct Ofb {
mut:
	b        Block
	cipher   []byte
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
	mut buf_size := cipher.stream_buffer_size
	if buf_size < block_size {
		buf_size = block_size
	}

	x := Ofb{
		b: b
		cipher: []byte{len: block_size}
		out: []byte{len: buf_size}
		out_used: 0
	}

	copy(x.cipher, iv)
	return x
}

pub fn (x &Ofb) xor_key_stream(mut dst_ []byte, src_ []byte) {
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
			if x.out_used >= x.out.len - x.b.block_size {
				bs := x.b.block_size
				mut remain := x.out.len - x.out_used
				if remain <= x.out_used {
					copy(x.out, x.out[x.out_used..])

					x.out = x.out[..x.out.cap]

					for remain < x.out.len - bs {
						x.b.encrypt(mut x.cipher, x.cipher)
						copy(x.out[remain..], x.cipher)
						remain += bs
					}
					x.out = x.out[..remain]
					x.out_used = 0
				}
			}

			n := xor_bytes(mut dst, src, x.out[x.out_used..])

			dst = dst[n..]
			src = src[n..]
			x.out_used += n
		}
	}
}
