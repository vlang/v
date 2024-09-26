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
	next     []u8
	out      []u8
	out_used int
}

// free the resources taken by the Ctr `c`
@[unsafe]
pub fn (mut x Ctr) free() {
	$if prealloc {
		return
	}
	unsafe {
		// x.b.free() TODO add?
		x.out.free()
		x.next.free()
	}
}

// new_ctr returns a Ctr which encrypts/decrypts using the given Block in
// counter mode. The length of iv must be the same as the Block's block size.
pub fn new_ctr(b Block, iv []u8) Ctr {
	block_size := b.block_size
	if iv.len != block_size {
		panic('cipher.new_cfb: IV length must be equal block size')
	}
	return Ctr{
		b:        b
		out:      []u8{len: b.block_size}
		next:     iv.clone()
		out_used: block_size
	}
}

pub fn (mut x Ctr) xor_key_stream(mut dst []u8, src []u8) {
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

			n := xor_bytes(mut local_dst, local_src, x.out[x.out_used..])

			// increment counter
			for i := x.next.len - 1; i >= 0; i-- {
				x.next[i]++
				if x.next[i] != 0 {
					break
				}
			}

			local_dst = local_dst[n..]
			local_src = local_src[n..]
			x.out_used += n
		}
	}
}
