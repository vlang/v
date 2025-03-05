// The source code refers to the go standard library, which will be combined with AES in the future.

// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Cipher Feedback Mode (CFB).
// See NIST SP 800-38A, pp 11-13

module cipher

import crypto.internal.subtle

struct Cfb {
mut:
	b        Block
	next     []u8
	out      []u8
	out_used int

	decrypt bool
}

// free the resources taken by the Cfb `x`
@[unsafe]
pub fn (mut x Cfb) free() {
	$if prealloc {
		return
	}
	unsafe {
		// x.b.free() TODO add?
		x.out.free()
		x.next.free()
	}
}

// new_cfb_encrypter returns a `Cfb` which encrypts with cipher feedback mode,
// using the given Block. The iv must be the same length as the Block's block
// size
pub fn new_cfb_encrypter(b Block, iv []u8) Cfb {
	return new_cfb(b, iv, false)
}

// new_cfb_decrypter returns a `Cfb` which decrypts with cipher feedback mode,
// using the given Block. The iv must be the same length as the Block's block
// size
pub fn new_cfb_decrypter(b Block, iv []u8) Cfb {
	return new_cfb(b, iv, true)
}

fn new_cfb(b Block, iv []u8, decrypt bool) Cfb {
	block_size := b.block_size
	if iv.len != block_size {
		panic('cipher.new_cfb: IV length must be equal block size')
	}
	mut x := Cfb{
		b:        b
		out:      []u8{len: b.block_size}
		next:     []u8{len: b.block_size}
		out_used: block_size
		decrypt:  decrypt
	}
	copy(mut x.next, iv)
	return x
}

// xor_key_stream xors each byte in the given slice with a byte from the key stream.
pub fn (mut x Cfb) xor_key_stream(mut dst []u8, src []u8) {
	unsafe {
		mut local_dst := *dst
		mut local_src := src
		if local_dst.len < local_src.len {
			panic('crypto.cipher.xor_key_stream: output smaller than input')
		}

		if subtle.inexact_overlap(local_dst[..src.len], local_src) {
			panic('crypto.cipher.xor_key_stream: invalid buffer overlap')
		}

		for local_src.len > 0 {
			if x.out_used == x.out.len {
				x.b.encrypt(mut x.out, x.next)
				x.out_used = 0
			}

			if x.decrypt {
				copy(mut x.next[x.out_used..], local_src)
			}

			n := xor_bytes(mut local_dst, local_src, x.out[x.out_used..])
			if !x.decrypt {
				copy(mut x.next[x.out_used..], local_dst)
			}
			local_dst = local_dst[n..]
			local_src = local_src[n..]
			x.out_used += n
		}
	}
}
