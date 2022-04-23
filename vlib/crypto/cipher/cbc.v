// The source code refers to the go standard library, which will be combined with AES in the future.

// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Cipher block chaining (CBC) mode.
// CBC provides confidentiality by xoring (chaining) each plaintext block
// with the previous ciphertext block before applying the block cipher.
// See NIST SP 800-38A, pp 10-11
// NOTE this will be moved to crypto.cipher interface (joe-c)
module cipher

import crypto.internal.subtle

struct Cbc {
mut:
	b          Block
	block_size int
	iv         []u8
	tmp        []u8
}

// internal
fn new_des_cbc(b Block, iv []u8) Cbc {
	return Cbc{
		b: b
		block_size: b.block_size
		iv: iv.clone()
		tmp: []u8{len: b.block_size}
	}
}

// new_cbc returns a `DesCbc` which encrypts in cipher block chaining
// mode, using the given Block. The length of iv must be the same as the
// Block's block size.
pub fn new_cbc(b Block, iv []u8) Cbc {
	if iv.len != b.block_size {
		panic('crypto.cipher.new_cbc_encrypter: IV length must equal block size')
	}
	return new_des_cbc(b, iv)
}

// encrypt_blocks encrypts the blocks in `src_` to `dst_`.
// Please note: `dst_` is mutable for performance reasons.
pub fn (mut x Cbc) encrypt_blocks(mut dst_ []u8, src_ []u8) {
	unsafe {
		mut dst := *dst_
		mut src := src_
		if src.len % x.block_size != 0 {
			panic('crypto.cipher: input not full blocks')
		}
		if dst.len < src.len {
			panic('crypto.cipher: output smaller than input')
		}
		if subtle.inexact_overlap(dst[..src.len], src_) {
			panic('crypto.cipher: invalid buffer overlap')
		}
		mut iv := x.iv
		for src.len > 0 {
			// Write the xor to dst, then encrypt in place.
			xor_bytes(mut dst[..x.block_size], src[..x.block_size], iv)
			x.b.encrypt(mut dst[..x.block_size], dst[..x.block_size])
			// Move to the next block with this block as the next iv.
			iv = dst[..x.block_size]
			if x.block_size >= src.len {
				src = []
			} else {
				src = src[x.block_size..]
			}
			dst = dst[x.block_size..]
		}
		// Save the iv for the next crypt_blocks call.
		copy(mut x.iv, iv)
	}
}

// decrypt_blocks decrypts the blocks in `src` to `dst`.
// Please note: `dst` is mutable for performance reasons.
pub fn (mut x Cbc) decrypt_blocks(mut dst []u8, src []u8) {
	if src.len % x.block_size != 0 {
		panic('crypto.cipher: input not full blocks')
	}
	if dst.len < src.len {
		panic('crypto.cipher: output smaller than input')
	}
	if subtle.inexact_overlap((*dst)[..src.len], src) {
		panic('crypto.cipher: invalid buffer overlap')
	}
	if src.len == 0 {
		return
	}
	// For each block, we need to xor the decrypted data with the previous block's ciphertext (the iv).
	// To avoid making a copy each time, we loop over the blocks BACKWARDS.
	mut end := src.len
	mut start := end - x.block_size
	mut prev := start - x.block_size
	// Copy the last block of ciphertext in preparation as the new iv.
	copy(mut x.tmp, src[start..end])
	// Loop over all but the first block.
	for start > 0 {
		src_chunk := src[start..end]
		x.b.decrypt(mut (*dst)[start..end], src_chunk)
		xor_bytes(mut (*dst)[start..end], (*dst)[start..end], src[prev..start])
		end = start
		start = prev
		prev -= x.block_size
	}
	// The first block is special because it uses the saved iv.
	src_chunk := src[start..end]
	x.b.decrypt(mut (*dst)[start..end], src_chunk)
	xor_bytes(mut (*dst)[start..end], (*dst)[start..end], x.iv)
	// Set the new iv to the first block we copied earlier.
	x.iv = x.tmp
	x.tmp = x.iv
}

fn (mut x Cbc) set_iv(iv []u8) {
	if iv.len != x.iv.len {
		panic('cipher: incorrect length IV')
	}
	copy(mut x.iv, iv)
}
