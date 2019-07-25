// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// // Cipher block chaining (CBC) mode.

// // CBC provides confidentiality by xoring (chaining) each plaintext block
// // with the previous ciphertext block before applying the block cipher.

// // See NIST SP 800-38A, pp 10-11

module cipher

// import crypto.internal.subtle

// struct Cbc {
// mut:
// 	b          Blocker
// 	block_size int
// 	iv         []byte
// 	tmp        []byte
// }

// fn new_cbc(b Blocker, iv []byte) *Cbc {
// 	return &Cbc{
// 		b:          b,
// 		block_size: b.block_size(),
// 		iv:         dup(iv),
// 		// tmp:        make([]byte, b.block_size()),
// 		tmp:        [byte(0); b.block_size()],
// 	}
// }

// type CbcEncrypter Cbc

// // CbcEncAble is an interface implemented by ciphers that have a specific
// // optimized implementation of CBC encryption, like crypto/aes.
// // new_cbc_encrypter will check for this interface and return the specific
// // BlockMode if found.
// interface CbcEncAbler {
// 	new_cbc_encrypter(iv []byte) BlockModer
// }

// // new_cbc_encrypter returns a BlockMode which encrypts in cipher block chaining
// // mode, using the given Block. The length of iv must be the same as the
// // Block's block size.
// pub fn new_cbc_encrypter(b Blocker, iv []byte) BlockModer {
// 	if iv.len != b.block_size() {
// 		panic('crypto.cipher.new_cbc_encrypter: IV length must equal block size')
// 	}
// 	// if cbc, ok := b.(CbcEncAble); ok {
// 	// 	return cbc.new_cbc_encrypter(iv)
// 	// }
// 	return new_cbc(b, iv)
// }

// // fn (x &CbcEncrypter) block_size() int { return x.block_size }
// fn (x &Cbc) block_size() int { return x.block_size }

// // fn (x &CbcEncrypter) crypt_blocks(dst, src []byte) {
// fn (x &Cbc) crypt_blocks(dst, src []byte) {
// 	if src.len%x.block_size != 0 {
// 		panic('crypto.cipher: input not full blocks')
// 	}
// 	if dst.len < src.len {
// 		panic('crypto.cipher: output smaller than input')
// 	}
// 	// if subtle.inexact_overlap(dst[:src.len], src) {
// 		if subtle.inexact_overlap(dst.left(src.len), src) {
// 		panic('crypto.cipher: invalid buffer overlap')
// 	}

// 	mut iv := x.iv

// 	for src.len > 0 {
// 		// Write the xor to dst, then encrypt in place.
// 		// xor_bytes(dst[:x.block_size], src[:x.block_size], iv)
// 		// x.b.encrypt(dst[:x.block_size], dst[:x.block_size])
// 		xor_bytes(dst.left(x.block_size), src.left(x.block_size), iv)
// 		x.b.encrypt(dst.left(x.block_size), dst.left(x.block_size))

// 		// Move to the next block with this block as the next iv.
// 		// iv = dst[:x.block_size]
// 		// src = src[x.block_size:]
// 		// dst = dst[x.block_size:]
// 		iv = dst.left(x.block_size)
// 		src = src.right(x.block_size)
// 		dst = dst.right(x.block_size)
// 	}

// 	// Save the iv for the next crypt_blocks call.
// 	// copy(x.iv, iv)
// 	x.iv = iv
// }

// // fn (x &CbcEncrypter) set_iv(iv []byte) {
// fn (x &Cbc) set_iv(iv []byte) {
// 	if iv.len != x.iv.len {
// 		panic('cipher: incorrect length IV')
// 	}
// 	// copy(x.iv, iv)
// 	x.iv = iv
// }

// type Cbcdecrypter Cbc

// // cbcDecAble is an interface implemented by ciphers that have a specific
// // optimized implementation of CBC decryption, like crypto/aes.
// // new_cbc_decrypter will check for this interface and return the specific
// // BlockMode if found.
// interface cbcDecAbler {
// 	new_cbc_decrypter(iv []byte) BlockModer
// }

// // new_cbc_decrypter returns a BlockMode which decrypts in cipher block chaining
// // mode, using the given Block. The length of iv must be the same as the
// // Block's block size and must match the iv used to encrypt the data.
// pub fn new_cbc_decrypter(b Blocker, iv []byte) BlockModer {
// 	if iv.len != b.block_size() {
// 		panic('cipher.new_cbc_decrypter: IV length must equal block size')
// 	}
// 	// if cbc, ok := b.(cbcDecAble); ok {
// 	// 	return cbc.new_cbc_decrypter(iv)
// 	// }

// 	// return (&Cbcdecrypter)(new_cbc(b, iv))
// 	return new_cbc(b, iv)
// }

// fn (x &Cbcdecrypter) block_size() int { return x.block_size }

// fn (x &Cbcdecrypter) crypt_blocks(dst, src []byte) {
// 	if src.len%x.block_size != 0 {
// 		panic('crypto/cipher: input not full blocks')
// 	}
// 	if dst.len < src.len {
// 		panic('crypto/cipher: output smaller than input')
// 	}
// 	// if subtle.inexact_overlap(dst[:src.len], src) {
// 	if subtle.inexact_overlap(dst.left(src.len), src) {
// 		panic('crypto/cipher: invalid buffer overlap')
// 	}
// 	if src.len == 0 {
// 		return
// 	}

// 	// For each block, we need to xor the decrypted data with the previous block's ciphertext (the iv).
// 	// To avoid making a copy each time, we loop over the blocks BACKWARDS.
// 	mut end := src.len
// 	mut start := end - x.block_size
// 	mut prev := start - x.block_size

// 	// Copy the last block of ciphertext in preparation as the new iv.
// 	// copy(x.tmp, src[start:end])
// 	x.tmp = src.slice(start, end)

// 	// Loop over all but the first block.
// 	for start > 0 {
// 		// x.b.decrypt(dst[start:end], src[start:end])
// 		// xor_bytes(dst[start:end], dst[start:end], src[prev:start])
// 		x.b.decrypt(dst.slice(start, end), src.slice(start, end))
// 		xor_bytes(dst.slice(start, end), dst.slice(start, end), src.slice(prev, start))

// 		end = start
// 		start = prev
// 		prev -= x.block_size
// 	}

// 	// The first block is special because it uses the saved iv.
// 	// x.b.decrypt(dst[start:end], src[start:end])
// 	// xor_bytes(dst[start:end], dst[start:end], x.iv)
// 	x.b.decrypt(dst.slice(start, end), src.slice(start, end))
// 	xor_bytes(dst.slice(start, end), dst.slice(start, end), x.iv)


// 	// Set the new iv to the first block we copied earlier.
// 	x.iv = x.tmp
// 	x.tmp = x.iv
// }

// fn (x &Cbcdecrypter) set_iv(iv []byte) {
// 	if iv.len != x.iv.len {
// 		panic('cipher: incorrect length IV')
// 	}
// 	// copy(x.iv, iv)
// 	x.iv = iv
// }