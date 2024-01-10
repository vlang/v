// The source code refers to the go standard library

module des

import crypto.cipher
import crypto.internal.subtle
import encoding.binary

const block_size = 8

// A tripleDesCipher is an instance of TripleDES encryption.
struct TripleDesCipher {
	block_size int = des.block_size
mut:
	cipher1 DesCipher
	cipher2 DesCipher
	cipher3 DesCipher
}

// DesCipher is an instance of DES encryption.
struct DesCipher {
	block_size int = des.block_size
mut:
	subkeys [16]u64
}

// NewCipher creates and returns a new cipher.Block.
pub fn new_cipher(key []u8) cipher.Block {
	if key.len != 8 {
		panic('crypto.aes: invalid key size')
	}

	mut c := DesCipher{}
	c.generate_subkeys(key)
	return c
}

// creates 16 56-bit subkeys from the original key
fn (mut c DesCipher) generate_subkeys(key_bytes []u8) {
	// feistel_box_once.do(initFeistel_box)

	// apply PC1 permutation to key
	key := binary.big_endian_u64(key_bytes)
	permuted_key := permute_block(key, permuted_choice1[..])

	// rotate halves of permuted key according to the rotation schedule
	left_rotations := ks_rotate(u32(permuted_key >> 28))
	right_rotations := ks_rotate(u32(permuted_key << 4) >> 4)

	// generate subkeys
	for i := 0; i < 16; i++ {
		// combine halves to form 56-bit input to PC2
		pc2_input := u64(left_rotations[i]) << 28 | u64(right_rotations[i])
		// apply PC2 permutation to 7 byte input
		c.subkeys[i] = unpack(permute_block(pc2_input, permuted_choice2[..]))
	}
}

pub fn (c &DesCipher) encrypt(mut dst []u8, src []u8) {
	if src.len < des.block_size {
		panic('crypto/des: input not full block')
	}
	if dst.len < des.block_size {
		panic('crypto/des: output not full block')
	}
	if subtle.inexact_overlap(dst[..des.block_size], src[..des.block_size]) {
		panic('crypto/des: invalid buffer overlap')
	}
	encrypt_block(c.subkeys[..], mut dst, src)
}

pub fn (c &DesCipher) decrypt(mut dst []u8, src []u8) {
	if src.len < des.block_size {
		panic('crypto/des: input not full block')
	}
	if dst.len < des.block_size {
		panic('crypto/des: output not full block')
	}
	if subtle.inexact_overlap(dst[..des.block_size], src[..des.block_size]) {
		panic('crypto/des: invalid buffer overlap')
	}
	decrypt_block(c.subkeys[..], mut dst, src)
}

// NewTripleDesCipher creates and returns a new cipher.Block.
pub fn new_triple_des_cipher(key []u8) cipher.Block {
	if key.len != 24 {
		panic('crypto.des: invalid key size')
	}
	mut c := TripleDesCipher{}
	c.cipher1.generate_subkeys(key[..8])
	c.cipher2.generate_subkeys(key[8..16])
	c.cipher3.generate_subkeys(key[16..])
	return c
}

pub fn (c &TripleDesCipher) encrypt(mut dst []u8, src []u8) {
	if src.len < des.block_size {
		panic('crypto/des: input not full block')
	}
	if dst.len < des.block_size {
		panic('crypto/des: output not full block')
	}
	if subtle.inexact_overlap(dst[..des.block_size], src[..des.block_size]) {
		panic('crypto/des: invalid buffer overlap')
	}

	mut b := binary.big_endian_u64(src)
	b = permute_initial_block(b)
	mut left, mut right := u32(b >> 32), u32(b)

	left = (left << 1) | (left >> 31)
	right = (right << 1) | (right >> 31)

	for i := 0; i < 8; i++ {
		left, right = feistel(left, right, c.cipher1.subkeys[2 * i], c.cipher1.subkeys[2 * i + 1])
	}
	for i := 0; i < 8; i++ {
		right, left = feistel(right, left, c.cipher2.subkeys[15 - 2 * i], c.cipher2.subkeys[15 - (
			2 * i + 1)])
	}
	for i := 0; i < 8; i++ {
		left, right = feistel(left, right, c.cipher3.subkeys[2 * i], c.cipher3.subkeys[2 * i + 1])
	}

	left = (left << 31) | (left >> 1)
	right = (right << 31) | (right >> 1)

	pre_output := (u64(right) << 32) | u64(left)
	binary.big_endian_put_u64(mut dst, permute_final_block(pre_output))
}

pub fn (c &TripleDesCipher) decrypt(mut dst []u8, src []u8) {
	if src.len < des.block_size {
		panic('crypto/des: input not full block')
	}
	if dst.len < des.block_size {
		panic('crypto/des: output not full block')
	}
	if subtle.inexact_overlap(dst[..des.block_size], src[..des.block_size]) {
		panic('crypto/des: invalid buffer overlap')
	}

	mut b := binary.big_endian_u64(src)
	b = permute_initial_block(b)

	mut left, mut right := u32(b >> 32), u32(b)

	left = (left << 1) | (left >> 31)
	right = (right << 1) | (right >> 31)

	for i := 0; i < 8; i++ {
		left, right = feistel(left, right, c.cipher3.subkeys[15 - 2 * i], c.cipher3.subkeys[15 - (
			2 * i + 1)])
	}
	for i := 0; i < 8; i++ {
		right, left = feistel(right, left, c.cipher2.subkeys[2 * i], c.cipher2.subkeys[2 * i + 1])
	}
	for i := 0; i < 8; i++ {
		left, right = feistel(left, right, c.cipher1.subkeys[15 - 2 * i], c.cipher1.subkeys[15 - (
			2 * i + 1)])
	}

	left = (left << 31) | (left >> 1)
	right = (right << 31) | (right >> 1)

	pre_output := (u64(right) << 32) | u64(left)
	binary.big_endian_put_u64(mut dst, permute_final_block(pre_output))
}
