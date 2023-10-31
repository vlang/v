// The source code refers to the go standard library, which can be merged with AES later

module des

import encoding.binary

fn feistel(ll u32, rr u32, k0 u64, k1 u64) (u32, u32) {
	mut l := ll
	mut r := rr
	mut t := r ^ u32(k0 >> 32)

	l ^= feistel_box[7][t & 0x3f] ^ feistel_box[5][(t >> 8) & 0x3f] ^ feistel_box[3][(t >> 16) & 0x3f] ^ feistel_box[1][(t >> 24) & 0x3f]

	t = ((r << 28) | (r >> 4)) ^ u32(k0)
	l ^= feistel_box[6][t & 0x3f] ^ feistel_box[4][(t >> 8) & 0x3f] ^ feistel_box[2][(t >> 16) & 0x3f] ^ feistel_box[0][(t >> 24) & 0x3f]

	t = l ^ u32(k1 >> 32)
	r ^= feistel_box[7][t & 0x3f] ^ feistel_box[5][(t >> 8) & 0x3f] ^ feistel_box[3][(t >> 16) & 0x3f] ^ feistel_box[1][(t >> 24) & 0x3f]

	t = ((l << 28) | (l >> 4)) ^ u32(k1)
	r ^= feistel_box[6][t & 0x3f] ^ feistel_box[4][(t >> 8) & 0x3f] ^ feistel_box[2][(t >> 16) & 0x3f] ^ feistel_box[0][(t >> 24) & 0x3f]

	return l, r
}

fn crypt_block(subkeys []u64, mut dst []u8, src []u8, decrypt bool) {
	mut b := binary.big_endian_u64(src)
	b = permute_initial_block(b)

	mut left, mut right := u32(b >> 32), u32(b)

	left = (left << 1) | (left >> 31)
	right = (right << 1) | (right >> 31)

	if decrypt {
		for i := 0; i < 8; i++ {
			left, right = feistel(left, right, subkeys[15 - 2 * i], subkeys[15 - (2 * i + 1)])
		}
	} else {
		for i := 0; i < 8; i++ {
			left, right = feistel(left, right, subkeys[2 * i], subkeys[2 * i + 1])
		}
	}

	left = (left << 31) | (left >> 1)
	right = (right << 31) | (right >> 1)

	// switch left & right and perform final permutation
	pre_output := (u64(right) << 32) | u64(left)
	binary.big_endian_put_u64(mut dst, permute_final_block(pre_output))
}

// Encrypt one block from src into dst, using the subkeys.
pub fn encrypt_block(subkeys []u64, mut dst []u8, src []u8) {
	crypt_block(subkeys, mut dst, src, false)
}

// Decrypt one block from src into dst, using the subkeys.
fn decrypt_block(subkeys []u64, mut dst []u8, src []u8) {
	crypt_block(subkeys, mut dst, src, true)
}

// general purpose function to perform DES block permutations
fn permute_block(src u64, permutation []u8) u64 {
	mut block := u64(0)
	for position, n in permutation {
		bit := (src >> u64(u8(n))) & 1
		block |= bit << u64((permutation.len - 1) - position)
	}
	return block
}

// permuteInitial_block is equivalent to the permutation defined
// by initialPermutation.
fn permute_initial_block(b u64) u64 {
	// block = b7 b6 b5 b4 b3 b2 b1 b0 (8 bytes)
	mut block := b
	mut b1 := block >> 48
	mut b2 := block << 48
	block ^= b1 ^ b2 ^ b1 << 48 ^ b2 >> 48

	// block = b1 b0 b5 b4 b3 b2 b7 b6
	b1 = block >> 32 & 0xff00ff
	b2 = (block & 0xff00ff00)
	block ^= b1 << 32 ^ b2 ^ b1 << 8 ^ b2 << 24 // exchange b0 b4 with b3 b7

	// block is now b1 b3 b5 b7 b0 b2 b4 b6, the permutation:
	//                  ...  8
	//                  ... 24
	//                  ... 40
	//                  ... 56
	//  7  6  5  4  3  2  1  0
	// 23 22 21 20 19 18 17 16
	//                  ... 32
	//                  ... 48

	// exchange 4,5,6,7 with 32,33,34,35 etc.
	b1 = block & 0x0f0f00000f0f0000
	b2 = block & 0x0000f0f00000f0f0
	block ^= b1 ^ b2 ^ b1 >> 12 ^ b2 << 12

	// block is the permutation:
	//
	//   [+8]         [+40]
	//
	//  7  6  5  4
	// 23 22 21 20
	//  3  2  1  0
	// 19 18 17 16    [+32]

	// exchange 0,1,4,5 with 18,19,22,23
	b1 = block & 0x3300330033003300
	b2 = block & 0x00cc00cc00cc00cc
	block ^= b1 ^ b2 ^ b1 >> 6 ^ b2 << 6

	// block is the permutation:
	// 15 14
	// 13 12
	// 11 10
	//  9  8
	//  7  6
	//  5  4
	//  3  2
	//  1  0 [+16] [+32] [+64]

	// exchange 0,2,4,6 with 9,11,13,15:
	b1 = block & 0xaaaaaaaa55555555
	block ^= b1 ^ b1 >> 33 ^ b1 << 33

	// block is the permutation:
	// 6 14 22 30 38 46 54 62
	// 4 12 20 28 36 44 52 60
	// 2 10 18 26 34 42 50 58
	// 0  8 16 24 32 40 48 56
	// 7 15 23 31 39 47 55 63
	// 5 13 21 29 37 45 53 61
	// 3 11 19 27 35 43 51 59
	// 1  9 17 25 33 41 49 57
	return block
}

// permuteInitial_block is equivalent to the permutation defined
// by finalPermutation.
fn permute_final_block(b u64) u64 {
	// Perform the same bit exchanges as permuteInitial_block
	// but in reverse order.
	mut block := b
	mut b1 := block & 0xaaaaaaaa55555555
	block ^= b1 ^ b1 >> 33 ^ b1 << 33

	b1 = block & 0x3300330033003300
	mut b2 := block & 0x00cc00cc00cc00cc
	block ^= b1 ^ b2 ^ b1 >> 6 ^ b2 << 6

	b1 = block & 0x0f0f00000f0f0000
	b2 = block & 0x0000f0f00000f0f0
	block ^= b1 ^ b2 ^ b1 >> 12 ^ b2 << 12

	b1 = block >> 32 & 0xff00ff
	b2 = (block & 0xff00ff00)
	block ^= b1 << 32 ^ b2 ^ b1 << 8 ^ b2 << 24

	b1 = block >> 48
	b2 = block << 48
	block ^= b1 ^ b2 ^ b1 << 48 ^ b2 >> 48
	return block
}

// creates 16 28-bit blocks rotated according
// to the rotation schedule
fn ks_rotate(ain u32) []u32 {
	mut out := []u32{len: 16}
	mut last := ain
	for i := 0; i < 16; i++ {
		// 28-bit circular left shift
		left := (last << (4 + ks_rotations[i])) >> 4
		right := (last << 4) >> (32 - ks_rotations[i])
		out[i] = left | right
		last = out[i]
	}
	return out
}

// Expand 48-bit input to 64-bit, with each 6-bit block padded by extra two bits at the top.
// By doing so, we can have the input blocks (four bits each), and the key blocks (six bits each) well-aligned without
// extra shifts/rotations for alignments.
fn unpack(x u64) u64 {
	return ((x >> (6 * 1)) & 0xff) << (8 * 0) | ((x >> (6 * 3)) & 0xff) << (8 * 1) | ((x >> (6 * 5)) & 0xff) << (8 * 2) | ((x >> (6 * 7)) & 0xff) << (8 * 3) | ((x >> (6 * 0)) & 0xff) << (8 * 4) | ((x >> (6 * 2)) & 0xff) << (8 * 5) | ((x >> (6 * 4)) & 0xff) << (8 * 6) | ((x >> (6 * 6)) & 0xff) << (8 * 7)
}
