module chacha20

import encoding.binary

// This is building block for eXtended ChaCha20 stream cipher (XChaCha20)
// Its based on https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-xchacha-03
// Note: so, its maybe outdated...

// HChaCha20 nonce size
const h_nonce_size = 16

// xchacha20 are intermediary step to build xchacha20 and initialized the same way as the ChaCha20 cipher,
// except xchacha20 use a 128-bit (16 byte) nonce and has no counter to derive subkey
// see https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-xchacha-03#section-2.2
@[direct_array_access]
fn xchacha20(key []u8, nonce []u8) ![]u8 {
	// early bound check
	if key.len != key_size {
		return error('xchacha: Bad key size')
	}
	if nonce.len != h_nonce_size {
		return error('xchacha: Bad nonce size')
	}
	// initializes ChaCha20 state
	mut x := State{}
	x[0] = cc0
	x[1] = cc1
	x[2] = cc2
	x[3] = cc3

	x[4] = binary.little_endian_u32(key[0..4])
	x[5] = binary.little_endian_u32(key[4..8])
	x[6] = binary.little_endian_u32(key[8..12])
	x[7] = binary.little_endian_u32(key[12..16])

	x[8] = binary.little_endian_u32(key[16..20])
	x[9] = binary.little_endian_u32(key[20..24])
	x[10] = binary.little_endian_u32(key[24..28])
	x[11] = binary.little_endian_u32(key[28..32])

	// we have no counter
	x[12] = binary.little_endian_u32(nonce[0..4])
	x[13] = binary.little_endian_u32(nonce[4..8])
	x[14] = binary.little_endian_u32(nonce[8..12])
	x[15] = binary.little_endian_u32(nonce[12..16])

	// After initialization, proceed through the ChaCha20 rounds as usual.
	for i := 0; i < 10; i++ {
		// Diagonal round.
		qround_on_state(mut x, 0, 4, 8, 12) // 0
		qround_on_state(mut x, 1, 5, 9, 13) // 1
		qround_on_state(mut x, 2, 6, 10, 14) // 2
		qround_on_state(mut x, 3, 7, 11, 15) // 3

		// quarter diagonal round
		// Diagonal round.
		//   0 \  1 \  2 \  3
		//   5 \  6 \  7 \  4
		//  10 \ 11 \  8 \  9
		//  15 \ 12 \ 13 \ 14
		qround_on_state(mut x, 0, 5, 10, 15)
		qround_on_state(mut x, 1, 6, 11, 12)
		qround_on_state(mut x, 2, 7, 8, 13)
		qround_on_state(mut x, 3, 4, 9, 14)
	}

	// Once the 20 ChaCha rounds have been completed, the first 128 bits (16 bytes) and
	// last 128 bits (16 bytes) of the ChaCha state (both little-endian) are
	// concatenated, and this 256-bit (32 bytes) subkey is returned.
	mut out := []u8{len: 32}
	binary.little_endian_put_u32(mut out[0..4], x[0])
	binary.little_endian_put_u32(mut out[4..8], x[1])
	binary.little_endian_put_u32(mut out[8..12], x[2])
	binary.little_endian_put_u32(mut out[12..16], x[3])

	binary.little_endian_put_u32(mut out[16..20], x[12])
	binary.little_endian_put_u32(mut out[20..24], x[13])
	binary.little_endian_put_u32(mut out[24..28], x[14])
	binary.little_endian_put_u32(mut out[28..32], x[15])

	return out
}
