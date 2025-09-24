// Copyright © 2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// This file contains a building block for eXtended ChaCha20 stream cipher (XChaCha20) construction.
// Its based on https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-xchacha-03
// Note: so, its maybe outdated...
// Beside above draft that defines XChaCha20 construction with 32-bit internal counter,
// this XChaCha20 construction was expanded to support 64-bit counter.
// There are nothing RFC draft or published standard that can be used as a reference.
// Fortunatelly, this construct commonly implemented in popular chacha20 libraries.
module chacha20

import encoding.binary

// HChaCha20 nonce size
const h_nonce_size = 16

// hchacha20 are intermediary step to build XChaCha20 and initialized the same way as the ChaCha20 cipher,
// except hchacha20 use a 128-bit (16 byte) nonce and has no counter to derive subkey.
// See https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-xchacha-03#section-2.2
@[direct_array_access]
fn hchacha20(key []u8, nonce []u8) ![]u8 {
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
		// Column round.
		qround_on_state(mut x, 0, 4, 8, 12) // 0
		qround_on_state(mut x, 1, 5, 9, 13) // 1
		qround_on_state(mut x, 2, 6, 10, 14) // 2
		qround_on_state(mut x, 3, 7, 11, 15) // 3

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

	// Once the 20 ChaCh20 rounds have been completed, the first 128 bits (16 bytes) and
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

// derive_xchacha20_key_nonce derives a new key and nonce for eXtended ChaCha20 construction.
// It accepts boolean `flag64` flag as the last parameters.
// When its set into true, it would be used as an indicator of a 64-bit counter construction.
@[direct_array_access; inline]
fn derive_xchacha20_key_nonce(key []u8, nonce []u8, flag64 bool) !([]u8, []u8) {
	// Its only for x_nonce_size
	if nonce.len != x_nonce_size {
		return error('Bad nonce size for derive_xchacha20_key_nonce')
	}
	// derives a new key based on XChaCha20 construction
	// first, use 16 bytes of nonce used to derive the key
	new_key := hchacha20(key, nonce[0..16])!
	remaining_nonce := nonce[16..24].clone()

	// derive a new nonce based on the flag64 flag.
	// If flag64 was true, its intended to build XChaCha20 original variant with 64-bit counter.
	// Otherwise, its a XChaCha20 standard variant with 32-bit counter
	new_nonce := if flag64 {
		// use the remaining 8-bytes nonce
		remaining_nonce
	} else {
		// and the last of 8 bytes of nonce copied into to build nonce_size length of new nonce.
		mut nonce12 := []u8{len: nonce_size}
		_ := copy(mut nonce12[4..12], remaining_nonce)
		nonce12
	}

	return new_key, new_nonce
}
