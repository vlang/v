// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module bigint

import crypto.rand
import math.big

// int_big creates a random `big.Integer` with range `[0, n)`.
// Returns an error if `n` is 0 or negative.
// This function used to live in `crypto.rand`; it moved here so that
// `crypto.rand` itself does not depend on `math.big`, which keeps
// `crypto.rand` usable from lower-level modules without introducing
// import cycles.
pub fn int_big(n big.Integer) !big.Integer {
	if n.signum < 1 {
		return error('`n` cannot be 0 or negative.')
	}

	max := n - big.integer_from_int(1)
	len := max.bit_len()

	if len == 0 {
		// max = n - 1, if max = 0 then return max, as it is the only valid integer in [0, 1)
		return max
	}

	// k is the maximum byte length needed to encode a value < n
	k := (len + 7) / 8

	// b is the number of bits in the most significant byte of n-1
	mut b := u8(len % 8)
	if b == 0 {
		b = 8
	}

	mut result := big.Integer{}
	for {
		mut bytes := rand.read(k)!

		// Clear bits in the first byte to increase the probability that the result is < max
		bytes[0] &= u8(int(1 << b) - 1)

		result = big.integer_from_bytes(bytes)
		if result < max {
			break
		}
	}
	return result
}
