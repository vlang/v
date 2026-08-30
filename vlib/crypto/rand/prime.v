// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

import math.big

// prime returns a random prime of exactly `bits` bits, drawn from the operating
// system's source of entropy.
//
// The two highest bits and the lowest bit are set. The lowest makes the
// candidate odd; the second highest means the product of two `bits`-sized
// primes has exactly `2 * bits` bits, as RSA key generation expects.
//
// Primality comes from `big.Integer.is_probably_prime`, whose error probability
// is negligible but not zero.
pub fn prime(bits int) !big.Integer {
	if bits < 2 {
		return error('crypto.rand: prime requires at least 2 bits')
	}
	if bits == 2 {
		return if int_u64(2)! == 0 { big.two_int } else { big.integer_from_int(3) }
	}
	for {
		mut candidate := random_bits(bits)!
		candidate.set_bit(u32(bits - 1), true)
		candidate.set_bit(u32(bits - 2), true)
		candidate.set_bit(0, true)
		if candidate.is_probably_prime(0) {
			return candidate
		}
	}
	return error('crypto.rand: unreachable')
}

// safe_prime returns a random prime `p` of exactly `bits` bits for which
// `(p - 1) / 2` is also prime. Safe primes are used as Diffie-Hellman moduli,
// where they rule out small subgroup attacks. They are far rarer than ordinary
// primes, so this is much slower than `prime`.
pub fn safe_prime(bits int) !big.Integer {
	if bits < 3 {
		return error('crypto.rand: safe_prime requires at least 3 bits')
	}
	for {
		// q first, so the expensive test runs on the smaller number.
		q := prime(bits - 1)!
		p := q.left_shift(1) + big.one_int
		if p.bit_len() == bits && p.is_probably_prime(0) {
			return p
		}
	}
	return error('crypto.rand: unreachable')
}

// random_bits returns a uniformly distributed integer below `2^bits`.
fn random_bits(bits int) !big.Integer {
	if bits <= 0 {
		return big.zero_int
	}
	nbytes := (bits + 7) / 8
	mut buf := bytes(nbytes)!
	// Clear the bits above the requested length in the leading byte.
	excess := nbytes * 8 - bits
	if excess > 0 {
		buf[0] &= u8(0xff >> excess)
	}
	return big.integer_from_bytes(buf)
}
