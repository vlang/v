import crypto.rand
import math.big

fn test_prime_bit_length() {
	for bits in [8, 16, 32, 64, 128, 256] {
		p := rand.prime(bits) or { panic(err) }
		assert p.bit_len() == bits, 'requested ${bits} bits, got ${p.bit_len()}'
	}
}

fn test_prime_is_prime() {
	for bits in [16, 64, 128, 256] {
		p := rand.prime(bits) or { panic(err) }
		assert p.is_odd()
		assert p.is_probably_prime(40), '${p} is not prime'
	}
}

fn test_prime_second_highest_bit_is_set() {
	// Guarantees that the product of two primes of the same size has exactly
	// twice as many bits, which RSA modulus generation depends on.
	for _ in 0 .. 5 {
		p := rand.prime(64) or { panic(err) }
		q := rand.prime(64) or { panic(err) }
		assert (p * q).bit_len() == 128
	}
}

fn test_prime_is_random() {
	mut seen := map[string]bool{}
	for _ in 0 .. 8 {
		p := rand.prime(128) or { panic(err) }
		seen[p.str()] = true
	}
	assert seen.len == 8, 'prime() returned duplicates'
}

fn test_prime_small_sizes() {
	// Two bits leaves only 2 and 3.
	for _ in 0 .. 10 {
		p := rand.prime(2) or { panic(err) }
		assert p == big.two_int || p == big.integer_from_int(3)
	}
	p3 := rand.prime(3) or { panic(err) }
	assert p3 == big.integer_from_int(5) || p3 == big.integer_from_int(7)
}

fn test_prime_rejects_invalid_sizes() {
	if _ := rand.prime(1) {
		assert false, 'prime(1) should have failed'
	}
	if _ := rand.prime(0) {
		assert false, 'prime(0) should have failed'
	}
	if _ := rand.prime(-8) {
		assert false, 'prime(-8) should have failed'
	}
}

fn test_safe_prime() {
	p := rand.safe_prime(32) or { panic(err) }
	assert p.bit_len() == 32
	assert p.is_probably_prime(40)
	q := (p - big.one_int).right_shift(1)
	assert q.is_probably_prime(40), '(p - 1) / 2 is not prime'
}

fn test_safe_prime_rejects_invalid_sizes() {
	if _ := rand.safe_prime(2) {
		assert false, 'safe_prime(2) should have failed'
	}
}
