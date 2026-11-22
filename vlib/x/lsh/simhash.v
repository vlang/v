module lsh

// simhash produces a 64-bit cosine-similarity fingerprint using 512 seeded
// Rademacher (+/-1) hyperplanes with majority voting into the 64 bits.
// Rademacher coefficients give the same sign-of-dot-product decision as
// Gaussian ones without a transcendental draw per dimension.
pub fn simhash(vec []f64, seed u64) u64 {
	return simhash_with_projections(vec, seed, 64, 512)
}

// simhash_with_projections produces a fingerprint of at most 64 bits using
// the given number of seeded Rademacher hyperplanes.
pub fn simhash_with_projections(vec []f64, seed u64, bits int, projections int) u64 {
	if bits < 1 || bits > 64 {
		return 0
	}
	n := if projections > 0 { projections } else { 512 }
	mut rng := new_prng(seed)
	mut votes := []int{len: bits, init: 0}
	mut coeff := []f64{len: vec.len, init: 0.0}
	for p in 0 .. n {
		for j in 0 .. vec.len {
			coeff[j] = rng.sign()
		}
		mut dot := 0.0
		for j in 0 .. vec.len {
			dot += coeff[j] * vec[j]
		}
		if dot >= 0.0 {
			votes[p % bits]++
		} else {
			votes[p % bits]--
		}
	}
	mut fingerprint := u64(0)
	for b in 0 .. bits {
		if votes[b] > 0 {
			fingerprint |= u64(1) << u64(b)
		}
	}
	return fingerprint
}

// simhash_similarity estimates cosine similarity from two 64-bit fingerprints.
pub fn simhash_similarity(a u64, b u64) f64 {
	return simhash_similarity_bits(a, b, 64)
}

// simhash_similarity_bits estimates similarity from two fingerprints whose
// length is limited to the lowest `bits` bits.
pub fn simhash_similarity_bits(a u64, b u64, bits int) f64 {
	n := if bits < 1 || bits > 64 { 64 } else { bits }
	mask := if n == 64 { max_u64() } else { (u64(1) << u64(n)) - 1 }
	return 1.0 - f64(hamming64(a & mask, b & mask)) / f64(n)
}
