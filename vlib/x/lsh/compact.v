module lsh

// bbit_signature keeps only the lowest `bits` bits of each MinHash value.
pub fn bbit_signature(sig []u64, bits int) []u64 {
	if bits < 1 || bits >= 64 {
		return sig.clone()
	}
	mask := (u64(1) << u64(bits)) - 1
	mut out := []u64{len: sig.len}
	for i, v in sig {
		out[i] = v & mask
	}
	return out
}

// bbit_jaccard corrects the estimated Jaccard index for the false collisions
// introduced by truncating signatures to `bits` bits.
pub fn bbit_jaccard(a []u64, b []u64, bits int) f64 {
	p := jaccard_estimate(a, b)
	if bits < 1 || bits >= 64 {
		return p
	}
	false_rate := 1.0 / f64(u64(1) << u64(bits))
	denom := 1.0 - false_rate
	if denom == 0.0 {
		return p
	}
	est := (p - false_rate) / denom
	if est < 0.0 {
		return 0.0
	}
	if est > 1.0 {
		return 1.0
	}
	return est
}

// one_permutation_signature takes the k smallest values from a single seeded
// permutation, trading accuracy for much smaller signatures.
pub fn one_permutation_signature[T](items []T, conf MinHashConfig, hash_fn fn (T) u64, k int) []u64 {
	mut rng := new_prng(conf.seed)
	a := rng.below(universal_prime - 1) + 1
	b := rng.below(universal_prime)
	mut values := []u64{len: items.len}
	for i, item in items {
		values[i] = permute(hash_fn(item), a, b)
	}
	values.sort()
	n := if k > 0 && k < values.len { k } else { values.len }
	return values[..n].clone()
}
