module lsh

import math

fn pow_small(base f64, exponent int) f64 {
	if exponent <= 0 {
		return 1.0
	}
	mut result := 1.0
	for _ in 0 .. exponent {
		result *= base
	}
	return result
}

@[direct_array_access]
fn is_empty_signature(sig []u64) bool {
	for v in sig {
		if v != max_u64() {
			return false
		}
	}
	return true
}

// jaccard_estimate approximates set Jaccard similarity from two MinHash
// signatures. Two empty sets score 1.0; one empty set scores 0.0.
@[direct_array_access]
pub fn jaccard_estimate(a []u64, b []u64) f64 {
	if a.len == 0 || b.len == 0 || a.len != b.len {
		return 0.0
	}
	a_empty := is_empty_signature(a)
	b_empty := is_empty_signature(b)
	if a_empty && b_empty {
		return 1.0
	}
	if a_empty || b_empty {
		return 0.0
	}
	mut matches := 0
	for i in 0 .. a.len {
		if a[i] == b[i] {
			matches++
		}
	}
	return f64(matches) / f64(a.len)
}

// jaccard_strings computes the exact Jaccard index of two string sets.
pub fn jaccard_strings(a []string, b []string) f64 {
	if a.len == 0 && b.len == 0 {
		return 1.0
	}
	mut left := map[string]bool{}
	for s in a {
		left[s] = true
	}
	mut right := map[string]bool{}
	for s in b {
		right[s] = true
	}
	mut intersection := 0
	for s, _ in left {
		if s in right {
			intersection++
		}
	}
	mut union_size := 0
	for _, _ in left {
		union_size++
	}
	for s, _ in right {
		if s !in left {
			union_size++
		}
	}
	if union_size == 0 {
		return 1.0
	}
	return f64(intersection) / f64(union_size)
}

// weighted_jaccard computes the generalized Jaccard index of two
// non-negative weight vectors.
@[direct_array_access]
pub fn weighted_jaccard(a []f64, b []f64) f64 {
	if a.len != b.len {
		return 0.0
	}
	mut intersection := 0.0
	mut union_size := 0.0
	for i in 0 .. a.len {
		mut x := a[i]
		mut y := b[i]
		if x < 0.0 {
			x = 0.0
		}
		if y < 0.0 {
			y = 0.0
		}
		intersection += if x < y { x } else { y }
		union_size += if x > y { x } else { y }
	}
	if union_size == 0.0 {
		return 1.0
	}
	return intersection / union_size
}

// cosine_similarity computes the cosine of the angle between two vectors.
@[direct_array_access]
pub fn cosine_similarity(a []f64, b []f64) f64 {
	if a.len != b.len {
		return 0.0
	}
	mut dot := 0.0
	mut norm_a := 0.0
	mut norm_b := 0.0
	for i in 0 .. a.len {
		dot += a[i] * b[i]
		norm_a += a[i] * a[i]
		norm_b += b[i] * b[i]
	}
	if norm_a == 0.0 || norm_b == 0.0 {
		return 0.0
	}
	return dot / (math.sqrt(norm_a) * math.sqrt(norm_b))
}

@[direct_array_access]
fn popcount64(x u64) int {
	mut v := x
	v = v - ((v >> 1) & 0x5555555555555555)
	v = (v & 0x3333333333333333) + ((v >> 2) & 0x3333333333333333)
	v = (v + (v >> 4)) & 0x0f0f0f0f0f0f0f0f
	return int((v * 0x0101010101010101) >> 56)
}

// hamming64 returns the number of differing bits in two u64 fingerprints.
pub fn hamming64(a u64, b u64) int {
	return popcount64(a ^ b)
}
