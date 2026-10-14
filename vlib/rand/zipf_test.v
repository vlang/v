module rand

import math

fn zipf_probability(v f64, s f64, k int, n int) f64 {
	num := math.pow(v + f64(k), -s)
	mut total := 0.0
	for i in 0 .. n {
		total += math.pow(v + f64(i), -s)
	}
	return num / total
}

fn test_zipf_range_and_invalid_config() {
	seed([u32(0x1234ABCD), u32(0x9E3779B9)])
	for _ in 0 .. 200 {
		k := zipf(s: 1.5, v: 1.0, n: 50)!
		assert k >= 0 && k <= 49, 'rank must stay in 0..n-1'
	}
	zipf(s: 0.0, v: 1.0, n: 10) or {
		assert err.msg().contains('s must be > 0')
		return
	}
	assert false, 's <= 0 must error'
	zipf(s: 1.0, v: 0.5, n: 10) or {
		assert err.msg().contains('v must be >= 1')
		return
	}
	assert false, 'v < 1 must error'
	zipf(s: 1.0, v: 1.0, n: 0) or {
		assert err.msg().contains('n must be positive')
		return
	}
	assert false, 'n < 1 must error'
}

fn test_zipf_matches_analytic_distribution() {
	seed([u32(0x1234ABCD), u32(0x9E3779B9)])
	n := 100
	draws := 50000
	mut counts := []i64{len: n, init: 0}
	for _ in 0 .. draws {
		k := zipf(s: 1.5, v: 1.0, n: n)!
		counts[k] += 1
	}
	// each rank must show its analytic share (5% absolute tolerance on a
	// 50k-sample histogram)
	for k in 0 .. 10 {
		expected := zipf_probability(1.0, 1.5, k, n)
		observed := f64(counts[k]) / f64(draws)
		diff := if observed > expected { observed - expected } else { expected - observed }
		assert diff < 0.05, 'rank ${k}: observed ${observed} vs expected ${expected}'
	}
}

fn test_zipf_monotone_mass() {
	seed([u32(0x1234ABCD), u32(0x9E3779B9)])
	n := 100
	mut per_rank := []i64{len: n, init: 0}
	for _ in 0 .. 20000 {
		k := zipf(s: 1.5, v: 1.0, n: n)!
		per_rank[k] += 1
	}
	mut bins := []i64{len: 4, init: 0}
	for k in 0 .. n {
		mut idx := 0
		if k <= 4 {
			idx = 0
		} else if k <= 9 {
			idx = 1
		} else if k <= 14 {
			idx = 2
		} else {
			idx = 3
		}
		bins[idx] += per_rank[k]
	}
	// bins have unequal widths (5, 5, 5 and 85 ranks), so compare per-rank mass
	assert bins[0] / 5 > bins[1] / 5, 'lower ranks must dominate'
	assert bins[1] / 5 > bins[2] / 5
	assert bins[2] / 5 > bins[3] / 85
}

fn test_zipf_deterministic_under_seed() {
	seed([u32(0x1234ABCD), u32(0x9E3779B9)])
	mut first := []int{}
	for _ in 0 .. 50 {
		first << zipf(s: 1.2, v: 1.0, n: 30)!
	}
	seed([u32(0x1234ABCD), u32(0x9E3779B9)])
	mut second := []int{}
	for _ in 0 .. 50 {
		second << zipf(s: 1.2, v: 1.0, n: 30)!
	}
	assert first == second, 'reseeding must reproduce the sequence'
}
