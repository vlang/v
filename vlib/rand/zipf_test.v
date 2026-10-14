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

fn test_zipf_range() {
	seed([u32(0x1234ABCD), u32(0x9E3779B9)])
	for _ in 0 .. 200 {
		k := zipf(s: 1.5, v: 1.0, n: 50)!
		assert k >= 0 && k <= 49, 'rank must stay in 0..n-1'
	}
}

fn test_zipf_rejects_invalid_config() {
	configs := [ZipfConfig{ s: 0.0 }, ZipfConfig{ v: 0.5 }, ZipfConfig{ n: 0 }]
	messages := ['s must be > 0', 'v must be >= 1', 'n must be positive']
	for i, conf in configs {
		if _ := zipf(conf) {
			assert false, 'invalid configuration must be rejected: ${conf}'
		} else {
			assert err.msg().contains(messages[i])
		}
	}
}

fn test_zipf_rejects_nonfinite_config() {
	for value in [math.nan(), math.inf(1), math.inf(-1)] {
		for conf in [ZipfConfig{ s: value }, ZipfConfig{ v: value }] {
			if _ := zipf(conf) {
				assert false, 'nonfinite configuration must be rejected'
			} else {
				assert err.msg().contains('must be finite')
			}
		}
	}
}

fn test_zipf_retains_relative_weights_at_extreme_parameters() {
	mut rng := get_current_rng()
	mut steep := rng.new_zipf(s: 1000.0, v: 10.0, n: 10)!
	// All raw (10+k)^(-1000) weights underflow, but rank zero dominates.
	for _ in 0 .. 100 {
		assert steep.next() == 0
	}
	mut huge := rng.new_zipf(s: 1e308, v: 1e308, n: 4)!
	// (1 + k/v)^(-v) approaches exp(-k), rather than a uniform law.
	expected_first := 1.0 / (1.0 + math.exp(-1.0) + math.exp(-2.0) + math.exp(-3.0))
	assert math.abs(huge.cdf[0] - expected_first) < 1e-12
	assert huge.cdf[3] == 1.0
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
