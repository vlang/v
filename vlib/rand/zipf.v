module rand

import math

// ZipfConfig configures the Zipf distribution: rank k (0-based) has
// probability proportional to (v + k)^(-s), for k in 0..n-1.
pub struct ZipfConfig {
pub:
	// s is the exponent (must be > 0)
	s f64 = 1.5
	// v is the offset (must be >= 1)
	v f64 = 1.0
	// n is the number of ranks (must be >= 1)
	n int = 100
}

// Zipf is a Zipf-distributed variate generator. It builds the exact cumulative
// distribution once (O(n)) and draws by binary search (O(log n)), which keeps
// the match to the analytic probabilities easy to verify — Go's O(1)
// rejection-inversion trick is left as a future optimization.
pub struct Zipf {
mut:
	rng &PRNG
	cdf []f64
}

// new_zipf builds a generator for the given configuration. Invalid configs
// (s <= 0, v < 1, n < 1) are errors. Deterministic under the PRNG, so
// reseeding reproduces sequences.
pub fn (mut rng PRNG) new_zipf(conf ZipfConfig) !Zipf {
	if conf.s <= 0.0 {
		return error('rand: Zipf exponent s must be > 0')
	}
	if conf.v < 1.0 {
		return error('rand: Zipf offset v must be >= 1')
	}
	if conf.n < 1 {
		return error('rand: Zipf rank count n must be positive')
	}
	mut cdf := []f64{len: conf.n, init: 0.0}
	mut sum := 0.0
	for k in 0 .. conf.n {
		sum += math.pow(conf.v + f64(k), -conf.s)
		cdf[k] = sum
	}
	// normalize to 1.0
	for k in 0 .. conf.n {
		cdf[k] /= sum
	}
	return Zipf{
		rng: rng
		cdf: cdf
	}
}

// next returns one Zipf-distributed rank in 0..n-1.
pub fn (mut z Zipf) next() int {
	r := z.rng.f64()
	mut lo := 0
	mut hi := z.cdf.len - 1
	for lo < hi {
		mid := (lo + hi) / 2
		if r < z.cdf[mid] {
			hi = mid
		} else {
			lo = mid + 1
		}
	}
	return lo
}

// zipf draws one Zipf-distributed rank in 0..n-1 using the given RNG.
pub fn (mut rng PRNG) zipf(conf ZipfConfig) !int {
	mut z := rng.new_zipf(conf)!
	return z.next()
}

// zipf draws one Zipf-distributed rank in 0..n-1 using the default PRNG.
pub fn zipf(conf ZipfConfig) !int {
	return default_rng.zipf(conf)
}
