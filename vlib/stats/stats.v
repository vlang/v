module stats

import math

// mean returns the arithmetic mean of `values`.
pub fn mean(values []f64) f64 {
	if values.len == 0 {
		return 0
	}
	mut sum := 0.0
	for v in values {
		sum += v
	}
	return sum / f64(values.len)
}

// variance returns the population variance of `values`.
pub fn variance(values []f64) f64 {
	if values.len == 0 {
		return 0
	}
	m := mean(values)
	mut s := 0.0
	for v in values {
		d := v - m
		s += d * d
	}
	return s / f64(values.len)
}

// stddev returns the population standard deviation of `values`.
pub fn stddev(values []f64) f64 {
	return math.sqrt(variance(values))
}

// min returns the smallest value in `values`.
pub fn min(values []f64) f64 {
	if values.len == 0 {
		return 0
	}
	mut m := values[0]
	for v in values {
		if v < m {
			m = v
		}
	}
	return m
}

// max returns the largest value in `values`.
pub fn max(values []f64) f64 {
	if values.len == 0 {
		return 0
	}
	mut m := values[0]
	for v in values {
		if v > m {
			m = v
		}
	}
	return m
}
