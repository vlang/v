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

// median returns the median of `values` (sorted copy).
pub fn median(values []f64) f64 {
	if values.len == 0 {
		return 0
	}
	mut s := values.clone()
	s.sort()
	if s.len % 2 == 1 {
		return s[s.len / 2]
	}
	return (s[s.len / 2 - 1] + s[s.len / 2]) / 2.0
}

// quantile returns the p-quantile (0..1) of `values` using linear interpolation.
pub fn quantile(values []f64, p f64) f64 {
	if values.len == 0 {
		return 0
	}
	if p <= 0 {
		return min(values)
	}
	if p >= 1 {
		return max(values)
	}
	mut s := values.clone()
	s.sort()
	pos := p * f64(s.len - 1)
	lo := int(pos)
	hi := if lo + 1 < s.len { lo + 1 } else { lo }
	frac := pos - f64(lo)
	return s[lo] + (s[hi] - s[lo]) * frac
}
