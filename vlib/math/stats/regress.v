module stats

import math

// correlation returns the Pearson product-moment correlation coefficient of
// data1 and data2, in [-1, 1]. It is the V-idiomatic equivalent of Python's
// statistics.correlation. Constant input (zero variance) returns nan, as the
// coefficient is undefined there.
pub fn correlation[T](data1 []T, data2 []T) T {
	if data1.len != data2.len {
		panic('stats.correlation: slices must have equal length')
	}
	if data1.len == 0 {
		return T(math.nan())
	}
	m1 := mean(data1)
	m2 := mean(data2)
	sd1 := population_stddev_mean(data1, m1)
	sd2 := population_stddev_mean(data2, m2)
	if sd1 == T(0) || sd2 == T(0) {
		return T(math.nan())
	}
	return covariance_mean(data1, data2, m1, m2) / (sd1 * sd2)
}

// LinearRegression is the least-squares fit of y = intercept + slope * x.
pub struct LinearRegression[T] {
pub:
	intercept T
	slope     T
}

// linear_regression fits a straight line to the paired samples by ordinary
// least squares. It is the V-idiomatic equivalent of Python's
// statistics.linear_regression. Identical x values (or a single sample) are
// an error, since the slope is then undefined.
pub fn linear_regression[T](data1 []T, data2 []T) !LinearRegression[T] {
	if data1.len != data2.len {
		return error('stats: linear_regression slices must have equal length')
	}
	n := data1.len
	if n < 2 {
		return error('stats: linear_regression needs at least 2 samples')
	}
	sx := sum_f64(data1)
	sy := sum_f64(data2)
	sxx := sum_squares(data1)
	sxy := sum_products(data1, data2)
	nf := f64(n)
	den := nf * sxx - sx * sx
	if den == 0 {
		return error('stats: linear_regression slope is undefined for identical x')
	}
	slope := (nf * sxy - sx * sy) / den
	intercept := (sy - slope * sx) / nf
	return LinearRegression[T]{
		intercept: T(intercept)
		slope:     T(slope)
	}
}

// predict returns the fitted value at x.
pub fn (lr &LinearRegression[T]) predict(x T) T {
	return lr.intercept + lr.slope * x
}

// quantiles returns the n-1 cut points dividing sorted_data into n
// equal-probability intervals (Python's statistics.quantiles, default method).
pub fn quantiles[T](sorted_data []T, n int) ![]T {
	if n < 1 {
		return error('stats: quantiles needs n >= 1')
	}
	if sorted_data.len < 2 {
		return error('stats: quantiles needs at least 2 data points')
	}
	mut out := []T{cap: n - 1}
	for i in 1 .. n {
		out << quantile(sorted_data, T(f64(i) / f64(n)))!
	}
	return out
}

// multimode returns every value occurring with the highest frequency, in
// ascending order for numeric input (Python's statistics.multimode).
pub fn multimode[T](data []T) []T {
	if data.len == 0 {
		return []T{}
	}
	mut counts := map[string]int{}
	mut values := []T{}
	mut best := 0
	for v in data {
		key := typeof_key(v)
		if key !in counts {
			values << v
			counts[key] = 0
		}
		counts[key]++
		if counts[key] > best {
			best = counts[key]
		}
	}
	mut out := []T{}
	for v in values {
		if counts[typeof_key(v)] == best {
			out << v
		}
	}
	return out
}

// NormalDist is the normal (Gaussian) distribution with the given mean and
// standard deviation, the V-idiomatic equivalent of Python's
// statistics.NormalDist.
pub struct NormalDist {
pub:
	mean f64
	std  f64
}

// new_normal_dist creates a normal distribution (std must be positive).
pub fn new_normal_dist(mean f64, std f64) !NormalDist {
	if std < 0 {
		return error('stats: normal distribution needs std >= 0')
	}
	return NormalDist{mean, std}
}

// pdf returns the probability density at x.
pub fn (nd &NormalDist) pdf(x f64) f64 {
	if nd.std == 0 {
		return if x == nd.mean { math.inf(1) } else { 0.0 }
	}
	z := (x - nd.mean) / nd.std
	return math.exp(-0.5 * z * z) / (nd.std * math.sqrt(2 * math.pi))
}

// cdf returns the cumulative probability P(X <= x).
pub fn (nd &NormalDist) cdf(x f64) f64 {
	if nd.std == 0 {
		return if x < nd.mean { 0.0 } else { 1.0 }
	}
	z := (x - nd.mean) / (nd.std * math.sqrt(2.0))
	return 0.5 * (1.0 + math.erf(z))
}

// inv_cdf returns the value x with P(X <= x) == p (p must be in (0, 1)).
// It uses the Acklam inverse-normal approximation followed by one
// Halley refinement step, accurate to ~1e-15.
pub fn (nd &NormalDist) inv_cdf(p f64) !f64 {
	if p <= 0.0 || p >= 1.0 {
		return error('stats: inv_cdf needs 0 < p < 1')
	}
	return nd.mean + nd.std * inv_normal(p)
}

// sum_f64 sums the samples as f64.
fn sum_f64[T](data []T) f64 {
	mut s := 0.0
	for v in data {
		s += f64(v)
	}
	return s
}

// sum_squares sums the squares as f64.
fn sum_squares[T](data []T) f64 {
	mut s := 0.0
	for v in data {
		f := f64(v)
		s += f * f
	}
	return s
}

// sum_products sums the pairwise products as f64.
fn sum_products[T](a []T, b []T) f64 {
	mut s := 0.0
	for i in 0 .. a.len {
		s += f64(a[i]) * f64(b[i])
	}
	return s
}

// typeof_key renders a value as a map key so multimode can count any type
// that stringifies.
fn typeof_key[T](v T) string {
	return '${v}'
}

// a_coeff, b_coeff, c_coeff and d_coeff are the Acklam inverse-normal-CDF
// coefficients (rational approximations).
const a_coeff = [
	f64(-3.969683028665376e+01),
	f64(2.209460984245205e+02),
	f64(-2.759285104469687e+02),
	f64(1.383577518672690e+02),
	f64(-3.066479806614716e+01),
	f64(2.506628277459239e+00),
]
const b_coeff = [
	f64(-5.447609879822406e+01),
	f64(1.615858368580409e+02),
	f64(-1.556989798598866e+02),
	f64(6.680131188771972e+01),
	f64(-1.328068155288572e+01),
]
const c_coeff = [
	f64(-7.784894002430293e-03),
	f64(-3.223964580411365e-01),
	f64(-2.400758277161838e+00),
	f64(-2.549732539343734e+00),
	f64(4.374664141464968e+00),
	f64(2.938163982698783e+00),
]
const d_coeff = [
	f64(7.784695709041462e-03),
	f64(3.224671290700398e-01),
	f64(2.445134137142996e+00),
	f64(3.754408661907416e+00),
]

// horner evaluates a polynomial given in descending-power coefficient order.
fn horner(coeffs []f64, x f64) f64 {
	mut acc := f64(0)
	for c in coeffs {
		acc = acc * x + c
	}
	return acc
}

// inv_normal is the inverse standard-normal CDF (Acklam + one Halley step).
fn inv_normal(p f64) f64 {
	p_low := 0.02425
	mut x := 0.0
	if p < p_low {
		q := math.sqrt(-2.0 * math.log(p))
		x = horner(c_coeff, q) / (q * horner(d_coeff, q) + 1.0)
	} else if p <= 1.0 - p_low {
		q := p - 0.5
		r := q * q
		x = q * horner(a_coeff, r) / (r * horner(b_coeff, r) + 1.0)
	} else {
		q := math.sqrt(-2.0 * math.log(1.0 - p))
		x = -horner(c_coeff, q) / (q * horner(d_coeff, q) + 1.0)
	}
	// one Halley refinement step, using the exact CDF through erfc
	e := 0.5 * math.erfc(-x / math.sqrt(2.0)) - p
	u := e * math.sqrt(2.0 * math.pi) * math.exp(x * x / 2.0)
	return x - u / (1.0 + x * u / 2.0)
}
