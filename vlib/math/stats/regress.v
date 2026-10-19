module stats

import math

// correlation returns the Pearson correlation coefficient as f64, including
// for integer samples. Empty or constant input returns NaN; lengths must match.
pub fn correlation[T](data1 []T, data2 []T) f64 {
	if data1.len != data2.len { panic('stats.correlation: slices must have equal length') }
	if data1.len == 0 { return math.nan() }
	_, _, sxx, syy, sxy, _, _ := centered_sums(data1, data2)
	if sxx == 0 || syy == 0 { return math.nan() }
	// Divide in two steps to avoid overflowing the product of the deviations.
	return math.max(-1.0, math.min(1.0, (sxy / math.sqrt(sxx)) / math.sqrt(syy)))
}

// LinearRegression is the least-squares fit of y = intercept + slope * x.
// Coefficients are f64 even when the input samples are integers.
pub struct LinearRegression[T] {
pub:
	intercept f64
	slope     f64
}

// linear_regression fits paired numeric samples by ordinary least squares.
// Fewer than two samples or identical x values return an error.
pub fn linear_regression[T](data1 []T, data2 []T) !LinearRegression[T] {
	if data1.len != data2.len {
		return error('stats: linear_regression slices must have equal length')
	}
	if data1.len < 2 { return error('stats: linear_regression needs at least 2 samples') }
	mx, my, sxx, _, sxy, scale_x, scale_y := centered_sums(data1, data2)
	if sxx == 0 { return error('stats: linear_regression slope is undefined for identical x') }
	factor := sxy / sxx
	scale_ratio := scale_y / scale_x
	slope := if math.is_inf(scale_ratio, 0) {
		(factor / scale_x) * scale_y
	} else {
		factor * scale_ratio
	}
	return LinearRegression[T]{ intercept: my - slope * mx, slope: slope }
}

// predict returns the fitted value at x as f64.
pub fn (lr &LinearRegression[T]) predict(x T) f64 {
	return lr.intercept + lr.slope * f64(x)
}

// quantiles returns n-1 cut points as f64 using inclusive linear interpolation
// between the ordered samples. Input must be sorted in ascending order.
pub fn quantiles[T](sorted_data []T, n int) ![]f64 {
	if n < 1 { return error('stats: quantiles needs n >= 1') }
	if sorted_data.len < 2 { return error('stats: quantiles needs at least 2 data points') }
	mut data := []f64{cap: sorted_data.len}
	for value in sorted_data { data << f64(value) }
	mut out := []f64{cap: n - 1}
	for i in 1 .. n { out << quantile(data, f64(i) / f64(n))! }
	return out
}

// multimode returns every value occurring with the highest frequency, in
// first-seen order. Values are grouped by equality, not by their string form.
pub fn multimode[T](data []T) []T {
	if data.len == 0 {
		return []T{}
	}
	mut counts := []int{}
	mut values := []T{}
	mut best := 0
	for value in data {
		mut index := -1
		for i, existing in values {
			if existing == value {
				index = i
				break
			}
		}
		if index == -1 {
			index = values.len
			values << value
			counts << 0
		}
		counts[index]++
		if counts[index] > best { best = counts[index] }
	}
	mut out := []T{}
	for i, value in values { if counts[i] == best { out << value } }
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

// new_normal_dist creates a distribution with finite mean and std >= 0.
// A zero standard deviation represents a point mass at the mean.
pub fn new_normal_dist(mean f64, std f64) !NormalDist {
	if std < 0 || math.is_nan(std) || math.is_inf(std, 0) || math.is_nan(mean) || math.is_inf(mean, 0) {
		return error('stats: normal distribution needs finite mean and std >= 0')
	}
	return NormalDist{mean, std}
}

// pdf returns the probability density at x.
pub fn (nd &NormalDist) pdf(x f64) f64 {
	if math.is_nan(x) { return x }
	if nd.std == 0 {
		return if x == nd.mean { math.inf(1) } else { 0.0 }
	}
	z := nd.standardized_value(x)
	exponent := -0.5 * z * z
	density := math.exp(exponent)
	if density < 2.2250738585072014e-308 && nd.std < 1 {
		// Scaling a subnormal density can restore a normal result; use logarithms first.
		return math.exp(exponent - math.log(nd.std) - 0.5 * math.log(2 * math.pi))
	}
	return density / math.sqrt(2 * math.pi) / nd.std
}

// cdf returns the cumulative probability P(X <= x).
pub fn (nd &NormalDist) cdf(x f64) f64 {
	if math.is_nan(x) { return x }
	if nd.std == 0 {
		return if x < nd.mean { 0.0 } else { 1.0 }
	}
	z := nd.standardized_value(x) / math.sqrt(2.0)
	return 0.5 * math.erfc(-z)
}

// inv_cdf returns the value x with P(X <= x) == p (p must be in (0, 1)).
// It uses the Acklam inverse-normal approximation followed by one
// Halley refinement step where its exponential remains finite.
pub fn (nd &NormalDist) inv_cdf(p f64) !f64 {
	if math.is_nan(p) || p <= 0.0 || p >= 1.0 {
		return error('stats: inv_cdf needs 0 < p < 1')
	}
	z := inv_normal(p)
	product := nd.std * z
	if math.is_inf(product, 0) {
		// Combine in standard units when the mean can cancel product overflow.
		return nd.std * (z + nd.mean / nd.std)
	}
	return nd.mean + product
}

// standardized_value avoids subtracting opposite finite extremes before scaling.
fn (nd &NormalDist) standardized_value(x f64) f64 {
	difference := x - nd.mean
	if math.is_inf(difference, 0) && !math.is_inf(x, 0) {
		return x / nd.std - nd.mean / nd.std
	}
	return difference / nd.std
}

// centered_sums shifts samples before scaling their centered products,
// preserving small variations at large offsets and avoiding square overflow.
fn centered_sums[T](a []T, b []T) (f64, f64, f64, f64, f64, f64, f64) {
	mut x0 := f64(a[0])
	mut y0 := f64(b[0])
	// Opposite finite extremes can overflow their difference; use zero there.
	for i in 0 .. a.len {
		if math.is_inf(f64(a[i]) - x0, 0) { x0 = 0.0 }
		if math.is_inf(f64(b[i]) - y0, 0) { y0 = 0.0 }
	}
	mut scale_x := 0.0
	mut scale_y := 0.0
	for i in 0 .. a.len {
		scale_x = math.max(scale_x, math.abs(f64(a[i]) - x0))
		scale_y = math.max(scale_y, math.abs(f64(b[i]) - y0))
	}
	if scale_x == 0 { scale_x = 1.0 }
	if scale_y == 0 { scale_y = 1.0 }
	nf := f64(a.len)
	mut dx := 0.0
	mut dy := 0.0
	for i in 0 .. a.len {
		dx += ((f64(a[i]) - x0) / scale_x) / nf
		dy += ((f64(b[i]) - y0) / scale_y) / nf
	}
	mut sxx := 0.0
	mut syy := 0.0
	mut sxy := 0.0
	for i in 0 .. a.len {
		x := (f64(a[i]) - x0) / scale_x - dx
		y := (f64(b[i]) - y0) / scale_y - dy
		sxx += x * x
		syy += y * y
		sxy += x * y
	}
	return x0 + dx * scale_x, y0 + dy * scale_y, sxx, syy, sxy, scale_x, scale_y
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
	// At extreme tails exp(x*x/2) overflows; retain the finite approximation.
	if math.abs(x) >= 37.0 { return x }
	// one Halley refinement step, using the exact CDF through erfc
	e := 0.5 * math.erfc(-x / math.sqrt(2.0)) - p
	u := e * math.sqrt(2.0 * math.pi) * math.exp(x * x / 2.0)
	return x - u / (1.0 + x * u / 2.0)
}
