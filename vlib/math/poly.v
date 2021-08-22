module math

import math.internal

fn poly_n_eval(c []f64, n int, x f64) f64 {
	if c.len == 0 {
		panic('coeficients can not be empty')
	}
	len := int(min(c.len, n))
	mut ans := c[len - 1]
	for e in c[..len - 1] {
		ans = e + x * ans
	}
	return ans
}

fn poly_n_1_eval(c []f64, n int, x f64) f64 {
	if c.len == 0 {
		panic('coeficients can not be empty')
	}
	len := int(min(c.len, n)) - 1
	mut ans := c[len - 1]
	for e in c[..len - 1] {
		ans = e + x * ans
	}
	return ans
}

[inline]
fn poly_eval(c []f64, x f64) f64 {
	return poly_n_eval(c, c.len, x)
}

[inline]
fn poly_1_eval(c []f64, x f64) f64 {
	return poly_n_1_eval(c, c.len, x)
}

// data for a Chebyshev series over a given interval
struct ChebSeries {
pub:
	c     []f64 // coefficients
	order int   // order of expansion
	a     f64   // lower interval point
	b     f64   // upper interval point
}

fn (cs ChebSeries) eval_e(x f64) (f64, f64) {
	mut d := 0.0
	mut dd := 0.0
	y := (2.0 * x - cs.a - cs.b) / (cs.b - cs.a)
	y2 := 2.0 * y
	mut e_ := 0.0
	mut temp := 0.0
	for j := cs.order; j >= 1; j-- {
		temp = d
		d = y2 * d - dd + cs.c[j]
		e_ += abs(y2 * temp) + abs(dd) + abs(cs.c[j])
		dd = temp
	}
	temp = d
	d = y * d - dd + 0.5 * cs.c[0]
	e_ += abs(y * temp) + abs(dd) + 0.5 * abs(cs.c[0])
	return d, f64(internal.f64_epsilon) * e_ + abs(cs.c[cs.order])
}
