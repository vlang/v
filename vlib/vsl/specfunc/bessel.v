// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module specfunc

import vsl.math

const (
	HUGE = 1e+300
	two_m27 = 1.0 / (1<<27) // 2**-27 0x3e40000000000000
	two_m13 = 1.0 / (1<<13) // 2**-13 0x3f20000000000000
	two_m29 = 1.0 / (1<<29) // 2**-29 0x3e10000000000000
	two_m54 = 1.0 / (1<<54) // 2**-54 0x3c90000000000000
	two129 = 1<<129 // 2**129 0x4800000000000000
	two302 = 1<<302 // 2**302 0x52D0000000000000
	// J0R0/J0S0 on [0, 2]
	J0R02 = 1.56249999999999947958e-02 // 0x3F8FFFFFFFFFFFFD
	J0R03 = -1.89979294238854721751e-04 // 0xBF28E6A5B61AC6E9
	J0R04 = 1.82954049532700665670e-06 // 0x3EBEB1D10C503919
	J0R05 = -4.61832688532103189199e-09 // 0xBE33D5E773D63FCE
	J0S01 = 1.56191029464890010492e-02 // 0x3F8FFCE882C8C2A4
	J0S02 = 1.16926784663337450260e-04 // 0x3F1EA6D2DD57DBF4
	J0S03 = 5.13546550207318111446e-07 // 0x3EA13B54CE84D5A9
	J0S04 = 1.16614003333790000205e-09 // 0x3E1408BCF4745D8F
	J1R00 = -6.25000000000000000000e-02 // 0xBFB0000000000000
	J1R01 = 1.40705666955189706048e-03 // 0x3F570D9F98472C61
	J1R02 = -1.59955631084035597520e-05 // 0xBEF0C5C6BA169668
	J1R03 = 4.96727999609584448412e-08 // 0x3E6AAAFA46CA0BD9
	J1S01 = 1.91537599538363460805e-02 // 0x3F939D0B12637E53
	J1S02 = 1.85946785588630915560e-04 // 0x3F285F56B9CDF664
	J1S03 = 1.17718464042623683263e-06 // 0x3EB3BFF8333F8498
	J1S04 = 5.04636257076217042715e-09 // 0x3E35AC88C97DFF2C
	J1S05 = 1.23542274426137913908e-11 // 0x3DAB2ACFCFB97ED8
	Y0U00 = -7.38042951086872317523e-02 // 0xBFB2E4D699CBD01F
	Y0U01 = 1.76666452509181115538e-01 // 0x3FC69D019DE9E3FC
	Y0U02 = -1.38185671945596898896e-02 // 0xBF8C4CE8B16CFA97
	Y0U03 = 3.47453432093683650238e-04 // 0x3F36C54D20B29B6B
	Y0U04 = -3.81407053724364161125e-06 // 0xBECFFEA773D25CAD
	Y0U05 = 1.95590137035022920206e-08 // 0x3E5500573B4EABD4
	Y0U06 = -3.98205194132103398453e-11 // 0xBDC5E43D693FB3C8
	Y0V01 = 1.27304834834123699328e-02 // 0x3F8A127091C9C71A
	Y0V02 = 7.60068627350353253702e-05 // 0x3F13ECBBF578C6C1
	Y0V03 = 2.59150851840457805467e-07 // 0x3E91642D7FF202FD
	Y0V04 = 4.41110311332675467403e-10 // 0x3DFE50183BD6D9EF
	Y1U00 = -1.96057090646238940668e-01 // 0xBFC91866143CBC8A
	Y1U01 = 5.04438716639811282616e-02 // 0x3FA9D3C776292CD1
	Y1U02 = -1.91256895875763547298e-03 // 0xBF5F55E54844F50F
	Y1U03 = 2.35252600561610495928e-05 // 0x3EF8AB038FA6B88E
	Y1U04 = -9.19099158039878874504e-08 // 0xBE78AC00569105B8
	Y1V00 = 1.99167318236649903973e-02 // 0x3F94650D3F4DA9F0
	Y1V01 = 2.02552581025135171496e-04 // 0x3F2A8C896C257764
	Y1V02 = 1.35608801097516229404e-06 // 0x3EB6C05A894E8CA6
	Y1V03 = 6.22741452364621501295e-09 // 0x3E3ABF1D5BA69A86
	Y1V04 = 1.66559246207992079114e-11 // 0x3DB25039DACA772A
)
// bessel_j0 returns the order-zero Bessel function of the first kind.
//
// special cases are:
// bessel_j0(±inf) = 0
// bessel_j0(0) = 1
// bessel_j0(nan) = nan
pub fn bessel_j0(x_ f64) f64 {
	mut x := x_
	if math.is_nan(x) {
		return x
	}
	if math.is_inf(x, 0) {
		return 0
	}
	if x == 0.0 {
		return f64(1.0)
	}
	x = math.abs(x)
	if x >= 2.0 {
		s,c := math.sincos(x)
		mut ss := s - c
		mut cc := s + c
		// make sure x+x does not overflow
		if x < math.max_f64 / 2.0 {
			z := -math.cos(x + x)
			if s * c < 0.0 {
				cc = z / ss
			}
			else {
				ss = z / cc
			}
		}
		// j0(x) = 1/sqrt(pi) * (P(0,x)*cc - Q(0,x)*ss) / sqrt(x)
		// y0(x) = 1/sqrt(pi) * (P(0,x)*ss + Q(0,x)*cc) / sqrt(x)
		mut z := f64(0)
		if x > two129 {
			// |x| > ~6.8056e+38
			z = (f64(1.0) / math.sqrt_pi) * cc / math.sqrt(x)
		}
		else {
			u := pzero(x)
			v := qzero(x)
			z = (f64(1.0) / math.sqrt_pi) * (u * cc - v * ss) / math.sqrt(x)
		}
		return z // |x| >= 2.0
	}
	if x < two_m13 {
		// |x| < ~1.2207e-4
		if x < two_m27 {
			return f64(1.0) // |x| < ~7.4506e-9
		}
		return f64(1.0) - 0.25 * x * x // ~7.4506e-9 < |x| < ~1.2207e-4
	}
	z := x * x
	r := z * (J0R02 + z * (J0R03 + z * (J0R04 + z * J0R05)))
	s := f64(1.0) + z * (J0S01 + z * (J0S02 + z * (J0S03 + z * J0S04)))
	if x < 1.0 {
		return f64(1.0) + z * (-0.25 + (r / s)) // |x| < 1.00
	}
	u := f64(0.5) * x
	return (f64(1.0) + u) * (f64(1.0) - u) + z * (r / s) // 1.0 < |x| < 2.0
}
// bessel_j1 returns the order-one Bessel function of the first kind.
//
// special cases are:
// bessel_j1(±inf) = 0
// bessel_j1(nan) = nan
pub fn bessel_j1(x_ f64) f64 {
	mut x := x_
	if math.is_nan(x) {
		return x
	}
	if math.is_inf(x, 0) || x == 0 {
		return f64(0)
	}
	mut sign := false
	if x < 0.0 {
		x = -x
		sign = true
	}
	if x >= 2 {
		s,c := math.sincos(x)
		mut ss := -s - c
		mut cc := s - c
		// make sure x+x does not overflow
		if x < math.max_f64 / 2 {
			z := math.cos(x + x)
			if s * c > 0 {
				cc = z / ss
			}
			else {
				ss = z / cc
			}
		}
		// j1(x) = 1/sqrt(pi) * (P(1,x)*cc - Q(1,x)*ss) / sqrt(x)
		// y1(x) = 1/sqrt(pi) * (P(1,x)*ss + Q(1,x)*cc) / sqrt(x)
		mut z := f64(0)
		if x > two129 {
			z = (f64(1.0) / math.sqrt_pi) * cc / math.sqrt(x)
		}
		else {
			u := pone(x)
			v := qone(x)
			z = (f64(1.0) / math.sqrt_pi) * (u * cc - v * ss) / math.sqrt(x)
		}
		if sign {
			return -z
		}
		return z
	}
	if x < two_m27 {
		// |x|<2**-27
		return f64(0.5) * x // inexact if x!=0 necessary
	}
	mut z := x * x
	mut r := z * (J1R00 + z * (J1R01 + z * (J1R02 + z * J1R03)))
	s := f64(1.0) + z * (J1S01 + z * (J1S02 + z * (J1S03 + z * (J1S04 + z * J1S05))))
	r *= x
	z = f64(0.5) * x + r / s
	if sign {
		return -z
	}
	return z
}

// bessel_jn returns the order-n Bessel function of the first kind.
//
// special cases are:
// bessel_jn(n, ±inf) = 0
// bessel_jn(n, nan) = nan
pub fn bessel_jn(n_ int, x_ f64) f64 {
	mut n := n_
	mut x := x_
	if math.is_nan(x) {
		return x
	}
	if math.is_inf(x, 0) {
		return f64(0)
	}
	// J(-n, x) = (-1)**n * J(n, x), J(n, -x) = (-1)**n * J(n, x)
	// Thus, J(-n, x) = J(n, -x)
	if n == 0 {
		return bessel_j0(x)
	}
	if x == 0 {
		return f64(0)
	}
	if n < 0 {
		n = -n
		x = -x
	}
	if n == 1 {
		return bessel_j1(x)
	}
	mut sign := false
	if x < 0 {
		x = -x
		if n & 1 == 1 {
			sign = true // odd n and negative x
		}
	}
	mut b := f64(0)
	if f64(n) <= x {
		// Safe to use J(n+1,x)=2n/x *J(n,x)-J(n-1,x)
		if x >= two302 {
			// x > 2**302
			// (x >> n**2)
			// Jn(x) = cos(x-(2n+1)*pi/4)*sqrt(2/x*pi)
			// Yn(x) = sin(x-(2n+1)*pi/4)*sqrt(2/x*pi)
			// Let s=sin(x), c=cos(x),
			// xn=x-(2n+1)*pi/4, sqt2 = sqrt(2),then
			//
			// n    sin(xn)*sqt2    cos(xn)*sqt2
			// ----------------------------------
			// 0     s-c             c+s
			// 1    -s-c            -c+s
			// 2    -s+c            -c-s
			// 3     s+c             c-s
			mut temp := f64(0)
			s,c := math.sincos(x)
			n3 := n & 3
			if n3 == 0 {
				temp = c + s
			}
			if n3 == 1 {
				temp = -c + s
			}
			if n3 == 2 {
				temp = -c - s
			}
			if n3 == 3 {
				temp = c - s
			}
			b = (f64(1.0) / math.sqrt_pi) * temp / math.sqrt(x)
		}
		else {
			b = bessel_j1(x)
			mut i := 1
			mut a := bessel_j0(x)
			for ; i < n; i++ {
				a_ := a
				a = b
				b = b * (f64(i + i) / x) - a_ // avoid underflow
			}
		}
	}
	else {
		if x < two_m29 {
			// x < 2**-29
			// x is tiny, return the first Taylor expansion of J(n,x)
			// J(n,x) = 1/n!*(x/2)**n  - ...
			if n > 33 {
				// underflow
				b = 0
			}
			else {
				temp := x * 0.5
				b = temp
				mut a := f64(1.0)
				for i := 2; i <= n; i++ {
					a *= f64(i) // a = n!
					b *= temp // b = (x/2)**n
				}
				b /= a
			}
		}
		else {
			// use backward recurrence
			// x      x**2      x**2
			// J(n,x)/J(n-1,x) =  ----   ------   ------   .....
			// 2n  - 2(n+1) - 2(n+2)
			//
			// 1      1        1
			// (for large x)   =  ----  ------   ------   .....
			// 2n   2(n+1)   2(n+2)
			// -- - ------ - ------ -
			// x     x         x
			//
			// Let w = 2n/x and h=2/x, then the above quotient
			// is equal to the continued fraction:
			// 1
			// = -----------------------
			// 1
			// w - -----------------
			// 1
			// w+h - ---------
			// w+2h - ...
			//
			// To determine how many terms needed, let
			// Q(0) = w, Q(1) = w(w+h) - 1,
			// Q(k) = (w+k*h)*Q(k-1) - Q(k-2),
			// When Q(k) > 1e4	good for single
			// When Q(k) > 1e9	good for double
			// When Q(k) > 1e17	good for quadruple
			// determine k
			w := f64(n + n) / x
			h := f64(2.0) / x
			mut q0 := w
			mut z := w + h
			mut q1 := w * z - 1
			mut k := 1
			for q1 < 1e+9 {
				k++
				z += h
				q0_ := q0
				q0 = q1
				q1 = z * q1 - q0_
			}
			m := n + n
			mut t := f64(0)
			for i := 2 * (n + k); i >= m; i -= 2 {
				t = f64(1.0) / (f64(i) / x - t)
			}
			mut a := t
			b = 1
			// estimate log((2/x)**n*n!) = n*log(2/x)+n*ln(n)
			// Hence, if n*(log(2n/x)) > ...
			// single 8.8722839355e+01
			// double 7.09782712893383973096e+02
			// long double 1.1356523406294143949491931077970765006170e+04
			// then recurrent value may overflow and the result is
			// likely underflow to zero
			mut tmp := f64(n)
			v := f64(2.0) / x
			tmp = tmp * math.log(math.abs(v * tmp))
			if tmp < 7.09782712893383973096e+02 {
				for i := n - 1; i > 0; i-- {
					di := f64(i + i)
					a_ := a
					a = b
					b = b * di / x - a_
				}
			}
			else {
				for i := n - 1; i > 0; i-- {
					di := f64(i + i)
					a_ := a
					a = b
					b = b * di / x - a_
					// scale b to avoid spurious overflow
					if b > 1e+100 {
						a /= b
						t /= b
						b = 1
					}
				}
			}
			b = t * bessel_j0(x) / b
		}
	}
	if sign {
		return -b
	}
	return b
}

// bessel_y0 returns the order-zero Bessel function of the second kind.
//
// special cases are:
// bessel_y0(+inf) = 0
// bessel_y0(0) = -inf
// bessel_y0(x < 0) = nan
// bessel_y0(nan) = nan
pub fn bessel_y0(x f64) f64 {
	// special cases
	if x < 0 || math.is_nan(x) {
		return math.nan()
	}
	if math.is_inf(x, 1) {
		return f64(0)
	}
	if x == 0 {
		return math.inf(-1)
	}
	if x >= 2.0 {
		// |x| >= 2.0
		// y0(x) = sqrt(2/(pi*x))*(p0(x)*sin(x0)+q0(x)*cos(x0))
		// where x0 = x-pi/4
		// Better formula:
		// cos(x0) = cos(x)cos(pi/4)+sin(x)sin(pi/4)
		// =  1/sqrt(2) * (sin(x) + cos(x))
		// sin(x0) = sin(x)cos(3pi/4)-cos(x)sin(3pi/4)
		// =  1/sqrt(2) * (sin(x) - cos(x))
		// To avoid cancellation, use
		// sin(x) +- cos(x) = -cos(2x)/(sin(x) -+ cos(x))
		// to compute the worse one.
		s,c := math.sincos(x)
		mut ss := s - c
		mut cc := s + c
		// j0(x) = 1/sqrt(pi) * (P(0,x)*cc - Q(0,x)*ss) / sqrt(x)
		// y0(x) = 1/sqrt(pi) * (P(0,x)*ss + Q(0,x)*cc) / sqrt(x)
		// make sure x+x does not overflow
		if x < math.max_f64 / 2.0 {
			z := -math.cos(x + x)
			if s * c < 0.0 {
				cc = z / ss
			}
			else {
				ss = z / cc
			}
		}
		mut z := f64(0)
		if x > two129 {
			// |x| > ~6.8056e+38
			z = (f64(1.0) / math.sqrt_pi) * ss / math.sqrt(x)
		}
		else {
			u := pzero(x)
			v := qzero(x)
			z = (f64(1.0) / math.sqrt_pi) * (u * ss + v * cc) / math.sqrt(x)
		}
		return z // |x| >= 2.0
	}
	if x <= two_m27 {
		return Y0U00 + (f64(2.0) / math.pi) * math.log(x) // |x| < ~7.4506e-9
	}
	z := x * x
	u := Y0U00 + z * (Y0U01 + z * (Y0U02 + z * (Y0U03 + z * (Y0U04 + z * (Y0U05 + z * Y0U06)))))
	v := f64(1.0) + z * (Y0V01 + z * (Y0V02 + z * (Y0V03 + z * Y0V04)))
	return u / v + (f64(2.0) / math.pi) * bessel_j0(x) * math.log(x) // ~7.4506e-9 < |x| < 2.0
}
// bessel_y1 returns the order-one Bessel function of the second kind.
//
// special cases are:
// bessel_y1(+inf) = 0
// bessel_y1(0) = -inf
// bessel_y1(x < 0) = nan
// bessel_y1(nan) = nan
pub fn bessel_y1(x f64) f64 {
	if x < 0 || math.is_nan(x) {
		return math.nan()
	}
	if math.is_inf(x, 1) {
		return f64(0)
	}
	if x == 0 {
		return math.inf(-1)
	}
	if x >= 2.0 {
		s,c := math.sincos(x)
		mut ss := -s - c
		mut cc := s - c
		// make sure x+x does not overflow
		if x < math.max_f64 / 2.0 {
			z := math.cos(x + x)
			if s * c > 0.0 {
				cc = z / ss
			}
			else {
				ss = z / cc
			}
		}
		// y1(x) = sqrt(2/(pi*x))*(p1(x)*sin(x0)+q1(x)*cos(x0))
		// where x0 = x-3pi/4
		// Better formula:
		// cos(x0) = cos(x)cos(3pi/4)+sin(x)sin(3pi/4)
		// =  1/sqrt(2) * (sin(x) - cos(x))
		// sin(x0) = sin(x)cos(3pi/4)-cos(x)sin(3pi/4)
		// = -1/sqrt(2) * (cos(x) + sin(x))
		// To avoid cancellation, use
		// sin(x) +- cos(x) = -cos(2x)/(sin(x) -+ cos(x))
		// to compute the worse one.
		mut z := f64(0)
		if x > two129 {
			z = (f64(1.0) / math.sqrt_pi) * ss / math.sqrt(x)
		}
		else {
			u := pone(x)
			v := qone(x)
			z = (f64(1.0) / math.sqrt_pi) * (u * ss + v * cc) / math.sqrt(x)
		}
		return z
	}
	if x <= two_m54 {
		// x < 2**-54
		return -(f64(2.0) / math.pi) / x
	}
	z := x * x
	u := Y1U00 + z * (Y1U01 + z * (Y1U02 + z * (Y1U03 + z * Y1U04)))
	v := f64(1.0) + z * (Y1V00 + z * (Y1V01 + z * (Y1V02 + z * (Y1V03 + z * Y1V04))))
	return x * (u / v) + (2.0 / math.pi) * (bessel_j1(x) * math.log(x) - 1.0 / x)
}

// bessel_yn returns the order-n Bessel function of the second kind.
//
// special cases are:
// bessel_yn(n, +inf) = 0
// bessel_yn(n ≥ 0, 0) = -inf
// bessel_yn(n < 0, 0) = +inf if n is odd, -inf if n is even
// bessel_yn(n, x < 0) = nan
// bessel_yn(n, nan) = nan
pub fn bessel_yn(n_ int, x f64) f64 {
	mut n := n_
	if x < 0.0 || math.is_nan(x) {
		return math.nan()
	}
	if math.is_inf(x, 1) {
		return f64(0)
	}
	if n == 0 {
		return bessel_y0(x)
	}
	if x == 0 {
		if n < 0 && n & 1 == 1 {
			return math.inf(1)
		}
		return math.inf(-1)
	}
	mut sign := false
	if n < 0 {
		n = -n
		if n & 1 == 1 {
			sign = true // sign true if n < 0 && |n| odd
		}
	}
	if n == 1 {
		if sign {
			return -bessel_y1(x)
		}
		return bessel_y1(x)
	}
	mut b := f64(0)
	if x >= two302 {
		// x > 2**302
		// (x >> n**2)
		// bessel_jn(x) = cos(x-(2n+1)*pi/4)*sqrt(2/x*pi)
		// bessel_yn(x) = sin(x-(2n+1)*pi/4)*sqrt(2/x*pi)
		// Let s=sin(x), c=cos(x),
		// xn=x-(2n+1)*pi/4, sqt2 = sqrt(2),then
		//
		// n	sin(xn)*sqt2	cos(xn)*sqt2
		// ----------------------------------
		// 0	 s-c		 c+s
		// 1	-s-c 		-c+s
		// 2	-s+c		-c-s
		// 3	 s+c		 c-s
		mut temp := f64(0)
		s,c := math.sincos(x)
		n3 := n & 3
		if n3 == 0 {
			temp = s - c
		}
		if n3 == 1 {
			temp = -s - c
		}
		if n3 == 2 {
			temp = -s + c
		}
		if n3 == 3 {
			temp = s + c
		}
		b = (f64(1.0) / math.sqrt_pi) * temp / math.sqrt(x)
	}
	else {
		mut a := bessel_y0(x)
		b = bessel_y1(x)
		// quit if b is -inf
		for i := 1; i < n && !math.is_inf(b, -1); i++ {
			a_ := a
			a = b
			b = (f64(i + i) / x) * b - a_
		}
	}
	if sign {
		return -b
	}
	return b
}

// The asymptotic expansions of pzero is
// 1 - 9/128 s**2 + 11025/98304 s**4 - ..., where s = 1/x.
// For x >= 2, We approximate pzero by
// pzero(x) = 1 + (R/S)
// where  R = pJ0R0 + pR1*s**2 + pR2*s**4 + ... + pR5*s**10
// S = 1 + pJ0S0*s**2 + ... + pS4*s**10
// and
// | pzero(x)-1-R/S | <= 2  ** ( -60.26)
pub fn pzero(x f64) f64 {
	mut p := []f64
	mut q := []f64
	if x >= 8.0 {
		p = P0R8
		q = P0S8
	}
	else if x >= 4.5454 {
		p = P0R5
		q = P0S5
	}
	else if x >= 2.8571 {
		p = P0R3
		q = P0S3
	}
	else if x >= 2 {
		p = P0R2
		q = P0S2
	}
	z := f64(1.0) / (x * x)
	r := p[0] + z * (p[1] + z * (p[2] + z * (p[3] + z * (p[4] + z * p[5]))))
	s := f64(1.0) + z * (q[0] + z * (q[1] + z * (q[2] + z * (q[3] + z * q[4]))))
	return f64(1.0) + r / s
}

// For x >= 8, the asymptotic expansions of pone is
// 1 + 15/128 s**2 - 4725/2**15 s**4 - ..., where s = 1/x.
// We approximate pone by
// pone(x) = 1 + (R/S)
// where R = pr0 + pr1*s**2 + pr2*s**4 + ... + pr5*s**10
// S = 1 + ps0*s**2 + ... + ps4*s**10
// and
// | pone(x)-1-R/S | <= 2**(-60.06)
pub fn pone(x f64) f64 {
	mut p := []f64
	mut q := []f64
	if x >= 8.0 {
		p = P1R8
		q = P1S8
	}
	else if x >= 4.5454 {
		p = P1R5
		q = P1S5
	}
	else if x >= 2.8571 {
		p = P1R3
		q = P1S3
	}
	else if x >= 2 {
		p = P1R2
		q = P1S2
	}
	z := f64(1.0) / (x * x)
	r := p[0] + z * (p[1] + z * (p[2] + z * (p[3] + z * (p[4] + z * p[5]))))
	s := f64(1.0) + z * (q[0] + z * (q[1] + z * (q[2] + z * (q[3] + z * q[4]))))
	return f64(1.0) + r / s
}

// For x >= 8, the asymptotic expansions of qzero is
// -1/8 s + 75/1024 s**3 - ..., where s = 1/x.
// We approximate pzero by
// qzero(x) = s*(-1.25 + (R/S))
// where R = qJ0R0 + qR1*s**2 + qR2*s**4 + ... + qR5*s**10
// S = 1 + qJ0S0*s**2 + ... + qS5*s**12
// and
// | qzero(x)/s +1.25-R/S | <= 2**(-61.22)
pub fn qzero(x f64) f64 {
	mut p := []f64
	mut q := []f64
	if x >= 8.0 {
		p = Q0R8
		q = Q0S8
	}
	else if x >= 4.5454 {
		p = Q0R5
		q = Q0S5
	}
	else if x >= 2.8571 {
		p = Q0R3
		q = Q0S3
	}
	else if x >= 2 {
		p = Q0R2
		q = Q0S2
	}
	z := f64(1.0) / (x * x)
	r := p[0] + z * (p[1] + z * (p[2] + z * (p[3] + z * (p[4] + z * p[5]))))
	s := f64(1.0) + z * (q[0] + z * (q[1] + z * (q[2] + z * (q[3] + z * (q[4] + z * q[5])))))
	return (f64(-0.125) + r / s) / x
}

// For x >= 8, the asymptotic expansions of qone is
// 3/8 s - 105/1024 s**3 - ..., where s = 1/x.
// We approximate qone by
// qone(x) = s*(0.375 + (R/S))
// where R = qr1*s**2 + qr2*s**4 + ... + qr5*s**10
// S = 1 + qs1*s**2 + ... + qs6*s**12
// and
// | qone(x)/s -0.375-R/S | <= 2**(-61.13)
pub fn qone(x f64) f64 {
	mut p := []f64
	mut q := []f64
	if x >= 8 {
		p = Q1R8
		q = Q1S8
	}
	else if x >= 4.5454 {
		p = Q1R5
		q = Q1S5
	}
	else if x >= 2.8571 {
		p = Q1R3
		q = Q1S3
	}
	else if x >= 2 {
		p = Q1R2
		q = Q1S2
	}
	z := f64(1.0) / (x * x)
	r := p[0] + z * (p[1] + z * (p[2] + z * (p[3] + z * (p[4] + z * p[5]))))
	s := f64(1.0) + z * (q[0] + z * (q[1] + z * (q[2] + z * (q[3] + z * (q[4] + z * q[5])))))
	return (f64(0.375) + r / s) / x
}
