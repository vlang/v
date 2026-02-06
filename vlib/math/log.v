module math

const two54 = f64(1.80143985094819840000e+16)
const ivln10 = f64(4.34294481903251816668e-01)
const log10_2hi = f64(3.01029995663611771306e-01)
const log10_2lo = f64(3.69423907715893078616e-13)

// log_n returns log base b of x
pub fn log_n(x f64, b f64) f64 {
	y := log(x)
	z := log(b)
	return y / z
}

// log10 returns the decimal logarithm of x.
// The special cases are the same as for log.
// log10(10**N) = N  for N=0,1,...,22.
pub fn log10(x f64) f64 {
	// https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/ieee754/dbl-64/e_log10.c

	mut x_ := x
	mut hx := i64(f64_bits(x_))
	mut k := i32(0)
	if hx < i64(0x0010000000000000) {
		// x < 2**-1022
		if hx & 0x7fffffffffffffff == 0 {
			return inf(-1) // log(+-0)=-inf
		}
		if hx < 0 {
			return (x_ - x_) / (x_ - x_) // log(-#) = NaN
		}
		k = k - 54
		x_ *= two54 // subnormal number, scale up x
		hx = i64(f64_bits(x_))
	}

	// scale up resulted in a NaN number
	if hx >= u64(0x7ff0000000000000) {
		return x_ + x_
	}

	k = k + i32((u64((hx >> 52) - 1023)))
	i := i32((u64(k) & 0x8000000000000000) >> 63)
	hx = (hx & 0x000fffffffffffff) | (u64(0x3ff - i) << 52)
	y := f64(k + i)
	/*
	if FIX_INT_FP_CONVERT_ZERO && y == 0.0 {
		y = 0.0
	}
	*/
	x_ = f64_from_bits(u64(hx))
	z := y * log10_2lo + ivln10 * log(x_)
	return z + y * log10_2hi
}

// log2 returns the binary logarithm of x.
// The special cases are the same as for log.
pub fn log2(x f64) f64 {
	frac, exp := frexp(x)
	// Make sure exact powers of two give an exact answer.
	// Don't depend on log(0.5)*(1/ln2)+exp being exactly exp-1.
	if frac == 0.5 {
		return f64(exp - 1)
	}
	return log(frac) * (1.0 / ln2) + f64(exp)
}

// log1p returns log(1+x)
pub fn log1p(x f64) f64 {
	y := 1.0 + x
	z := y - 1.0
	return log(y) - (z - x) / y // cancels errors with IEEE arithmetic
}

// log_b returns the binary exponent of x.
//
// special cases are:
// log_b(±inf) = +inf
// log_b(0) = -inf
// log_b(nan) = nan
pub fn log_b(x f64) f64 {
	if x == 0 {
		return inf(-1)
	}
	if is_inf(x, 0) {
		return inf(1)
	}
	if is_nan(x) {
		return x
	}
	return f64(ilog_b_(x))
}

// ilog_b returns the binary exponent of x as an integer.
//
// special cases are:
// ilog_b(±inf) = max_i32
// ilog_b(0) = min_i32
// ilog_b(nan) = max_i32
pub fn ilog_b(x f64) int {
	if x == 0 {
		return int(min_i32)
	}
	if is_nan(x) {
		return int(max_i32)
	}
	if is_inf(x, 0) {
		return int(max_i32)
	}
	return ilog_b_(x)
}

// ilog_b returns the binary exponent of x. It assumes x is finite and
// non-zero.
fn ilog_b_(x_ f64) int {
	x, exp := normalize(x_)
	return int((f64_bits(x) >> shift) & mask) - bias + exp
}

// log returns the natural logarithm of x
//
// Method :
//   1. Argument Reduction: find k and f such that
//                      x = 2^k * (1+f),
//         where  sqrt(2)/2 < 1+f < sqrt(2) .
//
//   2. Approximation of log(1+f).
//      Let s = f/(2+f) ; based on log(1+f) = log(1+s) - log(1-s)
//               = 2s + 2/3 s**3 + 2/5 s**5 + .....,
//               = 2s + s*R
//      We use a special Remez algorithm on [0,0.1716] to generate
//      a polynomial of degree 14 to approximate R The maximum error
//      of this polynomial approximation is bounded by 2**-58.45. In
//      other words,
//                      2      4      6      8      10      12      14
//          R(z) ~ Lg1*s +Lg2*s +Lg3*s +Lg4*s +Lg5*s  +Lg6*s  +Lg7*s
//      (the values of Lg1 to Lg7 are listed in the program)
//      and
//          |      2          14          |     -58.45
//          | Lg1*s +...+Lg7*s    -  R(z) | <= 2
//          |                             |
//      Note that 2s = f - s*f = f - hfsq + s*hfsq, where hfsq = f*f/2.
//      In order to guarantee error in log below 1ulp, we compute log
//      by
//              log(1+f) = f - s*(f - R)        (if f is not too large)
//              log(1+f) = f - (hfsq - s*(hfsq+R)).     (better accuracy)
//
//      3. Finally,  log(x) = k*ln2 + log(1+f).
//                          = k*ln2_hi+(f-(hfsq-(s*(hfsq+R)+k*ln2_lo)))
//         Here ln2 is split into two floating point number:
//                      ln2_hi + ln2_lo,
//         where n*ln2_hi is always exact for |n| < 2000.
//
// Special cases:
//      log(x) is NaN with signal if x < 0 (including -inf) ;
//      log(+inf) is +inf; log(0) is -inf with signal;
//      log(NaN) is that NaN with no signal.
//
// Accuracy:
//      according to an error analysis, the error is always less than
//      1 ulp (unit in the last place).
pub fn log(a f64) f64 {
	ln2_hi := 6.93147180369123816490e-01 // 3fe62e42 fee00000
	ln2_lo := 1.90821492927058770002e-10 // 3dea39ef 35793c76
	l1 := 6.666666666666735130e-01 // 3FE55555 55555593
	l2 := 3.999999999940941908e-01 // 3FD99999 9997FA04
	l3 := 2.857142874366239149e-01 // 3FD24924 94229359
	l4 := 2.222219843214978396e-01 // 3FCC71C5 1D8E78AF
	l5 := 1.818357216161805012e-01 // 3FC74664 96CB03DE
	l6 := 1.531383769920937332e-01 // 3FC39A09 D078C69F
	l7 := 1.479819860511658591e-01 // 3FC2F112 DF3E5244

	x := a
	if is_nan(x) || is_inf(x, 1) {
		return x
	} else if x < 0 {
		return nan()
	} else if x == 0 {
		return inf(-1)
	}

	mut f1, mut ki := frexp(x)
	if f1 < sqrt2 / 2 {
		f1 *= 2
		ki--
	}

	f := f1 - 1
	k := f64(ki)

	// compute
	s := f / (2 + f)
	s2 := s * s
	s4 := s2 * s2
	t1 := s2 * (l1 + s4 * (l3 + s4 * (l5 + s4 * l7)))
	t2 := s4 * (l2 + s4 * (l4 + s4 * l6))
	r := t1 + t2
	hfsq := 0.5 * f * f
	return k * ln2_hi - ((hfsq - (s * (hfsq + r) + k * ln2_lo)) - f)
}
