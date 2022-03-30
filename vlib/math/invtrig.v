module math

// The original C code, the long comment, and the constants below were
// from http://netlib.sandia.gov/cephes/cmath/atan.c, available from
// http://www.netlib.org/cephes/ctgz.
// The go code is a version of the original C.
//
// atan.c
// Inverse circular tangent (arctangent)
//
// SYNOPSIS:
// double x, y, atan()
// y = atan( x )
//
// DESCRIPTION:
// Returns radian angle between -pi/2.0 and +pi/2.0 whose tangent is x.
//
// Range reduction is from three intervals into the interval from zero to 0.66.
// The approximant uses a rational function of degree 4/5 of the form
// x + x**3 P(x)/Q(x).
//
// ACCURACY:
// Relative error:
// arithmetic   domain    # trials  peak     rms
// DEC       -10, 10   50000     2.4e-17  8.3e-18
// IEEE      -10, 10   10^6      1.8e-16  5.0e-17
//
// Cephes Math Library Release 2.8:  June, 2000
// Copyright 1984, 1987, 1989, 1992, 2000 by Stephen L. Moshier
//
// The readme file at http://netlib.sandia.gov/cephes/ says:
// Some software in this archive may be from the book _Methods and
// Programs for Mathematical Functions_ (Prentice-Hall or Simon & Schuster
// International, 1989) or from the Cephes Mathematical Library, a
// commercial product. In either event, it is copyrighted by the author.
// What you see here may be used freely but it comes with no support or
// guarantee.
//
// The two known misprints in the book are repaired here in the
// source listings for the gamma function and the incomplete beta
// integral.
//
// Stephen L. Moshier
// moshier@na-net.ornl.gov
// pi/2.0 = PIO2 + morebits
// tan3pio8 = tan(3*pi/8)

const (
	morebits = 6.123233995736765886130e-17
	tan3pio8 = 2.41421356237309504880
)

// xatan evaluates a series valid in the range [0, 0.66].
[inline]
fn xatan(x f64) f64 {
	xatan_p0 := -8.750608600031904122785e-01
	xatan_p1 := -1.615753718733365076637e+01
	xatan_p2 := -7.500855792314704667340e+01
	xatan_p3 := -1.228866684490136173410e+02
	xatan_p4 := -6.485021904942025371773e+01
	xatan_q0 := 2.485846490142306297962e+01
	xatan_q1 := 1.650270098316988542046e+02
	xatan_q2 := 4.328810604912902668951e+02
	xatan_q3 := 4.853903996359136964868e+02
	xatan_q4 := 1.945506571482613964425e+02
	mut z := x * x
	z = z * ((((xatan_p0 * z + xatan_p1) * z + xatan_p2) * z + xatan_p3) * z + xatan_p4) / (((((z +
		xatan_q0) * z + xatan_q1) * z + xatan_q2) * z + xatan_q3) * z + xatan_q4)
	z = x * z + x
	return z
}

// satan reduces its argument (known to be positive)
// to the range [0, 0.66] and calls xatan.
[inline]
fn satan(x f64) f64 {
	if x <= 0.66 {
		return xatan(x)
	}
	if x > math.tan3pio8 {
		return pi / 2.0 - xatan(1.0 / x) + f64(math.morebits)
	}
	return pi / 4 + xatan((x - 1.0) / (x + 1.0)) + 0.5 * f64(math.morebits)
}

// atan returns the arctangent, in radians, of x.
//
// special cases are:
// atan(±0) = ±0
// atan(±inf) = ±pi/2.0
pub fn atan(x f64) f64 {
	if x == 0 {
		return x
	}
	if x > 0 {
		return satan(x)
	}
	return -satan(-x)
}

// atan2 returns the arc tangent of y/x, using
// the signs of the two to determine the quadrant
// of the return value.
//
// special cases are (in order):
// atan2(y, nan) = nan
// atan2(nan, x) = nan
// atan2(+0, x>=0) = +0
// atan2(-0, x>=0) = -0
// atan2(+0, x<=-0) = +pi
// atan2(-0, x<=-0) = -pi
// atan2(y>0, 0) = +pi/2.0
// atan2(y<0, 0) = -pi/2.0
// atan2(+inf, +inf) = +pi/4
// atan2(-inf, +inf) = -pi/4
// atan2(+inf, -inf) = 3pi/4
// atan2(-inf, -inf) = -3pi/4
// atan2(y, +inf) = 0
// atan2(y>0, -inf) = +pi
// atan2(y<0, -inf) = -pi
// atan2(+inf, x) = +pi/2.0
// atan2(-inf, x) = -pi/2.0
pub fn atan2(y f64, x f64) f64 {
	// special cases
	if is_nan(y) || is_nan(x) {
		return nan()
	}
	if y == 0.0 {
		if x >= 0 && !signbit(x) {
			return copysign(0, y)
		}
		return copysign(pi, y)
	}
	if x == 0.0 {
		return copysign(pi / 2.0, y)
	}
	if is_inf(x, 0) {
		if is_inf(x, 1) {
			if is_inf(y, 0) {
				return copysign(pi / 4, y)
			}
			return copysign(0, y)
		}
		if is_inf(y, 0) {
			return copysign(3.0 * pi / 4.0, y)
		}
		return copysign(pi, y)
	}
	if is_inf(y, 0) {
		return copysign(pi / 2.0, y)
	}
	// Call atan and determine the quadrant.
	q := atan(y / x)
	if x < 0 {
		if q <= 0 {
			return q + pi
		}
		return q - pi
	}
	return q
}

/*
Floating-point arcsine and arccosine.

	They are implemented by computing the arctangent
	after appropriate range reduction.
*/

// asin returns the arcsine, in radians, of x.
//
// special cases are:
// asin(±0) = ±0
// asin(x) = nan if x < -1 or x > 1
pub fn asin(x_ f64) f64 {
	mut x := x_
	if x == 0.0 {
		return x // special case
	}
	mut sign := false
	if x < 0.0 {
		x = -x
		sign = true
	}
	if x > 1.0 {
		return nan() // special case
	}
	mut temp := sqrt(1.0 - x * x)
	if x > 0.7 {
		temp = pi / 2.0 - satan(temp / x)
	} else {
		temp = satan(x / temp)
	}
	if sign {
		temp = -temp
	}
	return temp
}

// acos returns the arccosine, in radians, of x.
//
// special case is:
// acos(x) = nan if x < -1 or x > 1
[inline]
pub fn acos(x f64) f64 {
	if (x < -1.0) || (x > 1.0) {
		return nan()
	}
	if x > 0.5 {
		return f64(2.0) * asin(sqrt(0.5 - 0.5 * x))
	}
	mut z := pi / f64(4.0) - asin(x)
	z = z + math.morebits
	z = z + pi / f64(4.0)
	return z
}
