// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

// NOTE: mini_math.v exists, so that we can avoid `import math`,
// just for the math.log and math.sqrt functions needed for the
// non uniform random number redistribution functions.
// Importing math is relatively heavy, both in terms of compilation
// speed (more source to process), and in terms of increases in the
// generated executable sizes (if the rest of the program does not use
// math already; many programs do not need math, for example the
// compiler itself does not, while needing random number generation.

const sqrt2 = 1.41421356237309504880168872420969807856967187537694807317667974

[inline]
fn msqrt(a f64) f64 {
	if a == 0 {
		return a
	}
	mut x := a
	z, ex := frexp(x)
	w := x
	// approximate square root of number between 0.5 and 1
	// relative error of approximation = 7.47e-3
	x = 4.173075996388649989089e-1 + 5.9016206709064458299663e-1 * z // adjust for odd powers of 2
	if (ex & 1) != 0 {
		x *= rand.sqrt2
	}
	x = scalbn(x, ex >> 1)
	// newton iterations
	x = 0.5 * (x + w / x)
	x = 0.5 * (x + w / x)
	x = 0.5 * (x + w / x)
	return x
}

// a simplified approximation (without the edge cases), see math.log
fn mlog(a f64) f64 {
	ln2_lo := 1.90821492927058770002e-10
	ln2_hi := 0.693147180369123816490
	l1 := 0.6666666666666735130
	l2 := 0.3999999999940941908
	l3 := 0.2857142874366239149
	l4 := 0.2222219843214978396
	l5 := 0.1818357216161805012
	l6 := 0.1531383769920937332
	l7 := 0.1479819860511658591
	x := a
	mut f1, mut ki := frexp(x)
	if f1 < rand.sqrt2 / 2 {
		f1 *= 2
		ki--
	}
	f := f1 - 1
	k := f64(ki)
	s := f / (2 + f)
	s2 := s * s
	s4 := s2 * s2
	t1 := s2 * (l1 + s4 * (l3 + s4 * (l5 + s4 * l7)))
	t2 := s4 * (l2 + s4 * (l4 + s4 * l6))
	r := t1 + t2
	hfsq := 0.5 * f * f
	return k * ln2_hi - ((hfsq - (s * (hfsq + r) + k * ln2_lo)) - f)
}

fn frexp(x f64) (f64, int) {
	mut y := f64_bits(x)
	ee := int((y >> 52) & 0x7ff)
	if ee == 0 {
		if x != 0.0 {
			x1p64 := f64_from_bits(u64(0x43f0000000000000))
			z, e_ := frexp(x * x1p64)
			return z, e_ - 64
		}
		return x, 0
	} else if ee == 0x7ff {
		return x, 0
	}
	e_ := ee - 0x3fe
	y &= u64(0x800fffffffffffff)
	y |= u64(0x3fe0000000000000)
	return f64_from_bits(y), e_
}

fn scalbn(x f64, n_ int) f64 {
	mut n := n_
	x1p1023 := f64_from_bits(u64(0x7fe0000000000000))
	x1p53 := f64_from_bits(u64(0x4340000000000000))
	x1p_1022 := f64_from_bits(u64(0x0010000000000000))

	mut y := x
	if n > 1023 {
		y *= x1p1023
		n -= 1023
		if n > 1023 {
			y *= x1p1023
			n -= 1023
			if n > 1023 {
				n = 1023
			}
		}
	} else if n < -1022 {
		/*
		make sure final n < -53 to avoid double
	rounding in the subnormal range
		*/
		y *= x1p_1022 * x1p53
		n += 1022 - 53
		if n < -1022 {
			y *= x1p_1022 * x1p53
			n += 1022 - 53
			if n < -1022 {
				n = -1022
			}
		}
	}
	return y * f64_from_bits(u64((0x3ff + n)) << 52)
}

[inline]
fn f64_from_bits(b u64) f64 {
	return *unsafe { &f64(&b) }
}

[inline]
fn f64_bits(f f64) u64 {
	return *unsafe { &u64(&f) }
}
