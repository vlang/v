module big

/*
for a detailed explanation on these internal functions and the algorithms they
are based on refer to https://github.com/vlang/v/pull/18461
*/

// internal struct to make passing montgomery values simpler
struct MontgomeryContext {
	n  Integer // |modulus|
	ni Integer // n^(-1)
	rr Integer // for conversions
}

// montgomery calculates a montgomery context for reductions to montgomery space based on
// the modulus provided in the integer `m`; assume m is odd and m != 0
fn (m Integer) montgomery() MontgomeryContext {
	$if debug {
		assert m != zero_int
		assert m.is_odd()
	}

	n := m.abs()
	b := u32(n.bit_len())

	return MontgomeryContext{
		n: n
		// r = 2^(log_2(n))
		// ri := multiplicative inverse of r in the ring Z/nZ
		// ri * r == 1 (mod n)
		// ni = ((ri * 2^(log_2(n))) - 1) / n
		ni: (one_int.left_shift(b).mod_inv(n).left_shift(b) - one_int) / n
		rr: one_int.left_shift(b * 2) % n
	}
}

// mont_odd calculates `a^x (mod m)`, where `m` is odd by reducing `a` to montgomery space
// and then exponentiating the value using the sliding window method and montgomery multiplication
// -----
// assumes a, x > 1 and m is odd
@[direct_array_access]
fn (a Integer) mont_odd(x Integer, m Integer) Integer {
	$if debug {
		assert a > one_int && x > one_int
		assert m.is_odd()
	}

	window := get_window_size(u32(x.bit_len()))

	mut table := []Integer{len: 1 << window}

	ctx := m.montgomery()
	aa := if a.signum < 0 || a.abs_cmp(m) >= 0 {
		a % m
	} else {
		a
	}

	table[0] = aa.to_mont(ctx)

	{
		d := table[0].mont_mul(table[0], ctx)
		for i := 1; i < table.len; i++ {
			table[i] = table[i - 1].mont_mul(d, ctx)
		}
	}
	mut r := if m.digits.last() & (1 << (32 - 1)) != 0 {
		mut rdigits := []u32{len: m.digits.len}

		rdigits[0] = -m.digits[0]
		for i := 1; i < m.digits.len; i++ {
			rdigits[i] = ~m.digits[i]
		}

		Integer{
			digits: rdigits
			signum: 1
		}
	} else {
		one_int.to_mont(ctx)
	}

	mut start := true
	mut wstart := x.bit_len() - 1
	mut wvalue := 0
	mut wend := 0

	for {
		if !x.get_bit(u32(wstart)) {
			if !start {
				r = r.mont_mul(r, ctx)
			}
			if wstart == 0 {
				break
			}
			wstart--
			continue
		}

		wvalue = 1
		wend = 0
		for i := 1; i < window; i++ {
			if wstart - i < 0 {
				break
			}
			if x.get_bit(u32(wstart - i)) {
				wvalue <<= (i - wend)
				wvalue |= 1
				wend = i
			}
		}

		j := wend + 1
		if !start {
			for i := 0; i < j; i++ {
				r = r.mont_mul(r, ctx)
			}
		}

		r = r.mont_mul(table[wvalue >> 1], ctx)

		wstart -= j
		wvalue = 0
		start = false
		if wstart < 0 {
			break
		}
	}

	return r.from_mont(ctx)
}

// mont_even calculates `a^x (mod m)` where `m` is even. This is done by factoring the modulus
// `m` into an odd integer `m1` and an even multiple of 2, which we'll call m2.
// We then calculate:
// 		x1 = a^x (mod m1)
//		x2 = a^x (mod m2)
//
// Exponentiation with the modulus `m1` can be done using the traditional montgomery method,
// whereas `m2` is done using binary exponentiation modulo `m2`, which is fast seeing as we
// simply mask the low bits.
//
// The result `y` then satisfies (where `==` denotes congruence):
//		y == x1 (mod m1)
//		y == x2 (mod m2)
//
// We then use the Chinese Remainder Theorem (mixed-radix conversion algorithm) to calculate `y`.
//		y = x1 + m1 * t
// where
//		t = (x2 - x1) * m1^(-1) (mod m2)
//
// The multiplicative inverse of m1 in Z/m2Z exists since it is odd, therefore we can safely
// use the unchecked internal function.
//
// See Montgomery Reduction with Even Modulus by Çetin Kaya Koç
// (https://cetinkayakoc.net/docs/j34.pdf)
// -----
// assumes a, x > 1 and m is even
@[direct_array_access]
fn (a Integer) mont_even(x Integer, m Integer) Integer {
	$if debug {
		assert a > one_int && x > one_int
		assert !m.is_odd()
	}

	m1, j := m.rsh_to_set_bit()
	m2 := one_int.left_shift(j)

	$if debug {
		assert m1 * m2 == m
		assert m1.is_odd() && !m2.is_odd()
	}

	mut x1 := a.mont_odd(x, m1)
	mut x2 := a.exp_binary(x, m2)

	m2n := u32(m2.bit_len()) - 1

	m1i := m1.mod_inv(m2)

	$if debug {
		assert (m1i * m1).mask_bits(m2n) == one_int
	}

	t1 := x1.mask_bits(m2n)
	t2 := x2.mask_bits(m2n)

	t := (if t2.abs_cmp(t1) >= 0 {
		(t2 - t1).mask_bits(m2n)
	} else {
		// (x2 - x1) % m2 = 1 + ((~((x2 % m2) - (x1 % m2))) % m2)
		(t1 - t2).abs().bitwise_not().mask_bits(m2n) + one_int
	} * m1i).mask_bits(m2n)

	return x1 + m1 * t
}

// exp_binary calculates `a^x (mod m)`, where m is a power of 2
// -----
// assumes a, x > 1 and m = 2^n
@[direct_array_access]
fn (a Integer) exp_binary(x Integer, m Integer) Integer {
	$if debug {
		assert a > one_int && x > one_int
		assert m.is_power_of_2()
	}

	n := u32(m.bit_len()) - 1

	window := get_window_size(u32(x.bit_len()))

	mut table := []Integer{len: 1 << window}

	// table[i] = a^i + 1, since a^0 is known to be 1, there is no point
	// in eventually multiplying by one, so the for loop part continues until it
	// meets a block starting with a set bit
	table[0] = a.mask_bits(n)

	d := (table[0] * table[0]).mask_bits(n)
	for i := 1; i < table.len; i++ {
		table[i] = (table[i - 1] * d).mask_bits(n)
	}

	mut r := one_int

	mut start := true
	mut wstart := x.bit_len() - 1
	mut wend := 0
	mut wvalue := 1

	for wstart >= 0 {
		if !x.get_bit(u32(wstart)) {
			// no point squaring while r = 1
			if !start {
				r = (r * r).mask_bits(n)
			}
			if wstart == 0 {
				break
			}
			wstart--
			continue
		}

		// the bit x[wstart] is now known to be 1, so no reason to check it again
		for i := 1; i < window; i++ {
			if wstart - i < 0 {
				break
			}
			if x.get_bit(u32(wstart - i)) {
				wvalue <<= (i - wend) // i - wend is the amount of 0 bits that have been read
				wvalue |= 1
				wend = i
			}
		}

		j := wend + 1
		// same as before; r has not been populated yet, so squaring wouldn't do anything
		if !start {
			for i := 0; i < j; i++ {
				r = (r * r).mask_bits(n)
			}
		}

		r = (r * table[wvalue >> 1]).mask_bits(n)

		wstart -= j
		wvalue = 1
		wend = 0
		start = false
	}

	return r.mask_bits(n)
}

// generally sticking to a window size of 4 for sliding window exponentiation
// works well as the table stays relatively small and the blocks aren't too large.
// though in terms of larger exponents it's faster to use larger windows (going over
// a window size of 6, would cause extremely large table sizes, hindering performance)
//
// 6 is already a large window to use, requiring 64 elements in the table, so we'll
// limit it to only the largest of exponents
//
// according to the paper on montgomery multiplication by Shay Gueron, for the
// case of the exponent being 512 bits a window size of 5 is considered optimal
@[inline]
fn get_window_size(n u32) int {
	return if n > 768 {
		6
	} else if n > 256 {
		5
	} else if n > 32 {
		4
	} else {
		3
	}
}

// mont_mul performs multiplication of two variables in montgomery
// space and reduces the result to montgomery space
fn (a Integer) mont_mul(b Integer, ctx MontgomeryContext) Integer {
	if (a.digits.len + b.digits.len) > 2 * ctx.n.digits.len {
		return zero_int
	}

	t := a * b

	return t.from_mont(ctx)
}

fn (a Integer) to_mont(ctx MontgomeryContext) Integer {
	return a.mont_mul(ctx.rr, ctx)
}

// See Fig. 1. "A Montgomery Reduction lemma" in
// Efficient Software Implementations of Modular Exponentiation by Shay Gueron
// (https://eprint.iacr.org/2011/239.pdf)
fn (a Integer) from_mont(ctx MontgomeryContext) Integer {
	log2n := u32(ctx.n.bit_len())

	r := (a + ((a.mask_bits(log2n) * ctx.ni).mask_bits(log2n) * ctx.n)).right_shift(log2n)

	return if r.abs_cmp(ctx.n) >= 0 {
		r - ctx.n
	} else {
		r
	}
}
