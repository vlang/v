module big

import math.bits

// mod_inv_digit returns the inverse of the odd digit `d` modulo `2^digit_bits`.
//
// Newton's iteration doubles the number of correct bits each step, starting
// from the fact that an odd number is its own inverse modulo 8.
fn mod_inv_digit(d u64) u64 {
	mut inv := d // correct modulo 8
	for _ in 0 .. 5 {
		inv = (inv * (2 - d * inv)) & max_digit
	}
	return inv
}

// mul_add_digit computes `a * b + t` and splits it into a `digit_bits` wide
// digit and the carry above it. The incoming carry is folded into `t` by the
// caller, which keeps this to a single widening multiply.
@[inline]
fn mul_add_digit(a u64, b u64, t u64) (u64, u64) {
	hi, lo := bits.mul_add_64(a, b, t)
	// Operands are `digit_bits` wide, so `hi` cannot reach the top four bits
	// and shifting it up to rejoin the overflow of `lo` is safe.
	return lo & max_digit, (hi << (64 - digit_bits)) | (lo >> digit_bits)
}

// digit_at returns the digit of `a` at index `i`, or zero past its end, so the
// operands can be shorter than the modulus without a separate branch.
@[direct_array_access; inline]
fn (a Integer) digit_at(i int) u64 {
	if i < a.digits.len {
		return a.digits[i]
	}
	return 0
}

// mont_mul returns `a * b * R^-1 (mod n)`, with both operands and the result in
// montgomery form.
//
// Multiplication and reduction are interleaved, so the double width product is
// never materialised: a single `s + 2` digit accumulator is updated in place and
// reduced one digit at a time. The straightforward formulation instead builds
// `a * b`, masks it, multiplies by the inverse, masks again, multiplies by the
// modulus, adds and shifts, allocating at every step.
@[direct_array_access]
fn (a Integer) mont_mul(b Integer, ctx MontgomeryContext) Integer {
	s := ctx.n.digits.len
	if a.digits.len > s || b.digits.len > s {
		return zero_int
	}
	n := ctx.n.digits
	mut t := []u64{len: s + 2}
	for i in 0 .. s {
		bi := b.digit_at(i)

		// t += a * b[i]
		mut carry := u64(0)
		for j in 0 .. s {
			t[j], carry = mul_add_digit(a.digit_at(j), bi, t[j] + carry)
		}
		v := t[s] + carry
		t[s] = v & max_digit
		t[s + 1] = v >> digit_bits

		// Choose m so that the low digit of t + m * n vanishes, then divide by
		// the base by shifting the whole accumulator down one digit.
		m := (t[0] * ctx.n0inv) & max_digit
		carry = 0
		for j in 0 .. s {
			digit, next := mul_add_digit(m, n[j], t[j] + carry)
			carry = next
			if j > 0 {
				t[j - 1] = digit
			}
		}
		w := t[s] + carry
		t[s - 1] = w & max_digit
		t[s] = t[s + 1] + (w >> digit_bits)
		t[s + 1] = 0
	}

	// The accumulator spans s + 1 digits: when the modulus fills its top digit,
	// twice it does not fit in s, so the carry above them is part of the value.
	mut used := s + 1
	for used > 0 && t[used - 1] == 0 {
		used--
	}
	if used == 0 {
		return zero_int
	}
	// The result stays below 2n, so one conditional subtraction suffices.
	result := integer_from_digits(t, used)
	if result.abs_cmp(ctx.n) >= 0 {
		return result - ctx.n
	}
	return result
}

// integer_from_digits builds a positive Integer from the first `used` digits of
// `digits`, which must not end in a zero digit.
@[inline]
fn integer_from_digits(digits []u64, used int) Integer {
	return Integer{
		digits: digits[..used].clone()
		signum: 1
	}
}
