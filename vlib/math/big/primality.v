module big

import rand

// Every prime below 64, used to answer small inputs exactly.
const tiny_primes = [u64(2), 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61]!

// Products of the odd primes below 60, split so that each factor stays inside
// a 32 bit word. Trial division reduces `x` once per product instead of once
// per prime.
const primes_a = u64(3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 37)
const primes_b = u64(29 * 31 * 41 * 43 * 47 * 53)

// The bases that make Miller-Rabin deterministic below
// 3317044064679887385961981. Later rounds draw random bases instead: fixed
// bases beyond this point add nothing against a crafted input.
const deterministic_bases = [u64(2), 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]!

// is_probably_prime reports whether `x` is prime, with a probability of a
// composite being misreported below `1 / 4^rounds`.
//
// The test is deterministic for `x` below 3317044064679887385961981, where a
// fixed set of bases is provably sufficient, and probabilistic above it.
// Passing `rounds <= 0` selects a sane default (40 rounds, the value used for
// key generation in most cryptographic libraries).
//
// Note that a `true` result is a statement of probability, not certainty.
// Do not use this to validate an attacker-supplied modulus without also
// checking the surrounding protocol.
pub fn (x Integer) is_probably_prime(rounds int) bool {
	if x.signum <= 0 {
		return false
	}
	if x.bit_len() <= 6 {
		// Answer exactly from the table of primes below 64.
		return u64(x.int()) in tiny_primes
	}
	if !x.is_odd() {
		return false
	}
	// Trial division by the primes below 60. Rather than reducing `x` once per
	// prime, reduce it once per product and finish with cheap word-sized
	// modulos. `x` is larger than every prime involved, so a zero remainder
	// always means composite.
	ra := u32(x.mod_small(primes_a))
	rb := u32(x.mod_small(primes_b))
	if ra % 3 == 0 || ra % 5 == 0 || ra % 7 == 0 || ra % 11 == 0 || ra % 13 == 0 || ra % 17 == 0 || ra % 19 == 0 || ra % 23 == 0 || ra % 37 == 0 || rb % 29 == 0 || rb % 31 == 0 || rb % 41 == 0 || rb % 43 == 0 || rb % 47 == 0 || rb % 53 == 0 {
		return false
	}
	reps := if rounds <= 0 { 40 } else { rounds }
	return x.miller_rabin(reps) && x.lucas_probable_prime()
}

// mod_small returns `|x| mod m` for a modulus that fits in 32 bits.
//
// This walks the digit array directly instead of going through general
// division, so it allocates nothing. Each 60-bit digit is processed in two
// 30-bit halves so that every intermediate product stays below 2^64.
@[direct_array_access]
fn (x Integer) mod_small(m u64) u64 {
	if m == 0 {
		panic('math.big: division by zero')
	}
	if x.signum == 0 {
		return 0
	}
	half_bits := digit_bits / 2
	half_mask := (u64(1) << half_bits) - 1
	// 2^30 mod m, the factor applied when shifting in the next half digit.
	shift_mod := (u64(1) << half_bits) % m
	mut r := u64(0)
	for i := x.digits.len - 1; i >= 0; i-- {
		digit := x.digits[i]
		r = (r * shift_mod + (digit >> half_bits)) % m
		r = (r * shift_mod + (digit & half_mask)) % m
	}
	return r
}

// miller_rabin runs `rounds` iterations of the Miller-Rabin probabilistic
// primality test on `x`, which is required to be odd and greater than the
// largest entry of `small_primes`.
//
// Write `x - 1` as `d * 2^s` with `d` odd. For a prime `x` and any base `a`
// coprime to it, either `a^d == 1 (mod x)`, or `a^(d * 2^r) == -1 (mod x)` for
// some `0 <= r < s`. A base that satisfies neither is a witness that `x` is
// composite; at most a quarter of the bases can fail to witness a composite,
// which is where the error bound comes from.
//
// The first rounds use the small prime bases, which makes the result
// deterministic for `x < 3317044064679887385961981`.
fn (x Integer) miller_rabin(rounds int) bool {
	x_minus_one := x - one_int
	mut d := x_minus_one
	mut s := 0
	for !d.is_odd() {
		d = d.right_shift(1)
		s++
	}
	// `x` is odd here, so every round can share one montgomery context.
	// Deriving it costs a modular inverse, which is why it is hoisted out of
	// the loop rather than recomputed by each `big_mod_pow` call.
	ctx := x.montgomery()
	for i in 0 .. rounds {
		a := if i < deterministic_bases.len {
			integer_from_u64(deterministic_bases[i])
		} else {
			random_base(x)
		}
		if !x.passes_miller_rabin_round(a, d, s, x_minus_one, ctx) {
			return false
		}
	}
	return true
}

// passes_miller_rabin_round reports whether the base `a` fails to witness that
// `x` is composite, given the decomposition `x - 1 == d * 2^s` and a montgomery
// context derived from `x`.
fn (x Integer) passes_miller_rabin_round(a Integer, d Integer, s int, x_minus_one Integer, ctx MontgomeryContext) bool {
	mut y := a.mont_odd_with_ctx(d, x, ctx)
	if y == one_int || y == x_minus_one {
		return true
	}
	for _ in 1 .. s {
		y = (y * y) % x
		if y == x_minus_one {
			return true
		}
		if y == one_int {
			// A non-trivial square root of 1 exists modulo `x`, so `x` is
			// composite and no later squaring can rescue this base.
			return false
		}
	}
	return false
}

// random_base returns a uniformly distributed integer in `[2, x - 2]`, used as
// a Miller-Rabin base once the fixed small-prime bases are exhausted.
@[direct_array_access]
fn random_base(x Integer) Integer {
	// Build the candidate from the digit array directly: one `u64` of entropy
	// per 60-bit digit, rather than a byte array that throws away most of
	// every random word.
	mut digits := []u64{len: x.digits.len}
	for i in 0 .. digits.len {
		digits[i] = rand.u64() & max_digit
	}
	// Trim leading zero digits, which the Integer invariants forbid.
	mut n := digits.len
	for n > 0 && digits[n - 1] == 0 {
		n--
	}
	if n == 0 {
		return two_int
	}
	digits.trim(n)
	candidate := Integer{
		digits: digits
		signum: 1
	}
	// Map into [2, x - 2].
	return candidate % (x - integer_from_int(3)) + two_int
}

// lucas_probable_prime runs the "almost extra strong" Lucas probable prime
// test on `x`, using Baillie-OEIS parameter selection (method C).
//
// Combined with a base-2 Miller-Rabin round this forms the Baillie-PSW test.
// No composite is known to pass it, despite decades of searching, and a prize
// still stands for anyone who finds one.
//
// Method C searches for the smallest `p >= 3` with `Jacobi(p^2 - 4 / x) == -1`
// and fixes `Q = 1`. Holding Q at 1 removes the running power of Q that the
// general formulation has to carry, and only the V sequence is needed, so the
// whole test reduces to repeated squaring with no modular halving.
//
// `x` must be odd and greater than 1.
//
// References:
//   Baillie and Wagstaff, "Lucas Pseudoprimes", Math. Comp. 35(152), 1980.
//   Grantham, "Frobenius Pseudoprimes", Math. Comp. 70(234), 2000, Thm 2.3.
//   Baillie, "Extra strong Lucas pseudoprimes", OEIS A217719.
fn (x Integer) lucas_probable_prime() bool {
	// Find the smallest p >= 3 such that Jacobi(p^2 - 4 / x) == -1.
	mut p := u64(3)
	for {
		d := integer_from_u64(p * p - 4)
		j := jacobi(d, x)
		if j == -1 {
			break
		}
		if j == 0 {
			// d = p^2 - 4 = (p - 2)(p + 2), and the search started at p - 2 == 1,
			// so the shared factor has to be p + 2. That makes `x` prime only if
			// `x` is p + 2 itself.
			return x == integer_from_u64(p + 2)
		}
		if p == 40 {
			// A perfect square has Jacobi(d / x) == 1 for every d coprime to it,
			// so the search would never end. Non-squares are expected to succeed
			// within a handful of attempts, which makes this the right moment to
			// pay for one integer square root.
			root := x.isqrt()
			if root * root == x {
				return false
			}
		}
		if p > 10000 {
			// Believed impossible for a non-square. Fail closed rather than spin.
			return false
		}
		p++
	}

	// Arrange x + 1 == s * 2^r with s odd. The identity x + 1 is used rather
	// than x - 1 because Jacobi(d / x) is -1 here.
	mut s := x + one_int
	mut r := 0
	for !s.is_odd() {
		s = s.right_shift(1)
		r++
	}

	big_p := integer_from_u64(p)
	x_minus_two := x - two_int

	// Build V(s) by doubling the subscript from the top bit of s down, using
	//     V(2k)     = V(k)^2 - 2
	//     V(2k + 1) = V(k) V(k + 1) - P
	// which follow from V(j + k) = V(j)V(k) - V(k - j) with Q == 1.
	mut vk := two_int // V(0) = 2
	mut vk1 := big_p // V(1) = P
	for i := s.bit_len() - 1; i >= 0; i-- {
		if s.get_bit(u32(i)) {
			vk = (vk * vk1 + x - big_p) % x
			vk1 = (vk1 * vk1 + x_minus_two) % x
		} else {
			vk1 = (vk * vk1 + x - big_p) % x
			vk = (vk * vk + x_minus_two) % x
		}
	}

	// V(s) == +-2 (mod x), together with U(s) == 0, is the first acceptance
	// condition. U(s) is recovered without computing the U sequence at all:
	// Crandall and Pomerance eq. 3.13 gives U(k) = D^-1 (2 V(k+1) - P V(k)),
	// so U(s) == 0 exactly when P V(s) == 2 V(s+1) (mod x).
	if vk == two_int || vk == x_minus_two {
		lhs := (big_p * vk) % x
		rhs := (vk1.left_shift(1)) % x
		if lhs == rhs {
			return true
		}
	}

	// Otherwise accept if V(s * 2^t) == 0 (mod x) for some 0 <= t < r - 1.
	for _ in 0 .. r - 1 {
		if vk.signum == 0 {
			return true
		}
		if vk == two_int {
			// 2 is a fixed point of V -> V^2 - 2, so no later term can be zero.
			return false
		}
		vk = (vk * vk + x_minus_two) % x
	}
	return false
}

// jacobi returns the Jacobi symbol `(a / n)`, which is +1, -1 or 0.
// `n` must be positive and odd.
pub fn jacobi(a Integer, n Integer) int {
	if !n.is_odd() || n.signum <= 0 {
		panic('math.big: jacobi requires a positive odd modulus')
	}
	mut x := a % n
	if x.signum < 0 {
		x = x + n
	}
	mut y := n
	mut result := 1
	for x.signum != 0 {
		// Pull out factors of two, each contributing -1 when y is 3 or 5 mod 8.
		for !x.is_odd() {
			x = x.right_shift(1)
			y_mod_8 := (y % integer_from_int(8)).int()
			if y_mod_8 == 3 || y_mod_8 == 5 {
				result = -result
			}
		}
		// Quadratic reciprocity.
		x, y = y, x
		if (x % integer_from_int(4)).int() == 3 && (y % integer_from_int(4)).int() == 3 {
			result = -result
		}
		x = x % y
	}
	if y == one_int {
		return result
	}
	return 0
}
