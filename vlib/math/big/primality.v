module big

import rand

// Every prime below 64.
const tiny_primes = [u64(2), 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61]!

// The odd primes below 60, as two products that each fit in a 32 bit word.
const primes_a = u64(3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 37)
const primes_b = u64(29 * 31 * 41 * 43 * 47 * 53)

// Bases that make Miller-Rabin deterministic below 3317044064679887385961981.
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
		return u64(x.int()) in tiny_primes
	}
	if !x.is_odd() {
		return false
	}
	// Trial division, one reduction per product rather than one per prime.
	ra := u32(x.mod_small(primes_a))
	rb := u32(x.mod_small(primes_b))
	if ra % 3 == 0 || ra % 5 == 0 || ra % 7 == 0 || ra % 11 == 0 || ra % 13 == 0 || ra % 17 == 0 || ra % 19 == 0 || ra % 23 == 0 || ra % 37 == 0 || rb % 29 == 0 || rb % 31 == 0 || rb % 41 == 0 || rb % 43 == 0 || rb % 47 == 0 || rb % 53 == 0 {
		return false
	}
	reps := if rounds <= 0 { 40 } else { rounds }
	return x.miller_rabin(reps) && x.lucas_probable_prime()
}

// mod_small returns `|x| mod m` for a modulus that fits in 32 bits. It walks
// the digit array directly and allocates nothing. Each 60-bit digit is taken in
// two 30-bit halves so that every intermediate product stays below 2^64.
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
	shift_mod := (u64(1) << half_bits) % m
	mut r := u64(0)
	for i := x.digits.len - 1; i >= 0; i-- {
		digit := x.digits[i]
		r = (r * shift_mod + (digit >> half_bits)) % m
		r = (r * shift_mod + (digit & half_mask)) % m
	}
	return r
}

// miller_rabin runs `rounds` rounds of the Miller-Rabin test on the odd
// integer `x`.
//
// Writing `x - 1` as `d * 2^s`, a prime `x` satisfies `a^d == 1 (mod x)` or
// `a^(d * 2^r) == -1 (mod x)` for some `0 <= r < s`, for every base `a` coprime
// to it. A base satisfying neither witnesses that `x` is composite, and at most
// a quarter of the bases fail to witness one.
fn (x Integer) miller_rabin(rounds int) bool {
	x_minus_one := x - one_int
	mut d := x_minus_one
	mut s := 0
	for !d.is_odd() {
		d = d.right_shift(1)
		s++
	}
	// Hoisted out of the loop: deriving a context costs a modular inverse, and
	// every round shares the same modulus.
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
// `x` is composite, given `x - 1 == d * 2^s` and a context derived from `x`.
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
			// A non-trivial square root of 1 modulo `x`, so `x` is composite.
			return false
		}
	}
	return false
}

// random_base returns an integer in `[2, x - 2]`, used once the fixed bases
// are exhausted.
@[direct_array_access]
fn random_base(x Integer) Integer {
	mut digits := []u64{len: x.digits.len}
	for i in 0 .. digits.len {
		digits[i] = rand.u64() & max_digit
	}
	// Integer invariants forbid leading zero digits.
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
	return candidate % (x - integer_from_int(3)) + two_int
}

// lucas_probable_prime runs the "almost extra strong" Lucas probable prime test
// on the odd integer `x`, using Baillie-OEIS parameter selection (method C).
// Together with a base-2 Miller-Rabin round this forms the Baillie-PSW test,
// which no known composite passes.
//
// Method C takes the smallest `p >= 3` with `Jacobi(p^2 - 4 / x) == -1` and
// fixes `Q = 1`, which drops the running power of Q and leaves only the V
// sequence, so the test reduces to repeated squaring with no modular halving.
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
			// d = (p - 2)(p + 2) and the search began at p - 2 == 1, so the shared
			// factor is p + 2, and `x` is prime only if it is p + 2 itself.
			return x == integer_from_u64(p + 2)
		}
		if p == 40 {
			// A square has Jacobi(d / x) == 1 for every coprime d, so the search
			// would never end. Non-squares succeed within a few attempts.
			root := x.isqrt()
			if root * root == x {
				return false
			}
		}
		if p > 10000 {
			return false
		}
		p++
	}

	// x + 1 == s * 2^r with s odd; x + 1 rather than x - 1 because
	// Jacobi(d / x) is -1 here.
	mut s := x + one_int
	mut r := 0
	for !s.is_odd() {
		s = s.right_shift(1)
		r++
	}

	big_p := integer_from_u64(p)
	x_minus_two := x - two_int

	// Build V(s) by doubling the subscript from the top bit of s down:
	//     V(2k)     = V(k)^2 - 2
	//     V(2k + 1) = V(k) V(k + 1) - P
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

	// Accept on V(s) == +-2 with U(s) == 0. Crandall and Pomerance eq. 3.13
	// gives U(k) = D^-1 (2 V(k+1) - P V(k)), so U(s) == 0 exactly when
	// P V(s) == 2 V(s+1) (mod x), and the U sequence is never needed.
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
			// Fixed point of V -> V^2 - 2, so no later term can be zero.
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
		// Each factor of two contributes -1 when y is 3 or 5 mod 8.
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
