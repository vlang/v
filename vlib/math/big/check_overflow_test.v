// vtest vflags: -check-overflow
import math.big

// The digit arithmetic of math.big wraps on purpose in a few places (borrows, the low word of
// a product, the Montgomery multiplier). A program built with -check-overflow has to get the
// same results from it as any other build.

// all_ones returns 2^bits - 1, whose digits are all at their maximum, so sums and products
// of it carry out of each digit.
fn all_ones(bits u32) big.Integer {
	return big.one_int.left_shift(bits) - big.one_int
}

// square_of_all_ones returns (2^bits - 1)^2 through shifts, without a multiplication.
fn square_of_all_ones(bits u32) big.Integer {
	return big.one_int.left_shift(2 * bits) - big.one_int.left_shift(bits + 1) + big.one_int
}

fn test_checked_add_and_sub() {
	a := all_ones(240)
	assert (a + big.one_int).hex() == '1' + '0'.repeat(60)
	assert (a + big.one_int) - big.one_int == a
	assert a + a == a.left_shift(1)
	// every digit of the minuend borrows from the next one
	b := big.one_int.left_shift(240)
	assert (b - big.one_int).hex() == 'f'.repeat(60)
	assert (b - a) == big.one_int
	assert (big.one_int - b).neg() == a
}

fn test_checked_mul() {
	assert big.integer_from_u64(max_u64) * big.integer_from_u64(max_u64) == square_of_all_ones(64)
	// below the Karatsuba limit, between the Karatsuba and Toom-3 limits, above the Toom-3 limit
	for bits in [u32(600), 6000, 24000] {
		a := all_ones(bits)
		assert a * a == square_of_all_ones(bits), 'square of 2^${bits} - 1'
		assert a * big.integer_from_int(5) == a.left_shift(2) + a, '5 * (2^${bits} - 1)'
	}
}

fn test_checked_div() {
	seven := big.integer_from_int(7)
	for bits in [u32(64), 600, 6000] {
		divisor := all_ones(bits) - big.integer_from_int(12345)
		quotient := all_ones(bits + 130)
		remainder := all_ones(bits - 3)
		q, r := (divisor * quotient + remainder).div_mod(divisor)
		assert q == quotient, '${bits} bit divisor'
		assert r == remainder, '${bits} bit divisor'
		// a single digit divisor
		q1, r1 := (quotient * seven + big.three_int).div_mod(seven)
		assert q1 == quotient
		assert r1 == big.three_int
	}
}

fn test_checked_pow() {
	assert big.integer_from_int(3).pow(50).str() == '717897987691852588770249'
	assert big.integer_from_int(-7).pow(41).str() == '-44567640326363195900190045974568007'
	assert big.two_int.pow(1000) == big.one_int.left_shift(1000)
}

fn test_checked_shifts_and_bits() {
	a := all_ones(250)
	for amount in [u32(1), 59, 60, 61, 125, 600] {
		assert a.left_shift(amount).right_shift(amount) == a
		assert a.left_shift(amount) == a * big.one_int.left_shift(amount)
	}
	assert a.right_shift(190) == all_ones(60)
	// the low digits of the magnitude are zero, so the borrow reaches the digit that is read
	n := big.one_int.left_shift(120).neg()
	assert !n.get_bit(0)
	assert !n.get_bit(60)
	assert !n.get_bit(119)
	assert n.get_bit(120)
	assert n.get_bit(500)
}

fn test_checked_gcd() {
	g := big.integer_from_int(3).pow(50)
	a := g.left_shift(61)
	b := g * big.integer_from_int(5).pow(30)
	assert a.gcd(b) == g
	assert a.gcd_binary(b) == g
	assert a.gcd_euclid(b) == g
	// operands with more than eight digits
	h := all_ones(521)
	assert (h * a).gcd(h * b) == h * g
	assert (h * a).gcd_euclid(h * b) == h * g
}

fn test_checked_string_conversion() {
	decimal := '1234567890'.repeat(40)
	a := big.integer_from_string(decimal)!
	assert a.str() == decimal
	assert a.neg().str() == '-' + decimal
	hexadecimal := '123456789abcdef'.repeat(20)
	b := big.integer_from_radix(hexadecimal, 16)!
	assert b.hex() == hexadecimal
	assert big.integer_from_string(b.str())! == b
	assert all_ones(600).bin_str() == '1'.repeat(600)
	bytes, signum := b.bytes()
	assert signum == 1
	assert big.integer_from_bytes(bytes) == b
}

fn test_checked_modular_arithmetic() {
	// 2^127 - 1 is prime
	p := all_ones(127)
	exponent := p - big.one_int
	assert big.three_int.big_mod_pow(exponent, p)! == big.one_int
	assert big.three_int.mod_pow(126, p) == big.integer_from_int(3).pow(126) % p
	// an even modulus: 3^(p-1) is 1 modulo p and, as an even power of 3, 1 modulo 8
	assert big.three_int.big_mod_pow(exponent, p.left_shift(3))! == big.one_int
	// a power of two: 3 has order 2^198 modulo 2^200
	two_200 := big.one_int.left_shift(200)
	assert big.three_int.big_mod_pow(big.one_int.left_shift(198), two_200)! == big.one_int
	assert big.three_int.big_mod_pow(big.one_int.left_shift(197), two_200)! == big.one_int.left_shift(199) +
		big.one_int
	inverse := big.integer_from_int(12345).mod_inverse(p)!
	assert (inverse * big.integer_from_int(12345)) % p == big.one_int
	assert p.is_probably_prime(8)
	assert !(p * p).is_probably_prime(8)
	assert (p * p).isqrt() == p
}

fn test_checked_from_json_number() {
	mut a := big.Integer{}
	a.from_json_number('-123456789012345678901234567890')!
	assert a.str() == '-123456789012345678901234567890'
	// `.` sorts below `0`, so the digit test wraps for it
	mut b := big.Integer{}
	b.from_json_number('12.5') or {
		assert err.msg() == 'expected integer but got real number'
		return
	}
	assert false, 'a real number is not an integer'
}
