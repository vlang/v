module big

import math
import math.bits
import strings
import strconv

const digit_array = '0123456789abcdefghijklmnopqrstuvwxyz'.bytes()
// vfmt off
const radix_options = {
	2: 31, 3: 20, 4: 15, 5: 13, 6: 12, 7: 11, 8: 10, 9: 10, 10: 9,
	11: 9, 12: 8, 13: 8, 14: 8, 15: 8, 16: 7, 17: 7, 18: 7, 19: 7,
	20: 7, 21: 7, 22: 7, 23: 7, 24: 6, 25: 6, 26: 6, 27: 6, 28: 6,
	29: 6, 30: 6, 31: 6, 32: 6, 33: 6, 34: 6, 35: 6, 36: 6
}
// vfmt on

// big.Integer
// -----------
// It has the following properties:
// 1. Every "digit" is an integer in the range [0, 2^32).
// 2. The signum can be one of three values: -1, 0, +1 for
// 		negative, zero, and positive values, respectively.
// 3. There should be no leading zeros in the digit array.
// 4. The digits are stored in little endian format, that is,
//		the digits with a lower positional value (towards the right
//		when represented as a string) have a lower index, and vice versa.
pub struct Integer {
	digits []u32
pub:
	signum   int
	is_const bool
}

@[unsafe]
fn (mut x Integer) free() {
	if x.is_const {
		return
	}
	unsafe { x.digits.free() }
}

fn (x Integer) clone() Integer {
	return Integer{
		digits:   x.digits.clone()
		signum:   x.signum
		is_const: false
	}
}

fn int_signum(value int) int {
	if value == 0 {
		return 0
	}
	return if value < 0 { -1 } else { 1 }
}

// integer_from_int creates a new `big.Integer` from the given int value.
pub fn integer_from_int(value int) Integer {
	if value == 0 {
		return zero_int
	}
	return Integer{
		digits: [u32(iabs(value))]
		signum: int_signum(value)
	}
}

// integer_from_u32 creates a new `big.Integer` from the given u32 value.
pub fn integer_from_u32(value u32) Integer {
	if value == 0 {
		return zero_int
	}
	return Integer{
		digits: [value]
		signum: 1
	}
}

// integer_from_i64 creates a new `big.Integer` from the given i64 value.
pub fn integer_from_i64(value i64) Integer {
	if value == 0 {
		return zero_int
	}

	signum_value := if value < 0 { -1 } else { 1 }
	abs_value := u64(value * signum_value)

	lower := u32(abs_value)
	upper := u32(abs_value >> 32)

	if upper == 0 {
		return Integer{
			digits: [lower]
			signum: signum_value
		}
	} else {
		return Integer{
			digits: [lower, upper]
			signum: signum_value
		}
	}
}

// integer_from_u64 creates a new `big.Integer` from the given u64 value.
pub fn integer_from_u64(value u64) Integer {
	if value == 0 {
		return zero_int
	}

	lower := u32(value & 0x00000000ffffffff)
	upper := u32((value & 0xffffffff00000000) >> 32)

	if upper == 0 {
		return Integer{
			digits: [lower]
			signum: 1
		}
	} else {
		return Integer{
			digits: [lower, upper]
			signum: 1
		}
	}
}

@[params]
pub struct IntegerConfig {
pub:
	signum int = 1
}

// integer_from_bytes creates a new `big.Integer` from the given byte array.
// By default, positive integers are assumed.
// If you want a negative integer, use in the following manner:
// `value := big.integer_from_bytes(bytes, signum: -1)`
@[direct_array_access]
pub fn integer_from_bytes(oinput []u8, config IntegerConfig) Integer {
	// Thank you to Miccah (@mcastorina) for this implementation and relevant unit tests.
	if oinput.len == 0 {
		return integer_from_int(0)
	}
	// Ignore leading 0 bytes:
	mut first_non_zero_index := -1
	for i in 0 .. oinput.len {
		if oinput[i] != 0 {
			first_non_zero_index = i
			break
		}
	}
	if first_non_zero_index == -1 {
		return integer_from_int(0)
	}
	input := oinput[first_non_zero_index..]
	// pad input
	mut padded_input := []u8{len: int_max(0, ((input.len + 3) & ~0x3) - input.len), cap: (
		input.len + 3) & ~0x3}
	padded_input << input
	mut digits := []u32{len: padded_input.len / 4}
	// combine every 4 bytes into a u32 and insert into n.digits
	for i := 0; i < padded_input.len; i += 4 {
		x3 := u32(padded_input[i])
		x2 := u32(padded_input[i + 1])
		x1 := u32(padded_input[i + 2])
		x0 := u32(padded_input[i + 3])
		val := (x3 << 24) | (x2 << 16) | (x1 << 8) | x0
		digits[(padded_input.len - i) / 4 - 1] = val
	}
	return Integer{
		digits: digits
		signum: config.signum
	}
}

// integer_from_string creates a new `big.Integer` from the decimal digits specified in the given string.
// For other bases, use `big.integer_from_radix` instead.
pub fn integer_from_string(characters string) !Integer {
	return integer_from_radix(characters, 10)
}

// integer_from_radix creates a new `big.Integer` from the given string and radix.
pub fn integer_from_radix(all_characters string, radix u32) !Integer {
	if radix < 2 || radix > 36 {
		return error('math.big: Radix must be between 2 and 36 (inclusive)')
	}
	characters := all_characters.to_lower()
	validate_string(characters, radix) or { return err }
	return match radix {
		2 {
			integer_from_special_string(characters, 1)
		}
		16 {
			integer_from_special_string(characters, 4)
		}
		else {
			integer_from_regular_string(characters, radix)
		}
	}
}

@[direct_array_access]
fn validate_string(characters string, radix u32) ! {
	sign_present := characters.len > 0 && (characters[0] == `+` || characters[0] == `-`)

	start_index := if sign_present { 1 } else { 0 }

	for index := start_index; index < characters.len; index++ {
		digit := characters[index]
		value := digit_array.index(digit)

		if value == -1 {
			return error('math.big: Invalid character ${digit}')
		}
		if value >= radix {
			return error('math.big: Invalid character ${digit} for base ${radix}')
		}
	}
}

@[direct_array_access]
fn integer_from_special_string(characters string, chunk_size int) Integer {
	sign_present := characters[0] == `+` || characters[0] == `-`

	signum := if sign_present {
		if characters[0] == `-` { -1 } else { 1 }
	} else {
		1
	}

	start_index := if sign_present { 1 } else { 0 }

	mut big_digits := []u32{cap: ((characters.len * chunk_size) >> 5) + 1}
	mut current := u32(0)
	mut offset := 0
	for index := characters.len - 1; index >= start_index; index-- {
		digit := characters[index]
		value := u32(digit_array.index(digit))

		current |= value << offset
		offset += chunk_size

		if offset == 32 {
			big_digits << current
			current = u32(0)
			offset = 0
		}
	}

	// Store the accumulated value into the digit array
	if current != 0 {
		big_digits << current
	}

	shrink_tail_zeros(mut big_digits)

	return Integer{
		digits: big_digits
		signum: if big_digits.len == 0 { 0 } else { signum }
	}
}

@[direct_array_access]
fn integer_from_regular_string(characters string, radix u32) Integer {
	sign_present := characters.len > 0 && (characters[0] == `+` || characters[0] == `-`)

	signum := if sign_present {
		if characters[0] == `-` { -1 } else { 1 }
	} else {
		1
	}

	start_index := if sign_present { 1 } else { 0 }

	mut result := zero_int
	radix_int := integer_from_u32(radix)
	pow := radix_options[int(radix)]
	radix_pow := radix_int.pow(u32(pow))
	for i := start_index; i < characters.len; i += pow {
		end := math.min(i + pow, characters.len)
		num_str := characters[i..end]
		if num_str.len == pow {
			result *= radix_pow
		} else {
			result *= radix_int.pow(u32(num_str.len))
		}
		result += integer_from_u32(regular_string_to_radix(num_str, radix))
	}

	return Integer{
		digits: result.digits.clone()
		signum: result.signum * signum
	}
}

fn regular_string_to_radix(characters string, radix u32) u32 {
	mut result := u32(0)

	for c in characters {
		result = result * radix + u32(digit_array.index(c))
	}
	return result
}

// abs returns the absolute value of the integer `a`.
pub fn (a Integer) abs() Integer {
	return if a.signum == 0 {
		zero_int
	} else {
		Integer{
			digits: a.digits.clone()
			signum: 1
		}
	}
}

// neg returns the result of negation of the integer `a`.
pub fn (a Integer) neg() Integer {
	return if a.signum == 0 {
		zero_int
	} else {
		Integer{
			digits: a.digits.clone()
			signum: -a.signum
		}
	}
}

// + returns the sum of the integers `augend` and `addend`.
pub fn (augend Integer) + (addend Integer) Integer {
	// Quick exits
	if augend.signum == 0 {
		return addend.clone()
	}
	if addend.signum == 0 {
		return augend.clone()
	}
	// Non-zero cases
	if augend.signum == addend.signum {
		return augend.add(addend)
	}
	// Unequal signs
	if augend.abs_cmp(addend) < 0 {
		return augend.subtract(addend).neg()
	} else {
		return augend.subtract(addend)
	}
}

// - returns the difference of the integers `minuend` and `subtrahend`
pub fn (minuend Integer) - (subtrahend Integer) Integer {
	// Quick exits
	if minuend.signum == 0 {
		return subtrahend.neg()
	}
	if subtrahend.signum == 0 {
		return minuend.clone()
	}
	// Non-zero cases
	if minuend.signum == subtrahend.signum {
		return minuend.subtract(subtrahend)
	}
	// Unequal signs:
	return minuend.add(subtrahend)
}

fn (integer Integer) add(addend Integer) Integer {
	a := integer.digits
	b := addend.digits
	mut storage := []u32{len: imax(a.len, b.len) + 1}
	add_digit_array(a, b, mut storage)
	return Integer{
		signum: integer.signum
		digits: storage
	}
}

fn (integer Integer) subtract(subtrahend Integer) Integer {
	cmp := integer.abs_cmp(subtrahend)
	if cmp == 0 {
		return zero_int
	}
	a, b := if cmp > 0 { integer, subtrahend } else { subtrahend, integer }
	mut storage := []u32{len: a.digits.len}
	subtract_digit_array(a.digits, b.digits, mut storage)
	return Integer{
		signum: cmp * a.signum
		digits: storage
	}
}

// * returns the product of the integers `multiplicand` and `multiplier`.
pub fn (multiplicand Integer) * (multiplier Integer) Integer {
	// Quick exits
	if multiplicand.signum == 0 || multiplier.signum == 0 {
		return zero_int
	}
	if multiplicand == one_int {
		return multiplier.clone()
	}
	if multiplier == one_int {
		return multiplicand.clone()
	}
	// The final sign is the product of the signs
	mut storage := []u32{len: multiplicand.digits.len + multiplier.digits.len}
	multiply_digit_array(multiplicand.digits, multiplier.digits, mut storage)
	return Integer{
		signum: multiplicand.signum * multiplier.signum
		digits: storage
	}
}

// div_mod_internal is an entirely unchecked (in terms of division by zero) method for division.
// This should only be used for internal calculations involving a definitive non-zero
// divisor.
//
// DO NOT use this method if the divisor has any chance of being 0.
fn (dividend Integer) div_mod_internal(divisor Integer) (Integer, Integer) {
	mut q := []u32{cap: int_max(1, dividend.digits.len - divisor.digits.len + 1)}
	mut r := []u32{cap: dividend.digits.len}
	mut q_signum := 0
	mut r_signum := 0

	divide_digit_array(dividend.digits, divisor.digits, mut q, mut r)
	if dividend.signum > 0 && divisor.signum > 0 {
		q_signum = 1
		r_signum = 1
	} else if dividend.signum > 0 && divisor.signum < 0 {
		q_signum = -1
		r_signum = 1
	} else if dividend.signum < 0 && divisor.signum > 0 {
		q_signum = -1
		r_signum = -1
	} else {
		q_signum = 1
		r_signum = -1
	}
	quotient := Integer{
		signum: if q.len == 0 { 0 } else { q_signum }
		digits: q
	}
	remainder := Integer{
		signum: if r.len == 0 { 0 } else { r_signum }
		digits: r
	}
	return quotient, remainder
}

// div_mod returns the quotient and remainder from the division of the integers `dividend`
// divided by `divisor`.
//
// WARNING: this method will panic if `divisor == 0`. Refer to div_mod_checked for a safer version.
@[inline]
pub fn (dividend Integer) div_mod(divisor Integer) (Integer, Integer) {
	if _unlikely_(divisor.signum == 0) {
		panic('math.big: Cannot divide by zero')
	}
	return dividend.div_mod_internal(divisor)
}

// div_mod_checked returns the quotient and remainder from the division of the integers `dividend`
// divided by `divisor`. An error is returned if `divisor == 0`.
@[inline]
pub fn (dividend Integer) div_mod_checked(divisor Integer) !(Integer, Integer) {
	if _unlikely_(divisor.signum == 0) {
		return error('math.big: Cannot divide by zero')
	}
	return dividend.div_mod_internal(divisor)
}

// / returns the quotient of `dividend` divided by `divisor`.
//
// WARNING: this method will panic if `divisor == 0`. For a division method that returns a Result
// refer to `div_checked`.
@[inline]
pub fn (dividend Integer) / (divisor Integer) Integer {
	q, _ := dividend.div_mod(divisor)
	return q
}

// % returns the remainder of `dividend` divided by `divisor`.
//
// WARNING: this method will panic if `divisor == 0`. For a modular division method that
// returns a Result refer to `mod_checked`.
// Note: in V, `assert big.integer_from_i64(-10) % big.integer_from_i64(7) == big.integer_from_i64(-3)` passes.
// In other words, the result is negative 3, and is NOT positive 4.
@[inline]
pub fn (dividend Integer) % (divisor Integer) Integer {
	_, r := dividend.div_mod(divisor)
	return r
}

// div_checked returns the quotient of `dividend` divided by `divisor`
// or an error if `divisor == 0`.
@[inline]
pub fn (dividend Integer) div_checked(divisor Integer) !Integer {
	q, _ := dividend.div_mod_checked(divisor)!
	return q
}

// mod_checked returns the remainder of `dividend` divided by `divisor`
// or an error if `divisor == 0`.
@[inline]
pub fn (dividend Integer) mod_checked(divisor Integer) !Integer {
	_, r := dividend.div_mod_checked(divisor)!
	return r
}

// modulo_euclid returns the result of mathematical modulus.
// The result is always non-negative for positive `divisor`.
//
// WARNING: this method will panic if `divisor == 0`.
@[inline]
pub fn (dividend Integer) mod_euclid(divisor Integer) Integer {
	r := dividend % divisor
	if r < zero_int {
		return r + divisor.abs()
	} else {
		return r
	}
}

// mod_euclid_checked returns the result of mathematical modulus.
// The result is always non-negative for positive `divisor`
// or an error if `divisor == 0`.
@[inline]
pub fn (dividend Integer) mod_euclid_checked(divisor Integer) !Integer {
	r := dividend.mod_checked(divisor)!
	if r < zero_int {
		return r + divisor.abs()
	} else {
		return r
	}
}

// mask_bits is the equivalent of `a % 2^n` (only when `a >= 0`), however doing a full division
// run for this would be a lot of work when we can simply "cut off" all bits to the left of
// the `n`th bit.
@[direct_array_access]
fn (a Integer) mask_bits(n u32) Integer {
	$if debug {
		assert a.signum >= 0
	}

	if a.digits.len == 0 || n == 0 {
		return zero_int
	}

	w := n / 32
	b := n % 32

	if w >= a.digits.len {
		return a
	}

	return Integer{
		digits: if b == 0 {
			mut storage := []u32{len: int(w)}
			for i := 0; i < storage.len; i++ {
				storage[i] = a.digits[i]
			}
			storage
		} else {
			mut storage := []u32{len: int(w) + 1}
			for i := 0; i < storage.len; i++ {
				storage[i] = a.digits[i]
			}
			storage[w] &= ~(u32(-1) << b)
			storage
		}
		signum: 1
	}
}

// pow returns the integer `base` raised to the power of the u32 `exponent`.
pub fn (base Integer) pow(exponent u32) Integer {
	if exponent == 0 {
		return one_int
	}
	if exponent == 1 {
		return base.clone()
	}
	mut n := exponent
	mut x := base
	mut y := one_int
	for n > 1 {
		if n & 1 == 1 {
			y *= x
		}
		x *= x
		n >>= 1
	}
	return x * y
}

// mod_pow returns the integer `base` raised to the power of the u32 `exponent` modulo the integer `modulus`.
pub fn (base Integer) mod_pow(exponent u32, modulus Integer) Integer {
	if exponent == 0 {
		return one_int
	}
	if exponent == 1 {
		return base % modulus
	}
	mut n := exponent
	mut x := base % modulus
	mut y := one_int
	for n > 1 {
		if n & 1 == 1 {
			y = (y * x) % modulus
		}
		x = (x * x) % modulus
		n >>= 1
	}
	return x * y % modulus
}

// big_mod_pow returns the integer `base` raised to the power of the integer `exponent` modulo the integer `modulus`.
@[direct_array_access]
pub fn (base Integer) big_mod_pow(exponent Integer, modulus Integer) !Integer {
	if exponent.signum < 0 {
		return error('math.big: Exponent needs to be non-negative.')
	}

	// this goes first as otherwise 1 could be returned incorrectly if base == 1
	if modulus.bit_len() <= 1 {
		return zero_int
	}

	// x^0 == 1 || 1^x == 1
	if exponent.signum == 0 || base.bit_len() == 1 {
		return one_int
	}

	// 0^x == 0 (x != 0 due to previous clause)
	if base.signum == 0 {
		return zero_int
	}

	if exponent.bit_len() == 1 {
		// x^1 without mod == x
		if modulus.signum == 0 {
			return base
		}
		// x^1 (mod m) === x % m
		return base % modulus
	}

	// the amount of precomputation in windowed exponentiation (done in the montgomery and binary
	// windowed exponentiation algorithms) is far too costly for small sized exponents, so
	// we redirect the call to mod_pow
	return if exponent.digits.len > 1 {
		if modulus.is_odd() {
			// modulus is odd, therefore we use the normal
			// montgomery modular exponentiation algorithm
			base.mont_odd(exponent, modulus)
		} else if modulus.is_power_of_2() {
			base.exp_binary(exponent, modulus)
		} else {
			base.mont_even(exponent, modulus)
		}
	} else {
		base.mod_pow(exponent.digits[0], modulus)
	}
}

// inc increments `a` by 1 in place.
pub fn (mut a Integer) inc() {
	a = a + one_int
}

// dec decrements `a` by 1 in place.
pub fn (mut a Integer) dec() {
	a = a - one_int
}

// == returns `true` if the integers `a` and `b` are equal in value and sign.
@[inline]
pub fn (a Integer) == (b Integer) bool {
	return a.signum == b.signum && a.digits.len == b.digits.len && a.digits == b.digits
}

// abs_cmp returns the result of comparing the magnitudes of the integers `a` and `b`.
// It returns a negative int if `|a| < |b|`, 0 if `|a| == |b|`, and a positive int if `|a| > |b|`.
@[inline]
pub fn (a Integer) abs_cmp(b Integer) int {
	return compare_digit_array(a.digits, b.digits)
}

// < returns `true` if the integer `a` is less than `b`.
pub fn (a Integer) < (b Integer) bool {
	// Quick exits based on signum value:
	if a.signum < b.signum {
		return true
	}
	if a.signum > b.signum {
		return false
	}
	// They have equal sign
	signum := a.signum
	if signum == 0 { // Are they both zero?
		return false
	}
	// If they are negative, the one with the larger absolute value is smaller
	cmp := a.abs_cmp(b)
	return if signum < 0 { cmp > 0 } else { cmp < 0 }
}

// get_bit checks whether the bit at the given index is set.
@[direct_array_access]
pub fn (a Integer) get_bit(i u32) bool {
	target_index := i / 32
	offset := i % 32
	if target_index >= a.digits.len {
		return false
	}
	return (a.digits[target_index] >> offset) & 1 != 0
}

// set_bit sets the bit at the given index to the given value.
pub fn (mut a Integer) set_bit(i u32, value bool) {
	target_index := i / 32
	offset := i % 32

	if target_index >= a.digits.len {
		if value {
			a = one_int.left_shift(i).bitwise_or(a)
		}
		return
	}

	mut copy := a.digits.clone()

	if value {
		copy[target_index] |= 1 << offset
	} else {
		copy[target_index] &= ~(1 << offset)
	}

	a = Integer{
		signum: a.signum
		digits: copy
	}
}

// bitwise_or returns the "bitwise or" of the integers `|a|` and `|b|`.
//
// Note: both operands are treated as absolute values.
pub fn (a Integer) bitwise_or(b Integer) Integer {
	mut result := []u32{len: imax(a.digits.len, b.digits.len)}
	bitwise_or_digit_array(a.digits, b.digits, mut result)
	return Integer{
		digits: result
		signum: if result.len == 0 { 0 } else { 1 }
	}
}

// bitwise_and returns the "bitwise and" of the integers `|a|` and `|b|`.
//
// Note: both operands are treated as absolute values.
pub fn (a Integer) bitwise_and(b Integer) Integer {
	mut result := []u32{len: imax(a.digits.len, b.digits.len)}
	bitwise_and_digit_array(a.digits, b.digits, mut result)
	return Integer{
		digits: result
		signum: if result.len == 0 { 0 } else { 1 }
	}
}

// bitwise_not returns the "bitwise not" of the integer `|a|`.
//
// Note: the integer is treated as an absolute value.
pub fn (a Integer) bitwise_not() Integer {
	mut result := []u32{len: a.digits.len}
	bitwise_not_digit_array(a.digits, mut result)
	return Integer{
		digits: result
		signum: if result.len == 0 { 0 } else { 1 }
	}
}

// bitwise_com returns "bitwise complement" of integer `a`.
//
// Note: this function consider the sign of the input.
pub fn (a Integer) bitwise_com() Integer {
	return if a.signum == -1 {
		a.abs() - one_int
	} else {
		(a + one_int).neg()
	}
}

// bitwise_xor returns the "bitwise exclusive or" of the integers `|a|` and `|b|`.
//
// Note: both operands are treated as absolute values.
pub fn (a Integer) bitwise_xor(b Integer) Integer {
	mut result := []u32{len: imax(a.digits.len, b.digits.len)}
	bitwise_xor_digit_array(a.digits, b.digits, mut result)
	return Integer{
		digits: result
		signum: if result.len == 0 { 0 } else { 1 }
	}
}

// left_shift returns the integer `a` shifted left by `amount` bits.
@[direct_array_access]
pub fn (a Integer) left_shift(amount u32) Integer {
	if a.signum == 0 {
		return a
	}
	if amount == 0 {
		return a
	}
	normalised_amount := amount & 31
	digit_offset := int(amount >> 5)
	mut new_array := []u32{len: a.digits.len + digit_offset}
	for index in 0 .. a.digits.len {
		new_array[index + digit_offset] = a.digits[index]
	}
	if normalised_amount > 0 {
		shift_digits_left(new_array, normalised_amount, mut new_array)
	}
	return Integer{
		digits: new_array
		signum: a.signum
	}
}

// right_shift returns the integer `a` shifted right by `amount` bits.
@[direct_array_access]
pub fn (a Integer) right_shift(amount u32) Integer {
	if a.signum == 0 {
		return a
	}
	if amount == 0 {
		return a
	}
	normalised_amount := amount & 31
	digit_offset := int(amount >> 5)
	if digit_offset >= a.digits.len {
		return zero_int
	}
	mut new_array := []u32{len: a.digits.len - digit_offset}
	for index in 0 .. new_array.len {
		new_array[index] = a.digits[index + digit_offset]
	}
	if normalised_amount > 0 {
		shift_digits_right(new_array, normalised_amount, mut new_array)
	}
	return Integer{
		digits: new_array
		signum: if new_array.len > 0 { a.signum } else { 0 }
	}
}

// bin_str returns the binary string representation of the integer `a`.
@[direct_array_access]
pub fn (integer Integer) bin_str() string {
	// We have the zero integer
	if integer.signum == 0 {
		return '0'
	}
	// Add the sign if present
	sign_needed := integer.signum == -1
	mut result_builder := strings.new_builder(integer.bit_len() + if sign_needed { 1 } else { 0 })
	if sign_needed {
		result_builder.write_string('-')
	}

	result_builder.write_string(u32_to_binary_without_lz(integer.digits[integer.digits.len - 1]))

	for index := integer.digits.len - 2; index >= 0; index-- {
		result_builder.write_string(u32_to_binary_with_lz(integer.digits[index]))
	}
	return result_builder.str()
}

// hex returns the hexadecimal string representation of the integer `a`.
@[direct_array_access]
pub fn (integer Integer) hex() string {
	// We have the zero integer
	if integer.signum == 0 {
		return '0'
	}
	// Add the sign if present
	sign_needed := integer.signum == -1
	mut result_builder := strings.new_builder(integer.digits.len * 8 +
		if sign_needed { 1 } else { 0 })
	if sign_needed {
		result_builder.write_string('-')
	}

	result_builder.write_string(u32_to_hex_without_lz(integer.digits[integer.digits.len - 1]))

	for index := integer.digits.len - 2; index >= 0; index-- {
		result_builder.write_string(u32_to_hex_with_lz(integer.digits[index]))
	}
	return result_builder.str()
}

// radix_str returns the string representation of the integer `a` in the specified radix.
pub fn (integer Integer) radix_str(radix u32) string {
	if integer.signum == 0 || radix == 0 {
		return '0'
	}
	return match radix {
		2 {
			integer.bin_str()
		}
		16 {
			integer.hex()
		}
		else {
			integer.general_radix_str(int(radix))
		}
	}
}

fn (integer Integer) general_radix_str(radix int) string {
	$if debug {
		assert radix != 0
	}
	divisor := integer_from_int(radix).pow(u32(radix_options[radix]))

	mut current := integer.abs()
	mut new_current := zero_int
	mut digit := zero_int
	mut sb := strings.new_builder(integer.digits.len * radix_options[radix])
	mut st := []string{cap: integer.digits.len * radix_options[radix]}
	for current.signum > 0 {
		new_current, digit = current.div_mod_internal(divisor)
		st << general_str(new_current, digit, radix)
		current = new_current
	}
	if integer.signum == -1 {
		sb.write_string('-')
	}
	for st.len > 0 {
		sb.write_string(st.pop())
	}
	return sb.str()
}

fn general_str(quotient Integer, remainder Integer, radix int) string {
	if quotient.signum == 0 && remainder.signum == 0 {
		return '0'
	}
	divisor := integer_from_int(radix)

	mut current := remainder.abs()
	mut new_current := zero_int
	mut digit := zero_int
	mut sb := strings.new_builder(radix_options[radix])
	mut st := []u8{cap: radix_options[radix]}
	for current.signum > 0 {
		new_current, digit = current.div_mod_internal(divisor)
		st << digit_array[digit.int()]
		current = new_current
	}
	if quotient.signum > 0 {
		sb.write_string(strings.repeat(48, radix_options[radix] - st.len))
	}
	for st.len > 0 {
		sb.write_u8(st.pop())
	}
	return sb.str()
}

// str returns the decimal string representation of the integer `a`.
pub fn (integer Integer) str() string {
	return integer.radix_str(10)
}

fn u32_to_binary_without_lz(value u32) string {
	return strconv.format_uint(value, 2)
}

fn u32_to_binary_with_lz(value u32) string {
	mut result_builder := strings.new_builder(32)
	binary_result := strconv.format_uint(value, 2)

	result_builder.write_string(strings.repeat(`0`, 32 - binary_result.len))
	result_builder.write_string(binary_result)

	return result_builder.str()
}

fn u32_to_hex_without_lz(value u32) string {
	return strconv.format_uint(value, 16)
}

fn u32_to_hex_with_lz(value u32) string {
	mut result_builder := strings.new_builder(8)
	hex_result := strconv.format_uint(value, 16)

	result_builder.write_string(strings.repeat(`0`, 8 - hex_result.len))
	result_builder.write_string(hex_result)

	return result_builder.str()
}

// int returns the integer value of the integer `a`.
// NOTE: This may cause loss of precision.
@[direct_array_access]
pub fn (a Integer) int() int {
	if a.signum == 0 {
		return 0
	}
	// Check for minimum value int
	if a.digits[0] == 2147483648 && a.signum == -1 {
		return -2147483648
	}
	// Rest of the values should be fine
	value := int(a.digits[0] & 0x7fffffff)
	return value * a.signum
}

// bytes returns the a byte representation of the integer a, along with the signum int.
// NOTE: The byte array returned is in big endian order.
@[direct_array_access]
pub fn (a Integer) bytes() ([]u8, int) {
	if a.signum == 0 {
		return []u8{len: 0}, 0
	}
	mut result := []u8{cap: a.digits.len * 4}
	mut mask := u32(0xff000000)
	mut offset := 24
	mut non_zero_found := false
	for index := a.digits.len - 1; index >= 0; {
		value := u8((a.digits[index] & mask) >> offset)
		non_zero_found = non_zero_found || value != 0
		if non_zero_found {
			result << value
		}
		mask >>= 8
		offset -= 8
		if offset < 0 {
			mask = u32(0xff000000)
			offset = 24
			index--
		}
	}
	return result, a.signum
}

// factorial returns the factorial of the integer `a`.
pub fn (a Integer) factorial() Integer {
	if a.signum == 0 {
		return one_int
	}
	mut product := one_int
	mut current := a
	for current.signum != 0 {
		product *= current
		current.dec()
	}
	return product
}

// isqrt returns the closest integer square root of the integer `a`.
//
// WARNING: this method will panic if `a < 0`. Refer to isqrt_checked for a safer version.
@[inline]
pub fn (a Integer) isqrt() Integer {
	return a.isqrt_checked() or { panic(err) }
}

// isqrt returns the closest integer square root of the integer `a`.
// An error is returned if `a < 0`.
pub fn (a Integer) isqrt_checked() !Integer {
	if a.signum < 0 {
		return error('math.big: Cannot calculate square root of negative integer')
	}
	if a.signum == 0 {
		return a
	}
	if a.digits.len == 1 && a.digits.last() == 1 {
		return a
	}

	mut shift := a.bit_len()
	if shift & 1 == 1 {
		shift += 1
	}
	mut result := zero_int
	for shift >= 0 {
		result = result.left_shift(1)
		larger := result + one_int
		if (larger * larger).abs_cmp(a.right_shift(u32(shift))) <= 0 {
			result = larger
		}
		shift -= 2
	}
	return result
}

@[inline]
fn bi_min(a Integer, b Integer) Integer {
	return if a < b { a } else { b }
}

@[inline]
fn bi_max(a Integer, b Integer) Integer {
	return if a > b { a } else { b }
}

// gcd returns the greatest common divisor of the two integers `a` and `b`.
pub fn (a Integer) gcd(b Integer) Integer {
	// The cutoff is determined empirically, using vlib/v/tests/bench/math_big_gcd/bench_euclid.v .
	if b.digits.len < 8 {
		return a.gcd_euclid(b)
	}
	return a.gcd_binary(b)
}

// gcd_binary returns the greatest common divisor of the two integers `a` and `b`.
// Note that gcd_binary is faster than gcd_euclid, for large integers (over 8 bytes long).
// Inspired by the 2013-christmas-special by D. Lemire & R. Corderoy https://en.algorithmica.org/hpc/analyzing-performance/gcd/
// For more information, refer to the Wikipedia article: https://en.wikipedia.org/wiki/Binary_GCD_algorithm
// Discussion and further information: https://lemire.me/blog/2013/12/26/fastest-way-to-compute-the-greatest-common-divisor/
pub fn (a Integer) gcd_binary(b Integer) Integer {
	if a.signum == 0 {
		return b.abs()
	}
	if b.signum == 0 {
		return a.abs()
	}
	if a.abs_cmp(one_int) == 0 || b.abs_cmp(one_int) == 0 {
		return one_int
	}

	mut aa, az := a.abs().rsh_to_set_bit()
	mut bb, bz := b.abs().rsh_to_set_bit()
	shift := umin(az, bz)

	for aa.signum != 0 {
		diff := bb - aa
		bb = bi_min(aa, bb)
		aa, _ = diff.abs().rsh_to_set_bit()
	}
	return bb.left_shift(shift)
}

// gcd_euclid returns the greatest common divisor of the two integers `a` and `b`.
// Note that gcd_euclid is faster than gcd_binary, for very-small-integers up to 8-byte/u64.
pub fn (a Integer) gcd_euclid(b Integer) Integer {
	if a.signum == 0 {
		return b.abs()
	}
	if b.signum == 0 {
		return a.abs()
	}
	if a.signum < 0 {
		return a.neg().gcd_euclid(b)
	}
	if b.signum < 0 {
		return a.gcd_euclid(b.neg())
	}
	mut x := a
	mut y := b
	mut r := x % y
	for r.signum != 0 {
		x = y
		y = r
		r = x % y
	}
	return y
}

// mod_inverse calculates the multiplicative inverse of the integer `a` in the ring `ℤ/nℤ`.
// Therefore, the return value `x` satisfies `a * x == 1 (mod m)`.
// An error is returned if `a` and `n` are not relatively prime, i.e. `gcd(a, n) != 1` or
// if n <= 1
@[inline]
pub fn (a Integer) mod_inverse(n Integer) !Integer {
	return if n.bit_len() <= 1 {
		error('math.big: Modulus `n` must be greater than 1')
	} else if a.gcd(n) != one_int {
		error('math.big: No multiplicative inverse')
	} else {
		a.mod_inv(n)
	}
}

// this is an internal function, therefore we assume valid inputs,
// i.e. m > 1 and gcd(a, m) = 1
// see pub fn mod_inverse for details on the result
// -----
// the algorithm is based on the Extended Euclidean algorithm which computes `ax + by = d`
// in this case `b` is the input integer `a` and `a` is the input modulus `m`. The extended
// Euclidean algorithm calculates the greatest common divisor `d` and two coefficients `x` and `y`
// satisfying the above equality.
//
// For the sake of clarity, we refer to the input integer `a` as `b` and the integer `m` as `a`.
// If `gcd(a, b) = d = 1` then the coefficient `y` is known to be the multiplicative inverse of
// `b` in ring `Z/aZ`, since reducing `ax + by = 1` by `a` yields `by == 1 (mod a)`.
@[direct_array_access]
fn (a Integer) mod_inv(m Integer) Integer {
	mut n := Integer{
		digits: m.digits.clone()
		signum: 1
	}
	mut b := a
	mut x := one_int
	mut y := zero_int
	if b.signum < 0 || b.abs_cmp(n) >= 0 {
		b = b % n
	}
	mut sign := -1

	for b != zero_int {
		q, r := if n.bit_len() == b.bit_len() {
			one_int, n - b
		} else {
			// safe because the loop terminates if b == 0
			n.div_mod_internal(b)
		}

		n = b
		b = r

		// tmp := q * x + y
		tmp := if q == one_int {
			x
		} else if q.digits.len == 1 && q.digits[0] & (q.digits[0] - 1) == 0 {
			x.left_shift(u32(bits.trailing_zeros_32(q.digits[0])))
		} else {
			q * x
		} + y

		y = x
		x = tmp
		sign = -sign
	}

	if sign < 0 {
		y = m - y
	}

	$if debug {
		assert n == one_int
	}

	return if y.signum > 0 && y.abs_cmp(m) < 0 {
		y
	} else {
		y % m
	}
}

// rsh_to_set_bit returns the integer `x` shifted right until it is odd and an exponent satisfying
// `x = x1 * 2^n`
// we don't return `2^n`, because the caller may be able to use `n` without allocating an Integer
@[direct_array_access; inline]
fn (x Integer) rsh_to_set_bit() (Integer, u32) {
	if x.digits.len == 0 {
		return zero_int, 0
	}

	mut n := u32(0)
	for x.digits[n] == 0 {
		n++
	}
	n = (n << 5) + u32(bits.trailing_zeros_32(x.digits[n]))
	return x.right_shift(n), n
}

// is_odd returns true if the integer `x` is odd, therefore an integer of the form `2k + 1`.
// An input of 0 returns false.
@[direct_array_access; inline]
pub fn (x Integer) is_odd() bool {
	return x.digits.len != 0 && x.digits[0] & 1 == 1
}

// is_power_of_2 returns true when the integer `x` satisfies `2^n`, where `n >= 0`
@[direct_array_access; inline]
pub fn (x Integer) is_power_of_2() bool {
	if x.signum <= 0 {
		return false
	}

	// check if all but the most significant digit are 0
	for i := 0; i < x.digits.len - 1; i++ {
		if x.digits[i] != 0 {
			return false
		}
	}
	n := x.digits.last()
	return n & (n - u32(1)) == 0
}

// bit_len returns the number of bits required to represent the integer `a`.
@[inline]
pub fn (x Integer) bit_len() int {
	if x.signum == 0 {
		return 0
	}
	if x.digits.len == 0 {
		return 0
	}
	return x.digits.len * 32 - bits.leading_zeros_32(x.digits.last())
}
