module big

import math
import math.bits
import strings
import strconv

const (
	digit_array = '0123456789abcdefghijklmnopqrstuvwxyz'.bytes()
)

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

[unsafe]
fn (mut x Integer) free() {
	if x.is_const {
		return
	}
	unsafe { x.digits.free() }
}

fn (x Integer) clone() Integer {
	return Integer{
		digits: x.digits.clone()
		signum: x.signum
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
		digits: [u32(math.abs(value))]
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

[params]
pub struct IntegerConfig {
	signum int = 1
}

// integer_from_bytes creates a new `big.Integer` from the given byte array.
// By default, positive integers are assumed.
// If you want a negative integer, use in the following manner:
// `value := big.integer_from_bytes(bytes, signum: -1)`
[direct_array_access]
pub fn integer_from_bytes(input []u8, config IntegerConfig) Integer {
	// Thank you to Miccah (@mcastorina) for this implementation and relevant unit tests.
	if input.len == 0 {
		return integer_from_int(0)
	}
	// pad input
	mut padded_input := []u8{len: ((input.len + 3) & ~0x3) - input.len, cap: (input.len + 3) & ~0x3}
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
pub fn integer_from_string(characters string) ?Integer {
	return integer_from_radix(characters, 10)
}

// integer_from_radix creates a new `big.Integer` from the given string and radix.
pub fn integer_from_radix(all_characters string, radix u32) ?Integer {
	if radix < 2 || radix > 36 {
		return error('Radix must be between 2 and 36 (inclusive)')
	}
	characters := all_characters.to_lower()
	validate_string(characters, radix)?
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

[direct_array_access]
fn validate_string(characters string, radix u32) ? {
	sign_present := characters[0] == `+` || characters[0] == `-`

	start_index := if sign_present { 1 } else { 0 }

	for index := start_index; index < characters.len; index++ {
		digit := characters[index]
		value := big.digit_array.index(digit)

		if value == -1 {
			return error('Invalid character $digit')
		}
		if value >= radix {
			return error('Invalid character $digit for base $radix')
		}
	}
}

[direct_array_access]
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
		value := u32(big.digit_array.index(digit))

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

[direct_array_access]
fn integer_from_regular_string(characters string, radix u32) Integer {
	sign_present := characters[0] == `+` || characters[0] == `-`

	signum := if sign_present {
		if characters[0] == `-` { -1 } else { 1 }
	} else {
		1
	}

	start_index := if sign_present { 1 } else { 0 }

	mut result := zero_int
	radix_int := integer_from_u32(radix)

	for index := start_index; index < characters.len; index++ {
		digit := characters[index]
		value := big.digit_array.index(digit)

		result *= radix_int
		result += integer_from_int(value)
	}

	return Integer{
		digits: result.digits.clone()
		signum: result.signum * signum
	}
}

// abs returns the absolute value of the integer.
pub fn (integer Integer) abs() Integer {
	return if integer.signum == 0 {
		zero_int
	} else {
		Integer{
			digits: integer.digits.clone()
			signum: 1
		}
	}
}

// neg returns the result of negation of the integer.
pub fn (integer Integer) neg() Integer {
	return if integer.signum == 0 {
		zero_int
	} else {
		Integer{
			digits: integer.digits.clone()
			signum: -integer.signum
		}
	}
}

pub fn (integer Integer) + (addend Integer) Integer {
	// Quick exits
	if integer.signum == 0 {
		return addend.clone()
	}
	if addend.signum == 0 {
		return integer.clone()
	}
	// Non-zero cases
	return if integer.signum == addend.signum {
		integer.add(addend)
	} else { // Unequal signs
		integer.subtract(addend)
	}
}

pub fn (integer Integer) - (subtrahend Integer) Integer {
	// Quick exits
	if integer.signum == 0 {
		return subtrahend.neg()
	}
	if subtrahend.signum == 0 {
		return integer.clone()
	}
	// Non-zero cases
	return if integer.signum == subtrahend.signum {
		integer.subtract(subtrahend)
	} else {
		integer.add(subtrahend)
	}
}

fn (integer Integer) add(addend Integer) Integer {
	a := integer.digits
	b := addend.digits
	mut storage := []u32{len: math.max(a.len, b.len) + 1}
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

pub fn (integer Integer) * (multiplicand Integer) Integer {
	// Quick exits
	if integer.signum == 0 || multiplicand.signum == 0 {
		return zero_int
	}
	if integer == one_int {
		return multiplicand.clone()
	}
	if multiplicand == one_int {
		return integer.clone()
	}
	// The final sign is the product of the signs
	mut storage := []u32{len: integer.digits.len + multiplicand.digits.len}
	multiply_digit_array(integer.digits, multiplicand.digits, mut storage)
	return Integer{
		signum: integer.signum * multiplicand.signum
		digits: storage
	}
}

// div_mod returns the quotient and remainder of the integer division.
pub fn (integer Integer) div_mod(divisor Integer) (Integer, Integer) {
	// Quick exits
	if divisor.signum == 0 {
		panic('Cannot divide by zero')
	}
	if integer.signum == 0 {
		return zero_int, zero_int
	}
	if divisor == one_int {
		return integer.clone(), zero_int
	}
	if divisor.signum == -1 {
		q, r := integer.div_mod(divisor.neg())
		return q.neg(), r
	}
	if integer.signum == -1 {
		q, r := integer.neg().div_mod(divisor)
		if r.signum == 0 {
			return q.neg(), zero_int
		} else {
			return q.neg() - one_int, divisor - r
		}
	}
	// Division for positive integers
	mut q := []u32{cap: integer.digits.len - divisor.digits.len + 1}
	mut r := []u32{cap: integer.digits.len}
	divide_digit_array(integer.digits, divisor.digits, mut q, mut r)
	quotient := Integer{
		signum: if q.len == 0 { 0 } else { 1 }
		digits: q
	}
	remainder := Integer{
		signum: if r.len == 0 { 0 } else { 1 }
		digits: r
	}
	return quotient, remainder
}

pub fn (a Integer) / (b Integer) Integer {
	q, _ := a.div_mod(b)
	return q
}

pub fn (a Integer) % (b Integer) Integer {
	_, r := a.div_mod(b)
	return r
}

// pow returns the integer `a` raised to the power of the u32 `exponent`.
pub fn (a Integer) pow(exponent u32) Integer {
	if exponent == 0 {
		return one_int
	}
	if exponent == 1 {
		return a.clone()
	}
	mut n := exponent
	mut x := a
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

// mod_pow returns the integer `a` raised to the power of the u32 `exponent` modulo the integer `divisor`.
pub fn (a Integer) mod_pow(exponent u32, divisor Integer) Integer {
	if exponent == 0 {
		return one_int
	}
	if exponent == 1 {
		return a % divisor
	}
	mut n := exponent
	mut x := a % divisor
	mut y := one_int
	for n > 1 {
		if n & 1 == 1 {
			y *= x % divisor
		}
		x *= x % divisor
		n >>= 1
	}
	return x * y % divisor
}

// big_mod_power returns the integer `a` raised to the power of the integer `exponent` modulo the integer `divisor`.
[direct_array_access]
pub fn (a Integer) big_mod_pow(exponent Integer, divisor Integer) Integer {
	if exponent.signum < 0 {
		panic('Exponent needs to be non-negative.')
	}
	if exponent.signum == 0 {
		return one_int
	}
	mut x := a % divisor
	mut y := one_int
	mut n := u32(0)

	// For all but the last digit of the exponent
	for index in 0 .. exponent.digits.len - 1 {
		n = exponent.digits[index]
		for _ in 0 .. 32 {
			if n & 1 == 1 {
				y *= x % divisor
			}
			x *= x % divisor
			n >>= 1
		}
	}

	// Last digit of the exponent
	n = exponent.digits.last()
	for n > 1 {
		if n & 1 == 1 {
			y *= x % divisor
		}
		x *= x % divisor
		n >>= 1
	}

	return x * y % divisor
}

// inc returns the integer `a` incremented by 1.
pub fn (mut a Integer) inc() {
	a = a + one_int
}

// dec returns the integer `a` decremented by 1.
pub fn (mut a Integer) dec() {
	a = a - one_int
}

pub fn (a Integer) == (b Integer) bool {
	return a.signum == b.signum && a.digits.len == b.digits.len && a.digits == b.digits
}

// abs_cmp returns the result of comparing the magnitudes of the integers `a` and `b`.
// It returns a negative int if `|a| < |b|`, 0 if `|a| == |b|`, and a positive int if `|a| > |b|`.
pub fn (a Integer) abs_cmp(b Integer) int {
	return compare_digit_array(a.digits, b.digits)
}

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

fn check_sign(a Integer) {
	if a.signum < 0 {
		panic('Bitwise operations are only supported for nonnegative integers')
	}
}

// get_bit checks whether the bit at the given index is set.
[direct_array_access]
pub fn (a Integer) get_bit(i u32) bool {
	check_sign(a)
	target_index := i / 32
	offset := i % 32
	if target_index >= a.digits.len {
		return false
	}
	return (a.digits[target_index] >> offset) & 1 != 0
}

// set_bit sets the bit at the given index to the given value.
pub fn (mut a Integer) set_bit(i u32, value bool) {
	check_sign(a)
	target_index := i / 32
	offset := i % 32

	if target_index >= a.digits.len {
		if value {
			a = one_int.lshift(i).bitwise_or(a)
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

// bitwise_or returns the "bitwise or" of the integers `a` and `b`.
pub fn (a Integer) bitwise_or(b Integer) Integer {
	check_sign(a)
	check_sign(b)
	mut result := []u32{len: math.max(a.digits.len, b.digits.len)}
	bitwise_or_digit_array(a.digits, b.digits, mut result)
	return Integer{
		digits: result
		signum: if result.len == 0 { 0 } else { 1 }
	}
}

// bitwise_and returns the "bitwise and" of the integers `a` and `b`.
pub fn (a Integer) bitwise_and(b Integer) Integer {
	check_sign(a)
	check_sign(b)
	mut result := []u32{len: math.max(a.digits.len, b.digits.len)}
	bitwise_and_digit_array(a.digits, b.digits, mut result)
	return Integer{
		digits: result
		signum: if result.len == 0 { 0 } else { 1 }
	}
}

// bitwise_not returns the "bitwise not" of the integer `a`.
pub fn (a Integer) bitwise_not() Integer {
	check_sign(a)
	mut result := []u32{len: a.digits.len}
	bitwise_not_digit_array(a.digits, mut result)
	return Integer{
		digits: result
		signum: if result.len == 0 { 0 } else { 1 }
	}
}

// bitwise_xor returns the "bitwise exclusive or" of the integers `a` and `b`.
pub fn (a Integer) bitwise_xor(b Integer) Integer {
	check_sign(a)
	check_sign(b)
	mut result := []u32{len: math.max(a.digits.len, b.digits.len)}
	bitwise_xor_digit_array(a.digits, b.digits, mut result)
	return Integer{
		digits: result
		signum: if result.len == 0 { 0 } else { 1 }
	}
}

// lshift returns the integer `a` shifted left by `amount` bits.
[direct_array_access]
pub fn (a Integer) lshift(amount u32) Integer {
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

// rshift returns the integer `a` shifted right by `amount` bits.
[direct_array_access]
pub fn (a Integer) rshift(amount u32) Integer {
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
		signum: a.signum
	}
}

// binary_str returns the binary string representation of the integer `a`.
[direct_array_access]
pub fn (integer Integer) binary_str() string {
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
[direct_array_access]
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
	if integer.signum == 0 {
		return '0'
	}
	return match radix {
		2 {
			integer.binary_str()
		}
		16 {
			integer.hex()
		}
		else {
			integer.general_radix_str(radix)
		}
	}
}

fn (integer Integer) general_radix_str(radix u32) string {
	divisor := integer_from_u32(radix)

	mut current := integer.abs()
	mut new_current := zero_int
	mut digit := zero_int
	mut rune_array := []rune{cap: current.digits.len * 4}
	for current.signum > 0 {
		new_current, digit = current.div_mod(divisor)
		rune_array << big.digit_array[digit.int()]
		unsafe { digit.free() }
		unsafe { current.free() }
		current = new_current
	}
	if integer.signum == -1 {
		rune_array << `-`
	}

	rune_array.reverse_in_place()
	return rune_array.string()
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
pub fn (a Integer) int() int {
	if a.signum == 0 {
		return 0
	}
	value := int(a.digits[0] & 0x7fffffff)
	return value * a.signum
}

// bytes returns the a byte representation of the integer a, along with the signum int.
// NOTE: The byte array returned is in big endian order.
[direct_array_access]
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

// isqrt returns the closest integer square root of the given integer.
pub fn (a Integer) isqrt() Integer {
	if a.signum < 0 {
		panic('Cannot obtain square root of negative integer')
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
		result = result.lshift(1)
		larger := result + one_int
		if (larger * larger).abs_cmp(a.rshift(u32(shift))) <= 0 {
			result = larger
		}
		shift -= 2
	}
	return result
}

[inline]
fn bi_min(a Integer, b Integer) Integer {
	return if a < b { a } else { b }
}

[inline]
fn bi_max(a Integer, b Integer) Integer {
	return if a > b { a } else { b }
}

[direct_array_access]
fn (bi Integer) msb() u32 {
	for idx := 0; idx < bi.digits.len; idx += 1 {
		word := bi.digits[idx]
		if word > 0 {
			return u32((idx * 32) + bits.trailing_zeros_32(word))
		}
	}
	return u32(32)
}

// gcd returns the greatest common divisor of the two integers `a` and `b`.
pub fn (a Integer) gcd(b Integer) Integer {
	if a.signum == 0 {
		return b.abs()
	}
	if b.signum == 0 {
		return a.abs()
	}
	if a.signum < 0 {
		return a.neg().gcd(b)
	}
	if b.signum < 0 {
		return a.gcd(b.neg())
	}

	if a.digits.len + b.digits.len <= 2 {
		return gcd_euclid(a, b)
	} else {
		return gcd_binary(a, b)
	}
}

fn gcd_euclid(x Integer, y Integer) Integer {
	mut a := x
	mut b := y
	mut r := a % b
	for r.signum != 0 {
		a = b
		b = r
		r = a % b
	}
	return b
}

// Inspired by the 2013-christmas-special by D. Lemire & R. Corderoy https://en.algorithmica.org/hpc/analyzing-performance/gcd/
// For more information, refer to the Wikipedia article: https://en.wikipedia.org/wiki/Binary_GCD_algorithm
// Discussion and further information: https://lemire.me/blog/2013/12/26/fastest-way-to-compute-the-greatest-common-divisor/
fn gcd_binary(x Integer, y Integer) Integer {
	mut a := x
	mut b := y

	mut az := a.msb()
	bz := b.msb()
	shift := math.min(az, bz)
	b = b.rshift(bz)

	for a.signum != 0 {
		a = a.rshift(az)
		diff := b - a
		az = diff.msb()
		b = bi_min(a, b)
		a = diff.abs()
	}
	return b.lshift(shift)
}

// bit_len returns the number of bits required to represent the integer `a`.
[inline]
pub fn (x Integer) bit_len() int {
	if x.signum == 0 {
		return 0
	}
	if x.digits.len == 0 {
		return 0
	}
	return x.digits.len * 32 - bits.leading_zeros_32(x.digits.last())
}
