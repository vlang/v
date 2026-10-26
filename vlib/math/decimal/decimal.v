// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module decimal

import math.big
import strings

// default_division_precision is the minimum number of digits after the decimal
// point that the `/` operator keeps for an inexact quotient.
pub const default_division_precision = 16

// zero is the canonical zero value for Decimal.
pub const zero = Decimal{
	coefficient_value: big.zero_int
	scale_value:       0
}

// Decimal represents an arbitrary-precision fixed-point decimal value.
// Its numeric value is `coefficient * 10^-scale`.
pub struct Decimal {
	coefficient_value big.Integer
	scale_value       int
}

// RoundingMode selects how `round` and `div` treat the digits they discard.
pub enum RoundingMode {
	half_up   // round to nearest; ties away from zero
	half_even // round to nearest; ties to the even neighbour (banker's rounding)
	half_down // round to nearest; ties towards zero
	up        // away from zero
	down      // towards zero (truncate)
	ceiling   // towards positive infinity
	floor     // towards negative infinity
}

// new creates a decimal from `coefficient * 10^-scale` and keeps `scale`, so
// `new(500, 2)` is `5.00`.
pub fn new(coefficient big.Integer, scale int) Decimal {
	if scale < 0 {
		panic('math.decimal: scale cannot be negative')
	}
	return Decimal{
		coefficient_value: coefficient
		scale_value:       scale
	}
}

// from_int creates a decimal from an int.
pub fn from_int(value int) Decimal {
	return Decimal{
		coefficient_value: big.integer_from_int(value)
		scale_value:       0
	}
}

// from_i64 creates a decimal from an i64.
pub fn from_i64(value i64) Decimal {
	return Decimal{
		coefficient_value: big.integer_from_i64(value)
		scale_value:       0
	}
}

// from_u64 creates a decimal from a u64.
pub fn from_u64(value u64) Decimal {
	return Decimal{
		coefficient_value: big.integer_from_u64(value)
		scale_value:       0
	}
}

// from_minor_units creates a decimal from an amount in units of `10^-precision`,
// so `from_minor_units(1999, 2)` is `19.99` and `from_minor_units(500, 2)` is `5.00`.
pub fn from_minor_units(units i64, precision int) !Decimal {
	if precision < 0 {
		return error('math.decimal: precision cannot be negative')
	}
	return new(big.integer_from_i64(units), precision)
}

// from_string parses a decimal string with an optional base-10 exponent.
// Exponents from -100 to 100 are supported; larger magnitudes return an error.
// The digits after the decimal point set the scale, so `5.00` keeps scale 2;
// an exponent shifts it, so `1.50e1` is `15.0`.
pub fn from_string(input string) !Decimal {
	value := input.trim_space()
	if value.len == 0 {
		return error('math.decimal: empty input')
	}
	mut negative := false
	mut seen_dot := false
	mut digit_count := 0
	mut scale := 0
	mut digits := strings.new_builder(value.len)
	mut exponent := 0
	mut exponent_negative := false
	mut exponent_sign_seen := false
	mut exponent_digits := 0
	mut in_exponent := false
	for index, ch in value {
		if index == 0 && (ch == `+` || ch == `-`) {
			negative = ch == `-`
			continue
		}
		if in_exponent {
			if ch == `+` || ch == `-` {
				if exponent_digits > 0 || exponent_sign_seen {
					return error('math.decimal: invalid decimal value `${input}`')
				}
				exponent_sign_seen = true
				exponent_negative = ch == `-`
				continue
			}
			if ch >= `0` && ch <= `9` {
				exponent_digits++
				digit := int(ch - `0`)
				// Check before multiplying to reject large exponents without integer overflow.
				if exponent > 10 || (exponent == 10 && digit > 0) {
					return error('math.decimal: exponent out of range `${input}`')
				}
				exponent = exponent * 10 + digit
				continue
			}
			return error('math.decimal: invalid decimal value `${input}`')
		}
		if ch == `e` || ch == `E` {
			if digit_count == 0 {
				return error('math.decimal: invalid decimal value `${input}`')
			}
			in_exponent = true
			continue
		}
		if ch >= `0` && ch <= `9` {
			digits.write_u8(ch)
			digit_count++
			if seen_dot {
				scale++
			}
			continue
		}
		if ch == `.` && !seen_dot {
			seen_dot = true
			continue
		}
		return error('math.decimal: invalid decimal value `${input}`')
	}
	if digit_count == 0 {
		return error('math.decimal: invalid decimal value `${input}`')
	}
	if in_exponent && exponent_digits == 0 {
		return error('math.decimal: invalid decimal value `${input}`')
	}
	if exponent_negative {
		exponent = -exponent
	}
	mut coefficient_string := digits.str()
	if negative {
		coefficient_string = '-' + coefficient_string
	}
	mut coefficient := big.integer_from_string(coefficient_string)!
	// Apply exponent to scale: value = coefficient * 10^(exponent - scale)
	mut final_scale := scale - exponent
	if final_scale < 0 {
		// multiply coefficient by 10^(-final_scale)
		coefficient = coefficient * pow10(-final_scale)
		final_scale = 0
	}
	return new(coefficient, final_scale)
}

// coefficient returns the integer coefficient stored by `d`.
pub fn (d Decimal) coefficient() big.Integer {
	return d.coefficient_value
}

// scale returns the number of decimal places stored by `d`.
pub fn (d Decimal) scale() int {
	return d.scale_value
}

// normalize returns `d` without trailing zeros after the decimal point, so
// `5.00` becomes `5` and `1.250` becomes `1.25`. The value does not change.
pub fn (d Decimal) normalize() Decimal {
	return strip_zeros(d.coefficient_value, d.scale_value, 0)
}

// is_zero returns true when `d == 0`.
pub fn (d Decimal) is_zero() bool {
	return d.coefficient_value.signum == 0
}

// abs returns the absolute value of `d`.
pub fn (d Decimal) abs() Decimal {
	return Decimal{
		coefficient_value: d.coefficient_value.abs()
		scale_value:       d.scale_value
	}
}

// neg returns the negated value of `d`.
pub fn (d Decimal) neg() Decimal {
	return Decimal{
		coefficient_value: d.coefficient_value.neg()
		scale_value:       d.scale_value
	}
}

// sign returns -1, 0 or 1 when `d` is negative, zero or positive.
pub fn (d Decimal) sign() int {
	return d.coefficient_value.signum
}

// min returns the smaller of `a` and `b`, or `a` when they are equal.
pub fn min(a Decimal, b Decimal) Decimal {
	if b < a {
		return b
	}
	return a
}

// max returns the larger of `a` and `b`, or `a` when they are equal.
pub fn max(a Decimal, b Decimal) Decimal {
	if a < b {
		return b
	}
	return a
}

// rem returns the remainder of dividing `dividend` by `divisor` with the
// quotient truncated towards zero. Like `%` on V integers, the result takes
// the sign of `dividend`: `-7.5 rem 2` is `-1.5`. Its scale is the larger of
// the two scales.
pub fn (dividend Decimal) rem(divisor Decimal) !Decimal {
	if divisor.coefficient_value.signum == 0 {
		return error('math.decimal: cannot divide by zero')
	}
	scale := max_scale(dividend.scale_value, divisor.scale_value)
	_, remainder := dividend.rescaled_coefficient(scale).abs().div_mod(divisor.rescaled_coefficient(scale).abs())
	if dividend.coefficient_value.signum < 0 {
		return new(remainder.neg(), scale)
	}
	return new(remainder, scale)
}

// to_minor_units returns `d` as an amount in units of `10^-precision`, so
// `19.99` with precision 2 is `1999`. It returns an error when `d` has more
// than `precision` digits after the decimal point, not counting trailing
// zeros (round it first), or when the amount does not fit in an i64.
pub fn (d Decimal) to_minor_units(precision int) !i64 {
	if precision < 0 {
		return error('math.decimal: precision cannot be negative')
	}
	value := d.normalize()
	if value.scale_value > precision {
		return error('math.decimal: value has more than ${precision} decimal places')
	}
	if value.coefficient_value.signum == 0 {
		return 0
	}
	// Any non-zero coefficient times 10^19 exceeds max_i64; reject before
	// computing a possibly huge power of ten.
	if precision - value.scale_value > 18 {
		return error('math.decimal: value does not fit in i64')
	}
	return to_i64(value.rescaled_coefficient(precision))
}

fn to_i64(value big.Integer) !i64 {
	bytes, signum := value.bytes()
	if bytes.len > 8 {
		return error('math.decimal: value does not fit in i64')
	}
	mut magnitude := u64(0)
	for b in bytes {
		magnitude = (magnitude << 8) | u64(b)
	}
	if signum < 0 {
		if magnitude > u64(1) << 63 {
			return error('math.decimal: value does not fit in i64')
		}
		// Subtract before negating so that 2^63 maps to min_i64 without overflow.
		return -i64(magnitude - 1) - 1
	}
	if magnitude > u64(max_i64) {
		return error('math.decimal: value does not fit in i64')
	}
	return i64(magnitude)
}

// str returns `d` in plain decimal notation with exactly `d.scale()` digits
// after the decimal point, so `5.00` stays `5.00`.
pub fn (d Decimal) str() string {
	if d.scale_value == 0 {
		return d.coefficient_value.str()
	}
	mut prefix := ''
	if d.coefficient_value.signum < 0 {
		prefix = '-'
	}
	absolute := d.coefficient_value.abs().str()
	if d.scale_value >= absolute.len {
		padding := strings.repeat(`0`, d.scale_value - absolute.len)
		return '${prefix}0.${padding}${absolute}'
	}
	split := absolute.len - d.scale_value
	return '${prefix}${absolute[..split]}.${absolute[split..]}'
}

// + returns the exact sum of `left` and `right`, with the larger of their scales.
pub fn (left Decimal) + (right Decimal) Decimal {
	scale := max_scale(left.scale_value, right.scale_value)
	return new(left.rescaled_coefficient(scale) + right.rescaled_coefficient(scale), scale)
}

// - returns the exact difference of `left` and `right`, with the larger of
// their scales.
pub fn (left Decimal) - (right Decimal) Decimal {
	scale := max_scale(left.scale_value, right.scale_value)
	return new(left.rescaled_coefficient(scale) - right.rescaled_coefficient(scale), scale)
}

// * returns the exact product of `left` and `right`, with the sum of their scales.
pub fn (left Decimal) * (right Decimal) Decimal {
	return new(left.coefficient_value * right.coefficient_value, left.scale_value +
		right.scale_value)
}

// / divides `dividend` by `divisor`. Its preferred scale is
// `dividend.scale() - divisor.scale()` (at least 0): an exact quotient keeps
// only as many more places as it needs, so `10.00 / 4` is `2.50` and `1 / 4`
// is `0.25`. An inexact quotient is rounded half up to the larger of
// `default_division_precision` and the preferred scale. It panics on a zero
// divisor; use `div` where the divisor can be zero.
pub fn (dividend Decimal) / (divisor Decimal) Decimal {
	if divisor.coefficient_value.signum == 0 {
		panic('math.decimal: cannot divide by zero')
	}
	preferred_scale := max_scale(dividend.scale_value - divisor.scale_value, 0)
	precision := max_scale(default_division_precision, preferred_scale)
	quotient, remainder := scaled_div_mod(dividend, divisor, precision)
	negative := dividend.coefficient_value.signum * divisor.coefficient_value.signum < 0
	if remainder.signum == 0 {
		coefficient := if negative { quotient.neg() } else { quotient }
		return strip_zeros(coefficient, precision, preferred_scale)
	}
	return rounded(quotient, remainder, scaled_divisor(dividend, divisor), negative, precision,
		.half_up)
}

// div_prec divides `dividend` by `divisor` and rounds half up to exactly
// `precision` digits after the decimal point.
pub fn (dividend Decimal) div_prec(divisor Decimal, precision int) !Decimal {
	return dividend.div(divisor, precision, .half_up)
}

// div divides `dividend` by `divisor` and rounds the quotient to exactly
// `precision` digits after the decimal point using `mode`.
pub fn (dividend Decimal) div(divisor Decimal, precision int, mode RoundingMode) !Decimal {
	if precision < 0 {
		return error('math.decimal: precision cannot be negative')
	}
	if divisor.coefficient_value.signum == 0 {
		return error('math.decimal: cannot divide by zero')
	}
	quotient, remainder := scaled_div_mod(dividend, divisor, precision)
	negative := dividend.coefficient_value.signum * divisor.coefficient_value.signum < 0
	return rounded(quotient, remainder, scaled_divisor(dividend, divisor), negative, precision,
		mode)
}

// round returns `d` rounded to exactly `precision` digits after the decimal
// point using `mode`. A value with fewer digits is padded with zeros, so
// `1.5` rounded to 3 places is `1.500`.
pub fn (d Decimal) round(precision int, mode RoundingMode) !Decimal {
	if precision < 0 {
		return error('math.decimal: precision cannot be negative')
	}
	if d.scale_value <= precision {
		return new(d.rescaled_coefficient(precision), precision)
	}
	divisor := pow10(d.scale_value - precision)
	quotient, remainder := d.coefficient_value.abs().div_mod(divisor)
	return rounded(quotient, remainder, divisor, d.coefficient_value.signum < 0, precision,
		mode)
}

// == returns true when `left` and `right` represent the same value, whatever
// their scales, so `5.0 == 5.00`.
pub fn (left Decimal) == (right Decimal) bool {
	scale := max_scale(left.scale_value, right.scale_value)
	return left.rescaled_coefficient(scale) == right.rescaled_coefficient(scale)
}

// < returns true when `left` is smaller than `right`.
pub fn (left Decimal) < (right Decimal) bool {
	scale := max_scale(left.scale_value, right.scale_value)
	return left.rescaled_coefficient(scale) < right.rescaled_coefficient(scale)
}

// strip_zeros removes trailing zeros from `coefficient * 10^-scale`, lowering
// the scale no further than `min_scale`.
fn strip_zeros(coefficient big.Integer, scale int, min_scale int) Decimal {
	if coefficient.signum == 0 {
		return new(coefficient, min_scale)
	}
	mut stripped_coefficient := coefficient
	mut stripped_scale := scale
	for stripped_scale > min_scale {
		quotient, remainder := stripped_coefficient.div_mod(big.c10)
		if remainder.signum != 0 {
			break
		}
		stripped_coefficient = quotient
		stripped_scale--
	}
	return Decimal{
		coefficient_value: stripped_coefficient
		scale_value:       stripped_scale
	}
}

fn (d Decimal) rescaled_coefficient(scale int) big.Integer {
	if scale <= d.scale_value || d.coefficient_value.signum == 0 {
		return d.coefficient_value
	}
	return d.coefficient_value * pow10(scale - d.scale_value)
}

fn pow10(exponent int) big.Integer {
	if exponent <= 0 {
		return big.one_int
	}
	return big.c10.pow(u32(exponent))
}

fn scaled_divisor(dividend Decimal, divisor Decimal) big.Integer {
	return divisor.coefficient_value.abs() * pow10(dividend.scale_value)
}

fn scaled_div_mod(dividend Decimal, divisor Decimal, precision int) (big.Integer, big.Integer) {
	numerator := dividend.coefficient_value.abs() * pow10(divisor.scale_value + precision)
	return numerator.div_mod(scaled_divisor(dividend, divisor))
}

// rounded builds the decimal `±(quotient + remainder/divisor) * 10^-scale`,
// rounding the magnitude to an integer with `mode`. `quotient` and `remainder`
// are the non-negative results of dividing the magnitude by `divisor`.
fn rounded(quotient big.Integer, remainder big.Integer, divisor big.Integer, negative bool, scale int, mode RoundingMode) Decimal {
	mut magnitude := quotient
	if remainder.signum != 0 && rounds_away(quotient, remainder, divisor, negative, mode) {
		magnitude += big.one_int
	}
	if negative {
		magnitude = magnitude.neg()
	}
	return new(magnitude, scale)
}

// rounds_away reports whether a non-zero `remainder` moves the magnitude
// `quotient` away from zero.
fn rounds_away(quotient big.Integer, remainder big.Integer, divisor big.Integer, negative bool, mode RoundingMode) bool {
	half := (remainder * big.two_int).abs_cmp(divisor)
	return match mode {
		.half_up { half >= 0 }
		.half_even { half > 0 || (half == 0 && quotient.is_odd()) }
		.half_down { half > 0 }
		.up { true }
		.down { false }
		.ceiling { !negative }
		.floor { negative }
	}
}

fn max_scale(a int, b int) int {
	if a > b {
		return a
	}
	return b
}
