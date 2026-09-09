// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module decimal

import math.big
import strings

// default_division_precision is used by the `/` operator when the quotient
// needs a rounded decimal expansion.
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

// new creates a decimal from `coefficient * 10^-scale`.
pub fn new(coefficient big.Integer, scale int) Decimal {
	if scale < 0 {
		panic('math.decimal: scale cannot be negative')
	}
	return normalize(coefficient, scale)
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

// from_string parses a decimal string without exponent notation.
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
	for index, ch in value {
		if index == 0 && (ch == `+` || ch == `-`) {
			negative = ch == `-`
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
	mut coefficient_string := digits.str()
	if negative {
		coefficient_string = '-' + coefficient_string
	}
	coefficient := big.integer_from_string(coefficient_string)!
	return new(coefficient, scale)
}

// coefficient returns the integer coefficient stored by `d`.
pub fn (d Decimal) coefficient() big.Integer {
	return d.coefficient_value
}

// scale returns the number of decimal places stored by `d`.
pub fn (d Decimal) scale() int {
	return d.scale_value
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

// str returns the canonical decimal representation of `d`.
pub fn (d Decimal) str() string {
	if d.coefficient_value.signum == 0 {
		return '0'
	}
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

// + returns the exact sum of `left` and `right`.
pub fn (left Decimal) + (right Decimal) Decimal {
	scale := max_scale(left.scale_value, right.scale_value)
	return new(left.rescaled_coefficient(scale) + right.rescaled_coefficient(scale), scale)
}

// - returns the exact difference of `left` and `right`.
pub fn (left Decimal) - (right Decimal) Decimal {
	scale := max_scale(left.scale_value, right.scale_value)
	return new(left.rescaled_coefficient(scale) - right.rescaled_coefficient(scale), scale)
}

// * returns the exact product of `left` and `right`.
pub fn (left Decimal) * (right Decimal) Decimal {
	return new(left.coefficient_value * right.coefficient_value, left.scale_value +
		right.scale_value)
}

// / divides `dividend` by `divisor` using `default_division_precision`.
pub fn (dividend Decimal) / (divisor Decimal) Decimal {
	return dividend.div_prec(divisor, default_division_precision) or { panic(err) }
}

// div_prec divides `dividend` by `divisor` and rounds half up to `precision`
// digits after the decimal point.
pub fn (dividend Decimal) div_prec(divisor Decimal, precision int) !Decimal {
	if precision < 0 {
		return error('math.decimal: precision cannot be negative')
	}
	if divisor.coefficient_value.signum == 0 {
		return error('math.decimal: cannot divide by zero')
	}
	mut quotient, remainder := scaled_div_mod(dividend, divisor, precision)
	if remainder.signum != 0 {
		twice_remainder := remainder * big.two_int
		if !(twice_remainder < scaled_divisor(dividend, divisor)) {
			quotient += big.one_int
		}
	}
	if dividend.coefficient_value.signum * divisor.coefficient_value.signum < 0 {
		quotient = quotient.neg()
	}
	return new(quotient, precision)
}

// == returns true when `left` and `right` represent the same value.
pub fn (left Decimal) == (right Decimal) bool {
	return left.scale_value == right.scale_value
		&& left.coefficient_value == right.coefficient_value
}

// < returns true when `left` is smaller than `right`.
pub fn (left Decimal) < (right Decimal) bool {
	scale := max_scale(left.scale_value, right.scale_value)
	return left.rescaled_coefficient(scale) < right.rescaled_coefficient(scale)
}

fn normalize(coefficient big.Integer, scale int) Decimal {
	if coefficient.signum == 0 {
		return zero
	}
	mut normalized_coefficient := coefficient
	mut normalized_scale := scale
	for normalized_scale > 0 {
		quotient, remainder := normalized_coefficient.div_mod(big.c10)
		if remainder.signum != 0 {
			break
		}
		normalized_coefficient = quotient
		normalized_scale--
	}
	return Decimal{
		coefficient_value: normalized_coefficient
		scale_value:       normalized_scale
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

fn max_scale(a int, b int) int {
	if a > b {
		return a
	}
	return b
}
