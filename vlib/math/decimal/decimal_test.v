// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import math.big
import math.decimal

fn must_decimal(value string) decimal.Decimal {
	return decimal.from_string(value) or { panic(err) }
}

fn assert_parse_error(value string, expected string) {
	decimal.from_string(value) or {
		assert err.msg() == expected
		return
	}
	assert false
}

fn assert_div_error(dividend string, divisor string, precision int, expected string) {
	must_decimal(dividend).div_prec(must_decimal(divisor), precision) or {
		assert err.msg() == expected
		return
	}
	assert false
}

fn test_new_and_getters() {
	value := decimal.new(big.integer_from_string('123400') or { panic(err) }, 4)
	assert value.str() == '12.34'
	assert value.coefficient().str() == '1234'
	assert value.scale() == 2
}

fn test_from_ints() {
	assert decimal.from_int(42).str() == '42'
	assert decimal.from_i64(-99).str() == '-99'
	assert decimal.from_u64(123456789).str() == '123456789'
}

fn test_from_string_normalizes_trailing_zeroes() {
	assert must_decimal('00123.4500').str() == '123.45'
	assert must_decimal('-0.5000').str() == '-0.5'
	assert must_decimal('.25').str() == '0.25'
	assert must_decimal('5.').str() == '5'
	assert must_decimal('0.000').is_zero()
}

fn test_from_string_rejects_invalid_input() {
	assert_parse_error('', 'math.decimal: empty input')
	assert_parse_error('abc', 'math.decimal: invalid decimal value `abc`')
	assert_parse_error('1.2.3', 'math.decimal: invalid decimal value `1.2.3`')
	assert_parse_error('+-1', 'math.decimal: invalid decimal value `+-1`')
	assert_parse_error('.', 'math.decimal: invalid decimal value `.`')
}

fn test_add_sub_and_mul() {
	assert (must_decimal('1.23') + must_decimal('4.5')).str() == '5.73'
	assert (must_decimal('4.5') - must_decimal('1.23')).str() == '3.27'
	assert (must_decimal('12.5') * must_decimal('0.04')).str() == '0.5'
	assert (must_decimal('9999999999999999999999999999.99') + must_decimal('0.01')).str() == '10000000000000000000000000000'
}

fn test_neg_abs_and_compare() {
	assert must_decimal('-1.20').neg().str() == '1.2'
	assert must_decimal('-1.20').abs().str() == '1.2'
	assert must_decimal('1.20') == must_decimal('1.2')
	assert must_decimal('-2') < must_decimal('-1.99')
	assert must_decimal('0.009') < must_decimal('0.01')
}

fn test_div_prec_rounds_half_up() {
	assert must_decimal('1').div_prec(must_decimal('3'), 2)!.str() == '0.33'
	assert must_decimal('1').div_prec(must_decimal('6'), 2)!.str() == '0.17'
	assert must_decimal('-1').div_prec(must_decimal('6'), 2)!.str() == '-0.17'
	assert must_decimal('10').div_prec(must_decimal('4'), 2)!.str() == '2.5'
}

fn test_div_operator_uses_default_precision() {
	assert (must_decimal('1') / must_decimal('8')).str() == '0.125'
	assert (must_decimal('1') / must_decimal('3')).str() == '0.3333333333333333'
}

fn test_div_prec_errors() {
	assert_div_error('1', '0', 2, 'math.decimal: cannot divide by zero')
	assert_div_error('1', '2', -1, 'math.decimal: precision cannot be negative')
}
