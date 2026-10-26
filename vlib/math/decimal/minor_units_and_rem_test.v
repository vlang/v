// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import math.decimal

fn must_decimal(value string) decimal.Decimal {
	return decimal.from_string(value) or { panic(err) }
}

fn minor_units_error(value string, precision int) string {
	must_decimal(value).to_minor_units(precision) or { return err.msg() }
	return 'no error'
}

fn test_from_minor_units() {
	assert decimal.from_minor_units(1999, 2)!.str() == '19.99'
	assert decimal.from_minor_units(-5, 3)!.str() == '-0.005'
	assert decimal.from_minor_units(100, 2)!.str() == '1.00'
	assert decimal.from_minor_units(0, 2)!.is_zero()
	assert decimal.from_minor_units(min_i64, 0)!.str() == '-9223372036854775808'
	decimal.from_minor_units(1, -1) or {
		assert err.msg() == 'math.decimal: precision cannot be negative'
		return
	}
	assert false
}

fn test_to_minor_units() {
	assert must_decimal('19.99').to_minor_units(2)! == 1999
	assert must_decimal('19.9').to_minor_units(2)! == 1990
	assert must_decimal('5').to_minor_units(2)! == 500
	assert must_decimal('-0.01').to_minor_units(2)! == -1
	assert must_decimal('0').to_minor_units(30)! == 0
	assert must_decimal('1').to_minor_units(18)! == 1_000_000_000_000_000_000
	assert must_decimal('0.1').to_minor_units(19)! == 1_000_000_000_000_000_000
	// Trailing zeros do not count as places.
	assert must_decimal('5.000').to_minor_units(2)! == 500
	assert must_decimal('19.990').to_minor_units(2)! == 1999
	assert must_decimal('0.000').to_minor_units(0)! == 0
}

fn test_to_minor_units_i64_bounds() {
	assert must_decimal('9223372036854775807').to_minor_units(0)! == max_i64
	assert must_decimal('92233720368547758.07').to_minor_units(2)! == max_i64
	assert must_decimal('-9223372036854775808').to_minor_units(0)! == min_i64
	assert minor_units_error('9223372036854775808', 0) == 'math.decimal: value does not fit in i64'
	assert minor_units_error('-9223372036854775809', 0) == 'math.decimal: value does not fit in i64'
	assert minor_units_error('100000000000000000000', 0) == 'math.decimal: value does not fit in i64'
	assert minor_units_error('1', 19) == 'math.decimal: value does not fit in i64'
	assert minor_units_error('-1', 1000000) == 'math.decimal: value does not fit in i64'
}

fn test_to_minor_units_rejects_inexact_and_negative_precision() {
	assert minor_units_error('19.999', 2) == 'math.decimal: value has more than 2 decimal places'
	assert minor_units_error('0.5', 0) == 'math.decimal: value has more than 0 decimal places'
	assert minor_units_error('1', -1) == 'math.decimal: precision cannot be negative'
}

fn test_minor_units_round_trip() {
	for units in [i64(0), 1, -1, 1999, -123456789, max_i64, min_i64] {
		for precision in [0, 2, 6] {
			assert decimal.from_minor_units(units, precision)!.to_minor_units(precision)! == units
		}
	}
}

fn test_rem_takes_sign_of_dividend() {
	assert must_decimal('7').rem(must_decimal('3'))!.str() == '1'
	assert must_decimal('-7').rem(must_decimal('3'))!.str() == '-1'
	assert must_decimal('7').rem(must_decimal('-3'))!.str() == '1'
	assert must_decimal('-7').rem(must_decimal('-3'))!.str() == '-1'
	// The same signs as `%` on V integers.
	assert -7 % 3 == -1 && 7 % -3 == 1
}

fn test_rem_with_fractions() {
	assert must_decimal('5.5').rem(must_decimal('2'))!.str() == '1.5'
	assert must_decimal('-7.5').rem(must_decimal('2'))!.str() == '-1.5'
	assert must_decimal('1').rem(must_decimal('0.3'))!.str() == '0.1'
	assert must_decimal('-0.75').rem(must_decimal('0.5'))!.str() == '-0.25'
	assert must_decimal('6').rem(must_decimal('3'))!.is_zero()
	assert must_decimal('0.2').rem(must_decimal('5'))!.str() == '0.2'
	must_decimal('1').rem(decimal.zero) or {
		assert err.msg() == 'math.decimal: cannot divide by zero'
		return
	}
	assert false
}

fn test_sign() {
	assert must_decimal('-0.5').sign() == -1
	assert must_decimal('0.000').sign() == 0
	assert must_decimal('3').sign() == 1
}

fn test_min_and_max() {
	a := must_decimal('1.5')
	b := must_decimal('-2')
	assert decimal.min(a, b).str() == '-2'
	assert decimal.max(a, b).str() == '1.5'
	assert decimal.min(b, a).str() == '-2'
	assert decimal.max(b, a).str() == '1.5'
	// Equal values: both return the first argument.
	assert decimal.min(must_decimal('1.10'), must_decimal('1.1')).str() == '1.10'
	assert decimal.max(must_decimal('1.1'), must_decimal('1.10')).str() == '1.1'
}
