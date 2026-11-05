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
	assert value.coefficient().str() == '123400'
	assert value.scale() == 4
}

fn test_from_ints() {
	assert decimal.from_int(42).str() == '42'
	assert decimal.from_i64(-99).str() == '-99'
	assert decimal.from_u64(123456789).str() == '123456789'
}

fn test_from_string_formats_without_trailing_zeroes() {
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

fn test_from_string_exponent_notation() {
	cases := {
		'1e3':        '1000'
		'1.5E-2':     '0.015'
		'-12e+2':     '-1200'
		'+12E-1':     '1.2'
		'.25e1':      '2.5'
		'1.2e-2':     '0.012'
		'1.e2':       '100'
		' 1e2 ':      '100'
		'001.200e0':  '1.2'
		'1e000001':   '10'
		'1e-00001':   '0.1'
		'-0e+100':    '0'
		'+0.00e-100': '0'
	}
	for input, expected in cases {
		assert must_decimal(input).str() == expected, input
	}
	zeros := '0'.repeat(128)
	assert must_decimal('1e${zeros}').str() == '1'
	assert must_decimal('1e-${zeros}').str() == '1'
	assert must_decimal('1e+${zeros}100').str() == '1' + '0'.repeat(100)
	assert must_decimal('1e-${zeros}100').str() == '0.' + '0'.repeat(99) + '1'
	assert must_decimal('-0e-${zeros}').is_zero()
}

fn test_from_string_rejects_invalid_exponents() {
	for input in ['1e', '1e+', '1e-', 'e1', '.e1', '-e1', '1e--2', '1e+-2', '1e-+2', '1e++2', '1e2-',
		'1e2+', '1e2e3', '1E2E3', '1e2.0', '1 e2', '1e 2'] {
		assert_parse_error(input, 'math.decimal: invalid decimal value `${input}`')
	}
}

fn test_from_string_rejects_exponents_out_of_range() {
	for input in ['1e101', '1e-101', '1e+101', '1e2147483648', '1e-2147483648', '1e99999999999999999999',
		'1e-99999999999999999999', '1e00000000000000101', '1e-00000000000000101'] {
		assert_parse_error(input, 'math.decimal: exponent out of range `${input}`')
	}
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

struct RoundingCase {
	mode     decimal.RoundingMode
	expected []string
}

fn test_round_modes_to_integer() {
	// The reference table from java.math.RoundingMode.
	inputs := ['5.5', '2.5', '1.6', '1.1', '1.0', '-1.0', '-1.1', '-1.6', '-2.5', '-5.5']
	cases := [
		RoundingCase{.up, ['6', '3', '2', '2', '1', '-1', '-2', '-2', '-3', '-6']},
		RoundingCase{.down, ['5', '2', '1', '1', '1', '-1', '-1', '-1', '-2', '-5']},
		RoundingCase{.ceiling, ['6', '3', '2', '2', '1', '-1', '-1', '-1', '-2', '-5']},
		RoundingCase{.floor, ['5', '2', '1', '1', '1', '-1', '-2', '-2', '-3', '-6']},
		RoundingCase{.half_up, ['6', '3', '2', '1', '1', '-1', '-1', '-2', '-3', '-6']},
		RoundingCase{.half_down, ['5', '2', '2', '1', '1', '-1', '-1', '-2', '-2', '-5']},
		RoundingCase{.half_even, ['6', '2', '2', '1', '1', '-1', '-1', '-2', '-2', '-6']},
	]
	for c in cases {
		for i, input in inputs {
			got := must_decimal(input).round(0, c.mode)!.str()
			assert got == c.expected[i], '${c.mode} ${input}: got ${got}'
		}
	}
}

fn test_round_to_places() {
	vat := must_decimal('19.99') * must_decimal('0.21')
	assert vat.round(2, .half_up)!.str() == '4.2'
	assert vat.round(3, .down)!.str() == '4.197'
	assert must_decimal('0.125').round(2, .half_even)!.str() == '0.12'
	assert must_decimal('0.135').round(2, .half_even)!.str() == '0.14'
	assert must_decimal('0.125').round(2, .half_up)!.str() == '0.13'
	assert must_decimal('-0.004').round(2, .half_up)!.is_zero()
	assert must_decimal('-0.001').round(2, .floor)!.str() == '-0.01'
	assert must_decimal('9.995').round(2, .half_up)!.str() == '10'
	assert must_decimal('1.5').round(3, .up)!.str() == '1.5'
}

fn test_div_with_modes() {
	one := must_decimal('1')
	assert one.div(must_decimal('6'), 2, .down)!.str() == '0.16'
	assert one.div(must_decimal('-6'), 2, .floor)!.str() == '-0.17'
	assert one.div(must_decimal('-6'), 2, .ceiling)!.str() == '-0.16'
	assert one.div(must_decimal('8'), 2, .half_even)!.str() == '0.12'
	assert must_decimal('3').div(must_decimal('8'), 2, .half_even)!.str() == '0.38'
	assert one.div(must_decimal('8'), 2, .half_down)!.str() == '0.12'
	assert must_decimal('0.1').div(must_decimal('0.08'), 1, .half_even)!.str() == '1.2'
	assert must_decimal('10').div(must_decimal('4'), 0, .half_even)!.str() == '2'
}

fn test_round_and_div_errors() {
	mut messages := []string{}
	must_decimal('1.25').round(-1, .half_up) or { messages << err.msg() }
	must_decimal('1').div(decimal.zero, 2, .half_even) or { messages << err.msg() }
	must_decimal('1').div(must_decimal('3'), -1, .floor) or { messages << err.msg() }
	assert messages == ['math.decimal: precision cannot be negative',
		'math.decimal: cannot divide by zero', 'math.decimal: precision cannot be negative']
}

fn test_stored_scale_and_normalization() {
	for row in [['00123.4500', '1234500', '4', '123.45'], ['-0.5000', '-5000', '4', '-0.5'],
		['0.000', '0', '3', '0'], ['1.50e1', '150', '1', '15'], ['1.50e-1', '150', '3', '0.15'],
		['5.', '5', '0', '5'], ['+0.00e-100', '0', '102', '0']] {
		value := must_decimal(row[0])
		assert value.coefficient().str() == row[1], row[0]
		assert value.scale() == row[2].int(), row[0]
		assert value.str() == row[3], row[0]
		normalized := value.normalize()
		assert normalized == value
		assert normalized.str() == value.str()
	}
	assert must_decimal('5.00').normalize().scale() == 0
	assert must_decimal('-1.2500').normalize().scale() == 2
	assert must_decimal('0.000').normalize().scale() == 0
}

fn test_arithmetic_keeps_internal_scale() {
	a := must_decimal('1.50')
	b := must_decimal('2.5')
	sum := a + b
	assert sum.coefficient().str() == '400'
	assert sum.scale() == 2
	assert sum.str() == '4'
	difference := a - must_decimal('1.5')
	assert difference.is_zero()
	assert difference.scale() == 2
	assert difference.str() == '0'
	product := a * must_decimal('2.0')
	assert product.coefficient().str() == '3000'
	assert product.scale() == 3
	assert product.str() == '3'
	assert must_decimal('-1.20').abs().scale() == 2
	assert must_decimal('-1.20').neg().scale() == 2
	padded := a.round(3, .up)!
	assert padded.coefficient().str() == '1500'
	assert padded.scale() == 3
	assert padded.str() == '1.5'
	quotient := must_decimal('7').div(must_decimal('2'), 2, .half_up)!
	assert quotient.coefficient().str() == '350'
	assert quotient.scale() == 2
	assert quotient.str() == '3.5'
}

fn test_comparison_ignores_stored_scale() {
	assert must_decimal('5.0') == must_decimal('5.00')
	assert must_decimal('0') == must_decimal('0.000')
	assert must_decimal('-1.10') == must_decimal('-1.1')
	assert must_decimal('1.10') != must_decimal('1.101')
	assert !(must_decimal('5.0') < must_decimal('5.00'))
	assert must_decimal('5.0') <= must_decimal('5.00')
	assert must_decimal('-1.100') < must_decimal('-1.09')
}

fn test_str_fixed_rounds_half_up_and_pads() {
	for row in [['5.00', '2', '5.00'], ['5', '0', '5'], ['1.5', '3', '1.500'], ['1.234', '2', '1.23'],
		['1.235', '2', '1.24'], ['-1.235', '2', '-1.24'], ['9.995', '2', '10.00'],
		['-9.995', '2', '-10.00'], ['-0.004', '2', '0.00'], ['-0.005', '2', '-0.01'],
		['0.000', '4', '0.0000'], ['2.5', '0', '3'], ['-2.5', '0', '-3'],
		['0.0000001', '8', '0.00000010']] {
		value := must_decimal(row[0])
		assert value.str_fixed(row[1].int())! == row[2], row[0]
		assert value.scale() == must_decimal(row[0]).scale()
	}
	must_decimal('1.2').str_fixed(-1) or {
		assert err.msg() == 'math.decimal: precision cannot be negative'
		return
	}
	assert false
}

fn test_division_precision_is_independent_of_operand_scale() {
	for input in ['0.000000000000000000012', '-0.000000000000000000012', '1.000000000000000012'] {
		value := must_decimal(input)
		for divisor in ['1', '1.0', '1.00'] {
			result := value / must_decimal(divisor)
			assert result == value.div_prec(decimal.from_int(1), decimal.default_division_precision)!
			assert result.scale() == decimal.default_division_precision
			assert value.div_prec(must_decimal(divisor), 21)! == value
		}
	}
	assert (must_decimal('0.000000000000000000012') / decimal.from_int(1)).str() == '0'
	assert (must_decimal('10.00') / must_decimal('4')).str() == '2.5'
	assert (must_decimal('2') / must_decimal('-3')).str() == '-0.6666666666666667'
}
