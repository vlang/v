module strconv

import math

fn test_add_thousands_sep_basic() {
	assert add_thousands_sep('1', ' ') == '1'
	assert add_thousands_sep('123', ' ') == '123'
	assert add_thousands_sep('1234', ' ') == '1 234'
	assert add_thousands_sep('1234567', ' ') == '1 234 567'
	assert add_thousands_sep('12345678', ' ') == '12 345 678'
}

fn test_add_thousands_sep_sign_and_decimal() {
	assert add_thousands_sep('-1234567', ' ') == '-1 234 567'
	assert add_thousands_sep('+1234567', ' ') == '+1 234 567'
	assert add_thousands_sep('1234567.89', Separator{ integer: ' ', decimal: ',' }) == '1 234 567,89'
	assert add_thousands_sep('-1234567.89', Separator{ integer: ',', decimal: ',' }) == '-1,234,567,89'
}

fn test_add_thousands_sep_exponent_preserved_on_string_api() {
	// add_thousands_sep is the string-level API: an exponent it's given is
	// preserved as-is, it's format_thousands()'s job to expand exponents
	// before calling here.
	assert add_thousands_sep('1234567e10', ' ') == '1 234 567e10'
	assert add_thousands_sep('1.5e-07', ' ') == '1.5e-07'
}

fn test_add_thousands_sep_non_finite() {
	// Non-finite float representations must never be sliced into chunks.
	assert add_thousands_sep('Infinity', ',') == 'Infinity'
	assert add_thousands_sep('-Infinity', ',') == '-Infinity'
	assert add_thousands_sep('NaN', ',') == 'NaN'
	assert add_thousands_sep('inf', ',') == 'inf'
	assert add_thousands_sep('-inf', ',') == '-inf'
	assert add_thousands_sep('nan', ',') == 'nan'
}

fn test_expand_exponent() {
	assert expand_exponent('1234') == '1234'
	assert expand_exponent('1e+21') == '1' + '0'.repeat(21) + '.0'
	assert expand_exponent('1.234567e+06') == '1234567.0'
	assert expand_exponent('-2.5E10') == '-25000000000.0'
	assert expand_exponent('1.5e-07') == '0.00000015'
	assert expand_exponent('-1.5e-07') == '-0.00000015'
	assert expand_exponent('5e0') == '5.0'
}

fn test_format_thousands_int_types() {
	assert format_thousands(1234567, ' ') == '1 234 567'
	assert format_thousands(i64(-1234567), ',') == '-1,234,567'
	assert format_thousands(u64(18446744073709551615), ' ') == '18 446 744 073 709 551 615'
}

fn test_format_thousands_float_large_magnitude() {
	// This is the case from the review: a float large enough that a naive
	// `.str()` (on either the C or the JS backend) emits scientific
	// notation; format_thousands() must still group the full integer part.
	assert format_thousands(f64(1e21), ',') == '1,000,000,000,000,000,000,000.0'
	assert format_thousands(1234567891.42, '.') == '1.234.567.891.42'
}

fn test_format_thousands_float_non_finite() {
	assert format_thousands(math.inf(1), ',') == '+inf'
	assert format_thousands(math.inf(-1), ',') == '-inf'
	assert format_thousands(math.nan(), ',') == 'nan'
}
