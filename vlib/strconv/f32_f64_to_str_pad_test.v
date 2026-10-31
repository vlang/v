import strconv
import math

// The expected values are the ones that C's `printf("%.*e", n_digit, x)` prints. The listed
// cases are those in which the shortest decimal digits of `x`, that the functions work on,
// lead to the same result as its exact value (see the doc comments of the two functions).

struct F64PadCase {
	x       f64
	n_digit int
	want    string
}

struct F32PadCase {
	x       f32
	n_digit int
	want    string
}

const smallest_normal_f64 = math.f64_from_bits(0x0010_0000_0000_0000)
const largest_subnormal_f64 = math.f64_from_bits(0x000f_ffff_ffff_ffff)
const smallest_subnormal_f64 = math.f64_from_bits(1)
const smallest_normal_f32 = math.f32_from_bits(0x0080_0000)
const largest_subnormal_f32 = math.f32_from_bits(0x007f_ffff)
const smallest_subnormal_f32 = math.f32_from_bits(1)

fn check_f64(cases []F64PadCase) {
	for c in cases {
		got := strconv.f64_to_str_pad(c.x, c.n_digit)
		assert got == c.want, 'f64_to_str_pad(${c.x}, ${c.n_digit})'
	}
}

fn check_f32(cases []F32PadCase) {
	for c in cases {
		got := strconv.f32_to_str_pad(c.x, c.n_digit)
		assert got == c.want, 'f32_to_str_pad(${c.x}, ${c.n_digit})'
	}
}

fn test_f64_to_str_pad_ordinary_values() {
	check_f64([
		F64PadCase{1.0, 0, '1e+00'},
		F64PadCase{1.0, 1, '1.0e+00'},
		F64PadCase{1.0, 2, '1.00e+00'},
		F64PadCase{1.0, 8, '1.00000000e+00'},
		F64PadCase{1.0, 17, '1.00000000000000000e+00'},
		F64PadCase{-1.0, 0, '-1e+00'},
		F64PadCase{-1.0, 2, '-1.00e+00'},
		F64PadCase{0.5, 0, '5e-01'},
		F64PadCase{0.5, 1, '5.0e-01'},
		F64PadCase{1.5, 0, '2e+00'},
		F64PadCase{1.5, 1, '1.5e+00'},
		F64PadCase{1.5, 17, '1.50000000000000000e+00'},
		F64PadCase{-1.5, 0, '-2e+00'},
		F64PadCase{-1.5, 1, '-1.5e+00'},
		F64PadCase{123456.0, 0, '1e+05'},
		F64PadCase{123456.0, 1, '1.2e+05'},
		F64PadCase{123456.0, 2, '1.23e+05'},
		F64PadCase{123456.0, 3, '1.235e+05'},
		F64PadCase{123456.0, 8, '1.23456000e+05'},
		F64PadCase{123456.0, 17, '1.23456000000000000e+05'},
		F64PadCase{1234.5678, 0, '1e+03'},
		F64PadCase{1234.5678, 1, '1.2e+03'},
		F64PadCase{1234.5678, 2, '1.23e+03'},
		F64PadCase{1234.5678, 3, '1.235e+03'},
		F64PadCase{1234.5678, 7, '1.2345678e+03'},
		F64PadCase{1234.5678, 8, '1.23456780e+03'},
		F64PadCase{1234.5678, 16, '1.2345678000000000e+03'},
		F64PadCase{-1234.5678, 2, '-1.23e+03'},
		F64PadCase{0.1, 0, '1e-01'},
		F64PadCase{0.1, 1, '1.0e-01'},
		F64PadCase{0.1, 8, '1.00000000e-01'},
		F64PadCase{3.14159, 0, '3e+00'},
		F64PadCase{3.14159, 1, '3.1e+00'},
		F64PadCase{3.14159, 2, '3.14e+00'},
		F64PadCase{3.14159, 3, '3.142e+00'},
		F64PadCase{3.14159, 8, '3.14159000e+00'},
		F64PadCase{0.000123456, 0, '1e-04'},
		F64PadCase{0.000123456, 2, '1.23e-04'},
		F64PadCase{0.000123456, 8, '1.23456000e-04'},
		F64PadCase{6.02214076e23, 0, '6e+23'},
		F64PadCase{6.02214076e23, 2, '6.02e+23'},
		F64PadCase{6.02214076e23, 8, '6.02214076e+23'},
		F64PadCase{1e23, 0, '1e+23'},
		F64PadCase{1e23, 1, '1.0e+23'},
		F64PadCase{1e23, 8, '1.00000000e+23'},
	])
}

// the rounding to `n_digit` digits carries into a new leading digit in most of these
fn test_f64_to_str_pad_rounding_carry() {
	check_f64([
		F64PadCase{9.5, 0, '1e+01'},
		F64PadCase{9.5, 1, '9.5e+00'},
		F64PadCase{9.5, 17, '9.50000000000000000e+00'},
		F64PadCase{-9.5, 0, '-1e+01'},
		F64PadCase{9.95, 0, '1e+01'},
		F64PadCase{9.95, 2, '9.95e+00'},
		F64PadCase{9.95, 8, '9.95000000e+00'},
		F64PadCase{-9.95, 0, '-1e+01'},
		F64PadCase{99.5, 0, '1e+02'},
		F64PadCase{99.5, 1, '1.0e+02'},
		F64PadCase{99.5, 2, '9.95e+01'},
		F64PadCase{999984.0, 0, '1e+06'},
		F64PadCase{999984.0, 1, '1.0e+06'},
		F64PadCase{999984.0, 2, '1.00e+06'},
		F64PadCase{999984.0, 3, '1.000e+06'},
		F64PadCase{999984.0, 7, '9.9998400e+05'},
		F64PadCase{999984.0, 8, '9.99984000e+05'},
		F64PadCase{999984.0, 17, '9.99984000000000000e+05'},
		F64PadCase{-999984.0, 0, '-1e+06'},
		F64PadCase{-999984.0, 1, '-1.0e+06'},
		F64PadCase{0.99996, 0, '1e+00'},
		F64PadCase{0.99996, 1, '1.0e+00'},
		F64PadCase{0.99996, 2, '1.00e+00'},
		F64PadCase{0.99996, 3, '1.000e+00'},
		F64PadCase{0.99996, 7, '9.9996000e-01'},
		F64PadCase{0.99996, 8, '9.99960000e-01'},
		F64PadCase{-0.99996, 3, '-1.000e+00'},
		F64PadCase{999999.0, 0, '1e+06'},
		F64PadCase{999999.0, 1, '1.0e+06'},
		F64PadCase{999999.0, 2, '1.00e+06'},
		F64PadCase{999999.0, 3, '1.000e+06'},
		F64PadCase{999999.0, 7, '9.9999900e+05'},
	])
}

fn test_f64_to_str_pad_zero() {
	check_f64([
		F64PadCase{0.0, 0, '0e+00'},
		F64PadCase{0.0, 1, '0.0e+00'},
		F64PadCase{0.0, 2, '0.00e+00'},
		F64PadCase{0.0, 8, '0.00000000e+00'},
		F64PadCase{0.0, 17, '0.00000000000000000e+00'},
		F64PadCase{-0.0, 0, '-0e+00'},
		F64PadCase{-0.0, 1, '-0.0e+00'},
		F64PadCase{-0.0, 2, '-0.00e+00'},
		F64PadCase{-0.0, 8, '-0.00000000e+00'},
		F64PadCase{-0.0, 17, '-0.00000000000000000e+00'},
	])
}

fn test_f64_to_str_pad_limits() {
	check_f64([
		F64PadCase{math.max_f64, 0, '2e+308'},
		F64PadCase{math.max_f64, 1, '1.8e+308'},
		F64PadCase{math.max_f64, 2, '1.80e+308'},
		F64PadCase{math.max_f64, 8, '1.79769313e+308'},
		F64PadCase{math.max_f64, 16, '1.7976931348623157e+308'},
		F64PadCase{smallest_normal_f64, 0, '2e-308'},
		F64PadCase{smallest_normal_f64, 1, '2.2e-308'},
		F64PadCase{smallest_normal_f64, 2, '2.23e-308'},
		F64PadCase{smallest_normal_f64, 8, '2.22507386e-308'},
		F64PadCase{smallest_normal_f64, 16, '2.2250738585072014e-308'},
		F64PadCase{largest_subnormal_f64, 0, '2e-308'},
		F64PadCase{largest_subnormal_f64, 1, '2.2e-308'},
		F64PadCase{largest_subnormal_f64, 2, '2.23e-308'},
		F64PadCase{largest_subnormal_f64, 8, '2.22507386e-308'},
		F64PadCase{smallest_subnormal_f64, 0, '5e-324'},
		F64PadCase{-smallest_subnormal_f64, 0, '-5e-324'},
	])
}

fn test_f32_to_str_pad_ordinary_values() {
	check_f32([
		F32PadCase{1.0, 0, '1e+00'},
		F32PadCase{1.0, 1, '1.0e+00'},
		F32PadCase{1.0, 2, '1.00e+00'},
		F32PadCase{1.0, 8, '1.00000000e+00'},
		F32PadCase{1.0, 17, '1.00000000000000000e+00'},
		F32PadCase{-1.0, 0, '-1e+00'},
		F32PadCase{-1.0, 2, '-1.00e+00'},
		F32PadCase{0.5, 0, '5e-01'},
		F32PadCase{0.5, 1, '5.0e-01'},
		F32PadCase{1.5, 0, '2e+00'},
		F32PadCase{1.5, 1, '1.5e+00'},
		F32PadCase{1.5, 8, '1.50000000e+00'},
		F32PadCase{-1.5, 0, '-2e+00'},
		F32PadCase{-1.5, 1, '-1.5e+00'},
		F32PadCase{123456.0, 0, '1e+05'},
		F32PadCase{123456.0, 1, '1.2e+05'},
		F32PadCase{123456.0, 2, '1.23e+05'},
		F32PadCase{123456.0, 3, '1.235e+05'},
		F32PadCase{123456.0, 8, '1.23456000e+05'},
		F32PadCase{1234.5678, 0, '1e+03'},
		F32PadCase{1234.5678, 1, '1.2e+03'},
		F32PadCase{1234.5678, 2, '1.23e+03'},
		F32PadCase{1234.5678, 3, '1.235e+03'},
		F32PadCase{1234.5678, 7, '1.2345677e+03'},
		F32PadCase{-1234.5678, 2, '-1.23e+03'},
		F32PadCase{0.1, 0, '1e-01'},
		F32PadCase{0.1, 1, '1.0e-01'},
		F32PadCase{0.1, 7, '1.0000000e-01'},
		F32PadCase{3.14159, 0, '3e+00'},
		F32PadCase{3.14159, 1, '3.1e+00'},
		F32PadCase{3.14159, 2, '3.14e+00'},
		F32PadCase{3.14159, 3, '3.142e+00'},
		F32PadCase{0.000123456, 0, '1e-04'},
		F32PadCase{0.000123456, 2, '1.23e-04'},
		F32PadCase{0.000123456, 7, '1.2345600e-04'},
		F32PadCase{6.02214076e23, 0, '6e+23'},
		F32PadCase{6.02214076e23, 2, '6.02e+23'},
		F32PadCase{6.02214076e23, 7, '6.0221406e+23'},
		F32PadCase{1e23, 0, '1e+23'},
		F32PadCase{1e23, 1, '1.0e+23'},
		F32PadCase{1e23, 3, '1.000e+23'},
	])
}

// the rounding to `n_digit` digits carries into a new leading digit in most of these
fn test_f32_to_str_pad_rounding_carry() {
	check_f32([
		F32PadCase{9.5, 0, '1e+01'},
		F32PadCase{9.5, 1, '9.5e+00'},
		F32PadCase{9.5, 8, '9.50000000e+00'},
		F32PadCase{-9.5, 0, '-1e+01'},
		F32PadCase{9.95, 0, '1e+01'},
		F32PadCase{9.95, 2, '9.95e+00'},
		F32PadCase{9.95, 3, '9.950e+00'},
		F32PadCase{-9.95, 0, '-1e+01'},
		F32PadCase{99.5, 0, '1e+02'},
		F32PadCase{99.5, 1, '1.0e+02'},
		F32PadCase{99.5, 2, '9.95e+01'},
		F32PadCase{999984.0, 0, '1e+06'},
		F32PadCase{999984.0, 1, '1.0e+06'},
		F32PadCase{999984.0, 2, '1.00e+06'},
		F32PadCase{999984.0, 3, '1.000e+06'},
		F32PadCase{999984.0, 7, '9.9998400e+05'},
		F32PadCase{999984.0, 8, '9.99984000e+05'},
		F32PadCase{999984.0, 17, '9.99984000000000000e+05'},
		F32PadCase{-999984.0, 0, '-1e+06'},
		F32PadCase{-999984.0, 1, '-1.0e+06'},
		F32PadCase{0.99996, 0, '1e+00'},
		F32PadCase{0.99996, 1, '1.0e+00'},
		F32PadCase{0.99996, 2, '1.00e+00'},
		F32PadCase{0.99996, 3, '1.000e+00'},
		F32PadCase{-0.99996, 3, '-1.000e+00'},
		F32PadCase{999999.0, 0, '1e+06'},
		F32PadCase{999999.0, 1, '1.0e+06'},
		F32PadCase{999999.0, 2, '1.00e+06'},
		F32PadCase{999999.0, 3, '1.000e+06'},
		F32PadCase{999999.0, 7, '9.9999900e+05'},
	])
}

fn test_f32_to_str_pad_zero() {
	check_f32([
		F32PadCase{0.0, 0, '0e+00'},
		F32PadCase{0.0, 1, '0.0e+00'},
		F32PadCase{0.0, 2, '0.00e+00'},
		F32PadCase{0.0, 8, '0.00000000e+00'},
		F32PadCase{0.0, 17, '0.00000000000000000e+00'},
		F32PadCase{-0.0, 0, '-0e+00'},
		F32PadCase{-0.0, 1, '-0.0e+00'},
		F32PadCase{-0.0, 2, '-0.00e+00'},
		F32PadCase{-0.0, 8, '-0.00000000e+00'},
		F32PadCase{-0.0, 17, '-0.00000000000000000e+00'},
	])
}

fn test_f32_to_str_pad_limits() {
	check_f32([
		F32PadCase{math.max_f32, 0, '3e+38'},
		F32PadCase{math.max_f32, 1, '3.4e+38'},
		F32PadCase{math.max_f32, 2, '3.40e+38'},
		F32PadCase{math.max_f32, 7, '3.4028235e+38'},
		F32PadCase{smallest_normal_f32, 0, '1e-38'},
		F32PadCase{smallest_normal_f32, 1, '1.2e-38'},
		F32PadCase{smallest_normal_f32, 2, '1.18e-38'},
		F32PadCase{smallest_normal_f32, 7, '1.1754944e-38'},
		F32PadCase{largest_subnormal_f32, 0, '1e-38'},
		F32PadCase{largest_subnormal_f32, 1, '1.2e-38'},
		F32PadCase{largest_subnormal_f32, 2, '1.18e-38'},
		F32PadCase{largest_subnormal_f32, 7, '1.1754942e-38'},
		F32PadCase{smallest_subnormal_f32, 0, '1e-45'},
		F32PadCase{-smallest_subnormal_f32, 0, '-1e-45'},
	])
}

fn test_to_str_pad_inf_and_nan() {
	assert strconv.f64_to_str_pad(math.inf(1), 3) == '+inf'
	assert strconv.f64_to_str_pad(math.inf(-1), 3) == '-inf'
	assert strconv.f64_to_str_pad(math.nan(), 3) == 'nan'
	assert strconv.f32_to_str_pad(f32(math.inf(1)), 3) == '+inf'
	assert strconv.f32_to_str_pad(f32(math.inf(-1)), 3) == '-inf'
	assert strconv.f32_to_str_pad(f32(math.nan()), 3) == 'nan'
}

// a negative `n_digit` is the same as 0
fn test_to_str_pad_negative_n_digit() {
	assert strconv.f64_to_str_pad(1234.5, -1) == '1e+03'
	assert strconv.f64_to_str_pad(0.0, -1) == '0e+00'
	assert strconv.f32_to_str_pad(1234.5, -1) == '1e+03'
	assert strconv.f32_to_str_pad(0.0, -1) == '0e+00'
}

// the zeros that are appended must fit in the buffer of the result
fn test_to_str_pad_many_digits() {
	assert strconv.f64_to_str_pad(1.5, 60) == '1.5' + '0'.repeat(59) + 'e+00'
	assert strconv.f64_to_str_pad(-0.0, 60) == '-0.' + '0'.repeat(60) + 'e+00'
	assert strconv.f32_to_str_pad(1.5, 60) == '1.5' + '0'.repeat(59) + 'e+00'
	assert strconv.f32_to_str_pad(-0.0, 60) == '-0.' + '0'.repeat(60) + 'e+00'
	assert strconv.f32_to_str_pad(math.max_f32, 60) == '3.4028235' + '0'.repeat(53) + 'e+38'
}

// f32_to_str and f64_to_str round with the same code
fn test_to_str_rounding_carry() {
	assert strconv.f64_to_str(999984.0, 1) == '1.0e+06'
	assert strconv.f64_to_str(9.5, 0) == '1e+01'
	assert strconv.f64_to_str(0.99996, 3) == '1.000e+00'
	assert strconv.f32_to_str(999984.0, 1) == '1.0e+06'
	assert strconv.f32_to_str(9.5, 0) == '1e+01'
	assert strconv.f32_to_str(0.99996, 3) == '1.000e+00'
	assert strconv.f32_to_str(1.0, 0) == '1e+00'
	assert strconv.f32_to_str(-1234.5, 0) == '-1e+03'
	assert f64(999984.0).strsci(1) == '1.0e+06'
	assert f32(999984.0).strsci(1) == '1.0e+06'
}

fn test_v_sprintf_e_zero_and_rounding_carry() {
	assert unsafe { strconv.v_sprintf('%e', 0.0) } == '0.000000e+00'
	assert unsafe { strconv.v_sprintf('%.0e', 0.0) } == '0e+00'
	assert unsafe { strconv.v_sprintf('%.1e', 0.0) } == '0.0e+00'
	assert unsafe { strconv.v_sprintf('%+.1e', 0.0) } == '+0.0e+00'
	assert unsafe { strconv.v_sprintf('%.1E', 0.0) } == '0.0E+00'
	assert unsafe { strconv.v_sprintf('[%12.3e]', 0.0) } == '[   0.000e+00]'
	assert unsafe { strconv.v_sprintf('[%-12.3e]', 0.0) } == '[0.000e+00   ]'
	assert unsafe { strconv.v_sprintf('[%012.3e]', 0.0) } == '[0000.000e+00]'
	assert unsafe { strconv.v_sprintf('%.0e', 9.5) } == '1e+01'
	assert unsafe { strconv.v_sprintf('%.1e', 999984.0) } == '1.0e+06'
	assert unsafe { strconv.v_sprintf('%.1e', -999984.0) } == '-1.0e+06'
	assert unsafe { strconv.v_sprintf('%+.2e', 999984.0) } == '+1.00e+06'
	assert unsafe { strconv.v_sprintf('%.3E', 0.99996) } == '1.000E+00'
	assert unsafe { strconv.v_sprintf('[%12.3e]', 999984.0) } == '[   1.000e+06]'
	assert unsafe { strconv.v_sprintf('[%012.3e]', -999984.0) } == '[-001.000e+06]'
	// no rounding carry
	assert unsafe { strconv.v_sprintf('%e', 999984.0) } == '9.999840e+05'
	assert unsafe { strconv.v_sprintf('%.3e', -1234.5678) } == '-1.235e+03'
}

fn test_format_es_zero_and_rounding_carry() {
	assert strconv.format_es(0.0, strconv.BF_param{ len1: 0 }) == '0e+00'
	assert strconv.format_es(0.0, strconv.BF_param{ len1: 1 }) == '0.0e+00'
	assert strconv.format_es(0.0, strconv.BF_param{ len1: 3, sign_flag: true }) == '+0.000e+00'
	assert strconv.format_es(0.0, strconv.BF_param{ len1: 3, rm_tail_zero: true }) == '0e+00'
	assert strconv.format_es(0.0, strconv.BF_param{ len0: 12, len1: 3 }) == '   0.000e+00'
	assert strconv.format_es(999984.0, strconv.BF_param{ len1: 0 }) == '1e+06'
	assert strconv.format_es(999984.0, strconv.BF_param{ len1: 1 }) == '1.0e+06'
	assert strconv.format_es(-999984.0, strconv.BF_param{ len1: 1, positive: false }) == '-1.0e+06'
	assert strconv.format_es(999984.0, strconv.BF_param{ len1: 3, rm_tail_zero: true }) == '1e+06'
	assert strconv.format_es(0.99996, strconv.BF_param{ len0: 12, len1: 3 }) == '   1.000e+00'
}
