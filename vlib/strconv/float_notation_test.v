module strconv

import math

fn test_f32_to_str_pad_uses_scientific_notation() {
	assert f32_to_str_pad(f32(34.2), 2) == '3.42e+01'
	assert f32_to_str_pad(f32(34.2), 8) == '3.42000000e+01'
	assert f32_to_str_pad(f32(0.33333334), 2) == '3.33e-01'
	assert f32_to_str_pad(f32(0.33333334), 8) == '3.33333340e-01'
	assert f32_to_str_pad(f32(12345.678), 2) == '1.23e+04'
	assert f32_to_str_pad(f32(12345.678), 8) == '1.23456780e+04'
}

fn test_f32_to_str_pad_handles_zero_and_negatives() {
	assert f32_to_str_pad(f32(0.0), 4) == '0e+00'
	assert f32_to_str_pad(f32(-1.5), 2) == '-1.50e+00'
}

fn test_f32_to_str_pad_specials() {
	assert f32_to_str_pad(f32(math.inf(1)), 4) == '+inf'
	assert f32_to_str_pad(f32(math.inf(-1)), 4) == '-inf'
	assert f32_to_str_pad(f32(math.nan()), 4) == 'nan'
}

fn test_f64_to_str_pad_uses_scientific_notation() {
	assert f64_to_str_pad(34.2, 2) == '3.42e+01'
	assert f64_to_str_pad(34.2, 8) == '3.42000000e+01'
	assert f64_to_str_pad(0.3333333333333333, 2) == '3.33e-01'
	assert f64_to_str_pad(0.3333333333333333, 8) == '3.33333333e-01'
	assert f64_to_str_pad(12345.678, 2) == '1.23e+04'
	assert f64_to_str_pad(12345.678, 8) == '1.23456780e+04'
}

fn test_f64_to_str_pad_handles_zero_and_negatives() {
	assert f64_to_str_pad(0.0, 4) == '0e+00'
	assert f64_to_str_pad(-1.5, 2) == '-1.50e+00'
}

fn test_f64_to_str_pad_specials() {
	assert f64_to_str_pad(math.inf(1), 4) == '+inf'
	assert f64_to_str_pad(math.inf(-1), 4) == '-inf'
	assert f64_to_str_pad(math.nan(), 4) == 'nan'
}

fn test_ftoa_64_uses_scientific_notation() {
	assert ftoa_64(3.14) == '3.14e+00'
	assert ftoa_64(1.0) == '1e+00'
	assert ftoa_64(0.0) == '0e+00'
	assert ftoa_64(-1.5) == '-1.5e+00'
	assert ftoa_64(math.inf(1)) == '+inf'
	assert ftoa_64(math.nan()) == 'nan'
}

fn test_ftoa_32_uses_scientific_notation() {
	assert ftoa_32(f32(3.14)) == '3.14e+00'
	assert ftoa_32(f32(1.0)) == '1e+00'
	assert ftoa_32(f32(0.0)) == '0e+00'
	assert ftoa_32(f32(-1.5)) == '-1.5e+00'
	assert ftoa_32(f32(math.inf(1))) == '+inf'
	assert ftoa_32(f32(math.nan())) == 'nan'
}

fn test_f32_to_str_l_with_dot_appends_dot_to_integers() {
	assert f32_to_str_l_with_dot(f32(34.2)) == '34.2'
	assert f32_to_str_l_with_dot(f32(34.7)) == '34.7'
	assert f32_to_str_l_with_dot(f32(0.0)) == '0.0'
	assert f32_to_str_l_with_dot(f32(-0.0)) == '-0.0'
	assert f32_to_str_l_with_dot(f32(1.5)) == '1.5'
	assert f32_to_str_l_with_dot(f32(1.0 / 3.0)) == '0.33333334'
}

fn test_f64_to_str_l_with_dot_appends_dot_to_integers() {
	assert f64_to_str_l_with_dot(34.7) == '34.7'
	assert f64_to_str_l_with_dot(0.0) == '0.0'
	assert f64_to_str_l_with_dot(1.5) == '1.5'
	assert f64_to_str_l_with_dot(-12.5) == '-12.5'
	assert f64_to_str_l_with_dot(1.0 / 3.0) == '0.3333333333333333'
}

fn test_fxx_to_str_l_parse_expands_scientific_notation() {
	assert fxx_to_str_l_parse('34.22e+00') == '34.22'
	assert fxx_to_str_l_parse('1.0e+03') == '1000.0'
	assert fxx_to_str_l_parse('1.0e-03') == '0.0010'
	assert fxx_to_str_l_parse('-1.5e+01') == '-15.0'
	assert fxx_to_str_l_parse('0e+00') == '0.0'
}

fn test_fxx_to_str_l_parse_keeps_signed_specials() {
	assert fxx_to_str_l_parse('+inf') == '+inf'
	assert fxx_to_str_l_parse('-inf') == '-inf'
	assert fxx_to_str_l_parse('nan') == 'nan'
}

fn test_fxx_to_str_l_parse_with_dot_matches_parse() {
	assert fxx_to_str_l_parse_with_dot('34.22e+00') == '34.22'
	assert fxx_to_str_l_parse_with_dot('1.0e+03') == '1000.0'
	assert fxx_to_str_l_parse_with_dot('1.0e-03') == '0.0010'
	assert fxx_to_str_l_parse_with_dot('-1.5e+01') == '-15.0'
	assert fxx_to_str_l_parse_with_dot('0e+00') == '0.0'
	assert fxx_to_str_l_parse_with_dot('-inf') == '-inf'
	assert fxx_to_str_l_parse_with_dot('nan') == 'nan'
}
