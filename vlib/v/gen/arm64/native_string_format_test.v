module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.types

fn test_native_zero_padding_preserves_signs_values_width_and_special_floats() {
	$if macos && arm64 {
		run_native_string_format_fixture('zero_padding', 'module main
fn C.exit(int)
fn v3_string_zpad(s string, width int) string { return "" }
fn v3_f64_zpad(s string, width int) string { return "" }
fn v3_int_zpad(n int, width int) string { return "" }
fn v3_i64_zpad(n i64, width int) string { return "" }
fn v3_u64_zpad(n u64, width int) string { return "" }
fn main() {
	if v3_string_zpad("12", 5) != "00012" { C.exit(1) }
	if v3_string_zpad("-12", 5) != "-0012" { C.exit(2) }
	if v3_string_zpad("+12", 5) != "+0012" { C.exit(3) }
	if v3_string_zpad("", 3) != "000" { C.exit(4) }
	if v3_string_zpad("12345", 3) != "12345" { C.exit(5) }
	if v3_string_zpad("12", -5) != "12" { C.exit(6) }
	if v3_f64_zpad("-1.25", 8) != "-0001.25" { C.exit(7) }
	if v3_f64_zpad("inf", 6) != "   inf" { C.exit(8) }
	if v3_f64_zpad("-inf", 6) != "  -inf" { C.exit(9) }
	if v3_f64_zpad("nan", 6) != "   nan" { C.exit(10) }
	if v3_f64_zpad("inf", -6) != "inf   " { C.exit(11) }
	if v3_f64_zpad("", 3) != "   " { C.exit(12) }
	if v3_int_zpad(-42, 6) != "-00042" { C.exit(13) }
	if v3_i64_zpad(i64(-9223372036854775807) - 1, 22) != "-009223372036854775808" { C.exit(14) }
	if v3_u64_zpad(u64(18446744073709551615), 22) != "0018446744073709551615" { C.exit(15) }
	if v3_u64_zpad(u64(0), 3) != "000" { C.exit(16) }
}
')
	}
}

fn test_native_string_formatting_adds_signs_and_preserves_non_ascii_bytes() {
	$if macos && arm64 {
		run_native_string_format_fixture('string_formatting', 'module main
fn C.exit(int)
fn v3_string_upper_ascii(s string) string { return "" }
fn v3_string_plus_sign(s string) string { return "" }
fn v3_string_rpad_zero(s string, width int) string { return "" }
fn main() {
	if v3_string_upper_ascii("Abc_äÉ123") != "ABC_äÉ123" { C.exit(1) }
	if v3_string_upper_ascii("") != "" { C.exit(2) }
	if v3_string_plus_sign("12") != "+12" { C.exit(3) }
	if v3_string_plus_sign("-12") != "-12" { C.exit(4) }
	if v3_string_plus_sign("+12") != "+12" { C.exit(5) }
	if v3_string_plus_sign("") != "+" { C.exit(6) }
	if v3_string_rpad_zero("-1.2", 7) != "-1.2000" { C.exit(7) }
	if v3_string_rpad_zero("12", -5) != "12" { C.exit(8) }
	if v3_string_rpad_zero("", 3) != "000" { C.exit(9) }
}
')
	}
}

fn test_native_float_formatting_uses_values_precision_exponents_and_dynamic_storage() {
	$if macos && arm64 {
		run_native_string_format_fixture('float_formatting', 'module main
fn C.exit(int)
fn v3_f64_fixed(value f64, precision int) string { return "" }
fn v3_f64_exp(value f64, precision int, upper int) string { return "" }
fn v3_f64_general(value f64, precision int, upper int) string { return "" }
fn v3_f64_trimmed(value f64, precision int) string { return "" }
fn main() {
	if v3_f64_fixed(1.25, 2) != "1.25" { C.exit(1) }
	if v3_f64_fixed(-1.25, 3) != "-1.250" { C.exit(2) }
	if v3_f64_fixed(2.5, 0) != "3" { C.exit(3) }
	if v3_f64_fixed(-2.5, 0) != "-3" { C.exit(4) }
	long := v3_f64_fixed(1.5, 200)
	if long.len != 202 || long[201] != 48 { C.exit(5) }
	if v3_f64_exp(1.25, 2, 0) != "1.25e+00" { C.exit(6) }
	if v3_f64_exp(1.25, 2, 1) != "1.25E+00" { C.exit(7) }
	if v3_f64_general(1250000.0, 3, 0) != "1.25e+06" { C.exit(8) }
	if v3_f64_general(1250000.0, 3, 1) != "1.25E+06" { C.exit(9) }
	if v3_f64_trimmed(1.5, 3) != "1.5" { C.exit(10) }
	if v3_f64_trimmed(1000000.0, 4) != "1e+06" { C.exit(11) }
	if v3_f64_trimmed(0.00000125, 3) != "1.25e-06" { C.exit(12) }
	if v3_f64_trimmed(0.0, 3) != "0" { C.exit(13) }
	if v3_f64_fixed(1.25, 1) != "1.3" { C.exit(14) }
	if v3_f64_fixed(-1.25, 1) != "-1.3" { C.exit(15) }
	if v3_f64_fixed(1.125, 2) != "1.13" { C.exit(16) }
	if v3_f64_fixed(2.15, 1) != "2.1" { C.exit(17) }
	if v3_f64_fixed(2.675, 2) != "2.67" { C.exit(18) }
	if v3_f64_fixed(9.995, 2) != "10.00" { C.exit(19) }
	if v3_f64_fixed(0.1, 20) != "0.10000000000000000000" { C.exit(20) }
	if v3_f64_fixed(1e23, 2) != "100000000000000000000000.00" { C.exit(21) }
	if v3_f64_trimmed(0.0, 40) != "0" { C.exit(22) }
}
')
	}
}

fn test_native_float_strings_roundtrip_values_and_expand_decimal_notation() {
	$if macos && arm64 {
		run_native_string_format_fixture('float_strings', 'module main
fn C.exit(int)
fn f64_to_str_l(value f64) string { return "" }
fn f64_to_str_l_with_dot(value f64) string { return "" }
fn f32_to_str_l(value f32) string { return "" }
fn f32_to_str_l_with_dot(value f32) string { return "" }
fn main() {
	if f64_to_str_l(1.25) != "1.25" { C.exit(1) }
	if f64_to_str_l(0.1) != "0.1" { C.exit(2) }
	if f64_to_str_l_with_dot(-42.0) != "-42.0" { C.exit(3) }
	if f64_to_str_l(1000000.0) != "1000000.0" { C.exit(4) }
	if f64_to_str_l(0.000001) != "0.000001" { C.exit(5) }
	if f32_to_str_l(f32(0.1)) != "0.1" { C.exit(6) }
	if f32_to_str_l_with_dot(f32(1.5)) != "1.5" { C.exit(7) }
	if f64_to_str_l(123.1234567891011121) != "123.12345678910111" { C.exit(8) }
	if f64_to_str_l(1e23) != "100000000000000000000000.0" { C.exit(9) }
	if f64_to_str_l(-1.234e23) != "-123400000000000000000000.0" { C.exit(10) }
	if f64_to_str_l(1.234e-7) != "0.0000001234" { C.exit(11) }
	if f32_to_str_l(f32(1e23)) != "100000000000000000000000.0" { C.exit(12) }
	if f32_to_str_l(f32(1.234e-7)) != "0.0000001234" { C.exit(13) }
}
')
	}
}

fn test_native_character_formatting_encodes_ascii_and_utf8() {
	$if macos && arm64 {
		run_native_string_format_fixture('characters', 'module main
fn C.exit(int)
fn v3_char_string(code int) string { return "" }
fn main() {
	if v3_char_string(65) != "A" { C.exit(1) }
	if v3_char_string(0xe9) != "é" { C.exit(2) }
	if v3_char_string(0x20ac) != "€" { C.exit(3) }
	if v3_char_string(0x1f600) != "😀" { C.exit(4) }
	if v3_char_string(0x110000) != "" { C.exit(5) }
	if v3_char_string(0).len != 1 { C.exit(6) }
}
')
	}
}

fn test_native_interpolation_preserves_constant_strings_and_scalar_values() {
	$if macos && arm64 {
		run_native_string_format_fixture('constant_interpolation', r'module main
const label = "expected"
const chained = label + "!"
const yes = true
const value = 1.5
fn C.exit(int)
fn println(s string) {}
fn main() {
	println("V ${label}")
	println("V ${chained}")
	println("bool ${yes}")
	println("float ${value}")
	println("${yes} ${value}")
	if "V ${label}" != "V expected" { C.exit(1) }
	if "V ${chained}" != "V expected!" { C.exit(2) }
	if "${yes} ${value}" != "true 1.5" { C.exit(3) }
}
')
	}
}

fn run_native_string_format_fixture(name string, source string) {
	path := os.join_path(os.vtmp_dir(), 'arm64_format_${name}_${os.getpid()}.v')
	output := path.all_before_last('.')
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
	}
	os.write_file(path, source) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	m := ssa.build_with_used(a, map[string]bool{}, tc)
	mut g := Gen.new(m)
	g.gen()
	g.write_and_link(output)
	result := os.exec([output])
	assert result.exit_code == 0, 'fixture ${name}: ${result.exit_code}: ${result.output}'
}
