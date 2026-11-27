module arm64

$if macos && arm64 {
	import os
	import v.parser
	import v.pref
	import v.ssa
	import v.transform
	import v.types
}

fn test_native_int_uses_platform_width() {
	$if macos && arm64 {
		source := 'module main
#include <unistd.h>
#include <stdlib.h>
fn C.exit(int)
fn increment(value int) int { return value + 1 }
fn read_int(value &int) int { return *value }
fn write_int(value &int, n int) { unsafe { *value = n } }
fn C.strcoll(&char, &char) int
fn C.strtoul(&char, &&char, int) uint
__global C.optind int
fn native_compare_strings() int { return C.strcoll(c"a", c"b") }
fn native_text() string { return "abc" }
struct Box { value int }
fn main() {
	if sizeof(int) != 8 || sizeof(i32) != 4 { C.exit(1) }
	mut value := int(2147483647)
	value++
	if value != 2147483648 { C.exit(2) }
	if increment(value) != 2147483649 { C.exit(3) }
	box := Box{value: 4294967296}
	if box.value != 4294967296 || sizeof(Box) != 8 { C.exit(4) }
	mut numbers := [int(4294967296), int(-3), int(2147483648)]
	numbers.sort()
	if numbers[0] != -3 || numbers[1] != 2147483648 || numbers[2] != 4294967296 { C.exit(5) }
	text := native_text()
	if read_int(&text.len) != 3 { C.exit(6) }
	capacity := numbers.cap
	flags := numbers.flags
	write_int(&numbers.len, 4294967296)
	if read_int(&numbers.len) != 4294967296 || numbers.cap != capacity || numbers.flags != flags { C.exit(7) }
	write_int(&numbers.len, 3)
	if numbers[0] != -3 || numbers[2] != 4294967296 { C.exit(8) }
	if native_compare_strings() >= 0 { C.exit(9) }
	compare := C.strcoll
	if compare(c"a", c"b") >= 0 { C.exit(10) }
	original_optind := C.optind
	C.optind = -7
	if C.optind != -7 { C.exit(11) }
	C.optind = original_optind
	if C.strtoul(c"4294967296", unsafe { nil }, 10) != uint(4294967296) { C.exit(12) }
	parse_unsigned := C.strtoul
	if parse_unsigned(c"4294967296", unsafe { nil }, 10) != uint(4294967296) { C.exit(13) }
}
'
		path := os.join_path(os.vtmp_dir(), 'arm64_int_width_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
			os.rm(output + '_c') or {}
		}
		os.write_file(path, source) or { panic(err) }
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.annotate_types()
		assert tc.errors.len == 0, tc.errors.str()
		transform.transform(mut a, tc)
		m := ssa.build_with_used(a, map[string]bool{}, tc)
		mut g := Gen.new(m)
		g.gen()
		g.write_and_link(output)
		result := os.exec([output])
		assert result.exit_code == 0, result.output
		compiled := os.exec([@VEXE, '-b', 'c', '-enable-globals', '-nocache', '-gc', 'none', '-o',
			output + '_c', path])
		assert compiled.exit_code == 0, compiled.output
		c_result := os.exec([output + '_c'])
		assert c_result.exit_code == 0, c_result.output
	}
}

fn test_native_int_limits_match_the_target_width() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_int_limits_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, 'fn main() {
	assert min_int == -9223372036854775807 - 1
	assert max_int == 9223372036854775807
	assert min_i32 == -2147483648
	assert max_i32 == 2147483647
}
') or { panic(err) }
		compiled := os.exec([@VEXE, '-b', 'arm64', '-gc', 'none', '-o', output, path])
		assert compiled.exit_code == 0, compiled.output
		result := os.exec([output])
		assert result.exit_code == 0, result.output
	}
}

fn test_native_int_formatting_and_error_codes_keep_the_full_value() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_int_helpers_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, 'fn identity(n int) int { return n }
fn main() {
	n := identity(4294967296)
	assert "\${n:012d}" == "004294967296"
	assert "\${-n:013d}" == "-004294967296"
	err := error_with_code("wide", n)
	assert err.code() == n
	assert err.msg() == "wide"
	assert error_with_code("temporary", n).code() == n
	negative := error_with_code("negative", -n)
	assert negative.code() == -n
}
') or { panic(err) }
		compiled := os.exec([@VEXE, '-b', 'arm64', '-gc', 'none', '-o', output, path])
		assert compiled.exit_code == 0, compiled.output
		result := os.exec([output])
		assert result.exit_code == 0, result.output
	}
}

fn test_native_shift_counts_are_checked_at_the_operand_width() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_shift_counts_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, 'fn shift(count i64) {
	a := i8(-8)
	assert a >> count == 0
	assert a >>> count == 0
	assert i16(-8) << count == 0
	assert i64(-8) >> count == 0
}

fn narrow(count i64) i8 {
	return i8(-8) >> count
}

fn main() {
	shift(-1)
	shift(64)
	shift(65)
	assert narrow(7) == -1
	assert narrow(8) == 0
	assert narrow(31) == 0
}
')!
		for production in [false, true] {
			mut args := [@VEXE]
			if production {
				args << '-prod'
			}
			args << ['-b', 'arm64', '-gc', 'none', '-o', output, path]
			compiled := os.exec(args)
			assert compiled.exit_code == 0, compiled.output
			result := os.exec([output])
			assert result.exit_code == 0, result.output
		}
	}
}

fn test_native_string_int_parses_at_the_width_of_int() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_string_int_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
			os.rm(output + '_c') or {}
		}
		os.write_file(path, 'fn main() {
	inputs := ["0", "7", "-7", "2147483647", "2147483648", "-2147483648", "-2147483649",
		"3000000000", "4294967295", "4294967296", "-4294967296", "9223372036854775807",
		"9223372036854775808", "-9223372036854775808", "-9223372036854775809",
		"18446744073709551615", "18446744073709551616", "-18446744073709551616",
		"99999999999999999999", "0x7fffffffffffffff", "0x8000000000000000", "0xffffffffffffffff",
		"0x10000000000000000", "-0x8000000000000000", "-0x8000000000000001",
		"0o777777777777777777777", "0o1777777777777777777777", "0o2000000000000000000000",
		"0b1111111111111111111111111111111111111111111111111111111111111111",
		"0b10000000000000000000000000000000000000000000000000000000000000000", "4294967296_",
		"18446744073709551616_", "1_000", "_1", "1__0", "1.5", "12 ", "-", "+", "", "x", "0x",
		"0x_f", "+0b101", "-0O17", "007"]
	assert sizeof(int) == 8
	assert i64("3000000000".int()) == 3000000000
	assert i64("9223372036854775808".int()) == max_i64
	assert i64("-9223372036854775809".int()) == min_i64
	for s in inputs {
		println(s)
		println(s.int())
	}
}
')!
		native := os.exec([@VEXE, '-b', 'arm64', '-gc', 'none', '-o', output, path])
		assert native.exit_code == 0, native.output
		c := os.exec([@VEXE, '-b', 'c', '-nocache', '-gc', 'none', '-o', output + '_c', path])
		assert c.exit_code == 0, c.output
		native_result := os.exec([output])
		c_result := os.exec([output + '_c'])
		assert native_result.exit_code == 0, native_result.output
		assert c_result.exit_code == 0, c_result.output
		assert native_result.output == c_result.output
	}
}
