import os
import v3.cmdexec

const fastc_backend_v3_dir = os.dir(os.dir(@FILE))
const fastc_backend_vlib_dir = os.dir(fastc_backend_v3_dir)
const fastc_backend_v3_source = os.join_path(fastc_backend_v3_dir, 'v3.v')

fn test_fastc_backend_and_checked_fallback() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_backend_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := os.join_path(root, 'v3')
	build := cmdexec.run(@VEXE, ['-gc', 'none', '-path', '${fastc_backend_vlib_dir}|@vlib|@vmodules',
		'-o', v3_bin, fastc_backend_v3_source])
	assert build.exit_code == 0, build.output

	valid_source := os.join_path(root, 'valid.v')
	os.write_file(valid_source, 'module main

fn twice(value int) int {
	return value * 2
}

fn main() {
	value := twice(21)
	println(value)
}
') or {
		panic(err)
	}
	valid_binary := os.join_path(root, 'valid')
	valid_compile := cmdexec.run(v3_bin, ['-macos-v3-compat-c99', '-b', 'fastc', '-o', valid_binary,
		valid_source])
	assert valid_compile.exit_code == 0, valid_compile.output
	assert valid_compile.output.contains('fastc')
	assert valid_compile.output.contains('  check '), valid_compile.output
	retained_c := os.read_file(valid_binary + '.c') or { panic(err) }
	assert retained_c.contains('__typeof__((twice(21))) value = (twice(21));')
	assert !retained_c.contains('builtin__builtin_init')
	valid_run := cmdexec.run(valid_binary, [])
	assert valid_run.exit_code == 0, valid_run.output
	assert valid_run.output.trim_space() == '42'

	unused_source := os.join_path(root, 'unused.v')
	os.write_file(unused_source, 'module main

fn main() {
	x := 1
}
') or { panic(err) }
	unused_binary := os.join_path(root, 'unused')
	unused_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', unused_binary,
		unused_source])
	assert unused_compile.exit_code == 0, unused_compile.output
	assert unused_compile.output.count('unused variable: `x`') == 1, unused_compile.output
	unused_c := os.read_file(unused_binary + '.c') or { panic(err) }
	assert unused_c.contains('__typeof__((1)) x = (1);')

	strict_binary := os.join_path(root, 'strict')
	strict_compile := cmdexec.run(v3_bin, ['-silent', '-cstrict', '-b', 'fastc', '-o', strict_binary,
		valid_source])
	assert strict_compile.exit_code == 0, strict_compile.output
	strict_c := os.read_file(strict_binary + '.c') or { panic(err) }
	assert !strict_c.contains('__typeof__((twice(21))) value = (twice(21));')
	assert !strict_c.contains('V_FASTC_PRINT_SELECT')
	strict_run := cmdexec.run(strict_binary, [])
	assert strict_run.exit_code == 0, strict_run.output
	assert strict_run.output.trim_space() == '42'

	debug_binary := os.join_path(root, 'debug')
	debug_compile := cmdexec.run(v3_bin, ['-silent', '-g', '-b', 'fastc', '-o', debug_binary,
		valid_source])
	assert debug_compile.exit_code == 0, debug_compile.output
	debug_c := os.read_file(debug_binary + '.c') or { panic(err) }
	assert !debug_c.contains('__typeof__((twice(21))) value = (twice(21));')
	assert !debug_c.contains('V_FASTC_PRINT_SELECT')
	debug_run := cmdexec.run(debug_binary, [])
	assert debug_run.exit_code == 0, debug_run.output
	assert debug_run.output.trim_space() == '42'

	no_main_c_file := os.join_path(root, 'no_main.c')
	no_main_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-d', 'no_main', '-o',
		no_main_c_file, valid_source])
	assert no_main_compile.exit_code == 0, no_main_compile.output
	no_main_c := os.read_file(no_main_c_file) or { panic(err) }
	assert !no_main_c.contains('V_FASTC_PRINT_SELECT')
	assert !no_main_c.contains('int main(void) {')
	assert no_main_c.contains('main__main')

	invalid_source := os.join_path(root, 'invalid.v')
	os.write_file(invalid_source, 'module main

fn main() {
	value := missing_name
	println(value)
}
') or {
		panic(err)
	}
	invalid_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o',
		os.join_path(root, 'invalid'), invalid_source])
	assert invalid_compile.exit_code != 0
	assert invalid_compile.output.contains('undefined variable: `missing_name`'), invalid_compile.output
	assert !invalid_compile.output.to_lower().contains('tcc:'), invalid_compile.output

	immutable_source := os.join_path(root, 'immutable.v')
	os.write_file(immutable_source, 'module main

fn main() {
	value := 1
	value = 2
}
') or {
		panic(err)
	}
	immutable_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o',
		os.join_path(root, 'immutable'), immutable_source])
	assert immutable_compile.exit_code != 0
	assert immutable_compile.output.contains('immutable'), immutable_compile.output
	assert !immutable_compile.output.to_lower().contains('tcc:'), immutable_compile.output

	invalid_c := os.join_path(root, 'invalid.c')
	invalid_c_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', invalid_c,
		invalid_source])
	assert invalid_c_compile.exit_code != 0
	assert invalid_c_compile.output.contains('undefined variable: `missing_name`'), invalid_c_compile.output

	assert !os.exists(invalid_c)

	invalid_stdout_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', '-',
		invalid_source])
	assert invalid_stdout_compile.exit_code != 0
	assert invalid_stdout_compile.output.contains('undefined variable: `missing_name`'), invalid_stdout_compile.output

	assert !invalid_stdout_compile.output.contains('V_FASTC_PRINT_SELECT'), invalid_stdout_compile.output

	early_return_source := os.join_path(root, 'early_return.v')
	os.write_file(early_return_source, 'module main

fn main() {
	if true {
		return
	}
}
') or {
		panic(err)
	}
	early_return_binary := os.join_path(root, 'early_return')
	early_return_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', early_return_binary,
		early_return_source])
	assert early_return_compile.exit_code == 0, early_return_compile.output
	early_return_c := os.read_file(early_return_binary + '.c') or { panic(err) }
	assert early_return_c.contains('if (((bool)true)) {\n\t\treturn 0;\n\t}')
	early_return_run := cmdexec.run(early_return_binary, [])
	assert early_return_run.exit_code == 0, early_return_run.output

	range_source := os.join_path(root, 'range.v')
	os.write_file(range_source, 'module main

fn start() int {
	println("start")
	return 0
}

fn limit() int {
	println("limit")
	return 3
}

fn main() {
	for i in start() .. limit() {
		println(i)
	}
}
') or {
		panic(err)
	}
	range_binary := os.join_path(root, 'range')
	range_compile := cmdexec.run(v3_bin,
		['-silent', '-b', 'fastc', '-o', range_binary, range_source])
	assert range_compile.exit_code == 0, range_compile.output
	range_c := os.read_file(range_binary + '.c') or { panic(err) }
	assert range_c.contains('__v_fastc_range_start_0 = (start());')
	assert range_c.contains('__v_fastc_range_end_1 = (limit());')
	range_run := cmdexec.run(range_binary, [])
	assert range_run.exit_code == 0, range_run.output
	assert range_run.output.trim_space() == 'start\nlimit\n0\n1\n2'

	float_source := os.join_path(root, 'float.v')
	os.write_file(float_source, 'module main

fn main() {
	println(2.0)
	println(12.3456789)
}
') or {
		panic(err)
	}
	float_binary := os.join_path(root, 'float')
	float_compile := cmdexec.run(v3_bin,
		['-silent', '-b', 'fastc', '-o', float_binary, float_source])
	assert float_compile.exit_code == 0, float_compile.output
	float_run := cmdexec.run(float_binary, [])
	assert float_run.exit_code == 0, float_run.output
	assert float_run.output.trim_space() == '2.0\n12.3456789'

	bool_source := os.join_path(root, 'bool_results.v')
	os.write_file(bool_source, 'module main

fn main() {
	println(1 == 1)
	println(!false)
	println(true)
}
') or {
		panic(err)
	}
	bool_binary := os.join_path(root, 'bool_results')
	bool_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', bool_binary, bool_source])
	assert bool_compile.exit_code == 0, bool_compile.output
	bool_run := cmdexec.run(bool_binary, [])
	assert bool_run.exit_code == 0, bool_run.output
	assert bool_run.output.trim_space() == 'true\ntrue\ntrue'

	narrow_source := os.join_path(root, 'narrow_arithmetic.v')
	os.write_file(narrow_source, 'module main

fn show(a u8, b u8) {
	println(a + b)
}

fn main() {
	show(255, 1)
}
') or {
		panic(err)
	}
	narrow_binary := os.join_path(root, 'narrow_arithmetic')
	narrow_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', narrow_binary,
		narrow_source])
	assert narrow_compile.exit_code == 0, narrow_compile.output
	narrow_run := cmdexec.run(narrow_binary, [])
	assert narrow_run.exit_code == 0, narrow_run.output
	assert narrow_run.output.trim_space() == '0'

	min_int_source := os.join_path(root, 'inferred_min_int.v')
	os.write_file(min_int_source, 'module main

fn main() {
	mut x := -2147483648
	x--
	println(x)
}
') or {
		panic(err)
	}
	min_int_binary := os.join_path(root, 'inferred_min_int')
	min_int_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', min_int_binary,
		min_int_source])
	assert min_int_compile.exit_code == 0, min_int_compile.output
	min_int_c := os.read_file(min_int_binary + '.c') or { panic(err) }
	assert !min_int_c.contains('V_FASTC_PRINT_SELECT')
	min_int_run := cmdexec.run(min_int_binary, [])
	assert min_int_run.exit_code == 0, min_int_run.output
	assert min_int_run.output.trim_space() == '2147483647'

	composite_min_int_source := os.join_path(root, 'inferred_composite_min_int.v')
	os.write_file(composite_min_int_source, 'module main

fn main() {
	mut x := -2147483648 - 1
	println(x)
}
') or {
		panic(err)
	}
	composite_min_int_binary := os.join_path(root, 'inferred_composite_min_int')
	composite_min_int_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o',
		composite_min_int_binary, composite_min_int_source])
	assert composite_min_int_compile.exit_code == 0, composite_min_int_compile.output
	composite_min_int_c := os.read_file(composite_min_int_binary + '.c') or { panic(err) }
	assert !composite_min_int_c.contains('V_FASTC_PRINT_SELECT')
	composite_min_int_run := cmdexec.run(composite_min_int_binary, [])
	assert composite_min_int_run.exit_code == 0, composite_min_int_run.output
	assert composite_min_int_run.output.trim_space() == '2147483647'

	oversized_decimal_source := os.join_path(root, 'inferred_oversized_decimal.v')
	os.write_file(oversized_decimal_source, 'module main

fn main() {
	x := 2147483649 | 0
	println(x)
}
') or {
		panic(err)
	}
	oversized_decimal_binary := os.join_path(root, 'inferred_oversized_decimal')
	oversized_decimal_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o',
		oversized_decimal_binary, oversized_decimal_source])
	assert oversized_decimal_compile.exit_code == 0, oversized_decimal_compile.output
	oversized_decimal_c := os.read_file(oversized_decimal_binary + '.c') or { panic(err) }
	assert !oversized_decimal_c.contains('V_FASTC_PRINT_SELECT')
	oversized_decimal_run := cmdexec.run(oversized_decimal_binary, [])
	assert oversized_decimal_run.exit_code == 0, oversized_decimal_run.output
	assert oversized_decimal_run.output.trim_space() == '-2147483647'

	min_int_loop_source := os.join_path(root, 'inferred_min_int_loop.v')
	os.write_file(min_int_loop_source, 'module main

fn main() {
	mut iterations := 0
	for i := -2147483648; true; i-- {
		println(i)
		iterations++
		if iterations == 2 {
			break
		}
	}
}
') or {
		panic(err)
	}
	min_int_loop_binary := os.join_path(root, 'inferred_min_int_loop')
	min_int_loop_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', min_int_loop_binary,
		min_int_loop_source])
	assert min_int_loop_compile.exit_code == 0, min_int_loop_compile.output
	min_int_loop_c := os.read_file(min_int_loop_binary + '.c') or { panic(err) }
	assert !min_int_loop_c.contains('V_FASTC_PRINT_SELECT')
	min_int_loop_run := cmdexec.run(min_int_loop_binary, [])
	assert min_int_loop_run.exit_code == 0, min_int_loop_run.output
	assert min_int_loop_run.output.trim_space() == '-2147483648\n2147483647'

	high_hex_source := os.join_path(root, 'inferred_high_hex.v')
	os.write_file(high_hex_source, 'module main

fn main() {
	x := 0xffffffff | 0
	println(x)
}
') or {
		panic(err)
	}
	high_hex_binary := os.join_path(root, 'inferred_high_hex')
	high_hex_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', high_hex_binary,
		high_hex_source])
	assert high_hex_compile.exit_code == 0, high_hex_compile.output
	high_hex_c := os.read_file(high_hex_binary + '.c') or { panic(err) }
	assert !high_hex_c.contains('V_FASTC_PRINT_SELECT')
	high_hex_run := cmdexec.run(high_hex_binary, [])
	assert high_hex_run.exit_code == 0, high_hex_run.output
	assert high_hex_run.output.trim_space() == '-1'

	high_binary_source := os.join_path(root, 'inferred_high_binary.v')
	os.write_file(high_binary_source, 'module main

fn main() {
	x := 0b11111111111111111111111111111111 | 0
	println(x)
}
') or {
		panic(err)
	}
	high_binary_binary := os.join_path(root, 'inferred_high_binary')
	high_binary_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', high_binary_binary,
		high_binary_source])
	assert high_binary_compile.exit_code == 0, high_binary_compile.output
	high_binary_c := os.read_file(high_binary_binary + '.c') or { panic(err) }
	assert !high_binary_c.contains('V_FASTC_PRINT_SELECT')
	high_binary_run := cmdexec.run(high_binary_binary, [])
	assert high_binary_run.exit_code == 0, high_binary_run.output
	assert high_binary_run.output.trim_space() == '-1'

	parallel_assign_source := os.join_path(root, 'parallel_assign.v')
	os.write_file(parallel_assign_source, 'module main

fn main() {
	mut a := 1
	mut b := 2
	a, b = b, a
	println(a)
	println(b)
}
') or {
		panic(err)
	}
	parallel_assign_binary := os.join_path(root, 'parallel_assign')
	parallel_assign_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o',
		parallel_assign_binary, parallel_assign_source])
	assert parallel_assign_compile.exit_code == 0, parallel_assign_compile.output
	parallel_assign_c := os.read_file(parallel_assign_binary + '.c') or { panic(err) }
	assert !parallel_assign_c.contains('V_FASTC_PRINT_SELECT')
	parallel_assign_run := cmdexec.run(parallel_assign_binary, [])
	assert parallel_assign_run.exit_code == 0, parallel_assign_run.output
	assert parallel_assign_run.output.trim_space() == '2\n1'

	shift_source := os.join_path(root, 'oversized_shift.v')
	os.write_file(shift_source, 'module main

fn show(x int, n int) {
	println(x << n)
}

fn main() {
	show(1, 32)
}
') or {
		panic(err)
	}
	shift_binary := os.join_path(root, 'oversized_shift')
	shift_compile := cmdexec.run(v3_bin,
		['-silent', '-b', 'fastc', '-o', shift_binary, shift_source])
	assert shift_compile.exit_code == 0, shift_compile.output
	shift_run := cmdexec.run(shift_binary, [])
	assert shift_run.exit_code == 0, shift_run.output
	assert shift_run.output.trim_space() == '0'

	shift_assign_source := os.join_path(root, 'oversized_shift_assign.v')
	os.write_file(shift_assign_source, 'module main

fn shift(n int) {
	mut x := 1
	x <<= n
	println(x)
}

fn main() {
	shift(32)
}
') or {
		panic(err)
	}
	shift_assign_binary := os.join_path(root, 'oversized_shift_assign')
	shift_assign_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', shift_assign_binary,
		shift_assign_source])
	assert shift_assign_compile.exit_code == 0, shift_assign_compile.output
	shift_assign_run := cmdexec.run(shift_assign_binary, [])
	assert shift_assign_run.exit_code == 0, shift_assign_run.output
	assert shift_assign_run.output.trim_space() == '0'

	for is_modulo in [false, true] {
		case_name := if is_modulo { 'modulo' } else { 'division' }
		operator := if is_modulo { '%' } else { '/' }
		message := if is_modulo { 'modulo by zero' } else { 'division by zero' }
		zero_source := os.join_path(root, '${case_name}_by_zero.v')
		os.write_file(zero_source, 'module main

fn calculate(a int, b int) int {
	return a ${operator} b
}

fn main() {
	println(calculate(1, 0))
}
') or {
			panic(err)
		}
		zero_binary := os.join_path(root, '${case_name}_by_zero')
		zero_compile := cmdexec.run(v3_bin,
			['-silent', '-b', 'fastc', '-o', zero_binary, zero_source])
		assert zero_compile.exit_code == 0, zero_compile.output
		zero_run := cmdexec.run(zero_binary, [])
		assert zero_run.exit_code != 0, zero_run.output
		assert zero_run.output.contains('V panic: ${message}'), zero_run.output
	}

	sizeof_source := os.join_path(root, 'sizeof_string.v')
	os.write_file(sizeof_source, 'module main

fn main() {
	println(sizeof(string))
}
') or {
		panic(err)
	}
	sizeof_binary := os.join_path(root, 'sizeof_string')
	sizeof_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', sizeof_binary,
		sizeof_source])
	assert sizeof_compile.exit_code == 0, sizeof_compile.output
	sizeof_run := cmdexec.run(sizeof_binary, [])
	assert sizeof_run.exit_code == 0, sizeof_run.output
	expected_string_size := (sizeof(voidptr) + 2 * sizeof(int)).str()
	assert sizeof_run.output.trim_space() == expected_string_size

	string_index_source := os.join_path(root, 'string_index.v')
	os.write_file(string_index_source, "module main

fn main() {
	s := 'abc'
	println(s[0])
}
") or {
		panic(err)
	}
	string_index_binary := os.join_path(root, 'string_index')
	string_index_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', string_index_binary,
		string_index_source])
	assert string_index_compile.exit_code == 0, string_index_compile.output
	string_index_run := cmdexec.run(string_index_binary, [])
	assert string_index_run.exit_code == 0, string_index_run.output
	assert string_index_run.output.trim_space() == '97'

	c_string_source := os.join_path(root, 'c_string.v')
	os.write_file(c_string_source, "module main

fn main() {
	println(c'a')
}
") or { panic(err) }
	c_string_binary := os.join_path(root, 'c_string')
	c_string_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', c_string_binary,
		c_string_source])
	assert c_string_compile.exit_code == 0, c_string_compile.output
	c_string_c := os.read_file(c_string_binary + '.c') or { panic(err) }
	assert !c_string_c.contains('V_FASTC_PRINT_SELECT')
	c_string_run := cmdexec.run(c_string_binary, [])
	assert c_string_run.exit_code == 0, c_string_run.output
	assert c_string_run.output.trim_space() != '97', c_string_run.output

	mutable_interface_source := os.join_path(os.dir(@FILE), 'mutable_interface_array_value_test.v')
	mutable_interface_binary := os.join_path(root, 'mutable_interface_array_value_test')
	mutable_interface_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o',
		mutable_interface_binary, mutable_interface_source])
	assert mutable_interface_compile.exit_code == 0, mutable_interface_compile.output
	mutable_interface_run := cmdexec.run(mutable_interface_binary, [])
	assert mutable_interface_run.exit_code == 0, mutable_interface_run.output

	for loop_test in ['for_in_ref_map_test', 'for_in_map_of_pointers_test'] {
		loop_source := os.join_path(fastc_backend_vlib_dir, 'v', 'tests', 'loops', '${loop_test}.v')
		loop_binary := os.join_path(root, loop_test)
		loop_compile := cmdexec.run(v3_bin,
			['-silent', '-b', 'fastc', '-o', loop_binary, loop_source])
		assert loop_compile.exit_code == 0, loop_compile.output
		loop_run := cmdexec.run(loop_binary, [])
		assert loop_run.exit_code == 0, loop_run.output
	}

	literal_source := os.join_path(root, 'literal_values.v')
	os.write_file(literal_source, 'module main

fn main() {
	println(0_123)
	println(`★`)
}
') or {
		panic(err)
	}
	literal_binary := os.join_path(root, 'literal_values')
	literal_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', literal_binary,
		literal_source])
	assert literal_compile.exit_code == 0, literal_compile.output
	literal_c := os.read_file(literal_binary + '.c') or { panic(err) }
	assert literal_c.contains('println(123);')
	assert literal_c.contains('println(9733);')
	literal_run := cmdexec.run(literal_binary, [])
	assert literal_run.exit_code == 0, literal_run.output
	assert literal_run.output.trim_space() == '123\n9733'

	hex_escape_source := os.join_path(root, 'hex_escape.v')
	os.write_file(hex_escape_source, "module main

fn main() {
	println('\\x61ardvark')
}
") or {
		panic(err)
	}
	hex_escape_binary := os.join_path(root, 'hex_escape')
	hex_escape_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', hex_escape_binary,
		hex_escape_source])
	assert hex_escape_compile.exit_code == 0, hex_escape_compile.output
	hex_escape_c := os.read_file(hex_escape_binary + '.c') or { panic(err) }
	assert hex_escape_c.contains(r'println("\141ardvark");')
	hex_escape_run := cmdexec.run(hex_escape_binary, [])
	assert hex_escape_run.exit_code == 0, hex_escape_run.output
	assert hex_escape_run.output.trim_space() == 'aardvark'

	partial_octal_source := os.join_path(root, 'partial_octal_escape.v')
	os.write_file(partial_octal_source, r"module main

fn main() {
	println('\12')
}
") or {
		panic(err)
	}
	partial_octal_binary := os.join_path(root, 'partial_octal_escape')
	partial_octal_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o',
		partial_octal_binary, partial_octal_source])
	assert partial_octal_compile.exit_code == 0, partial_octal_compile.output
	partial_octal_c := os.read_file(partial_octal_binary + '.c') or { panic(err) }
	assert partial_octal_c.contains(r'println("\\12");')
	partial_octal_run := cmdexec.run(partial_octal_binary, [])
	assert partial_octal_run.exit_code == 0, partial_octal_run.output.bytes().str()
	assert partial_octal_run.output == '\\12\n', partial_octal_run.output.bytes().str()

	continued_string_source := os.join_path(root, 'continued_string.v')
	os.write_file(continued_string_source, r"module main

fn main() {
	println('left\
	   right')
}
") or {
		panic(err)
	}
	continued_string_binary := os.join_path(root, 'continued_string')
	continued_string_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o',
		continued_string_binary, continued_string_source])
	assert continued_string_compile.exit_code == 0, continued_string_compile.output
	continued_string_c := os.read_file(continued_string_binary + '.c') or { panic(err) }
	assert continued_string_c.contains(r'println("leftright");')
	continued_string_run := cmdexec.run(continued_string_binary, [])
	assert continued_string_run.exit_code == 0, continued_string_run.output
	assert continued_string_run.output.trim_space() == 'leftright'

	nul_source := os.join_path(root, 'nul_string.v')
	os.write_file(nul_source, 'module main

fn main() {
	println("a\\0b")
}
') or { panic(err) }
	nul_binary := os.join_path(root, 'nul_string')
	nul_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', nul_binary, nul_source])
	assert nul_compile.exit_code == 0, nul_compile.output
	nul_run := cmdexec.run(nul_binary, [])
	assert nul_run.exit_code == 0, nul_run.output.bytes().str()
	assert nul_run.output == 'a\0b\n', nul_run.output.bytes().str()

	wrapped_nul_source := os.join_path(root, 'wrapped_nul_string.v')
	os.write_file(wrapped_nul_source, "module main

fn main() {
	println('\\400tail')
}
") or {
		panic(err)
	}
	wrapped_nul_binary := os.join_path(root, 'wrapped_nul_string')
	wrapped_nul_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', wrapped_nul_binary,
		wrapped_nul_source])
	assert wrapped_nul_compile.exit_code == 0, wrapped_nul_compile.output
	wrapped_nul_c := os.read_file(wrapped_nul_binary + '.c') or { panic(err) }
	assert !wrapped_nul_c.contains('V_FASTC_PRINT_SELECT')
	wrapped_nul_run := cmdexec.run(wrapped_nul_binary, [])
	assert wrapped_nul_run.exit_code == 0, wrapped_nul_run.output.bytes().str()
	assert wrapped_nul_run.output == '\0tail\n', wrapped_nul_run.output.bytes().str()

	assert_source := os.join_path(root, 'assert_failure.v')
	os.write_file(assert_source, 'module main

fn main() {
	assert false
}
') or { panic(err) }
	assert_binary := os.join_path(root, 'assert_failure')
	assert_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', assert_binary,
		assert_source])
	assert assert_compile.exit_code == 0, assert_compile.output
	assert_run := cmdexec.run(assert_binary, [])
	assert assert_run.exit_code == 1, assert_run.output
	assert assert_run.output.contains('V panic: Assertion failed...'), assert_run.output
	assert assert_run.output.contains('${assert_source}:4: > assert false'), assert_run.output

	old_vjobs := os.getenv('VJOBS')
	os.setenv('VJOBS', '4', true)
	mut selfhosted_v3 := v3_bin
	for level in 1 .. 6 {
		next_v3 := os.join_path(root, 'v3_selfhosted_${level}')
		selfhost := cmdexec.run(selfhosted_v3, ['-silent', '-nocache', '-no-memory-limit',
			'-selfhost', '-b', 'fastc', '-o', next_v3, fastc_backend_v3_source])
		assert selfhost.exit_code == 0, 'fastc self-host level ${level}: ${selfhost.output}'
		assert os.is_executable(next_v3)
		selfhosted_v3 = next_v3
	}
	os.setenv('VJOBS', old_vjobs, true)

	selfhosted_binary := os.join_path(root, 'selfhosted_valid')
	selfhosted_compile := cmdexec.run(selfhosted_v3, ['-silent', '-b', 'fastc', '-o',
		selfhosted_binary, valid_source])
	assert selfhosted_compile.exit_code == 0, selfhosted_compile.output
	selfhosted_run := cmdexec.run(selfhosted_binary, [])
	assert selfhosted_run.exit_code == 0, selfhosted_run.output
	assert selfhosted_run.output.trim_space() == '42'
}
