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

	mutable_interface_source := os.join_path(os.dir(@FILE), 'mutable_interface_array_value_test.v')
	mutable_interface_binary := os.join_path(root, 'mutable_interface_array_value_test')
	mutable_interface_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o',
		mutable_interface_binary, mutable_interface_source])
	assert mutable_interface_compile.exit_code == 0, mutable_interface_compile.output
	mutable_interface_run := cmdexec.run(mutable_interface_binary, [])
	assert mutable_interface_run.exit_code == 0, mutable_interface_run.output

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
