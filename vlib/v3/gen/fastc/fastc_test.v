module fastc

import os
import v3.cmdexec
import v3.pref

fn test_generate_and_compile_without_flat_ast() {
	source := 'module main

fn main() {
	mut total := 0
	label := "total="
	for i in 0 .. 3 {
		total += twice(i)
	}
	if true {
		print(label)
		println(total)
	} else {
		println(0)
	}
}

fn twice(value int) int {
	return value * 2
}
'
	prefs := pref.new_preferences()
	c_source := generate(source, 'fastc_test.v', prefs) or { panic(err) }
	assert c_source.contains('__typeof__((0)) total = (0);')
	assert c_source.contains('string label = ("total=");')
	assert c_source.contains('__v_fastc_range_start_0 = (0);')
	assert c_source.contains('__v_fastc_range_end_1 = (3);')
	assert c_source.contains('int twice(int value);')
	assert !c_source.contains('v3.flat')

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == 'total=6'
}

fn test_unsupported_import_requests_normal_backend() {
	prefs := pref.new_preferences()
	mut failed := false
	_ := generate('module main\nimport os\nfn main() {}\n', 'imports.v', prefs) or {
		failed = true
		''
	}
	assert failed
}

fn test_bare_return_from_main_emits_zero() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn stop() {
	return
}

fn main() {
	if true {
		return
	}
}
',
		'bare_return.v', prefs) or { panic(err) }
	assert c_source.contains('void stop(void) {\n\treturn;\n}')
	assert c_source.contains('if (((bool)true)) {\n\t\treturn 0;\n\t}')
}

fn test_integer_range_caches_bounds() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn start() int {
	return 0
}

fn limit() int {
	return 3
}

fn main() {
	for i in start() .. limit() {
		println(i)
	}
}
',
		'range_bounds.v', prefs) or { panic(err) }
	assert c_source.contains('__v_fastc_range_start_0 = (start());')
	assert c_source.contains('__v_fastc_range_end_1 = (limit());')
	assert c_source.contains('i < (__v_fastc_range_end_1)')
	assert !c_source.contains('i < (limit())')
}

fn test_decimal_and_rune_literals_preserve_v_values() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	println(0_123)
	println(`★`)
}
',
		'literal_values.v', prefs) or { panic(err) }
	assert c_source.contains('println(123);')
	assert c_source.contains('println(9733);')
}

fn test_hex_string_escape_has_fixed_width_in_c() {
	prefs := pref.new_preferences()
	c_source := generate("module main\nfn main() { println('\\x61ardvark') }\n", 'hex_escape.v',
		prefs) or { panic(err) }
	assert c_source.contains(r'println("\141ardvark");')
}

fn test_runtime_sensitive_constructs_request_checked_lane() {
	prefs := pref.new_preferences()
	mut nul_failed := false
	_ := generate('module main

fn main() {
	println("a\\0b")
}
', 'nul_string.v', prefs) or {
		nul_failed = true
		''
	}
	assert nul_failed

	mut assert_failed := false
	_ := generate('module main

fn main() {
	assert false
}
', 'assert.v', prefs) or {
		assert_failed = true
		''
	}
	assert assert_failed
}

fn test_type_sensitive_expressions_request_checked_lane() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { println(1 == 1) }\n',
		'module main\nfn main() { println(!false) }\n',
		'module main\nfn show(a u8, b u8) { println(a + b) }\nfn main() { show(255, 1) }\n',
		'module main\nfn show(x int, n int) { println(x << n) }\nfn main() { show(1, 32) }\n',
		'module main\nfn shift(n int) { mut x := 1; x <<= n; println(x) }\nfn main() { shift(32) }\n',
		'module main\nfn shift(n int) { mut x := 1; x >>= n; println(x) }\nfn main() { shift(32) }\n',
		'module main\nfn shift(n int) { mut x := 1; x >>>= n; println(x) }\nfn main() { shift(32) }\n',
		'module main\nfn divide(a int, b int) int { return a / b }\nfn main() { println(divide(1, 0)) }\n',
		'module main\nfn modulo(a int, b int) int { return a % b }\nfn main() { println(modulo(1, 0)) }\n',
		'module main\nfn divide(b int) { mut x := 1; x /= b; println(x) }\nfn main() { divide(0) }\n',
		'module main\nfn modulo(b int) { mut x := 1; x %= b; println(x) }\nfn main() { modulo(0) }\n',
		'module main\nfn main() { println(sizeof(string)) }\n',
		"module main\nfn main() { s := 'abc'; println(s[0]) }\n",
		"module main\nfn main() { println(c'a') }\n",
		'module main\nfn main() { mut x := -2_147_483_648; x--; println(x) }\n',
		'module main\nfn main() { for i := -2_147_483_648; true; i-- { println(i); break } }\n',
		'module main\nfn main() { x := 0xffff_ffff | 0; println(x) }\n',
		'module main\nfn main() { mut a := 1; mut b := 2; a, b = b, a; println(a); println(b) }\n',
	] {
		mut failed := false
		_ := generate(source, 'typed_expression.v', prefs) or {
			failed = true
			''
		}
		assert failed
	}

	bool_c := generate('module main\nfn main() { println(true) }\n', 'bool_literal.v', prefs) or {
		panic(err)
	}
	assert bool_c.contains('println(((bool)true));')
	low_hex_c := generate('module main\nfn main() { x := 0x7fff_ffff | 0; println(x) }\n',
		'low_hex_literal.v', prefs) or { panic(err) }
	assert low_hex_c.contains('__typeof__((0x7fffffff|0)) x = (0x7fffffff|0);')
	call_c := generate('module main\nfn sum(a int, b int) int { return a + b }\nfn main() { println(sum(1, 2)) }\n',
		'call_comma.v', prefs) or { panic(err) }
	assert call_c.contains('println(sum(1,2));')
}
