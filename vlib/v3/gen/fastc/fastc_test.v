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
	assert c_source.contains('setvbuf(stdout, NULL, _IONBF, 0);')
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

fn test_top_level_statements_emit_main_directly() {
	prefs := pref.new_preferences()
	c_source := generate("println('Hello, World!')\n", 'hello_world.v', prefs) or { panic(err) }
	assert c_source.contains('int main(void) {')
	assert c_source.contains('println("Hello, World!");')
	assert c_source.contains('setvbuf(stdout, NULL, _IONBF, 0);')
}

fn test_unsupported_import_is_rejected() {
	prefs := pref.new_preferences()
	mut failed := false
	_ := generate('module main\nimport os\nfn main() {}\n', 'imports.v', prefs) or {
		failed = true
		''
	}
	assert failed
}

fn test_unresolved_names_are_rejected_before_c_emission() {
	prefs := pref.new_preferences()
	for source in [
		"module main\nfn main() { puts('hello') }\n",
		'module main\nfn main() { printf("hello") }\n',
		'module main\nfn main() { value := stdout; println(value) }\n',
	] {
		mut message := ''
		_ := generate(source, 'unresolved_name.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('fastc parser does not support unresolved name'), message
	}
}

fn test_declared_names_are_available_without_an_ast() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	println(later(2))
}

fn later(value int) int {
	return value + 1
}
',
		'declared_names.v', prefs) or { panic(err) }
	assert c_source.contains('println(later(2));')
}

fn test_narrow_integer_cast_expressions_are_rejected() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn main() {
	println(u8(255) + u8(1))
}
',
		'narrow_cast_expression.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('narrow integer cast expressions'), message
}

fn test_undeclared_function_signature_types_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn show(x size_t) { println(1) }\nfn main() { show(1) }\n',
		'module main\nfn value() size_t { return 1 }\nfn main() { println(value()) }\n',
	] {
		mut message := ''
		_ := generate(source, 'undeclared_signature_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('undeclared type `size_t`'), message
	}
}

fn test_declared_function_call_argument_types_are_validated() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn show(x bool) {
	println(x)
}

fn main() {
	show(2)
}
',
		'invalid_call_argument.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('argument 1 of type `integer literal`'), message
	assert message.contains('function `show` expecting `bool`'), message

	c_source := generate('module main

fn increment(x int) int {
	return x + 1
}

fn show(x bool) {
	println(x)
}

fn main() {
	value := 2
	flag := true
	println(increment(value))
	show(flag)
}
',
		'valid_call_arguments.v', prefs) or { panic(err) }
	assert c_source.contains('println(increment(value));')
	assert c_source.contains('show(flag);')
}

fn test_scanner_diagnostics_are_rejected() {
	prefs := pref.new_preferences()
	source := "module main\nfn main() { println('" + r'\_' + "') }\n"
	mut message := ''
	_ := generate(source, 'scanner_diagnostic.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('fastc scanner error'), message
	assert message.contains('`_` unknown escape sequence'), message
}

fn test_conditions_must_be_boolean() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { if 2 { println(1) } }\n',
		'module main\nfn main() { value := 2; for value { break } }\n',
		'module main\nfn main() { for 2 { break } }\n',
		'module main\nfn main() { for i := 0; 2; i++ { break } }\n',
	] {
		mut message := ''
		_ := generate(source, 'non_boolean_condition.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('condition of type'), message
		assert message.contains('instead of `bool`'), message
	}

	c_source := generate('module main

fn ready() bool {
	return true
}

fn main() {
	flag := true
	if flag {
		println(1)
	}
	for ready() {
		break
	}
	for i := 0; ready(); i++ {
		break
	}
}
',
		'boolean_conditions.v', prefs) or { panic(err) }
	assert c_source.contains('if (flag) {')
	assert c_source.contains('while (ready()) {')
	assert c_source.contains('; ready(); i++) {')
}

fn test_return_expression_type_is_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn value() bool { return 2 }\nfn main() { println(value()) }\n',
		'module main\nfn value() int { return true }\nfn main() { println(value()) }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_return_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('return expression of type'), message
		assert message.contains('function returning'), message
	}

	c_source := generate('module main

fn enabled() bool {
	return true
}

fn value() int {
	return 2
}

fn main() {
	println(enabled())
	println(value())
}
',
		'valid_return_types.v', prefs) or { panic(err) }
	assert c_source.contains('return ((bool)true);')
	assert c_source.contains('return 2;')
}

fn test_assignment_value_type_is_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { mut enabled := false; enabled = 2; println(enabled) }\n',
		'module main\nfn main() { mut count := 1; count = true; println(count) }\n',
		'module main\nfn main() { mut enabled := false; enabled += 1; println(enabled) }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_assignment_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('assignment of type'), message
		assert message.contains('of type'), message
	}

	c_source := generate('module main

fn ready() bool {
	return true
}

fn main() {
	mut enabled := false
	enabled = ready()
	mut count := 1
	count = 2
	count += 3
	println(enabled)
	println(count)
}
',
		'valid_assignment_types.v', prefs) or { panic(err) }
	assert c_source.contains('enabled=ready();')
	assert c_source.contains('count=2;')
	assert c_source.contains('count+=3;')
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

fn test_non_void_functions_must_return_on_every_path() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn value() int {}\nfn main() { println(value()) }\n',
		'module main\nfn value() int { if true { return 1 } }\nfn main() { println(value()) }\n',
		'module main\nfn value() int { return }\nfn main() { println(value()) }\n',
	] {
		mut message := ''
		_ := generate(source, 'non_void_fallthrough.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('fastc parser does not support'), message
	}
	c_source := generate('module main

fn value(flag bool) int {
	if flag {
		return 1
	} else {
		return 2
	}
}

fn main() {
	println(value(true))
}
',
		'non_void_returns.v', prefs) or { panic(err) }
	assert c_source.contains('return 1;')
	assert c_source.contains('return 2;')
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

fn test_decimal_literals_preserve_v_values() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	println(0_123)
}
', 'literal_values.v', prefs) or {
		panic(err)
	}
	assert c_source.contains('println(123);')
}

fn test_v_octal_literals_are_translated_to_gnu_c() {
	assert fastc_c_number('0o17')! == '017'
	assert fastc_c_number('0O7_1')! == '071'
	mut oversized_message := ''
	_ := fastc_c_number('0o20000000000') or {
		oversized_message = err.msg()
		''
	}
	assert oversized_message.contains('high-bit nondecimal literals')
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	println(0o17)
}
', 'octal_literal.v', prefs) or {
		panic(err)
	}
	assert c_source.contains('println(017);')
}

fn test_hex_string_escape_has_fixed_width_in_c() {
	prefs := pref.new_preferences()
	c_source := generate("module main\nfn main() { println('\\x61ardvark') }\n", 'hex_escape.v',
		prefs) or { panic(err) }
	assert c_source.contains(r'println("\141ardvark");')
}

fn test_partial_octal_string_escapes_are_reencoded() {
	assert fastc_c_string(r"'\1'")! == r'"\\1"'
	assert fastc_c_string(r"'\12'")! == r'"\\12"'
	assert fastc_c_string(r"'\123'")! == r'"\123"'
}

fn test_string_line_continuations_match_v_unescaping() {
	prefs := pref.new_preferences()
	source := r"module main

fn main() {
	println('left\
	   right')
}
"
	c_source := generate(source, 'continued_string.v', prefs) or { panic(err) }
	assert c_source.contains(r'println("leftright");')
	crlf_literal := "'left\\" + '\r\n' + "\t  right'"
	assert fastc_c_string(crlf_literal)! == '"leftright"'
	assert fastc_c_string(r"'left\nright'")! == r'"left\nright"'
}

fn test_runtime_sensitive_constructs_are_rejected() {
	prefs := pref.new_preferences()
	for source in ['module main

fn main() {
	println("a\\0b")
}
',
		"module main\nfn main() { println('\\400tail') }\n"] {
		mut nul_failed := false
		_ := generate(source, 'nul_string.v', prefs) or {
			nul_failed = true
			''
		}
		assert nul_failed
	}
	assert fastc_string_contains_nul(r'\400tail', false)
	assert !fastc_string_contains_nul(r'\401tail', false)
	non_nul_octal_c := generate("module main\nfn main() { println('\\401tail') }\n",
		'non_nul_octal_string.v', prefs) or { panic(err) }
	assert non_nul_octal_c.contains(r'println("\401tail");')

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

fn test_type_sensitive_expressions_are_rejected() {
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
		'module main\nfn main() { println(`A`) }\n',
		'module main\nfn show(r rune) { println(r) }\nfn main() { show(65) }\n',
		'module main\nfn main() { println(rune(65)) }\n',
		'module main\nfn show(p charptr) { println(p) }\nfn main() { unsafe { show(nil) } }\n',
		'module main\nfn main() { p := charptr(0); println(p) }\n',
		'module main\nfn main() { println(1 ^ 2 + 3) }\n',
		'module main\nfn main() { println(10 & 3 + 1) }\n',
		'module main\nfn main() { println(1 | 2 ^ 3) }\n',
		'module main\nfn main() { println(1 & 2 * 3) }\n',
		'module main\nfn main() { mut x := -2_147_483_648; x--; println(x) }\n',
		'module main\nfn main() { for i := -2_147_483_648; true; i-- { println(i); break } }\n',
		'module main\nfn main() { mut x := -2_147_483_648 - 1; println(x) }\n',
		'module main\nfn main() { x := 2_147_483_649 | 0; println(x) }\n',
		'module main\nfn main() { x := 0xffff_ffff | 0; println(x) }\n',
		'module main\nfn main() { x := 0b11111111111111111111111111111111 | 0; println(x) }\n',
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
	low_binary_c := generate('module main\nfn main() { x := 0b01111111111111111111111111111111 | 0; println(x) }\n',
		'low_binary_literal.v', prefs) or { panic(err) }
	assert low_binary_c.contains('__typeof__((0b01111111111111111111111111111111|0))')
	max_int_c := generate('module main\nfn main() { x := 2_147_483_647 - 1; println(x) }\n',
		'max_int_expression.v', prefs) or { panic(err) }
	assert max_int_c.contains('__typeof__((2147483647-1)) x = (2147483647-1);')
	call_c := generate('module main\nfn sum(a int, b int) int { return a + b }\nfn main() { println(sum(1, 2)) }\n',
		'call_comma.v', prefs) or { panic(err) }
	assert call_c.contains('println(sum(1,2));')
}
