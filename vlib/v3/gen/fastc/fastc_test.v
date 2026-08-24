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

fn test_generate_files_resolves_modules_without_an_ast() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_modules_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'mathutil')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'mathutil', 'mathutil.v')
	os.write_file(main_file,
		'module main\nimport mathutil\nfn main() { println(mathutil.twice(21)) }\n') or {
		panic(err)
	}
	os.write_file(module_file,
		'module mathutil\npub fn twice(value int) int { return value * 2 }\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('int mathutil__twice(int value);')
	assert c_source.contains('println(mathutil__twice(21));'), c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == '42'
}

fn test_generate_files_rejects_private_imported_functions() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_private_import_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'secrets')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'secrets', 'secrets.v')
	os.write_file(main_file,
		'module main\nimport secrets\nfn main() { println(secrets.secret()) }\n') or { panic(err) }
	os.write_file(module_file, 'module secrets\nfn secret() int { return 42 }\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('private function `secret` from imported module `secrets`'), message

	os.write_file(module_file, 'module secrets\npub fn secret() int { return 42 }\n') or {
		panic(err)
	}
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('println(secrets__secret());'), c_source
}

fn test_generate_files_rejects_private_imported_constants() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_private_constant_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'secrets')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'secrets', 'secrets.v')
	os.write_file(main_file, 'module main\nimport secrets\nfn main() { println(secrets.secret) }\n') or {
		panic(err)
	}
	os.write_file(module_file, 'module secrets\nconst secret = 42\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('private constant `secret` from imported module `secrets`'), message

	os.write_file(module_file, 'module secrets\npub const secret = 42\n') or { panic(err) }
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('println(secrets__secret);'), c_source
}

fn test_generate_files_rejects_private_imported_types() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_private_types_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'secrets')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'secrets', 'secrets.v')
	os.write_file(module_file, 'module secrets

struct SecretStruct {}
enum SecretEnum { value }
interface SecretInterface {}
union SecretUnion { value int }
type SecretAlias = int
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	for type_name in ['SecretStruct', 'SecretEnum', 'SecretInterface', 'SecretUnion', 'SecretAlias'] {
		os.write_file(main_file,
			'module main\nimport secrets\nfn consume(value secrets.${type_name}) {}\nfn main() {}\n') or {
			panic(err)
		}
		mut message := ''
		_ := generate_files([main_file], prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('private type `${type_name}` from imported module `secrets`'), message
	}

	os.write_file(module_file, 'module secrets\npub struct SecretStruct {}\n') or { panic(err) }
	os.write_file(main_file,
		'module main\nimport secrets\nfn consume(value secrets.SecretStruct) {}\nfn main() {}\n') or {
		panic(err)
	}
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('void consume(secrets__SecretStruct value);'), c_source
}

fn test_generate_files_restricts_unqualified_imported_type_lookup() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_type_scope_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'widgets')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'widgets', 'widgets.v')
	os.write_file(module_file, 'module widgets\npub struct Widget {}\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]

	os.write_file(main_file,
		'module main\nimport widgets\nfn consume(value Widget) {}\nfn main() {}\n') or {
		panic(err)
	}
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('undeclared type `Widget`'), message

	os.write_file(main_file,
		'module main\nimport widgets { Widget }\nfn consume(value Widget) {}\nfn main() {}\n') or {
		panic(err)
	}
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('void consume(widgets__Widget value);'), c_source

	os.write_file(module_file, 'module widgets\nstruct Widget {}\n') or { panic(err) }
	message = ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('private type `Widget` from imported module `widgets`'), message
}

fn test_disabled_function_attributes_emit_empty_stubs() {
	mut prefs := pref.new_preferences()
	prefs.user_defines = []
	c_source := generate('module main

@[if fastc_missing_define ?]
fn traced() {
	println("must not run")
}

fn main() {
	traced()
}
',
		'disabled_function_attribute.v', prefs) or { panic(err) }
	assert c_source.contains('void traced(void) {\n}')
	assert !c_source.contains('must not run')
}

fn test_compound_function_attributes_evaluate_the_complete_condition() {
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('linux', pref.host_arch()) or { panic(err) }
	c_source := generate('module main

@[if linux && windows]
fn impossible() {
	println("disabled compound condition")
}

@[if linux || windows]
fn supported() {
	println("enabled compound condition")
}

fn main() {
	impossible()
	supported()
}
',
		'compound_function_attribute.v', prefs) or { panic(err) }
	assert c_source.contains('void impossible(void) {\n}')
	assert !c_source.contains('disabled compound condition')
	assert c_source.contains('enabled compound condition')
}

fn test_initialized_global_value_is_emitted() {
	prefs := pref.new_preferences()
	c_source := generate('module main

__global answer = 42

fn main() {
	println(answer)
}
',
		'initialized_global.v', prefs) or { panic(err) }
	assert c_source.contains('static int answer;'), c_source
	assert c_source.contains('\tanswer = 42;'), c_source
	assert c_source.contains('v_fastc_init_globals();'), c_source
}

fn test_select_statements_are_rejected() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn main() {
	select {
		value := <-messages { println(value) }
		else { println(0) }
	}
}
',
		'select_statement.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('fastc parser does not support select statements'), message
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

fn test_match_branch_values_must_match_the_subject_type() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { x := 1; match x { true { println(1) } else {} } }\n',
		'module main\nfn main() { x := true; match x { 1 { println(1) } else {} } }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_match_branch_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('match branch value of type'), message
		assert message.contains('subject of type'), message
	}

	c_source := generate('module main

fn main() {
	x := 1
	match x {
		0, 1 { println(1) }
		else {}
	}
}
',
		'valid_match_branch_types.v', prefs) or { panic(err) }
	assert c_source.contains('if (((__v_fastc_match_'), c_source
	assert c_source.contains('== (0)) || '), c_source
	assert c_source.contains('== (1))'), c_source
}

fn test_primitive_cast_operands_and_unsafe_context_are_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { println(bool(2)) }\n',
		"module main\nfn main() { println(int('2')) }\n",
		'module main\nfn main() { println(string(2)) }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_primitive_cast.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('cast'), message
	}

	c_source := generate('module main

fn main() {
	println(bool(true))
	println(int(true))
	unsafe {
		println(bool(2))
	}
	println(unsafe { bool(0) })
}
',
		'valid_primitive_casts.v', prefs) or { panic(err) }
	assert c_source.contains('println(((bool)(2)));'), c_source
	assert c_source.contains('println(((bool)(0)));'), c_source
}

fn test_defer_is_emitted_when_its_lexical_scope_exits() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	if true {
		defer { println(1) }
		println(2)
	}
	println(3)
}
',
		'scoped_defer.v', prefs) or { panic(err) }
	print_two := c_source.index('println(2);') or { panic(c_source) }
	deferred_one := c_source.index_after('println(1);', print_two) or { panic(c_source) }
	print_three := c_source.index_after('println(3);', deferred_one) or { panic(c_source) }
	assert print_two < deferred_one
	assert deferred_one < print_three
}

fn test_return_expression_is_evaluated_before_deferred_blocks() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn value() int {
	mut x := 1
	defer { x = 2 }
	return x
}

fn main() {
	println(value())
}
',
		'return_before_defer.v', prefs) or { panic(err) }
	evaluation := c_source.index('__typeof__((x)) __v_fastc_return_') or { panic(c_source) }
	deferred_assignment := c_source.index_after('x=2;', evaluation) or { panic(c_source) }
	returned_temporary := c_source.index_after('return __v_fastc_return_', deferred_assignment) or {
		panic(c_source)
	}
	assert evaluation < deferred_assignment
	assert deferred_assignment < returned_temporary
}

fn test_mutable_function_parameters_require_mutable_arguments() {
	prefs := pref.new_preferences()
	mut pointer_message := ''
	_ := generate('module main

fn change(mut x int) {
	x = 2
}

fn main() {
	x := 1
	change(&x)
}
',
		'immutable_pointer_argument.v', prefs) or {
		pointer_message = err.msg()
		''
	}
	assert pointer_message.contains('requires a mutable argument written with `mut`'), pointer_message

	mut immutable_message := ''
	_ := generate('module main

fn change(mut x int) {
	x = 2
}

fn main() {
	x := 1
	change(mut x)
}
',
		'immutable_mut_argument.v', prefs) or {
		immutable_message = err.msg()
		''
	}
	assert immutable_message.contains('mutable argument `x` to function `change` is immutable'), immutable_message

	c_source := generate('module main

fn change(mut x int) {
	x = 2
}

fn main() {
	mut x := 1
	change(mut x)
	println(x)
}
',
		'mutable_argument.v', prefs) or { panic(err) }
	assert c_source.contains('void change(int* x)'), c_source
	assert c_source.contains('change(&x);'), c_source
}

fn test_match_expression_requires_else() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn main() {
	x := 2
	y := match x { 1 { 7 } }
	println(y)
}
',
		'non_exhaustive_match_expression.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('non-exhaustive match expression without `else`'), message

	c_source := generate('module main

fn main() {
	x := 2
	y := match x { 1 { 7 } else { 9 } }
	println(y)
}
',
		'exhaustive_match_expression.v', prefs) or { panic(err) }
	assert c_source.contains('? (7) : (9)')
}

fn test_match_statement_without_else_does_not_terminate_function() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn value(x int) int {
	match x {
		1 { return 7 }
	}
}

fn main() {
	println(value(1))
}
',
		'non_exhaustive_match_statement.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('non-void function `value` that can fall through'), message

	c_source := generate('module main

fn value(x int) int {
	match x {
		1 { return 7 }
		else { return 9 }
	}
}

fn main() {
	println(value(1))
}
',
		'exhaustive_match_statement.v', prefs) or { panic(err) }
	assert c_source.contains('else {\n\t\treturn 9;'), c_source
}

fn test_c_reserved_identifiers_are_escaped_consistently() {
	prefs := pref.new_preferences()
	c_source := generate('module main

struct Holder {
	auto int
}

fn calculate(holder Holder, register int) int {
	restrict := register
	return holder.auto + restrict
}

fn auto() int {
	return 42
}

fn main() {
	result := auto()
	auto := result
	println(auto)
}
',
		'reserved_identifiers.v', prefs) or { panic(err) }
	assert c_source.contains('int v_auto;'), c_source
	assert c_source.contains('int calculate(Holder holder, int v_register)'), c_source
	assert c_source.contains('__typeof__((v_register)) v_restrict = (v_register);'), c_source
	assert c_source.contains('return holder.v_auto+v_restrict;'), c_source
	assert c_source.contains('int v_auto(void)'), c_source
	assert c_source.contains(' v_auto = (result);'), c_source
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

fn test_c_style_loop_initializer_type_is_validated() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn main() {
	mut enabled := false
	for enabled = 2; enabled; enabled = false {
		println(1)
	}
}
',
		'invalid_loop_initializer.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('assignment of type `integer literal` to `enabled` of type `bool`'), message

	c_source := generate('module main

fn main() {
	mut enabled := false
	for enabled = true; enabled; enabled = false {}
}
',
		'valid_loop_initializer.v', prefs) or { panic(err) }
	assert c_source.contains('for (enabled = (((bool)true)); enabled; enabled=((bool)false)) {'), c_source
}

fn test_negative_integer_literals_are_rejected_for_unsigned_targets() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn take(x u32) { println(x) }\nfn main() { take(-1) }\n',
		'module main\nfn main() { mut value := u32(0); value = -1; println(value) }\n',
		'module main\nfn value() u32 { return -1 }\nfn main() { println(value()) }\n',
	] {
		mut message := ''
		_ := generate(source, 'negative_unsigned_literal.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('negative integer literal'), message
		assert message.contains('u32'), message
	}

	c_source := generate('module main

fn take(x u32) {
	println(x)
}

fn value() u32 {
	return 1
}

fn take_signed(x int) {
	println(x)
}

fn signed_value() int {
	return -1
}

fn main() {
	mut number := u32(0)
	number = 1
	take(1)
	println(value())
	mut signed := 0
	signed = -1
	take_signed(-1)
	println(signed_value())
}
',
		'positive_unsigned_literals.v', prefs) or { panic(err) }
	assert c_source.contains('number=1;')
	assert c_source.contains('take(1);')
	assert c_source.contains('return 1;')
	assert c_source.contains('signed=-1;')
	assert c_source.contains('take_signed(-1);')
	assert c_source.contains('return -1;')
}

fn test_main_must_not_return_a_value() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main\nfn main() int { return 7 }\n', 'value_returning_main.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('main function returning `int`'), message
}

fn test_main_must_not_have_parameters() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main(code int) {}\n',
		'module main\nfn main(code int) int { return code }\n',
	] {
		mut message := ''
		_ := generate(source, 'parameterized_main.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('main function with parameters'), message
	}
}

fn test_range_bounds_must_be_integers() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { for i in 0.0 .. 2.0 { println(i) } }\n',
		'module main\nfn main() { for i in 0 .. 2.0 { println(i) } }\n',
		'module main\nfn main() { for i in false .. true { println(i) } }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_range_bounds.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('range bounds of types'), message
		assert message.contains('must both be integers'), message
	}
}

fn test_literal_range_must_not_be_empty() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { for i in 4 .. 2 { println(i) } }\n',
		'module main\nfn main() { for i in 2 .. 2 { println(i) } }\n',
	] {
		mut message := ''
		_ := generate(source, 'empty_literal_range.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('empty range:'), message
		assert message.contains('will never execute'), message
	}

	c_source := generate('module main\nfn main() { for i in 2 .. 4 { println(i) } }\n',
		'valid_literal_range.v', prefs) or { panic(err) }
	assert c_source.contains('for (__typeof__((__v_fastc_range_start_0)) i = (__v_fastc_range_start_0); i < (__v_fastc_range_end_1); i++) {'), c_source
}

fn test_arithmetic_operands_must_be_numeric() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { println(true + false) }\n',
		'module main\nfn main() { value := true * false; println(value) }\n',
		'module main\nfn main() { mut value := true; value += false; println(value) }\n',
	] {
		mut message := ''
		_ := generate(source, 'non_numeric_arithmetic.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('arithmetic'), message
		assert message.contains('non-numeric') || message.contains('operands of types'), message
	}
}

fn test_nil_requires_an_unsafe_block() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main\nfn show(p &int) { println(*p) }\nfn main() { show(nil) }\n',
		'nil_outside_unsafe.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('`nil` outside an `unsafe` block'), message

	c_source := generate('module main\nfn accept(p &int) {}\nfn main() { unsafe { accept(nil) } }\n',
		'nil_inside_unsafe.v', prefs) or { panic(err) }
	assert c_source.contains('accept(NULL);')
}

fn test_bitwise_negation_requires_an_integer() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main\nfn main() { println(~true) }\n', 'bool_bit_not.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('bitwise negation of non-integer type `bool`'), message

	c_source := generate('module main\nfn main() { println(~1) }\n', 'integer_bit_not.v', prefs) or {
		panic(err)
	}
	assert c_source.contains('println(~1);')
}

fn test_value_only_expression_statements_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { 1 }\n',
		'module main\nfn main() { true }\n',
		'module main\nfn main() { value := 1; value }\n',
		'module main\nfn main() { int(1) }\n',
	] {
		mut message := ''
		_ := generate(source, 'value_expression_statement.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('value-only expression statement'), message
	}

	c_source := generate('module main\nfn touch() {}\nfn main() { mut count := 0; touch(); count++ }\n',
		'valid_expression_statements.v', prefs) or { panic(err) }
	assert c_source.contains('touch();')
	assert c_source.contains('count++;')
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
