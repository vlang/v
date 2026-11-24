import os
import v.tempname

fn export_attr_fixture_dir(name string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_export_attr_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

fn export_attr_write_project(root string, files map[string]string) {
	mut rels := files.keys()
	rels.sort()
	for rel in rels {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, files[rel]) or { panic(err) }
	}
}

fn export_attr_compile(main_file string, output string) os.Result {
	return os.exec([@VEXE, main_file, '-b', 'c', '-o', output])
}

fn export_attr_compile_without_memory_limit(main_file string, output string) os.Result {
	return os.exec([@VEXE, '-no-memory-limit', main_file, '-b', 'c', '-o', output])
}

fn test_imported_and_conditional_exports_use_expected_symbols() {
	root := export_attr_fixture_dir('imported_and_conditional')
	defer { os.rmdir_all(root) or {} }
	export_attr_write_project(root, {
		'main.v':       "module main

import expmod

fn C.raw_exported_answer() int

fn take_callback(cb fn () int) int {
	return cb()
}

@[if missing_export_flag ?]
@[export: 'raw_disabled_export']
fn disabled_export() int {
	return 2
}

@[export: 'raw_enabled_export']
fn enabled_export() int {
	return 7
}

fn main() {
	println(C.raw_exported_answer().str())
	println(expmod.exported_answer().str())
	println(take_callback(expmod.exported_answer).str())
	println(enabled_export().str())
}
"
		'expmod/mod.v': "module expmod

@[export: 'raw_exported_answer']
pub fn exported_answer() int {
	return helper_used()
}

fn helper_used() int {
	return 41
}

fn helper_unused() int {
	return 99
}
"
	})
	bin_path := os.join_path(root, 'app')
	compile := export_attr_compile(os.join_path(root, 'main.v'), bin_path)
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '41\n41\n41\n7', run.output

	c_code := os.read_file(bin_path + '.c') or { panic(err) }
	// Imported module bodies and export wrappers live in the module-cache unit. Their
	// declarations and the successful calls above prove they were rooted and linked.
	// The function keeps V's 64-bit `int`; its export presents the C ABI's `int`.
	assert c_code.contains('i64 expmod__exported_answer(void);'), c_code
	assert c_code.contains('int (raw_exported_answer)(void);'), c_code
	assert c_code.contains('raw_exported_answer()'), c_code
	assert c_code.contains('take_callback(expmod__exported_answer)'), c_code
	assert c_code.contains('int (raw_enabled_export)(void) {'), c_code
	assert !c_code.contains('raw_disabled_export'), c_code
}

fn test_export_natural_symbol_codegen_and_runtime() {
	root := export_attr_fixture_dir('natural_symbols')
	defer { os.rmdir_all(root) or {} }
	export_attr_write_project(root, {
		'main.v': "module main

@[export: 'natural_name']
fn natural_name(value f64) f64 {
	return value
}

@[export: 'natural_add']
fn natural_add(left int, right int) int {
	return left + right
}

@[export: 'renamed_add_export']
fn renamed_add(left int, right int) int {
	return left + right
}

fn take(cb fn (int, int) int) int {
	return cb(40, 2)
}

fn rint() int {
	return 1
}

@[export: 'v_rint']
fn exported_rint_collision() int {
	return 2
}

fn main() {
	println(natural_add(3_000_000_000, 1).str())
	println(take(natural_add).str())
	println(renamed_add(3_000_000_000, 2).str())
	println(rint().str())
	println(exported_rint_collision().str())
}
"
	})
	bin_path := os.join_path(root, 'app')
	compile := export_attr_compile(os.join_path(root, 'main.v'), bin_path)
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	assert !compile.output.contains('redefinition'), compile.output
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '3000000001\n42\n3000000002\n1\n2', run.output
	c_code := os.read_file(bin_path + '.c') or { panic(err) }
	assert c_code.count('double (natural_name)(double value) {') == 1, c_code
	assert !c_code.contains('main__natural_name'), c_code

	// An export presents the C ABI whether or not it is renamed: C callers pass and
	// read C `int`. V callers keep the 64-bit function under an internal name.
	assert c_code.contains('int (renamed_add_export)(int left, int right) {'), c_code
	natural_add_wrapper := 'int (natural_add)(int left, int right) {'
	assert c_code.count(natural_add_wrapper) == 1, c_code
	assert c_code.contains('i64 main__natural_add(i64 left, i64 right) {'), c_code
	assert c_code.all_after(natural_add_wrapper).all_before('\n}').contains('return main__natural_add(left, right);'), c_code
	assert c_code.contains('take(main__natural_add)'), c_code
	assert !c_code.contains('i64 (natural_add)('), c_code

	// Prefix the main-module function when an export owns its natural C symbol.
	rint_wrapper := 'int (v_rint)(void) {'
	assert c_code.count(rint_wrapper) == 1, c_code
	assert c_code.all_after(rint_wrapper).all_before('\n}').contains('return exported_rint_collision();'), c_code
}

fn test_natural_name_export_links_from_c_object_file() {
	$if windows {
		return
	}
	cc := os.find_abs_path_of_executable('cc') or { return }
	root := export_attr_fixture_dir('natural_object_file')
	defer { os.rmdir_all(root) or {} }
	export_attr_write_project(root, {
		'lib.v':  "module main

@[export: 'natural_mul']
pub fn natural_mul(left int, right int) int {
	return left * right
}
"
		'host.c': '#include <stdio.h>
extern int natural_mul(int left, int right);
int main(void) {
	printf("%d\\n", natural_mul(-6, 7));
	return 0;
}
'
	})
	object := os.join_path(root, 'lib.o')
	compile := os.exec([@VEXE, '-gc', 'none', '-cc', cc, '-is_o', '-o', object,
		os.join_path(root, 'lib.v')])
	assert compile.exit_code == 0, compile.output
	host := os.join_path(root, 'host')
	link := os.exec([cc, '-o', host, os.join_path(root, 'host.c'), object, '-lm', '-lpthread'])
	assert link.exit_code == 0, link.output
	run := os.exec([host])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '-42', run.output
}

fn test_export_main_collision_with_top_level_script_entry_is_rejected() {
	root := export_attr_fixture_dir('top_level_script_export_main_collision')
	defer { os.rmdir_all(root) or {} }
	export_attr_write_project(root, {
		'main.v': "
@[export: 'main']
fn exported_entry() {}

println('script')
"
	})
	compile := export_attr_compile(os.join_path(root, 'main.v'), os.join_path(root, 'app'))
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('export name `main` for `exported_entry`'), compile.output
	assert compile.output.contains('synthetic entry point `main`'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	assert !compile.output.contains('redefinition'), compile.output
}

fn test_export_main_collision_with_top_level_match_entry_is_rejected() {
	root := export_attr_fixture_dir('top_level_match_export_main_collision')
	defer { os.rmdir_all(root) or {} }
	export_attr_write_project(root, {
		'main.v': "
@[export: 'main']
fn exported_entry() {}

match 1 {
	1 { println('one') }
	else { println('other') }
}
"
	})
	compile := export_attr_compile(os.join_path(root, 'main.v'), os.join_path(root, 'app'))
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('export name `main` for `exported_entry`'), compile.output
	assert compile.output.contains('synthetic entry point `main`'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	assert !compile.output.contains('redefinition'), compile.output
}

fn test_export_main_collision_with_test_harness_entry_is_rejected() {
	root := export_attr_fixture_dir('test_harness_export_main_collision')
	defer { os.rmdir_all(root) or {} }
	export_attr_write_project(root, {
		'main_test.v': "
@[export: 'main']
fn exported_entry() {}

fn test_ok() {}
"
	})
	compile := export_attr_compile(os.join_path(root, 'main_test.v'), os.join_path(root,
		'app'))
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('export name `main` for `exported_entry`'), compile.output
	assert compile.output.contains('synthetic entry point `main`'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	assert !compile.output.contains('redefinition'), compile.output
}

fn test_export_main_in_non_main_test_module_reports_synthetic_collision() {
	root := export_attr_fixture_dir('test_harness_non_main_module_export_main')
	defer { os.rmdir_all(root) or {} }
	export_attr_write_project(root, {
		'foo_test.v': "module foo

@[export: 'main']
fn exported_entry() {}

fn test_ok() {}
"
	})
	compile := export_attr_compile(os.join_path(root, 'foo_test.v'), os.join_path(root,
		'app'))
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('export name `main` for `foo.exported_entry`'), compile.output
	assert compile.output.contains('synthetic entry point `main`'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	assert !compile.output.contains('redefinition'), compile.output
}

fn assert_export_attr_rejections(name string, files map[string]string, expected map[string][]string) {
	root := export_attr_fixture_dir(name)
	defer { os.rmdir_all(root) or {} }
	export_attr_write_project(root, files)
	compile := os.exec([@VEXE, '-b', 'c', '-nocolor', '-o', os.join_path(root, 'app'), root])
	assert compile.exit_code != 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	errors := compile.output.split_into_lines().filter(it.contains(': error: '))
	for file, messages in expected {
		file_errors := errors.filter(it.all_before(': error: ').contains('${file}:'))
		for message in messages {
			assert file_errors.any(it.contains(': error: ${message}')), 'expected `${message}` in ${file}\n${compile.output}'
		}
	}
}

fn test_export_attribute_checker_rejections() {
	assert_export_attr_rejections('checker_rejections', {
		'main.v':           'module main

import badexp

fn main() {
	println(badexp.answer().str())
}
'
		'duplicate.v':      "module main

@[export: 'raw_duplicate']
fn one() {}

@[export: 'raw_duplicate']
fn two() {}
"
		'runtime.v':        "module main

@[export: 'v_free']
fn collides_with_runtime_free() {}
"
		'invalid_names.v':  "module main

@[export: '1bad']
fn bad_digit() {}

@[export: 'for']
fn bad_keyword() {}
"
		'preamble_names.v': "module main

@[export: 'bool']
fn collides_with_preamble_bool() bool {
	return true
}

@[export: 'string']
fn collides_with_preamble_string() {}

@[export: 'voidptr']
fn collides_with_preamble_voidptr() {}

@[export: 'i8']
fn collides_with_preamble_i8() {}

@[export: 'true']
fn collides_with_preamble_true() {}

@[export: 'Array']
fn collides_with_runtime_array() {}

@[export: 'map']
fn collides_with_runtime_map() int {
	values := map[string]int{
		'a': 1
	}
	return values['a'] or { 0 }
}

@[export: 'DenseArray']
fn collides_with_runtime_dense_array() {}

@[export: 'SortedMap']
fn collides_with_runtime_sorted_map() {}

@[export: '__v_option']
fn collides_with_runtime_optional() {}
"
		'badexp/badexp.v':  "module badexp

@[export: '2bad']
pub fn answer() int {
	return 1
}
"
	}, {
		'duplicate.v':      ['duplicate export name `raw_duplicate`']
		'runtime.v':        ['export name `v_free`']
		'invalid_names.v':  ['1bad', 'for'].map('export name `${it}` should be a valid identifier')
		'preamble_names.v': ['bool', 'string', 'voidptr', 'i8', 'true', 'Array', 'map', 'DenseArray',
			'SortedMap', '__v_option'].map('export name `${it}` should be a valid identifier')
		'badexp.v':         ['export name `2bad` should be a valid identifier']
	})
}

fn test_export_attribute_parser_rejections() {
	assert_export_attr_rejections('parser_rejections', {
		'main.v':    'module main

fn main() {}
'
		'generic.v': "module main

@[export: 'raw_generic']
fn raw_generic[T](value T) T {
	return value
}
"
		'unnamed.v': "module main

@[export: 'raw_unnamed']
fn unnamed(int) int {
	return 0
}
"
	}, {
		'generic.v': ['generic functions cannot be exported']
		'unnamed.v': ['functions with type only params can not have bodies']
	})
}

fn test_export_wrappers_use_fixed_array_and_fn_pointer_return_types() {
	root := export_attr_fixture_dir('return_types')
	defer { os.rmdir_all(root) or {} }
	export_attr_write_project(root, {
		'main.v': "module main

@[export: 'raw_numbers']
fn numbers() [3]int {
	mut values := [3]int{}
	values[0] = 1
	values[1] = 2
	values[2] = 3
	return values
}

type Callback = fn () int

fn callback_value() int {
	return 7
}

@[export: 'raw_callback']
fn callback() Callback {
	return callback_value
}

fn main() {}
"
	})
	bin_path := os.join_path(root, 'app')
	compile := export_attr_compile(os.join_path(root, 'main.v'), bin_path)
	assert compile.exit_code == 0, compile.output
	c_code := os.read_file(bin_path + '.c') or { panic(err) }
	assert c_code.contains('_v_ret_Array_fixed_i64_3 numbers(void);'), c_code
	assert c_code.contains('_v_ret_Array_fixed_i64_3 (raw_numbers)(void);'), c_code
	assert c_code.contains('_v_ret_Array_fixed_i64_3 numbers(void) {'), c_code
	numbers_wrapper := '_v_ret_Array_fixed_i64_3 (raw_numbers)(void) {'
	assert c_code.contains(numbers_wrapper), c_code
	assert c_code.all_after(numbers_wrapper).all_before('\n}').contains('return numbers();'), c_code
	assert !c_code.contains('\nArray_fixed_i64_3 (raw_numbers)(void)'), c_code

	for name, return_type in {
		'callback':     'i64'
		'raw_callback': 'int'
	} {
		decl := if name == 'raw_callback' { '(${name})' } else { name }
		before_prototype, _ := c_code.split_once(' ${decl}(void);') or {
			panic('missing C prototype for ${name}')
		}
		typedef_name := before_prototype.all_after_last('\n')
		assert typedef_name.starts_with('_fn_ptr_'), typedef_name
		assert c_code.contains('typedef ${return_type} (*${typedef_name})(void);'), c_code
		assert c_code.contains('${typedef_name} ${decl}(void) {'), c_code
	}
	assert c_code.all_after(' (raw_callback)(void) {').all_before('\n}').contains('return callback();'), c_code
	assert !c_code.contains('fn_ptr:'), c_code
}

fn test_export_wrapper_for_veb_handler_forwards_implicit_ctx() {
	root := export_attr_fixture_dir('veb_implicit_ctx_export_wrapper')
	defer { os.rmdir_all(root) or {} }
	export_attr_write_project(root, {
		'main.v': "module main

import veb

pub struct Context {
	veb.Context
}

pub struct App {}

@[export: 'raw_show']
pub fn (app &App) show(id int) veb.Result {
	_ = id
	return ctx.text('ok')
}

@[export: 'raw_show_underscore']
pub fn (app &App) show_underscore(_ int) veb.Result {
	return ctx.text('ok')
}

fn main() {}
"
	})
	c_path := os.join_path(root, 'app.c')
	compile := export_attr_compile_without_memory_limit(os.join_path(root, 'main.v'),
		c_path)
	assert compile.exit_code == 0, compile.output
	c_code := os.read_file(c_path) or { panic(err) }
	for method, param in {
		'show':            'id'
		'show_underscore': '_2'
	} {
		signature := 'veb__Result (raw_${method})(main__App* app, main__Context* ctx, int ${param})'
		assert c_code.contains('${signature};'), c_code
		assert c_code.contains('${signature} {'), c_code
		body := c_code.all_after('${signature} {').all_before('\n}')
		assert body.contains('return App__${method}(app, ctx, ${param});'), body
	}
	assert !c_code.contains('return App__show_underscore(app, ctx, _1);'), c_code
}
