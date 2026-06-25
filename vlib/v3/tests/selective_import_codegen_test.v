import os

const selective_import_vexe = @VEXE
const selective_import_tests_dir = os.dir(@FILE)
const selective_import_v3_dir = os.dir(selective_import_tests_dir)
const selective_import_vlib_dir = os.dir(selective_import_v3_dir)
const selective_import_v3_src = os.join_path(selective_import_v3_dir, 'v3.v')

fn selective_import_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_selective_import_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${selective_import_vexe} -gc none -path "${selective_import_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${selective_import_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn selective_import_write_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn selective_import_write_project(name string, main_src string) string {
	return selective_import_write_project_with_extra(name, main_src, {})
}

fn selective_import_write_project_with_extra(name string, main_src string, extra_files map[string]string) string {
	root := os.join_path(os.temp_dir(), 'v3_selective_import_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	selective_import_write_file(root, 'v.mod', "Module { name: 'selective_import_test' }\n")
	selective_import_write_file(root, 'mymodules/main_functions.v', 'module mymodules

pub fn add_xy(x int, y int) int {
	return x + y
}

pub fn hidden_xy(x int, y int) int {
	return x * 100 + y
}
')
	selective_import_write_file(root, 'mymodules/submodule/sub_functions.v', 'module submodule

pub fn sub_xy(x int, y int) int {
	return x - y
}
')
	selective_import_write_file(root, 'main.v', main_src)
	for rel, src in extra_files {
		selective_import_write_file(root, rel, src)
	}
	return root
}

fn selective_import_compile_run(v3_bin string, name string, main_src string) (string, string) {
	root := selective_import_write_project(name, main_src)
	return selective_import_compile_run_root(v3_bin, root)
}

fn selective_import_compile_run_with_extra(v3_bin string, name string, main_src string, extra_files map[string]string) (string, string) {
	root := selective_import_write_project_with_extra(name, main_src, extra_files)
	return selective_import_compile_run_root(v3_bin, root)
}

fn selective_import_compile_run_root(v3_bin string, root string) (string, string) {
	bin := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${root} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	return run.output.trim_space(), generated
}

fn selective_import_compile_bad(v3_bin string, name string, main_src string) string {
	root := selective_import_write_project(name, main_src)
	return selective_import_compile_bad_root(v3_bin, name, root)
}

fn selective_import_compile_bad_with_extra(v3_bin string, name string, main_src string, extra_files map[string]string) string {
	root := selective_import_write_project_with_extra(name, main_src, extra_files)
	return selective_import_compile_bad_root(v3_bin, name, root)
}

fn selective_import_compile_bad_root(v3_bin string, name string, root string) string {
	bin := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${root} -b c -o ${bin}')
	assert compile.exit_code != 0, '${name}: compile unexpectedly succeeded: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), compile.output
	return compile.output
}

fn selective_import_ambiguous_modules() map[string]string {
	return {
		'a/a.v': 'module a

pub fn hit() int {
	return 1
}
'
		'b/b.v': 'module b

pub fn hit() int {
	return 2
}
'
	}
}

fn test_selective_import_calls_module_and_submodule_functions() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run(v3_bin, 'positive', 'module main

import mymodules { add_xy }
import mymodules.submodule { sub_xy }

fn main() {
	println(int_str(add_xy(2, 3)))
	println(int_str(sub_xy(10, 7)))
}
')
	assert output == '5\n3'
	assert generated.contains('mymodules__add_xy(2, 3)'), generated
	assert generated.contains('submodule__sub_xy(10, 7)'), generated
}

fn test_selective_import_symbol_can_be_used_as_function_value() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run(v3_bin, 'fn_value', 'module main

import mymodules { add_xy }

fn main() {
	f := add_xy
	println(int_str(f(2, 3)))
}
')
	assert output == '5'
	assert generated.contains('mymodules__add_xy'), generated
	assert generated.contains('f(2, 3)'), generated
	assert !generated.contains('int f = mymodules__add_xy'), generated
}

fn test_selective_import_function_value_roots_exact_symbol_with_imported_homonym() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run_with_extra(v3_bin,
		'fn_value_imported_homonym', 'module main

import a { choose }
import b

fn main() {
	f := choose
	println(int_str(f()))
}
', {
		'a/a.v': 'module a

pub fn choose() int {
	return 11
}
'
		'b/b.v': 'module b

pub fn choose() int {
	return 99
}
'
	})
	assert output == '11'
	assert generated.contains('a__choose'), generated
	assert !generated.contains('b__choose'), generated
}

fn test_selective_import_does_not_import_other_symbols_by_suffix() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad(v3_bin, 'hidden_symbol', 'module main

import mymodules { add_xy }

fn main() {
	println(int_str(hidden_xy(2, 3)))
}
')
	assert output.contains('unknown function `hidden_xy`'), output
}

fn test_selective_import_scope_is_file_local() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad_with_extra(v3_bin, 'file_local', 'module main

import mymodules { add_xy }

fn main() {
	println(int_str(add_xy(1, 2)))
}
', {
		'other.v': 'module main

fn from_other_file() int {
	return add_xy(3, 4)
}
'
	})
	assert output.contains('unknown function `add_xy`'), output
}

fn test_selective_import_same_symbol_from_two_modules_is_ambiguous() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad_with_extra(v3_bin, 'ambiguous_symbol', 'module main

import a { hit }
import b { hit }

fn main() {
	println(int_str(hit()))
}
',
		selective_import_ambiguous_modules())
	assert output.contains('ambiguous selective import `hit`'), output
}

fn test_duplicate_selective_import_fails_even_when_unused() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad_with_extra(v3_bin, 'ambiguous_unused', 'module main

import a { hit }
import b { hit }

fn main() {
	println("ok")
}
',
		selective_import_ambiguous_modules())
	assert output.contains('ambiguous selective import `hit`'), output
}

fn test_duplicate_selective_import_fails_even_with_local_homonym() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad_with_extra(v3_bin, 'ambiguous_local_homonym', 'module main

import a { hit }
import b { hit }

fn hit() int {
	return 3
}

fn main() {
	println(int_str(hit()))
}
',
		selective_import_ambiguous_modules())
	assert output.contains('ambiguous selective import `hit`'), output
}

fn test_duplicate_selective_import_function_value_reports_ambiguous() {
	v3_bin := selective_import_build_v3()
	output := selective_import_compile_bad_with_extra(v3_bin, 'ambiguous_fn_value', 'module main

import a { hit }
import b { hit }

fn main() {
	f := hit
	println(int_str(f()))
}
',
		selective_import_ambiguous_modules())
	assert output.contains('ambiguous selective import `hit`'), output
	assert !output.contains('unknown identifier `hit`'), output
}

fn test_local_function_keeps_priority_over_selective_import_homonym() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run(v3_bin, 'local_homonym', 'module main

import mymodules { add_xy }

fn add_xy(x int, y int) int {
	return x * y
}

fn main() {
	println(int_str(add_xy(2, 3)))
}
')
	assert output == '6'
	assert generated.contains('int add_xy(int x, int y)'), generated
	assert generated.contains('int__str(add_xy(2, 3))'), generated
	assert !generated.contains('int__str(mymodules__add_xy(2, 3))'), generated
}

fn test_selective_import_with_module_alias_keeps_symbol_authority() {
	v3_bin := selective_import_build_v3()
	output, generated := selective_import_compile_run(v3_bin, 'alias', 'module main

import mymodules as mm { add_xy }

fn main() {
	println(int_str(add_xy(4, 5)))
}
')
	assert output == '9'
	assert generated.contains('mymodules__add_xy(4, 5)'), generated
	assert !generated.contains('mm__add_xy'), generated
}
