import os

const export_attr_vexe = @VEXE
const export_attr_tests_dir = os.dir(@FILE)
const export_attr_v3_dir = os.dir(export_attr_tests_dir)
const export_attr_vlib_dir = os.dir(export_attr_v3_dir)
const export_attr_v3_src = os.join_path(export_attr_v3_dir, 'v3.v')

fn export_attr_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_export_attr_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${export_attr_vexe} -gc none -path "${export_attr_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${export_attr_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn export_attr_project(name string, files map[string]string) string {
	root := os.join_path(os.temp_dir(), 'v3_export_attr_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	mut rels := files.keys()
	rels.sort()
	for rel in rels {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, files[rel]) or { panic(err) }
	}
	return root
}

fn export_attr_compile(v3_bin string, main_file string, output string) os.Result {
	return os.execute('${v3_bin} ${main_file} -b c -o ${output}')
}

fn test_exported_imported_function_is_rooted_and_emitted_as_raw_symbol() {
	v3_bin := export_attr_build_v3()
	root := export_attr_project('imported_link', {
		'main.v':       'module main

import expmod

fn C.raw_exported_answer() int

fn take_callback(cb fn () int) int {
	return cb()
}

fn main() {
	println(C.raw_exported_answer().str())
	println(expmod.exported_answer().str())
	println(take_callback(expmod.exported_answer).str())
}
'
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
	compile := export_attr_compile(v3_bin, os.join_path(root, 'main.v'), bin_path)
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin_path)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '41\n41\n41', run.output

	c_code := os.read_file(bin_path + '.c') or { panic(err) }
	assert c_code.contains('int expmod__exported_answer(void) {'), c_code
	assert c_code.contains('int raw_exported_answer(void) {'), c_code
	assert c_code.contains('return expmod__exported_answer();'), c_code
	assert c_code.contains('raw_exported_answer()'), c_code
	assert c_code.contains('take_callback(expmod__exported_answer)'), c_code
	assert c_code.contains('expmod__helper_used('), c_code
	assert !c_code.contains('expmod__helper_unused('), c_code
}

fn test_duplicate_export_name_is_rejected() {
	v3_bin := export_attr_build_v3()
	root := export_attr_project('duplicate', {
		'main.v': "module main

@[export: 'raw_duplicate']
fn one() {}

@[export: 'raw_duplicate']
fn two() {}

fn main() {}
"
	})
	compile := export_attr_compile(v3_bin, os.join_path(root, 'main.v'), os.join_path(root, 'app'))
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('duplicate export name `raw_duplicate`'), compile.output
}

fn test_disabled_export_attr_does_not_register_raw_symbol() {
	v3_bin := export_attr_build_v3()
	root := export_attr_project('disabled_export', {
		'main.v': "module main

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
	println(enabled_export().str())
}
"
	})
	bin_path := os.join_path(root, 'app')
	compile := export_attr_compile(v3_bin, os.join_path(root, 'main.v'), bin_path)
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin_path)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '7', run.output

	c_code := os.read_file(bin_path + '.c') or { panic(err) }
	assert c_code.contains('int raw_enabled_export(void) {'), c_code
	assert !c_code.contains('raw_disabled_export'), c_code
}

fn test_export_name_collision_with_runtime_symbol_is_rejected() {
	v3_bin := export_attr_build_v3()
	root := export_attr_project('runtime_collision', {
		'main.v': "module main

@[export: 'v_free']
fn collides_with_runtime_free() {}

fn main() {}
"
	})
	compile := export_attr_compile(v3_bin, os.join_path(root, 'main.v'), os.join_path(root, 'app'))
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('export name `v_free`'), compile.output
}

fn test_export_name_collision_with_natural_symbol_is_rejected() {
	v3_bin := export_attr_build_v3()
	root := export_attr_project('natural_collision', {
		'main.v': "module main

@[export: 'natural_name']
fn natural_name() int {
	return 1
}

fn main() {}
"
	})
	compile := export_attr_compile(v3_bin, os.join_path(root, 'main.v'), os.join_path(root, 'app'))
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('export name `natural_name`'), compile.output
}

fn test_invalid_export_names_are_rejected() {
	v3_bin := export_attr_build_v3()
	root := export_attr_project('invalid_names', {
		'main.v': "module main

@[export: '1bad']
fn bad_digit() {}

@[export: 'for']
fn bad_keyword() {}

fn main() {}
"
	})
	compile := export_attr_compile(v3_bin, os.join_path(root, 'main.v'), os.join_path(root, 'app'))
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('invalid export name `1bad`'), compile.output
	assert compile.output.contains('invalid export name `for`'), compile.output
}

fn test_export_name_reserved_by_v3_c_preamble_is_rejected() {
	v3_bin := export_attr_build_v3()
	root := export_attr_project('preamble_reserved_name', {
		'main.v': "module main

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

@[export: 'Optional']
fn collides_with_runtime_optional() {}

fn main() {}
"
	})
	compile := export_attr_compile(v3_bin, os.join_path(root, 'main.v'), os.join_path(root, 'app'))
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('invalid export name `bool`'), compile.output
	assert compile.output.contains('invalid export name `string`'), compile.output
	assert compile.output.contains('invalid export name `voidptr`'), compile.output
	assert compile.output.contains('invalid export name `i8`'), compile.output
	assert compile.output.contains('invalid export name `true`'), compile.output
	assert compile.output.contains('invalid export name `Array`'), compile.output
	assert compile.output.contains('invalid export name `map`'), compile.output
	assert compile.output.contains('invalid export name `DenseArray`'), compile.output
	assert compile.output.contains('invalid export name `SortedMap`'), compile.output
	assert compile.output.contains('invalid export name `Optional`'), compile.output
}

fn test_generic_export_is_rejected_fail_closed() {
	v3_bin := export_attr_build_v3()
	root := export_attr_project('generic_export', {
		'main.v': "module main

@[export: 'raw_generic']
fn raw_generic[T](value T) T {
	return value
}

fn main() {}
"
	})
	compile := export_attr_compile(v3_bin, os.join_path(root, 'main.v'), os.join_path(root, 'app'))
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('generic function `raw_generic` cannot be exported'), compile.output
}

fn test_exported_function_must_name_params() {
	v3_bin := export_attr_build_v3()
	root := export_attr_project('unnamed_param', {
		'main.v': "module main

@[export: 'raw_unnamed']
fn unnamed(int) int {
	return 0
}

fn main() {}
"
	})
	compile := export_attr_compile(v3_bin, os.join_path(root, 'main.v'), os.join_path(root, 'app'))
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('must name all parameters'), compile.output
}
