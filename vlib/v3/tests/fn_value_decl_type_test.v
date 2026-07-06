import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fn_value_decl_type_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn run_good(v3_bin string, name string, src string) string {
	good_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed: ${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed: ${run.output}'
	return run.output.trim_space()
}

fn run_bad(v3_bin string, name string, src string) string {
	bad_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert compile.exit_code != 0, '${name}: compile unexpectedly succeeded: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: reached C compiler: ${compile.output}'
	return compile.output
}

fn gen_c(v3_bin string, name string, src string) string {
	src_path := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src_path, src) or { panic(err) }
	c_path := os.join_path(os.temp_dir(), 'v3_${name}.c')
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${c_path}')
	assert compile.exit_code == 0, '${name}: C output failed: ${compile.output}'
	assert os.exists(c_path), '${name}: missing generated C file ${c_path}'
	return os.read_file(c_path) or { panic(err) }
}

fn write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn write_project(name string, files map[string]string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	return root
}

fn run_project_good(v3_bin string, name string, files map[string]string) string {
	root := write_project(name, files)
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	main_path := os.join_path(root, 'main.v')
	compile := os.execute('${v3_bin} ${main_path} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed: ${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed: ${run.output}'
	return run.output.trim_space()
}

fn run_project_bad(v3_bin string, name string, files map[string]string) string {
	root := write_project(name, files)
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	main_path := os.join_path(root, 'main.v')
	compile := os.execute('${v3_bin} ${main_path} -b c -o ${bad_bin}')
	assert compile.exit_code != 0, '${name}: compile unexpectedly succeeded: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: reached C compiler: ${compile.output}'
	return compile.output
}

fn gen_project_c(v3_bin string, name string, files map[string]string) string {
	root := write_project(name, files)
	c_path := os.join_path(os.temp_dir(), 'v3_${name}.c')
	os.rm(c_path) or {}
	main_path := os.join_path(root, 'main.v')
	compile := os.execute('${v3_bin} ${main_path} -b c -o ${c_path}')
	assert compile.exit_code == 0, '${name}: C output failed: ${compile.output}'
	assert os.exists(c_path), '${name}: missing generated C file ${c_path}'
	return os.read_file(c_path) or { panic(err) }
}

fn imported_fn_value_project_files() map[string]string {
	return {
		'v.mod':           "Module { name: 'fn_value_imported' }\n"
		'worker/worker.v': "module worker

pub fn inc(i int) int {
	return i + 1
}

pub fn dec(i int) int {
	return i - 1
}

pub fn read_ok(i int) !int {
	if i < 0 {
		return error('negative')
	}
	return i + 40
}

pub fn read_other(i int) !int {
	return i + 50
}
"
		'main.v':          "module main

import worker

fn inc(i int) int {
	return i + 1000
}

struct S {
	inc int
}

fn read_from_local() !int {
	mut loader := worker.read_ok
	assert loader(2)! == 42
	loader = worker.read_other
	return loader(2)!
}

fn main() {
	mut f := worker.inc
	assert f(4) == 5
	f = worker.dec
	assert f(4) == 3
	assert inc(1) == 1001
	got := read_from_local() or { -1 }
	assert got == 52
	worker := S{
		inc: 7
	}
	local_f := worker.inc
	assert local_f == 7
	println('imported-ok')
}
"
	}
}

fn imported_fn_value_expected_shadow_project_files() map[string]string {
	return {
		'v.mod':           "Module { name: 'fn_value_expected_shadow' }\n"
		'worker/worker.v': 'module worker

pub fn inc(i int) int {
	return i + 1
}
'
		'main.v':          'module main

import worker

struct Holder {
	inc int
}

fn take(f fn (int) int) int {
	return f(4)
}

fn main() {
	worker := Holder{
		inc: 7
	}
	got := take(worker.inc)
	println(int_str(got))
}
'
	}
}

const optional_fn_src = "fn main() {
	f := fn (i int) ! {
		if i == 0 {
			return error('zero')
		}
		return
	}
	f(0) or {
		println('optional-ok')
		return
	}
	println('optional-bad')
}
"

const plain_fn_src = 'fn main() {
	f := fn (i int) int {
		return i + 2
	}
	println(int_str(f(5)))
	}
'

const local_ident_expected_shadow_src = 'fn foo() int {
	return 1
}

fn take(f fn () int) int {
	return f()
}

fn main() {
	foo := 10
	got := take(foo)
	println(int_str(got))
}
'

const assignment_expected_shadow_src = 'fn foo(i int) int {
	return i + 1
}

fn bar(i int) int {
	return i + 2
}

fn main() {
	mut f := bar
	foo := 10
	f = foo
}
'

const callback_arg_expected_shadow_src = 'fn foo(i int) int {
	return i + 1
}

fn call(f fn (int) int) int {
	return f(1)
}

fn main() {
	foo := 10
	call(foo)
}
'

const struct_field_expected_shadow_src = 'fn foo(i int) int {
	return i + 1
}

struct S {
	f fn (int) int
}

fn main() {
	foo := 10
	s := S{
		f: foo
	}
	_ = s
}
'

const local_ident_shadow_src = 'fn foo() int {
	return 1
}

fn main() {
	foo := 10
	f := foo
	println(int_str(f))
}
'

const local_fn_call_shadow_global_return_src = 'fn f() string {
	return "global"
}

fn make_int() int {
	return 12
}

fn main() {
	f := make_int
	x := f()
println(int_str(x))
}
'

const local_fn_value_call_return_shadow_src = "fn f() int {
	return 1
}

fn actual() string {
	return 'ok'
}

fn use(f fn () string) string {
	s := f()
	return s
}

fn main() {
println(use(actual))
}
"

const local_fn_value_ident_shadow_fn_src = 'fn foo(i int) int {
	return i + 100
}

fn other_callback(i int) int {
	return i + 2
}

fn main() {
	foo := other_callback
	f := foo
	println(int_str(f(5)))
}
'

const local_fn_value_expected_shadow_fn_src = 'fn foo(i int) int {
	return i + 100
}

fn other_callback(i int) int {
	return i + 2
}

fn take(f fn (int) int) int {
	return f(5)
}

fn main() {
	foo := other_callback
println(int_str(take(foo)))
}
'

fn test_local_fn_literal_decl_types_are_callable() {
	v3_bin := build_v3()
	assert run_good(v3_bin, 'fn_value_optional_void_local', optional_fn_src) == 'optional-ok'
	assert run_good(v3_bin, 'fn_value_plain_int_local', plain_fn_src) == '7'
	assert run_good(v3_bin, 'fn_value_local_ident_shadow', local_ident_shadow_src) == '10'
	assert run_good(v3_bin, 'fn_value_local_call_shadows_global_return',
		local_fn_call_shadow_global_return_src) == '12'
	assert run_good(v3_bin, 'fn_value_call_return_shadow', local_fn_value_call_return_shadow_src) == 'ok'
	assert run_good(v3_bin, 'fn_value_ident_shadowed_by_fn_typed_local',
		local_fn_value_ident_shadow_fn_src) == '7'
	assert run_good(v3_bin, 'fn_value_expected_shadowed_by_fn_typed_local',
		local_fn_value_expected_shadow_fn_src) == '7'
	assert run_good(v3_bin, 'fn_value_array_map_result_type', 'fn main() {
	i_to_str := fn (i int) string {
		return i.str()
	}
	a := [1, 2, 3].map(i_to_str)
	println(a == ["1", "2", "3"])
}
') == 'true'
	assert run_good(v3_bin, 'fn_value_named_local', 'fn add_one(i int) int {
	return i + 1
}

fn main() {
	f := add_one
	println(int_str(f(6)))
}
') == '7'
	assert run_good(v3_bin, 'fn_value_struct_field', 'struct S {
	f fn (int) int
}

fn main() {
	s := S{
		f: fn (i int) int {
			return i + 3
		}
	}
	println(int_str(s.f(4)))
}
') == '7'
	assert run_project_good(v3_bin, 'fn_value_imported_selector_local',
		imported_fn_value_project_files()) == 'imported-ok'
}

fn test_fn_value_expected_context_respects_value_shadowing() {
	v3_bin := build_v3()
	ident_output := run_bad(v3_bin, 'fn_value_expected_ident_shadow',
		local_ident_expected_shadow_src)
	assert !ident_output.contains('main__foo'), ident_output
	selector_output := run_project_bad(v3_bin, 'fn_value_expected_import_shadow',
		imported_fn_value_expected_shadow_project_files())
	assert !selector_output.contains('worker__inc'), selector_output
	assign_output := run_bad(v3_bin, 'fn_value_expected_assignment_shadow',
		assignment_expected_shadow_src)
	assert !assign_output.contains('main__foo'), assign_output
	callback_output := run_bad(v3_bin, 'fn_value_expected_callback_shadow',
		callback_arg_expected_shadow_src)
	assert !callback_output.contains('main__foo'), callback_output
	struct_output := run_bad(v3_bin, 'fn_value_expected_struct_field_shadow',
		struct_field_expected_shadow_src)
	assert !struct_output.contains('main__foo'), struct_output
}

fn test_local_fn_literal_decl_generates_fn_pointer_locals() {
	v3_bin := build_v3()
	optional_c := gen_c(v3_bin, 'fn_value_optional_void_local_c', optional_fn_src)
	assert !optional_c.contains('Optional f = __anon_fn'), optional_c
	assert !optional_c.contains('int f = __anon_fn'), optional_c
	assert optional_c.contains('typedef struct Optional (*_fn_ptr_'), optional_c
	assert optional_c.contains(' f = __anon_fn_'), optional_c

	plain_c := gen_c(v3_bin, 'fn_value_plain_int_local_c', plain_fn_src)
	assert !plain_c.contains('Optional f = __anon_fn'), plain_c
	assert !plain_c.contains('int f = __anon_fn'), plain_c
	assert plain_c.contains('typedef int (*_fn_ptr_'), plain_c
	assert plain_c.contains(' f = __anon_fn_'), plain_c

	shadow_c := gen_c(v3_bin, 'fn_value_local_ident_shadow_c', local_ident_shadow_src)
	assert shadow_c.contains('int foo = 10'), shadow_c
	assert shadow_c.contains('int f = foo'), shadow_c
	call_shadow_c := gen_c(v3_bin, 'fn_value_call_return_shadow_c',
		local_fn_value_call_return_shadow_src)
	assert call_shadow_c.contains('string s = f();'), call_shadow_c
	assert !call_shadow_c.contains('int s = f();'), call_shadow_c

	shadow_call_c := gen_c(v3_bin, 'fn_value_local_call_shadows_global_return_c',
		local_fn_call_shadow_global_return_src)
	assert shadow_call_c.contains('int x ='), shadow_call_c
	assert !shadow_call_c.contains('string x ='), shadow_call_c

	imported_c := gen_project_c(v3_bin, 'fn_value_imported_selector_local_c',
		imported_fn_value_project_files())
	assert !imported_c.contains('int f = worker__inc'), imported_c
	assert !imported_c.contains('int loader = worker__read_ok'), imported_c
	assert !imported_c.contains(' f = main__inc'), imported_c
	assert imported_c.contains('int local_f = worker.inc'), imported_c
	assert imported_c.contains('typedef int (*_fn_ptr_'), imported_c
	assert imported_c.contains(' f = worker__inc'), imported_c
	assert imported_c.contains('f = worker__dec'), imported_c
	assert imported_c.contains(' loader = worker__read_ok'), imported_c
	assert imported_c.contains('loader = worker__read_other'), imported_c
}
