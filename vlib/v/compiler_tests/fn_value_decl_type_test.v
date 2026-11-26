import os
import v.tempname

fn fn_value_new_fixture_dir() string {
	root := os.join_path(os.vtmp_dir(), 'v3_fn_value_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

fn fn_value_run_good(fixture_dir string, name string, src string) string {
	good_src := os.join_path(fixture_dir, 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(fixture_dir, 'v3_${name}')
	compile := os.exec([@VEXE, '${good_src}', '-b', 'c', '-o', good_bin])
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed: ${compile.output}'
	run := os.exec([good_bin])
	assert run.exit_code == 0, '${name}: run failed: ${run.output}'
	return run.output.trim_space()
}

fn fn_value_run_bad(fixture_dir string, name string, src string) string {
	bad_src := os.join_path(fixture_dir, 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(fixture_dir, 'v3_${name}')
	compile := os.exec([@VEXE, '${bad_src}', '-b', 'c', '-o', bad_bin])
	assert compile.exit_code != 0, '${name}: compile unexpectedly succeeded: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: reached C compiler: ${compile.output}'
	return compile.output
}

fn fn_value_gen_c(fixture_dir string, name string, src string) string {
	src_path := os.join_path(fixture_dir, 'v3_${name}.v')
	os.write_file(src_path, src) or { panic(err) }
	c_path := os.join_path(fixture_dir, 'v3_${name}.c')
	os.rm(c_path) or {}
	compile := os.exec([@VEXE, src_path, '-b', 'c', '-o', c_path])
	assert compile.exit_code == 0, '${name}: C output failed: ${compile.output}'
	assert os.exists(c_path), '${name}: missing generated C file ${c_path}'
	return os.read_file(c_path) or { panic(err) }
}

fn fn_value_write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn fn_value_write_project(fixture_dir string, name string, files map[string]string) string {
	root := os.join_path(fixture_dir, 'v3_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		fn_value_write_project_file(root, rel, src)
	}
	return root
}

fn fn_value_run_project_good(fixture_dir string, name string, files map[string]string) string {
	good_bin := fn_value_compile_good_project(fixture_dir, name, files)
	run := os.exec([good_bin])
	assert run.exit_code == 0, '${name}: run failed: ${run.output}'
	return run.output.trim_space()
}

fn fn_value_compile_good_project(fixture_dir string, name string, files map[string]string) string {
	root := fn_value_write_project(fixture_dir, name, files)
	good_bin := os.join_path(fixture_dir, 'v3_${name}')
	main_path := os.join_path(root, 'main.v')
	compile := os.exec([@VEXE, main_path, '-b', 'c', '-o', good_bin])
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed: ${compile.output}'
	return good_bin
}

struct FnValueRuntimeCase {
	name     string
	source   string
	expected string
}

fn fn_value_run_good_batch(fixture_dir string, name string, cases []FnValueRuntimeCase) {
	mut files := map[string]string{}
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for i, case in cases {
		module_name := 'fixture_${i}'
		imports << 'import ${module_name}'
		calls << "\t\t'${case.name}' { ${module_name}.exercise() }"
		source := 'module ${module_name}\n\n' + case.source
		files['${module_name}/${case.name}.v'] = source.replace_once('\nfn main()', '\npub fn exercise()')
	}
	files['main.v'] = "module main\n\nimport os\n${imports.join('\n')}\n\nfn main() {\n\tmatch os.args[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n"
	good_bin := fn_value_compile_good_project(fixture_dir, name, files)
	for case in cases {
		run := os.exec([good_bin, case.name])
		assert run.exit_code == 0, '${case.name}: run failed: ${run.output}'
		assert run.output.trim_space() == case.expected, '${case.name}: expected `${case.expected}` in\n${run.output}'
	}
}

fn fn_value_run_project_bad(fixture_dir string, name string, files map[string]string) string {
	root := fn_value_write_project(fixture_dir, name, files)
	bad_bin := os.join_path(fixture_dir, 'v3_${name}')
	main_path := os.join_path(root, 'main.v')
	compile := os.exec([@VEXE, '-b', 'c', '-nocolor', '-o', bad_bin, main_path])
	assert compile.exit_code != 0, '${name}: compile unexpectedly succeeded: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: reached C compiler: ${compile.output}'
	return compile.output
}

fn fn_value_check_shadow_rejections(fixture_dir string, cases map[string]string) {
	mut files := map[string]string{}
	mut imports := []string{cap: cases.len}
	for name, source in cases {
		imports << 'import ${name} as _'
		files['${name}/${name}.v'] = 'module ${name}\n\n' + source
	}
	files['main.v'] = 'module main\n\n${imports.join('\n')}\n\nfn main() {}\n'
	output := fn_value_run_project_bad(fixture_dir, 'fn_value_local_shadowing', files)
	assert !output.contains('main__foo'), output
	mut rejected_files := map[string]bool{}
	for line in output.split_into_lines() {
		location, _ := line.split_once(': error: ') or { continue }
		// Remove the line and column from the right so drive letters stay intact.
		file := location.all_before_last(':').all_before_last(':')
		rejected_files[os.file_name(file)] = true
	}
	for name, _ in cases {
		assert '${name}.v' in rejected_files, '${name}: expected a compiler error in\n${output}'
		assert !output.contains('${name}__foo'), output
	}
}

fn fn_value_gen_project_c(fixture_dir string, name string, files map[string]string) string {
	root := fn_value_write_project(fixture_dir, name, files)
	c_path := os.join_path(fixture_dir, 'v3_${name}.c')
	os.rm(c_path) or {}
	main_path := os.join_path(root, 'main.v')
	compile := os.exec([@VEXE, main_path, '-b', 'c', '-o', c_path])
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
	local_worker := S{
		inc: 7
	}
	local_f := local_worker.inc
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
	local_holder := Holder{
		inc: 7
	}
	got := take(local_holder.inc)
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

const optional_callback_param_src = 'fn accept_optional_callback(callback ?fn ()) {
	_ = callback
}

fn run_optional_callback_handler(handler fn (?fn ())) {
	handler(none)
}

fn accept_result_callback(callback !fn ()) {
	_ = callback
}

fn run_result_callback_handler(handler fn (!fn ())) {
	handler(fn () {})
}

fn main() {
	run_optional_callback_handler(accept_optional_callback)
	run_result_callback_handler(accept_result_callback)
	println("callback-ok")
}
'

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

const local_fn_value_optional_call_shadow_src = 'fn f() ?int {
	return 99
}

fn take(value ?int) int {
	return value or { -1 }
}

fn main() {
	f := fn () int {
		return 7
	}
	println(int_str(take(f())))
}
'

const drop_owned_callback_shadow_src = 'fn record(value int) {
	println(int_str(value))
}

fn invoke(drop_owned fn (int), value int) {
	drop_owned(value)
}

fn main() {
	invoke(record, 7)
}
'

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
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	fn_value_run_good_batch(fixture_dir, 'fn_value_locals', [
		FnValueRuntimeCase{
			name:     'fn_value_optional_void_local'
			source:   optional_fn_src
			expected: 'optional-ok'
		},
		FnValueRuntimeCase{
			name:     'fn_value_optional_callback_param'
			source:   optional_callback_param_src
			expected: 'callback-ok'
		},
		FnValueRuntimeCase{
			name:     'fn_value_plain_int_local'
			source:   plain_fn_src
			expected: '7'
		},
		FnValueRuntimeCase{
			name:     'fn_value_local_ident_shadow'
			source:   local_ident_shadow_src
			expected: '10'
		},
		FnValueRuntimeCase{
			name:     'fn_value_local_call_shadows_global_return'
			source:   local_fn_call_shadow_global_return_src
			expected: '12'
		},
		FnValueRuntimeCase{
			name:     'fn_value_call_return_shadow'
			source:   local_fn_value_call_return_shadow_src
			expected: 'ok'
		},
		FnValueRuntimeCase{
			name:     'fn_value_ident_shadowed_by_fn_typed_local'
			source:   local_fn_value_ident_shadow_fn_src
			expected: '7'
		},
		FnValueRuntimeCase{
			name:     'fn_value_expected_shadowed_by_fn_typed_local'
			source:   local_fn_value_expected_shadow_fn_src
			expected: '7'
		},
		FnValueRuntimeCase{
			name:     'fn_value_array_map_result_type'
			source:   'fn main() {
	i_to_str := fn (i int) string {
		return i.str()
	}
	a := [1, 2, 3].map(i_to_str)
	println(a == ["1", "2", "3"])
}
'
			expected: 'true'
		},
		FnValueRuntimeCase{
			name:     'fn_value_named_local'
			source:   'fn add_one(i int) int {
	return i + 1
}

fn main() {
	f := add_one
	println(int_str(f(6)))
}
'
			expected: '7'
		},
		FnValueRuntimeCase{
			name:     'fn_value_struct_field'
			source:   'struct S {
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
'
			expected: '7'
		},
	])
	assert fn_value_run_project_good(fixture_dir, 'fn_value_imported_selector_local',
		imported_fn_value_project_files()) == 'imported-ok'
}

fn test_imported_generic_fn_alias_keeps_parameters() {
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	out := fn_value_run_project_good(fixture_dir, 'fn_value_imported_generic_alias', {
		'v.mod':                 "Module { name: 'fn_value_imported_generic_alias' }\n"
		'callbacks/callbacks.v': 'module callbacks

pub type Callback[T] = fn (T) int
'
		'main.v':                'module main

import callbacks

fn twice(value int) int {
	return value * 2
}

fn invoke(callback callbacks.Callback[int]) int {
	return callback(21)
}

fn main() {
	println(int_str(invoke(twice)))
}
'
	})
	assert out == '42'
	bad_output := fn_value_run_project_bad(fixture_dir, 'fn_value_imported_generic_alias_mismatch', {
		'v.mod':                 "Module { name: 'fn_value_imported_generic_alias_mismatch' }\n"
		'callbacks/callbacks.v': 'module callbacks

pub type Callback[T] = fn (T) int
'
		'main.v':                'module main

import callbacks

fn wrong(value string) int {
	return value.len
}

fn invoke(callback callbacks.Callback[int]) int {
	return callback(21)
}

fn main() {
	_ := invoke(wrong)
}
'
	})
	assert bad_output.contains('cannot use `fn (string) int` as `fn (int) int` in argument 1 to `invoke`'), bad_output
}

fn test_resolved_fn_value_wins_over_imported_const_suffix() {
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	out := fn_value_run_project_good(fixture_dir, 'fn_value_imported_const_suffix', {
		'v.mod':                 "Module { name: 'fn_value_imported_const_suffix' }\n"
		'collision/collision.v': 'module collision

pub const foo = 7
'
		'main.v':                'module main

import collision

fn foo() int {
	return 42
}

fn take(callback fn () int) int {
	return callback()
}

fn main() {
	println(int_str(take(foo) + collision.foo))
}
'
	})
	assert out == '49'
}

fn test_main_runtime_shadowed_fn_values_use_declaration_symbol() {
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	out := fn_value_run_good(fixture_dir, 'fn_value_main_runtime_shadow_accept', 'fn accept(value int) int {
	return value + 1
}

fn consume(callback fn (int) int) int {
	return callback(41)
}

fn main() {
	callback := accept
	println(consume(callback))
	println(consume(accept))
}
')
	assert out == '42\n42'
}

fn test_address_of_local_fn_value_stores_the_callable_value() {
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	out := fn_value_run_good(fixture_dir, 'address_of_local_fn_value', 'struct Holder {
	f fn (int) int = unsafe { nil }
}

fn make_holder() Holder {
	offset := 4
	local := fn [offset] (value int) int {
		return value + offset
	}
	return Holder{
		f: &local
	}
}

fn main() {
	holder := make_holder()
	println(holder.f(3))
}
')
	assert out == '7'
}

fn test_pointer_to_local_fn_value_round_trips_through_a_deref() {
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	// The mirror image of the case above: here the pointer *is* the point, so the
	// `&` cannot be dropped - `*ref` would otherwise read the function's code as
	// if it were a stored function pointer.
	out := fn_value_run_good(fixture_dir, 'deref_of_address_of_local_fn_value', 'fn answer() int {
	return 42
}

fn main() {
	f := answer
	ref := &f
	deref := *ref
	println(deref())
}
')
	assert out == '42'
}

fn test_const_generic_fn_factory_value_call_uses_const_storage() {
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	out := fn_value_run_good(fixture_dir, 'const_generic_fn_factory_value_call', '
type Validator[T] = fn (value T) ?int

fn make_validator[T]() Validator[T] {
	return fn [T] (value T) ?int {
		return none
	}
}

const validate_int = make_validator[int]()

fn main() {
	println(validate_int(123) == none)
}
')
	assert out == 'true'
}

fn test_fn_value_expected_context_respects_value_shadowing() {
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	fn_value_check_shadow_rejections(fixture_dir, {
		'fn_value_expected_ident_shadow':        local_ident_expected_shadow_src
		'fn_value_expected_assignment_shadow':   assignment_expected_shadow_src
		'fn_value_expected_callback_shadow':     callback_arg_expected_shadow_src
		'fn_value_expected_struct_field_shadow': struct_field_expected_shadow_src
	})
	selector_output := fn_value_run_project_bad(fixture_dir, 'fn_value_expected_import_shadow',
		imported_fn_value_expected_shadow_project_files())
	assert !selector_output.contains('worker__inc'), selector_output
}

// A local cannot be redefined in a nested block, so the same name is bound in
// sibling blocks: each call must check against the binding of its own block.
fn test_variadic_fn_value_marker_uses_nearest_binding() {
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	output := fn_value_run_bad(fixture_dir, 'variadic_fn_value_shadowed_by_plain_fn', 'fn variadic(values ...int) int {
	return values.len
}

fn one(value int) int {
	return value
}

fn main() {
	{
		cb := variadic
		assert cb(1, 2) == 2
	}
	{
		cb := one
		_ := cb(1, 2)
	}
}
')
	assert output.contains('expected 1 argument, but got 2'), output
	assert output.count('error:') == 1, output
}

fn test_local_fn_literal_decl_generates_fn_pointer_locals() {
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	optional_c := fn_value_gen_c(fixture_dir, 'fn_value_optional_void_local_c', optional_fn_src)
	// `fn (i int) !` returns a result, so its fn pointer returns `__v_result`.
	assert !optional_c.contains('__v_option f = __anon_fn'), optional_c
	assert !optional_c.contains('__v_result f = __anon_fn'), optional_c
	assert !optional_c.contains('i64 f = __anon_fn'), optional_c
	assert optional_c.contains('typedef struct __v_result (*_fn_ptr_'), optional_c
	assert optional_c.contains(' f = __anon_fn_'), optional_c

	optional_callback_c := fn_value_gen_c(fixture_dir, 'fn_value_optional_callback_param_c',
		optional_callback_param_src)
	assert !optional_callback_c.contains('__v_option_fn_ptr:'), optional_callback_c
	assert optional_callback_c.contains('struct __v_option__fn_ptr_'), optional_callback_c

	plain_c := fn_value_gen_c(fixture_dir, 'fn_value_plain_int_local_c', plain_fn_src)
	assert !plain_c.contains('__v_option f = __anon_fn'), plain_c
	assert !plain_c.contains('i64 f = __anon_fn'), plain_c
	assert plain_c.contains('typedef i64 (*_fn_ptr_'), plain_c
	assert plain_c.contains(' f = __anon_fn_'), plain_c

	shadow_c := fn_value_gen_c(fixture_dir, 'fn_value_local_ident_shadow_c', local_ident_shadow_src)
	assert shadow_c.contains('i64 foo = 10'), shadow_c
	assert shadow_c.contains('i64 f = foo'), shadow_c
	call_shadow_c := fn_value_gen_c(fixture_dir, 'fn_value_call_return_shadow_c',
		local_fn_value_call_return_shadow_src)
	assert call_shadow_c.contains('string s = f();'), call_shadow_c
	assert !call_shadow_c.contains('i64 s = f();'), call_shadow_c

	shadow_call_c := fn_value_gen_c(fixture_dir, 'fn_value_local_call_shadows_global_return_c',
		local_fn_call_shadow_global_return_src)
	assert shadow_call_c.contains('i64 x ='), shadow_call_c
	assert !shadow_call_c.contains('string x ='), shadow_call_c

	imported_c := fn_value_gen_project_c(fixture_dir, 'fn_value_imported_selector_local_c',
		imported_fn_value_project_files())
	assert !imported_c.contains('i64 f = worker__inc'), imported_c
	assert !imported_c.contains('i64 loader = worker__read_ok'), imported_c
	assert !imported_c.contains(' f = main__inc'), imported_c
	assert imported_c.contains('i64 local_f = local_worker.inc'), imported_c
	assert imported_c.contains('typedef i64 (*_fn_ptr_'), imported_c
	assert imported_c.contains(' f = worker__inc'), imported_c
	assert imported_c.contains('f = worker__dec'), imported_c
	assert imported_c.contains(' loader = worker__read_ok'), imported_c
	assert imported_c.contains('loader = worker__read_other'), imported_c
}

fn test_shadowed_fn_value_calls_use_bound_callable_types() {
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	assert fn_value_run_good(fixture_dir, 'fn_value_optional_call_shadow',
		local_fn_value_optional_call_shadow_src) == '7'
	assert fn_value_run_good(fixture_dir, 'drop_owned_callback_shadow', drop_owned_callback_shadow_src) == '7'

	optional_c := fn_value_gen_c(fixture_dir, 'fn_value_optional_call_shadow_c',
		local_fn_value_optional_call_shadow_src)
	assert optional_c.contains('take((__v_option_i64){.ok = true, .value = f()})'), optional_c
	assert !optional_c.contains('take(f())'), optional_c

	drop_owned_c := fn_value_gen_c(fixture_dir, 'drop_owned_callback_shadow_c', drop_owned_callback_shadow_src)
	assert drop_owned_c.contains('drop_owned(value);'), drop_owned_c
}
