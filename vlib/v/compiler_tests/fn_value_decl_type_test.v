import os
import v.tempname

fn fn_value_new_fixture_dir() string {
	root := os.join_path(os.vtmp_dir(), 'v3_fn_value_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

fn fn_value_compile_good(fixture_dir string, name string, src string) string {
	good_src := os.join_path(fixture_dir, 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(fixture_dir, 'v3_${name}')
	compile := os.exec([@VEXE, '${good_src}', '-b', 'c', '-o', good_bin])
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed: ${compile.output}'
	return good_bin
}

fn fn_value_run_good(fixture_dir string, name string, src string) string {
	good_bin := fn_value_compile_good(fixture_dir, name, src)
	run := os.exec([good_bin])
	assert run.exit_code == 0, '${name}: run failed: ${run.output}'
	return run.output.trim_space()
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

fn fn_value_compile_good_batch(fixture_dir string, name string, cases []FnValueRuntimeCase) string {
	mut files := map[string]string{}
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for case in cases {
		module_name := case.name
		imports << 'import ${module_name}'
		calls << "\t\t'${case.name}' { ${module_name}.exercise() }"
		source := 'module ${module_name}\n\n' + case.source
		files['${module_name}/fixture.v'] = source.replace_once('\nfn main()', '\npub fn exercise()')
	}
	files['main.v'] = "module main\n\nimport os\n${imports.join('\n')}\n\nfn main() {\n\tmatch os.args[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n"
	return fn_value_compile_good_project(fixture_dir, name, files)
}

fn fn_value_c_definition(generated string, function string) string {
	_, definition := generated.rsplit_once('\n${function}(') or {
		panic('missing generated function ${function}')
	}
	assert definition.all_before('\n').ends_with(') {'), function
	return definition.all_before('\n}')
}

fn fn_value_check_literal_pointer(generated string, module_name string, result_type string) {
	definition := fn_value_c_definition(generated, 'void ${module_name}__exercise')
	declaration, _ := definition.split_once(' f = ${module_name}____anon_fn_') or {
		panic('missing function literal assignment in ${module_name}\n${definition}')
	}
	pointer_type := declaration.all_after_last('\n').trim_space()
	assert pointer_type.starts_with('_fn_ptr_'), definition
	assert generated.contains('typedef ${result_type} (*${pointer_type})('), '${module_name}: ${pointer_type} must return ${result_type}'
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

const fn_value_callback_alias_src = 'module callbacks

pub type Callback[T] = fn (T) int
'

fn imported_fn_value_project_files() map[string]string {
	return {
		'v.mod':                 "Module { name: 'fn_value_imported' }\n"
		'callbacks/callbacks.v': fn_value_callback_alias_src
		'collision/collision.v': 'module collision

pub const foo = 7
'
		'worker/worker.v':       "module worker

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
		'main.v':                "module main

import callbacks
import collision
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

fn twice(value int) int {
	return value * 2
}

fn invoke(callback callbacks.Callback[int]) int {
	return callback(21)
}

fn foo() int {
	return 42
}

fn take(callback fn () int) int {
	return callback()
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
	println(int_str(invoke(twice)))
	println(int_str(take(foo) + collision.foo))
}
"
	}
}

fn fn_value_binding_rejection_project_files() map[string]string {
	return {
		'v.mod':                 "Module { name: 'fn_value_expected_shadow' }\n"
		'callbacks/callbacks.v': fn_value_callback_alias_src
		'worker/worker.v':       'module worker

pub fn inc(i int) int {
	return i + 1
}
'
		'main.v':                'module main

import callbacks
import worker

struct Holder {
	inc int
}

fn take(f fn (int) int) int {
	return f(4)
}

fn wrong(value string) int {
	return value.len
}

fn invoke(callback callbacks.Callback[int]) int {
	return callback(21)
}

fn main() {
	local_holder := Holder{
		inc: 7
	}
	got := take(local_holder.inc)
	println(int_str(got))
	_ := invoke(wrong)
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
	cases := [
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
		FnValueRuntimeCase{
			name:     'address_of_local_fn_value'
			source:   'struct Holder {
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
'
			expected: '7'
		},
		// Dereferencing ref needs the address of the stored function pointer.
		FnValueRuntimeCase{
			name:     'deref_of_address_of_local_fn_value'
			source:   'fn answer() int {
	return 42
}

fn main() {
	f := answer
	ref := &f
	deref := *ref
	println(deref())
}
'
			expected: '42'
		},
	]
	bin := fn_value_compile_good_batch(fixture_dir, 'fn_value_locals', cases)
	generated := os.read_file(bin + '.c') or { panic(err) }
	// Check the typedef used by each local declaration, since other cases emit their own.
	fn_value_check_literal_pointer(generated, 'fn_value_optional_void_local', 'struct __v_result')
	fn_value_check_literal_pointer(generated, 'fn_value_plain_int_local', 'i64')
	optional_callback := fn_value_c_definition(generated, 'void fn_value_optional_callback_param__accept_optional_callback')
	option_type := optional_callback.all_before(' callback)')
	assert option_type.starts_with('__v_option__fn_ptr_'), optional_callback
	assert generated.contains('typedef struct ${option_type} {'), option_type
	assert !generated.contains('__v_option_fn_ptr:'), generated
	shadow := fn_value_c_definition(generated, 'void fn_value_local_ident_shadow__exercise')
	assert shadow.contains('i64 foo = 10'), shadow
	assert shadow.contains('i64 f = foo'), shadow
	call_shadow := fn_value_c_definition(generated, 'string fn_value_call_return_shadow__use')
	assert call_shadow.contains('string s = f();'), call_shadow
	assert !call_shadow.contains('i64 s = f();'), call_shadow
	shadow_call := fn_value_c_definition(generated, 'void fn_value_local_call_shadows_global_return__exercise')
	assert shadow_call.contains('i64 x ='), shadow_call
	assert !shadow_call.contains('string x ='), shadow_call
	for case in cases {
		run := os.exec([bin, case.name])
		assert run.exit_code == 0, '${case.name}: run failed: ${run.output}'
		assert run.output.trim_space() == case.expected, '${case.name}: expected `${case.expected}` in\n${run.output}'
	}
}

fn test_main_fn_values_use_their_declared_storage() {
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	out := fn_value_run_good(fixture_dir, 'fn_value_main_runtime', 'fn accept(value int) int {
	return value + 1
}

fn consume(callback fn (int) int) int {
	return callback(41)
}

type Validator[T] = fn (value T) ?int

fn make_validator[T]() Validator[T] {
	return fn [T] (value T) ?int {
		return none
	}
}

const validate_int = make_validator[int]()

fn main() {
	callback := accept
	println(consume(callback))
	println(consume(accept))
	println(validate_int(123) == none)
}
')
	assert out.split_into_lines() == ['42', '42', 'true'], out
}

fn test_fn_value_rejects_invalid_bindings() {
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	cases := {
		'fn_value_expected_ident_shadow':         local_ident_expected_shadow_src
		'fn_value_expected_assignment_shadow':    assignment_expected_shadow_src
		'fn_value_expected_callback_shadow':      callback_arg_expected_shadow_src
		'fn_value_expected_struct_field_shadow':  struct_field_expected_shadow_src
		// Each call must check the binding from its own sibling block.
		'variadic_fn_value_shadowed_by_plain_fn': 'fn variadic(values ...int) int {
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
'
	}
	mut files := fn_value_binding_rejection_project_files()
	mut imports := []string{cap: cases.len}
	for name, source in cases {
		imports << 'import ${name} as _'
		files['${name}/${name}.v'] = 'module ${name}\n\n' + source
	}
	files['main.v'] = files['main.v'].replace_once('import worker', 'import worker\n' + imports.join('\n'))
	output := fn_value_run_project_bad(fixture_dir, 'fn_value_binding_rejections', files)
	assert !output.contains('main__foo'), output
	assert !output.contains('worker__inc'), output
	for name, _ in cases {
		assert !output.contains('${name}__foo'), output
	}
	mut diagnostics := map[string][]string{}
	for line in output.split_into_lines() {
		location, message := line.split_once(': error: ') or { continue }
		// Remove the line and column from the right so drive letters stay intact.
		file := location.all_before_last(':').all_before_last(':')
		diagnostics[os.file_name(file)] << message
	}
	expected := {
		'fn_value_expected_ident_shadow.v':        'cannot use `int` as `fn () int` in argument 1 to `fn_value_expected_ident_shadow.take`'
		'fn_value_expected_assignment_shadow.v':   'cannot assign to `f`: expected `fn (int) int`, not `int`'
		'fn_value_expected_callback_shadow.v':     'cannot use `int` as `fn (int) int` in argument 1 to `fn_value_expected_callback_shadow.call`'
		'fn_value_expected_struct_field_shadow.v': 'cannot assign to field `f`: expected `fn (int) int`, not `fn () int`'
	}
	for file, message in expected {
		assert message in diagnostics[file], '${file}: missing ${message}\n${output}'
	}
	main_diagnostics := diagnostics['main.v']
	assert 'cannot use `int` as `fn (int) int` in argument 1 to `take`' in main_diagnostics, output
	assert 'cannot use `fn (string) int` as `fn (int) int` in argument 1 to `invoke`' in main_diagnostics, output
	assert diagnostics['variadic_fn_value_shadowed_by_plain_fn.v'] == ['expected 1 argument, but got 2'], output
}

fn test_imported_fn_values_keep_callable_types() {
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	bin := fn_value_compile_good_project(fixture_dir, 'fn_value_imported_selector_local',
		imported_fn_value_project_files())
	imported_c := os.read_file(bin + '.c') or { panic(err) }
	assert !imported_c.contains('i64 f = worker__inc'), imported_c
	assert !imported_c.contains('i64 loader = worker__read_ok'), imported_c
	assert !imported_c.contains(' f = main__inc'), imported_c
	assert imported_c.contains('i64 local_f = local_worker.inc'), imported_c
	declaration, _ := imported_c.split_once(' f = worker__inc;') or {
		panic('missing local assignment of worker.inc')
	}
	pointer_type := declaration.all_after_last('\n').trim_space()
	assert pointer_type.starts_with('_fn_ptr_'), pointer_type
	assert imported_c.contains('typedef i64 (*${pointer_type})('), pointer_type
	assert imported_c.contains('f = worker__dec'), imported_c
	assert imported_c.contains(' loader = worker__read_ok'), imported_c
	assert imported_c.contains('loader = worker__read_other'), imported_c
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['imported-ok', '42', '49'], run.output
}

fn test_shadowed_fn_value_calls_use_bound_callable_types() {
	fixture_dir := fn_value_new_fixture_dir()
	defer { os.rmdir_all(fixture_dir) or {} }
	bin := fn_value_compile_good(fixture_dir, 'fn_value_shadowed_calls', 'fn f() ?int {
	return 99
}

fn take(value ?int) int {
	return value or { -1 }
}

fn record(value int) {
	println(int_str(value))
}

fn invoke(drop_owned fn (int), value int) {
	drop_owned(value)
}

fn main() {
	f := fn () int {
		return 7
	}
	println(int_str(take(f())))
	invoke(record, 23)
}
')
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('take((__v_option_i64){.ok = true, .value = f()})'), generated
	assert !generated.contains('take(f())'), generated
	assert generated.contains('drop_owned(value);'), generated
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['7', '23'], run.output
}
