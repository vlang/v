import os
import v3.cmdexec
import v3.parser
import v3.pref
import v3.types

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn tmp_test_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
}

fn setup_v3_cache() {
	cache_dir := tmp_test_path('post_merge_review_fixes_cache')
	if os.getenv('V3CACHE') == cache_dir {
		return
	}
	os.rmdir_all(cache_dir) or {}
	os.rm(tmp_test_path('post_merge_review_fixes_test')) or {}
	os.setenv('V3CACHE', cache_dir, true)
}

fn build_v3() string {
	setup_v3_cache()
	v3_bin := tmp_test_path('post_merge_review_fixes_test')
	if os.is_executable(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn run_good(v3_bin string, name string, src string) string {
	return run_good_backend(v3_bin, name, 'c', src)
}

fn run_good_with_flags(v3_bin string, name string, flags string, src string) string {
	good_src := '${tmp_test_path(name)}.v'
	os.write_file(good_src, src) or { panic(err) }
	good_bin := tmp_test_path(name)
	compile := os.execute('${v3_bin} ${flags} ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: ${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: ${run.output}'
	return run.output.trim_space()
}

fn run_good_backend(v3_bin string, name string, backend string, src string) string {
	good_src := '${tmp_test_path(name)}.v'
	os.write_file(good_src, src) or { panic(err) }
	good_bin := tmp_test_path(name)
	compile := os.execute('${v3_bin} ${good_src} -b ${backend} -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: ${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: ${run.output}'
	return run.output.trim_space()
}

fn run_bad(v3_bin string, name string, src string, expected string) {
	bad_src := '${tmp_test_path(name)}.v'
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := tmp_test_path(name)
	compile := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert compile.exit_code != 0, '${name}: ${compile.output}'
	assert compile.output.contains(expected), '${name}: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: ${compile.output}'
}

fn check_good(name string, src string) {
	check_src := '${tmp_test_path(name)}.v'
	os.write_file(check_src, src) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(check_src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
}

fn gen_c(v3_bin string, name string, src string) string {
	src_path := '${tmp_test_path(name)}.v'
	os.write_file(src_path, src) or { panic(err) }
	c_path := '${tmp_test_path(name)}.c'
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${c_path}')
	assert compile.exit_code == 0, '${name}: ${compile.output}'
	assert os.exists(c_path)
	return os.read_file(c_path) or { panic(err) }
}

fn c_fn_body(c_source string, signature string) string {
	start := c_source.index(signature) or { return '' }
	open_rel := c_source[start..].index('{') or { return '' }
	body_start := start + open_rel
	mut depth := 0
	for i in body_start .. c_source.len {
		if c_source[i] == `{` {
			depth++
		} else if c_source[i] == `}` {
			depth--
			if depth == 0 {
				return c_source[start..i + 1]
			}
		}
	}
	return c_source[start..]
}

fn write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn run_good_project(v3_bin string, name string, files map[string]string, input string) string {
	return run_good_project_with_flags(v3_bin, name, '', files, input)
}

fn run_good_cached_project(v3_bin string, name string, files map[string]string, input string) string {
	root := '${tmp_test_path(name)}_project'
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	good_bin := tmp_test_path(name)
	compile := os.execute('${v3_bin} ${input_path} -o ${good_bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(good_bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

struct GoodProjectRun {
	run_output     string
	compile_output string
}

fn run_good_project_with_flags(v3_bin string, name string, flags string, files map[string]string, input string) string {
	return run_good_project_result(v3_bin, name, flags, files, input).run_output
}

fn run_good_project_result(v3_bin string, name string, flags string, files map[string]string, input string) GoodProjectRun {
	root := '${tmp_test_path(name)}_project'
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	good_bin := tmp_test_path(name)
	compile := os.execute('${v3_bin} ${flags} ${input_path} -b c -o ${good_bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(good_bin)
	assert run.exit_code == 0, run.output
	return GoodProjectRun{
		run_output:     run.output.trim_space()
		compile_output: compile.output
	}
}

fn run_good_project_relative_input(v3_bin string, name string, flags string, files map[string]string, input string) string {
	workspace := '${tmp_test_path(name)}_workspace'
	root := os.join_path(workspace, 'project')
	if os.exists(workspace) {
		os.rmdir_all(workspace) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	input_path := os.join_path('project', input)
	good_bin := tmp_test_path(name)
	compile :=
		os.execute('cd ${os.quoted_path(workspace)} && ${os.quoted_path(v3_bin)} ${flags} ${os.quoted_path(input_path)} -b c -o ${os.quoted_path(good_bin)}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(good_bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn run_bad_project(v3_bin string, name string, files map[string]string, inputs []string, expected string) {
	root := '${tmp_test_path(name)}_project'
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	mut input_paths := []string{cap: inputs.len}
	for input in inputs {
		input_paths << os.quoted_path(os.join_path(root, input))
	}
	bad_bin := tmp_test_path(name)
	compile := os.execute('${v3_bin} ${input_paths.join(' ')} -b c -o ${bad_bin}')
	assert compile.exit_code != 0, '${name}: ${compile.output}'
	assert compile.output.contains(expected), '${name}: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: ${compile.output}'
}

fn test_compiler_vexe_env_uses_running_executable() {
	v3_bin := build_v3()
	c_source := gen_c(v3_bin, 'compiler_vexe_env', 'fn main() {}')
	assert !c_source.contains('v3_vexe_target')
	assert !c_source.contains('fopen(v3_src')
	assert !c_source.contains('v3_checkout_vexe')
	assert !c_source.contains('v3_arg0')
	assert !c_source.contains('v3_src_real_result')
	assert c_source.contains('const char* v3_vexe = "')
	assert c_source.contains('_putenv_s("VEXE", v3_vexe);')
	assert c_source.contains('setenv("VEXE", v3_vexe, 1);')
}

fn test_c_bool_parameter_accepts_integer_argument() {
	check_good('c_bool_integer_argument', 'fn C.bool_probe(bool) int

fn main() {
	_ = C.bool_probe(0)
}
')
}

fn test_filelock_helpers_are_inlined_in_generated_c() {
	v3_bin := build_v3()
	c_source := gen_c(v3_bin, 'filelock_helpers_inline',
		'import os.filelock\n\nfn C.v_filelock_lock(i32, i32, i32, u64, u64) i32\nfn C.v_filelock_unlock(i32, u64, u64) i32\n\nfn main() {\n\t_ = filelock.LockMode.exclusive\n\t_ = C.v_filelock_lock(i32(-1), 1, 1, u64(0), u64(0))\n\t_ = C.v_filelock_unlock(i32(-1), u64(0), u64(0))\n}\n')
	assert !c_source.contains('filelock_helpers.h')
	assert c_source.contains('static inline int v_filelock_lock(')
	assert c_source.contains('static inline int v_filelock_unlock(')
	assert c_source.contains('#ifndef V_OS_FILELOCK_HELPERS_H')
	assert !c_source.contains('v_filelock_status')
	status_source := gen_c(v3_bin, 'filelock_custom_prefix_decl',
		'import os.filelock\n\nfn C.v_filelock_lock(i32, i32, i32, u64, u64) i32\nfn C.v_filelock_unlock(i32, u64, u64) i32\nfn C.v_filelock_status() int\n\nfn main() {\n\t_ = filelock.LockMode.exclusive\n\t_ = C.v_filelock_lock(i32(-1), 1, 1, u64(0), u64(0))\n\t_ = C.v_filelock_status()\n}\n')
	assert status_source.contains('int v_filelock_status(')
	out := run_good(v3_bin, 'filelock_user_names_not_helpers',
		'fn v_filelock_lock() int {\n\treturn 3\n}\n\nfn v_filelock_unlock() int {\n\treturn 4\n}\n\nfn main() {\n\tprintln(int_str(v_filelock_lock() + v_filelock_unlock()))\n}\n')
	assert out == '7'
}

fn test_imported_module_call_in_struct_default_has_no_receiver_arg() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'module_call_struct_default', {
		'v.mod':         "Module { name: 'module_call_struct_default' }\n"
		'myseed/seed.v': 'module myseed\n\npub fn next() int {\n\treturn 42\n}\n'
		'rng/rng.v':     'module rng\n\nimport myseed\n\npub struct Rng {\n\tvalue int = myseed.next()\n}\n\npub fn value() int {\n\tr := Rng{}\n\treturn r.value\n}\n'
		'main.v':        'module main\n\nimport rng\n\nfn main() {\n\tprintln(int_str(rng.value()))\n}\n'
	}, 'main.v')
	assert out == '42'
}

fn test_global_struct_defaults_follow_global_declaration_order() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'global_struct_default_declaration_order', 'struct State {
	value int = seed
}

__global (
	seed = 7
	state State
)

fn main() {
	println(int_str(state.value))
}
')
	assert out == '7'
	module_out := run_good_project(v3_bin, 'global_struct_default_owner_module', {
		'v.mod':               "Module { name: 'global_struct_default_owner_module' }\n"
		'defaults/defaults.v': 'module defaults\n\n__global base_value = 1\n\npub struct State {\npub:\n\tvalue int = base_value\n}\n\nfn init() {\n\tbase_value = 7\n}\n'
		'main.v':              'module main\n\nimport defaults\n\n__global state defaults.State\n\nfn main() {\n\tprintln(int_str(state.value))\n}\n'
	}, 'main.v')
	assert module_out == '7'
}

fn test_empty_fixed_array_of_function_arrays_resolves_element_type() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'empty_fixed_array_of_function_arrays', 'fn main() {
	callbacks := [2][]fn (){}
	println(int_str(callbacks[0].len))
	println(int_str(callbacks[1].len))
}
')
	assert out == '0\n0'
}

fn test_indexed_shift_assignments_guard_oversized_counts() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'indexed_shift_assign_oversized_counts', 'struct Counter {
mut:
	value int
}

fn next(mut calls Counter) int {
	calls.value++
	return 0
}

fn shift(mut calls Counter) u64 {
	calls.value++
	return 64
}

fn main() {
	mut calls := Counter{}
	mut left := [u64(1)]
	left[next(mut calls)] <<= shift(mut calls)
	println(int_str(calls.value))
	println(left[0].str())
	mut right := [u64(8)]
	right[next(mut calls)] >>= shift(mut calls)
	println(int_str(calls.value))
	println(right[0].str())
}
')
	assert out == '2\n0\n4\n0'
}

fn test_multi_return_assignment_requires_option_result_handling() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'unhandled_result_multi_decl_assign',
		"fn pair() !(int, string) {\n\treturn 3, 'ok'\n}\n\nfn main() {\n\ta, b := pair()\n\tprintln(int_str(a) + b)\n}\n",
		'requires `or {}`, `!`, or `?` handling')
	run_bad(v3_bin, 'unhandled_result_multi_assign',
		"fn pair() !(int, string) {\n\treturn 4, 'ok'\n}\n\nfn main() {\n\tmut a := 0\n\tmut b := ''\n\ta, b = pair()\n\tprintln(int_str(a) + b)\n}\n",
		'requires `or {}`, `!`, or `?` handling')
	out := run_good(v3_bin, 'handled_result_multi_decl_assign',
		"fn pair() !(int, string) {\n\treturn 5, 'ok'\n}\n\nfn main() {\n\ta, b := pair() or { panic(err) }\n\tprintln(int_str(a) + b)\n}\n")
	assert out == '5ok'
}

fn test_multi_assignment_checks_all_rhs_before_invalidating_smartcasts() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'multi_assign_smartcast_rhs', 'struct Foo {
	field int
}

struct Bar {}

type Value = Bar | Foo

fn replacement() Value {
	return Bar{}
}

fn main() {
	mut x := Value(Foo{
		field: 7
	})
	mut y := 0
	if x is Foo {
		x, y = replacement(), x.field
	}
	println(int_str(y))
}
')
	assert out == '7'
}

fn test_sum_type_rejects_pointer_variants() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'pointer_sum_variant', 'struct Foo {
	value int
}

type Item = &Foo | int

fn main() {}
',
		'sum type cannot hold a reference type')
	run_bad(v3_bin, 'pointer_alias_sum_variant', 'struct Foo {}

type FooPointer = &Foo
type Item = FooPointer | int

fn main() {}
',
		'sum type cannot hold a reference type')
	run_bad(v3_bin, 'is_pointer_value_variant_rejected', 'struct Foo {}

type Item = Foo | int

fn main() {
	item := Item(Foo{})
	if item is &Foo {
		println("wrong")
	}
}
',
		'`&Foo` is not a variant of sum type `Item`')
}

fn test_single_letter_enum_names_are_rejected() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'single_letter_enum_name', 'enum E {
	item
}

fn main() {}
',
		'single letter capital names are reserved for generic template types.')
}

fn test_nested_sum_is_check_evaluates_subject_once() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'nested_sum_is_subject_once', '__global calls int

struct Leaf {
	n int
}

type Inner = Leaf | int
type Outer = Inner | string

fn make_outer() Outer {
	calls++
	return Outer(Inner(Leaf{
		n: 7
	}))
}

fn main() {
	calls = 0
	if make_outer() is Leaf {
		println(int_str(calls))
	} else {
		println("missing")
	}
}
')
	assert out == '1'
}

fn test_is_check_treats_pointer_aliases_as_pointers() {
	v3_bin := build_v3()
	sum_out := run_good(v3_bin, 'is_pointer_alias_sum', 'struct Foo {
		value int
	}

struct Bar {}

type Value = Bar | Foo
type ValueRef = &Value

fn main() {
	mut value := Value(Foo{
		value: 7
	})
	r := ValueRef(&value)
	if r is Foo {
		println("foo")
	} else {
		println("other")
	}
}
')
	assert sum_out == 'foo'
	interface_out := run_good(v3_bin, 'is_pointer_alias_interface', 'interface Runner {
	run() int
}

struct Job {
	n int
}

fn (j Job) run() int {
	return j.n
}

type RunnerRef = &Runner

fn main() {
	mut runner := Runner(Job{
		n: 3
	})
	r := RunnerRef(&runner)
	if r is Job {
		println("job")
	} else {
		println("other")
	}
}
')
	assert interface_out == 'job'
}

fn test_interface_equality_includes_implicit_return_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_implicit_return_box', 'interface IValue {}

struct Value {
	n int
}

struct OtherValue {
	n int
}

fn make_value() IValue {
	return Value{
		n: 3
	}
}

fn make_assigned_value() IValue {
	mut value := IValue(Value{
		n: 3
	})
	value = OtherValue{
		n: 4
	}
	return value
}

fn same(value IValue) bool {
	return value == value
}

fn main() {
	println(same(make_value()).str())
	println(same(make_assigned_value()).str())
}
')
	assert out == 'true\ntrue'
}

fn test_interface_equality_includes_function_literal_return_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_function_literal_return_boxes', 'interface IValue {}

struct Value {
	n int
}

fn same(value IValue) bool {
	return value == value
}

fn use(make fn () IValue) IValue {
	return make()
}

fn main() {
	make := fn () IValue {
		return Value{
			n: 3
		}
	}
	println(same(make()).str())
	println(same(use(|| IValue(Value{
		n: 4
	}))).str())
}
')
	assert out == 'true\ntrue'
}

fn test_interface_equality_includes_container_literal_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_container_literal_box', 'interface IValue {}

struct Value {
	n int
}

fn same(values []IValue) bool {
	return values == values
}

fn main() {
	values := []IValue{Value{
		n: 3
	}}
	println(same(values).str())
}
')
	assert out == 'true'
}

fn test_interface_equality_includes_option_result_return_boxes() {
	v3_bin := build_v3()
	c_source := gen_c(v3_bin, 'interface_eq_option_result_return_boxes', 'interface IValue {}

struct OptionValue {
	n int
}

struct ResultValue {
	n int
}

fn make_option() ?IValue {
	return OptionValue{
		n: 3
	}
}

fn make_result() !IValue {
	return ResultValue{
		n: 4
	}
}

fn same(value IValue) bool {
	return value == value
}

fn main() {
	option_value := make_option() or { panic("missing option") }
	result_value := make_result() or { panic(err) }
	println(same(option_value).str())
	println(same(result_value).str())
}
')
	same_body := c_fn_body(c_source, 'bool same(IValue value) {')
	assert same_body.contains('OptionValue*'), same_body
	assert same_body.contains('ResultValue*'), same_body
}

fn test_interface_equality_includes_wrapped_option_result_boxes() {
	v3_bin := build_v3()
	c_source := gen_c(v3_bin, 'interface_eq_wrapped_option_result_boxes', 'interface IValue {}

struct OptionValue {
	n int
}

struct ResultValue {
	n int
}

fn make_option() ?OptionValue {
	return OptionValue{
		n: 3
	}
}

fn make_result() !ResultValue {
	return ResultValue{
		n: 4
	}
}

fn same(value IValue) bool {
	return value == value
}

fn main() {
	mut option_value := ?IValue(none)
	option_value = make_option()
	option_payload := option_value or { panic("missing option") }
	println(same(option_payload).str())
	result_payload := make_result() or { panic(err) }
	println(same(result_payload).str())
}
')
	same_body := c_fn_body(c_source, 'bool same(IValue value) {')
	assert same_body.contains('OptionValue*'), same_body
	assert same_body.contains('ResultValue*'), same_body
}

fn test_interface_equality_includes_multi_return_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_multi_return_box', 'interface IValue {}

struct Value {
	n int
}

fn same(value IValue) bool {
	return value == value
}

fn make_value() (IValue, int) {
	return Value{
		n: 3
	}, 7
}

fn main() {
	value, n := make_value()
	println(same(value).str())
	println(int_str(n))
}
')
	assert out == 'true\n7'
}

fn test_interface_equality_includes_multi_return_assignment_slot_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_multi_return_assignment_slot_box', 'interface IValue {}

struct Initial {}

struct Value {
	n int
}

fn same(value IValue) bool {
	return value == value
}

fn make_value() (Value, int) {
	return Value{
		n: 3
	}, 7
}

fn main() {
	mut value := IValue(Initial{})
	mut n := 0
	value, n = make_value()
	println(same(value).str())
	println(int_str(n))
}
')
	assert out == 'true\n7'
}

fn test_interface_equality_includes_forwarded_multi_return_slot_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_forwarded_multi_return_slot_box', 'interface IValue {}

struct Value {
	n int
}

fn make_value() (Value, int) {
	return Value{
		n: 3
	}, 7
}

fn forward_value() (IValue, int) {
	return make_value()
}

fn same(value IValue) bool {
	return value == value
}

fn main() {
	value, n := forward_value()
	println(same(value).str())
	println(int_str(n))
}
')
	assert out == 'true\n7'
}

fn test_interface_equality_includes_forwarded_multi_return_container_slot_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_forwarded_multi_return_container_slot_box', 'interface IValue {}

struct ArrayValue {
	n int
}

struct MapValue {
	n int
}

struct FixedValue {
	n int
}

struct FixedDynamicValue {
	n int
}

fn same(value IValue) bool {
	return value == value
}

fn make_values() ([]ArrayValue, map[string]MapValue, [1]FixedValue, int) {
	return [ArrayValue{
		n: 3
	}], {
		"item": MapValue{
			n: 5
		}
	}, [FixedValue{
		n: 11
	}]!, 7
}

fn forward_values() ([]IValue, map[string]IValue, [1]IValue, int) {
	return make_values()
}

fn make_fixed_dynamic() ([1]FixedDynamicValue, int) {
	return [FixedDynamicValue{
		n: 13
	}]!, 17
}

fn forward_fixed_dynamic() ([]IValue, int) {
	return make_fixed_dynamic()
}

fn main() {
	values, indexed, fixed, n := forward_values()
	fixed_dynamic, fixed_dynamic_n := forward_fixed_dynamic()
	println(same(values[0]).str())
	println(same(indexed["item"]).str())
	println(same(fixed[0]).str())
	println(int_str(n))
	println(same(fixed_dynamic[0]).str())
	println(int_str(fixed_dynamic_n))
}
')
	assert out == 'true\ntrue\ntrue\n7\ntrue\n17'
}

fn test_forwarded_multi_return_container_slots_are_converted() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'forwarded_multi_return_container_slots', 'interface IValue {
	get() int
}

struct Value {
	n int
}

fn (value Value) get() int {
	return value.n
}

fn make_values() ([]Value, map[string]Value, int) {
	return [Value{
		n: 3
	}], {
		"item": Value{
			n: 5
		}
	}, 7
}

fn forward_values() ([]IValue, map[string]IValue, int) {
	return make_values()
}

fn make_fixed() ([1]Value, int) {
	return [Value{
		n: 11
	}]!, 13
}

fn forward_fixed() ([1]IValue, int) {
	return make_fixed()
}

fn main() {
	values, indexed, n := forward_values()
	fixed, fixed_n := forward_fixed()
	println(int_str(values[0].get()))
	println(int_str(indexed["item"].get()))
	println(int_str(n))
	println(int_str(fixed[0].get()))
	println(int_str(fixed_n))
}
')
	assert out == '3\n5\n7\n11\n13'
}

fn test_forwarded_multi_return_option_result_payloads_are_converted() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'forwarded_multi_return_option_result_payloads', 'interface IValue {
	get() int
}

struct Value {
	n int
}

fn (value Value) get() int {
	return value.n
}

fn make_option() (?Value, int) {
	return Value{
		n: 3
	}, 5
}

fn forward_option() (?IValue, int) {
	return make_option()
}

fn make_result() (!Value, int) {
	return Value{
		n: 7
	}, 11
}

fn forward_result() (!IValue, int) {
	return make_result()
}

fn main() {
	option_value, option_n := forward_option()
	option_payload := option_value or { panic("missing option") }
	println(int_str(option_payload.get()))
	println(int_str(option_n))
	result_value, result_n := forward_result()
	result_payload := result_value or { panic(err) }
	println(int_str(result_payload.get()))
	println(int_str(result_n))
}
')
	assert out == '3\n5\n7\n11'
}

fn test_forwarded_wrapped_multi_return_slots_are_converted() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'forwarded_wrapped_multi_return_slots', 'interface IValue {
	get() int
}

struct OptionValue {
	n int
}

struct ResultValue {
	n int
}

fn (value OptionValue) get() int {
	return value.n
}

fn (value ResultValue) get() int {
	return value.n
}

fn make_option() ?(OptionValue, int) {
	return OptionValue{
		n: 3
	}, 5
}

fn forward_option() ?(IValue, int) {
	return make_option()
}

fn make_result() !(ResultValue, int) {
	return ResultValue{
		n: 7
	}, 11
}

fn forward_result() !(IValue, int) {
	return make_result()
}

fn main() {
	option_value, option_n := forward_option() or { panic("missing option") }
	println(int_str(option_value.get()))
	println(int_str(option_n))
	result_value, result_n := forward_result() or { panic(err) }
	println(int_str(result_value.get()))
	println(int_str(result_n))
}
')
	assert out == '3\n5\n7\n11'
}

fn test_interface_equality_includes_appended_element_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_appended_element_box', 'interface IValue {}

struct Value {
	n int
}

fn same(value IValue) bool {
	return value == value
}

fn main() {
	mut values := []IValue{}
	values << Value{
		n: 3
	}
	println(same(values[0]).str())
}
')
	assert out == 'true'
}

fn test_interface_equality_includes_channel_send_and_default_field_boxes() {
	v3_bin := build_v3()
	channel_out := run_good(v3_bin, 'interface_eq_channel_send_box', 'interface IValue {}

struct Value {
	n int
}

fn same(value IValue) bool {
	return value == value
}

fn main() {
	ch := chan IValue{cap: 1}
	ch <- Value{
		n: 3
	}
	value := <-ch
	println(same(value).str())
}
')
	assert channel_out == 'true'
	default_out := run_good(v3_bin, 'interface_eq_default_field_box', 'interface IValue {}

struct Value {
	n int
}

struct Holder {
	value IValue = Value{
		n: 3
	}
}

fn same(value IValue) bool {
	return value == value
}

fn main() {
	holder := Holder{}
	println(same(holder.value).str())
}
')
	assert default_out == 'true'
}

fn test_interface_equality_includes_struct_field_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_struct_field_box', 'interface IValue {}

struct Value {
	n int
}

struct Holder {
	value IValue
}

fn same(value IValue) bool {
	return value == value
}

fn main() {
	holder := Holder{
		value: Value{
			n: 3
		}
	}
	println(same(holder.value).str())
}
')
	assert out == 'true'
}

fn test_interface_equality_includes_or_fallback_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_or_fallback_box', 'interface IValue {}

struct Value {
	n int
}

fn same(value IValue) bool {
	return value == value
}

fn maybe_value() ?IValue {
	return none
}

fn main() {
	value := maybe_value() or {
		Value{
			n: 3
		}
	}
	println(same(value).str())
}
')
	assert out == 'true'
}

fn test_interface_equality_includes_or_success_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_or_success_box', 'interface IValue {}

struct Value {
	n int
}

fn same(value IValue) bool {
	return value == value
}

fn maybe_value() ?Value {
	return Value{
		n: 3
	}
}

fn make_value() IValue {
	return maybe_value() or { panic("missing value") }
}

fn main() {
	println(same(make_value()).str())
}
')
	assert out == 'true'
}

fn test_interface_auto_str_preludes_stay_inside_tag_guards() {
	v3_bin := build_v3()
	source := 'interface IValue {}

struct Wide {
	a string
	b string
}

struct Narrow {
	n int
}

fn render(value IValue) string {
	return "\${value}"
}

fn main() {
	wide := IValue(Wide{
		a: "a"
		b: "b"
	})
	narrow := IValue(Narrow{
		n: 7
	})
	println(render(narrow))
	println(render(wide))
}
'
	c_source := gen_c(v3_bin, 'interface_auto_str_guarded_preludes', source)
	render_body := c_fn_body(c_source, 'string render(IValue value) {')
	assert render_body.len > 0, c_source
	first_tag_guard := render_body.index('._typ') or { -1 }
	first_object_read := render_body.index('._object') or { -1 }
	assert first_tag_guard >= 0, render_body
	assert first_object_read > first_tag_guard, render_body
	out := run_good(v3_bin, 'interface_auto_str_guarded_preludes_run', source)
	assert out.contains('Narrow'), out
	assert out.contains('7'), out
	assert out.contains('Wide'), out
}

fn test_interface_equality_preludes_stay_inside_tag_guards() {
	v3_bin := build_v3()
	source := 'interface IValue {}

struct WithArray {
	values []int
}

struct Other {
	n int
}

fn same(left IValue, right IValue) bool {
	return left == right
}

fn main() {
	array_value := IValue(WithArray{
		values: [1, 2]
	})
	println(same(array_value, array_value).str())
	other := IValue(Other{
		n: 7
	})
	println(same(other, other).str())
}
'
	c_source := gen_c(v3_bin, 'interface_equality_guarded_preludes', source)
	same_body := c_fn_body(c_source, 'bool same(IValue left, IValue right) {')
	guard_pos := same_body.index('if (') or { -1 }
	array_cast_pos := same_body.index('WithArray*') or { -1 }
	assert guard_pos >= 0, same_body
	assert array_cast_pos > guard_pos, same_body
	out := run_good(v3_bin, 'interface_equality_guarded_preludes_run', source)
	assert out == 'true\ntrue'
}

fn test_ierror_aggregate_equality_preserves_message_and_code() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'ierror_aggregate_equality_message_code', 'struct Box {
	err IError
}

fn main() {
	first := Box{
		err: error_with_code("same", 1)
	}
	different_message := Box{
		err: error_with_code("different", 1)
	}
	different_code := Box{
		err: error_with_code("same", 2)
	}
	equal := Box{
		err: error_with_code("same", 1)
	}
	println((first == different_message).str())
	println((first == different_code).str())
	println((first == equal).str())
}
')
	assert out == 'false\nfalse\ntrue'
}

fn test_ierror_aggregate_equality_roots_custom_dispatch_methods() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'ierror_aggregate_equality_custom_dispatch_roots', 'struct CustomError {
	message string
	n       int
}

fn (err CustomError) msg() string {
	return err.message
}

fn (err CustomError) code() int {
	return err.n
}

struct Box {
	err IError
}

fn main() {
	first := Box{
		err: CustomError{
			message: "first"
			n: 1
		}
	}
	different := Box{
		err: CustomError{
			message: "different"
			n: 2
		}
	}
	equal := Box{
		err: CustomError{
			message: "first"
			n: 1
		}
	}
	println((first == different).str())
	println((first == equal).str())
}
')
	assert out == 'false\ntrue'
}

fn test_interface_equality_includes_receiver_method_call_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_receiver_method_call_box', 'interface IValue {}

struct Value {
	n int
}

struct Comparator {}

fn (c Comparator) same(value IValue) bool {
	_ = c
	return value == value
}

fn main() {
	comparator := Comparator{}
	println(comparator.same(Value{
		n: 3
	}).str())
}
')
	assert out == 'true'
}

fn test_interface_equality_includes_veb_handler_call_boxes() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'interface_eq_veb_handler_call_box', {
		'v.mod':     "Module { name: 'interface_eq_veb_handler_call_box' }\n"
		'veb/veb.v': 'module veb\n\npub struct Context {}\n\npub struct Result {}\n'
		'main.v':    'import veb

interface IValue {}

struct Value {
	n int
}

pub struct Context {
	veb.Context
}

pub struct App {}

fn same(value IValue) bool {
	return value == value
}

pub fn (app &App) handler(value IValue) veb.Result {
	_ = app
	println(same(value).str())
	return veb.Result{}
}

pub fn (app &App) index() veb.Result {
	app.handler(Value{
		n: 3
	})
	return veb.Result{}
}

fn main() {
	mut app := &App{}
	mut ctx := Context{}
	_ = app.index(mut ctx)
}
'
	}, 'main.v')
	assert out == 'true'
}

fn test_interface_equality_includes_variadic_call_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_variadic_call_boxes', 'interface IValue {}

struct Single {}
struct First {}
struct Second {}

fn same(value IValue) bool {
	return value == value
}

fn all_same(values ...IValue) bool {
	mut ok := true
	for value in values {
		ok = ok && same(value)
	}
	return ok
}

fn main() {
	println(all_same(Single{}).str())
	println(all_same(First{}, Second{}).str())
}
')
	assert out == 'true\ntrue'
}

fn test_interface_equality_includes_variadic_struct_call_field_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_variadic_struct_call_field_boxes', 'interface IValue {}

struct Value {
	n int
}

struct Holder {
	value IValue
	n     int
}

fn same(value IValue) bool {
	return value == value
}

fn sink(items ...Holder) bool {
	return items.len == 1 && items[0].n == 7 && same(items[0].value)
}

fn main() {
	println(sink(value: Value{
		n: 3
	}, n: 7).str())
}
')
	assert out == 'true'
}

fn test_interface_equality_includes_params_call_field_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_params_call_field_boxes', 'interface IValue {}

struct Value {
	n int
}

@[params]
struct SinkConfig {
	value IValue
	n     int
}

fn same(value IValue) bool {
	return value == value
}

fn sink(config SinkConfig) bool {
	return config.n == 7 && same(config.value)
}

fn main() {
	println(sink(value: Value{
		n: 3
	}, n: 7).str())
}
')
	assert out == 'true'
}

fn test_interface_equality_includes_regular_struct_call_field_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_regular_struct_call_field_boxes', 'interface IValue {}

struct Value {
	n int
}

struct Holder {
	value IValue
	n     int
}

fn same(value IValue) bool {
	return value == value
}

fn sink(holder Holder) bool {
	return holder.n == 7 && same(holder.value)
}

fn main() {
	println(sink(value: Value{
		n: 3
	}, n: 7).str())
}
')
	assert out == 'true'
}

fn test_interface_equality_includes_omitted_params_default_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_omitted_params_default_box', 'interface IValue {}

struct Value {
	n int
}

@[params]
struct SinkConfig {
	value IValue = Value{
		n: 3
	}
	n int
}

fn same(value IValue) bool {
	return value == value
}

fn sink(config SinkConfig) bool {
	return same(config.value)
}

fn main() {
	println(sink().str())
	println(sink(n: 7).str())
}
')
	assert out == 'true\ntrue'
}

fn test_empty_interface_equality_does_not_accept_unregistered_payloads() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'empty_interface_unregistered_payload_equality', 'interface Any {}

fn same(left Any, right Any) bool {
	return left == right
}

fn main() {
	println(same(Any([1]), Any([2])).str())
	println(same(Any(1), Any(1)).str())
	println(same(Any{}, Any{}).str())
}
')
	assert out == 'false\ntrue\ntrue'
}

fn test_select_receive_assignment_checks_lhs_type() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'select_receive_assign_bool_mismatch', 'fn main() {
	ch := chan int{}
	mut value := false
	select {
		value = <-ch {}
		else {}
	}
	println(value.str())
}
',
		'cannot assign `int` to `bool`')
	run_bad(v3_bin, 'select_receive_assign_string_mismatch',
		"fn main() {\n\tch := chan int{}\n\tmut value := ''\n\tselect {\n\t\tvalue = <-ch {}\n\t\telse {}\n\t}\n\tprintln(value)\n}\n",
		'cannot assign `int` to `string`')
}

fn test_select_receive_assignment_applies_destination_conversions() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'select_receive_assign_conversions', 'interface IValue {
	get() int
}

struct Value {
	n int
}

struct Initial {}

type Item = Value | int
type MaybeValue = ?Value
type InterfaceAlias = IValue
type NestedInterfaceAlias = InterfaceAlias
type ItemAlias = Item
type NestedItemAlias = ItemAlias
type ValueAlias = Value

fn (value Value) get() int {
	return value.n
}

fn (initial Initial) get() int {
	_ = initial
	return -1
}

fn interface_n(value IValue) int {
	return value.get()
}

fn sum_n(item Item) int {
	if item is Value {
		return item.n
	}
	return -1
}

fn option_n(value ?Value) int {
	unwrapped := value or { return -1 }
	return unwrapped.n
}

fn main() {
	interface_ch := chan Value{cap: 1}
	interface_ch <- Value{
		n: 3
	}
	mut interface_value := IValue(Initial{})
	select {
		interface_value = <-interface_ch {}
	}

	sum_ch := chan Value{cap: 1}
	sum_ch <- Value{
		n: 5
	}
	mut sum_value := Item(0)
	select {
		sum_value = <-sum_ch {}
	}

	option_ch := chan Value{cap: 1}
	option_ch <- Value{
		n: 7
	}
	mut option_value := ?Value(none)
	select {
		option_value = <-option_ch {}
	}

	aliased_option_ch := chan Value{cap: 1}
	aliased_option_ch <- Value{
		n: 9
	}
	mut aliased_option_value := ?MaybeValue(none)
	select {
		aliased_option_value = <-aliased_option_ch {}
	}

	aliased_interface_ch := chan ValueAlias{cap: 1}
	aliased_interface_ch <- ValueAlias{
		n: 11
	}
	mut aliased_interface_value := NestedInterfaceAlias(Initial{})
	select {
		aliased_interface_value = <-aliased_interface_ch {}
	}

	aliased_sum_ch := chan ValueAlias{cap: 1}
	aliased_sum_ch <- ValueAlias{
		n: 13
	}
	mut aliased_sum_value := NestedItemAlias(0)
	select {
		aliased_sum_value = <-aliased_sum_ch {}
	}

	interface_source_ch := chan InterfaceAlias{cap: 1}
	interface_source_ch <- Value{
		n: 15
	}
	mut interface_source_value := IValue(Initial{})
	select {
		interface_source_value = <-interface_source_ch {}
	}

	println(int_str(interface_n(interface_value)))
	println(int_str(sum_n(sum_value)))
	println(int_str(option_n(option_value)))
	println(int_str(option_n(aliased_option_value)))
	println(int_str(interface_n(aliased_interface_value)))
	println(int_str(sum_n(aliased_sum_value)))
	println(int_str(interface_n(interface_source_value)))
}
')
	assert out == '3\n5\n7\n9\n11\n13\n15'
}

fn test_select_receive_assignment_converts_container_elements() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'select_receive_assign_container_conversions', "interface IValue {
	get() int
}

struct Value {
	n int
}

fn (value Value) get() int {
	return value.n
}

fn main() {
	array_ch := chan []Value{cap: 1}
	array_ch <- [Value{
		n: 3
	}]
	mut values := []IValue{}
	select {
		values = <-array_ch {}
	}

	map_ch := chan map[string]Value{cap: 1}
	map_ch <- {
		'item': Value{
			n: 5
		}
	}
	mut indexed := map[string]IValue{}
	select {
		indexed = <-map_ch {}
	}

	println(int_str(values[0].get()))
	println(int_str(indexed['item'].get()))
}
")
	assert out == '3\n5'
}

fn test_select_dereferences_pointer_channels() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'select_pointer_channels', 'fn receive(ch &chan int) int {
	select {
		value := <-ch {
			return value
		}
	}
	return -1
}

fn send(ch &chan int, value int) {
	select {
		ch <- value {}
	}
}

fn main() {
	ch := chan int{cap: 1}
	ch <- 3
	println(int_str(receive(&ch)))
	send(&ch, 5)
	println(int_str(<-ch))
}
')
	assert out == '3\n5'
}

fn test_select_receive_assignment_reboxes_option_result_payloads() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'select_receive_reboxes_option_result_payloads', 'interface IValue {
	get() int
}

struct Value {
	n int
}

fn (value Value) get() int {
	return value.n
}

fn make_option() ?Value {
	return Value{
		n: 3
	}
}

fn make_result() !Value {
	return Value{
		n: 7
	}
}

fn initial_result() !IValue {
	return error("initial")
}

fn main() {
	option_ch := chan ?Value{cap: 1}
	option_ch <- make_option()
	mut option_value := ?IValue(none)
	select {
		option_value = <-option_ch {}
	}
	option_payload := option_value or { panic("missing option") }
	println(int_str(option_payload.get()))

	result_ch := chan Value{cap: 1}
	result_ch <- Value{
		n: 7
	}
	mut result_value := initial_result()
	select {
		result_value = <-result_ch {}
	}
	result_payload := result_value or { panic(err) }
	println(int_str(result_payload.get()))
}
')
	assert out == '3\n7'
}

fn test_interface_equality_includes_select_receive_assignment_boxes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_eq_select_receive_assignment_box', 'interface IValue {}

struct Initial {}

struct Value {
	n int
}

fn same(value IValue) bool {
	return value == value
}

fn main() {
	ch := chan Value{cap: 1}
	ch <- Value{
		n: 3
	}
	mut value := IValue(Initial{})
	select {
		value = <-ch {}
	}
	println(same(value).str())
}
')
	assert out == 'true'
}

fn test_select_lowering_roots_array_free() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'select_roots_array_free', 'fn main() {
	select {
		else {}
	}
}
')
	assert out == ''
}

fn test_select_compound_receive_assignment_is_rejected() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'select_compound_receive_assign', 'fn main() {
	ch := chan int{}
	mut value := 1
	select {
		value += <-ch {}
		else {}
	}
	println(int_str(value))
}
',
		'compound receive assignment `+=` is not supported in `select`')
}

fn test_select_assignment_cases_require_receive_rhs() {
	v3_bin := build_v3()
	for op in [':=', '=', '+='] {
		run_bad(v3_bin, 'select_non_receive_${op.replace('=', 'eq').replace(':', 'decl').replace('+',
			'plus')}', 'fn main() {
	mut value := 0
	select {
		value ${op} 1 {}
	}
	println(int_str(value))
}
',
			'select assignment case requires a channel receive on the right side')
	}
}

fn test_select_rejects_else_and_timeout_in_either_order() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'select_else_before_timeout', 'import time

fn main() {
	select {
		else {}
		10 * time.millisecond {}
	}
}
',
		'`else` and timeout value are mutually exclusive `select` keys')
	run_bad(v3_bin, 'select_timeout_before_else', 'import time

fn main() {
	select {
		10 * time.millisecond {}
		else {}
	}
}
',
		'`else` and timeout value are mutually exclusive `select` keys')
}

fn test_select_rejects_duplicate_timeouts() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'select_duplicate_timeouts', 'import time

fn main() {
	select {
		10 * time.millisecond {}
		20 * time.millisecond {}
	}
}
',
		'at most one timeout branch allowed in `select` block')
}

fn test_select_timeout_only_waits_and_runs_branch() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'select_timeout_only', 'import time

fn main() {
	mut fired := false
	select {
		time.millisecond {
			fired = true
		}
	}
	println(fired.str())
}
')
	assert out == 'true'
}

fn test_select_receive_declaration_requires_identifier() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'select_receive_decl_index_lhs', 'fn main() {
	ch := chan int{}
	mut values := [0]
	select {
		values[0] := <-ch {}
		else {}
	}
}
',
		'select receive declaration requires a plain identifier on the left side')
}

fn test_comptime_if_threads_expression_is_deferred() {
	v3_bin := build_v3()
	without_spawn := run_good(v3_bin, 'comptime_threads_expr_without_spawn', 'fn main() {
	value := $if threads { 41 } $else { 7 }
	println(int_str(value))
}
')
	assert without_spawn == '7'
	with_spawn := run_good(v3_bin, 'comptime_threads_expr_with_spawn', 'fn work() {}

fn main() {
	value := $if threads { 41 } $else { 7 }
	spawn work()
	println(int_str(value))
}
')
	assert with_spawn == '41'
}

fn test_comptime_if_threads_does_not_count_spawns_in_its_own_branches() {
	v3_bin := build_v3()
	statement_out := run_good(v3_bin, 'threads_statement_spawn_does_not_self_enable', 'fn work() {}

fn main() {
	$if threads {
		spawn work()
		println("threads")
	} $else {
		println("single")
	}
	value := $if threads { 41 } $else { 7 }
	println(int_str(value))
}
')
	assert statement_out == 'single\n7'

	top_level_out := run_good(v3_bin, 'threads_top_level_spawn_does_not_self_enable', '$if threads {
	fn selected_value() int {
		spawn work()
		return 41
	}
} $else {
	fn selected_value() int {
		return 7
	}
}

fn work() {}

fn main() {
	println(int_str(selected_value()))
}
')
	assert top_level_out == '7'

	import_out := run_good_project(v3_bin, 'threads_import_spawn_does_not_self_enable', {
		'v.mod':           "Module { name: 'threads_import_spawn_does_not_self_enable' }\n"
		'worker/worker.v': 'module worker\n\n$if threads {\n\tpub fn mode() string {\n\t\tspawn work()\n\t\treturn "threads"\n\t}\n} $else {\n\tpub fn mode() string {\n\t\treturn "single"\n\t}\n}\n\nfn work() {}\n'
		'main.v':          'module main\n\nimport worker\n\nfn main() {\n\tprintln(worker.mode())\n}\n'
	}, 'main.v')
	assert import_out == 'single'
}

fn test_comptime_if_threads_counts_spawns_in_non_builtin_threads_conditions() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'threads_data_condition_spawn', 'struct Config {
	threads int
}

fn work() {}

fn inspect[T]() {
	$for field in T.fields {
		$if field.name == "threads" {
			spawn work()
		}
	}
	mode := $if threads { "threads" } $else { "single" }
	println(mode)
}

fn main() {
	inspect[Config]()
}
')
	assert out == 'threads'
}

fn test_comptime_if_threads_counts_spawns_in_mixed_deferred_conditions() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'threads_mixed_deferred_condition_spawn', 'fn work() {}

fn activate[T]() {
	$if T is int || threads {
		spawn work()
	}
}

fn main() {
	activate[int]()
	mode := $if threads { "threads" } $else { "single" }
	println(mode)
}
')
	assert out == 'threads'
}

fn test_comptime_if_threads_mixed_conditions_keep_normal_flag_evaluation() {
	v3_bin := build_v3()
	out := run_good_with_flags(v3_bin, 'comptime_threads_mixed_conditions',
		'-d mixed_threads_flag', 'fn main() {
	$if mixed_threads_flag ? || threads {
		println("statement or")
	} $else {
		println("wrong statement or")
	}
	or_value := $if mixed_threads_flag ? || threads { 41 } $else { 7 }
	println(int_str(or_value))
	$if mixed_threads_flag ? && threads {
		println("wrong statement and")
	} $else {
		println("statement and")
	}
	and_value := $if mixed_threads_flag ? && threads { 41 } $else { 7 }
	println(int_str(and_value))
}
')
	assert out == 'statement or\n41\nstatement and\n7'
}

fn test_comptime_if_custom_threads_flags_are_not_deferred() {
	v3_bin := build_v3()
	source := '$if threads ? {
	fn top_level_value() int {
		return 41
	}
} $else {
	fn top_level_value() int {
		return 7
	}
}

fn main() {
	$if threads ? {
		println("optional enabled")
	} $else {
		println("optional disabled")
	}
	optional_value := $if threads ? { 41 } $else { 7 }
	println(int_str(optional_value))
	$if $d("threads", true) {
		println("define enabled")
	} $else {
		println("define disabled")
	}
	define_value := $if $d("threads", true) { 41 } $else { 7 }
	println(int_str(define_value))
	println(int_str(top_level_value()))
}
'
	without_define := run_good(v3_bin, 'comptime_custom_threads_default', source)
	assert without_define == 'optional disabled\n7\ndefine enabled\n41\n7'
	with_define := run_good_with_flags(v3_bin, 'comptime_custom_threads_enabled', '-d threads',
		source)
	assert with_define == 'optional enabled\n41\ndefine enabled\n41\n41'
}

fn test_top_level_comptime_if_threads_prunes_inactive_declarations_before_collect() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'top_level_threads_prunes_inactive_decl', '$if threads {
	fn selected_value() int {
		return 41
	}
} $else {
	fn selected_value() string {
		return "wrong"
	}
}

fn work() {}

fn main() {
	spawn work()
	println(int_str(selected_value()))
}
')
	assert out == '41'
}

fn test_comptime_if_threads_counts_spawns_in_imported_modules() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'threads_spawn_in_imported_module', {
		'v.mod':           "Module { name: 'threads_spawn_in_imported_module' }\n"
		'worker/worker.v': 'module worker\n\nfn work() {}\n\npub fn start() {\n\tspawn work()\n}\n'
		'main.v':          'module main\n\nimport worker\n\nfn main() {\n\tworker.start()\n\tmode := $if threads { "threads" } $else { "single" }\n\tprintln(mode)\n}\n'
	}, 'main.v')
	assert out == 'threads'
	nested_out := run_good_project(v3_bin, 'threads_spawn_in_nested_imported_module', {
		'v.mod':         "Module { name: 'threads_spawn_in_nested_imported_module' }\n"
		'foo/bar/bar.v': 'module bar\n\nfn work() {}\n\npub fn start() {\n\tspawn work()\n}\n'
		'main.v':        'module main\n\nimport foo.bar\n\nfn main() {\n\tbar.start()\n\tmode := $if threads { "threads" } $else { "single" }\n\tprintln(mode)\n}\n'
	}, 'main.v')
	assert nested_out == 'threads'
}

fn test_select_receive_assignment_invalidates_smartcast_before_branch_body() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'select_receive_assign_invalidates_smartcast', 'struct Foo {
	value int
}

struct Bar {
	value int
}

type Item = Bar | Foo

fn main() {
	mut item := Item(Foo{
		value: 1
	})
	ch := chan Item{cap: 1}
	ch <- Item(Bar{
		value: 2
	})
	if item is Foo {
		select {
			item = <-ch {
				if item is Bar {
					println(int_str(item.value))
				}
			}
		}
	}
}
')
	assert out == '2'
}

fn test_select_receive_assignment_does_not_invalidate_sibling_smartcasts() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'select_receive_assign_sibling_smartcast', 'struct Foo {
	value int
}

struct Bar {}

type Item = Bar | Foo

fn main() {
	mut item := Item(Foo{
		value: 7
	})
	ch := chan Item{}
	if item is Foo {
		select {
			item = <-ch {}
			else {
				println(int_str(item.value))
			}
		}
	}
}
')
	assert out == '7'
}

fn test_select_receive_declaration_shadows_outer_smartcast_only_in_branch() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'select_receive_decl_shadows_smartcast', 'struct Foo {
	value int
}

struct Bar {
	value int
}

type Item = Bar | Foo

fn main() {
	item := Item(Foo{
		value: 1
	})
	ch := chan Item{cap: 1}
	ch <- Item(Bar{
		value: 2
	})
	if item is Foo {
		select {
			item := <-ch {
				if item is Bar {
					println(int_str(item.value))
				}
			}
		}
		println(int_str(item.value))
	}
}
')
	assert out == '2\n1'
}

fn test_select_exception_branches_flush_defers() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'select_exception_branch_defers', 'import time

__global trace int

fn cleanup() {
	trace = trace * 10 + 2
}

fn main() {
	select {
		else {
			defer {
				cleanup()
			}
			trace = 1
		}
	}
	println(int_str(trace))
	trace = 0
	ch := chan int{}
	select {
		_ := <-ch {}
		1 * time.nanosecond {
			defer {
				cleanup()
			}
			trace = 3
		}
	}
	println(int_str(trace))
}
')
	assert out == '12\n32'
}

fn test_context_dependent_if_branches_infer_wrapper_types() {
	v3_bin := build_v3()
	opt_out := run_good(v3_bin, 'if_none_branch_infers_option',
		'fn maybe(flag bool) ?int {\n\treturn if flag { none } else { 3 }\n}\n\nfn main() {\n\tprintln(int_str(maybe(false) or { -1 }))\n\tprintln(int_str(maybe(true) or { -1 }))\n}\n')
	assert opt_out == '3\n-1'
	opt_assign_out := run_good(v3_bin, 'if_none_branch_uses_option_assignment_context',
		'fn main() {\n\tflag := false\n\tmut value := ?int(none)\n\tvalue = if flag { none } else { 8 }\n\tprintln(int_str(value or { -1 }))\n}\n')
	assert opt_assign_out == '8'
	res_out := run_good(v3_bin, 'if_error_branch_infers_result',
		"fn maybe(flag bool) !int {\n\treturn if flag { error('bad') } else { 4 }\n}\n\nfn main() {\n\tprintln(int_str(maybe(false) or { -1 }))\n\tprintln(int_str(maybe(true) or { -1 }))\n}\n")
	assert res_out == '4\n-1'
	code_out := run_good(v3_bin, 'if_error_with_code_branch_infers_result',
		"fn maybe(flag bool) !int {\n\treturn if flag { error_with_code('bad', 1) } else { 6 }\n}\n\nfn main() {\n\tprintln(int_str(maybe(false) or { -1 }))\n\tprintln(int_str(maybe(true) or { -1 }))\n}\n")
	assert code_out == '6\n-1'
	match_code_out := run_good(v3_bin, 'match_error_with_code_branch_infers_result',
		"fn maybe(n int) !int {\n\treturn match n {\n\t\t0 { error_with_code('bad', 2) }\n\t\telse { 7 }\n\t}\n}\n\nfn main() {\n\tprintln(int_str(maybe(1) or { -1 }))\n\tprintln(int_str(maybe(0) or { -1 }))\n}\n")
	assert match_code_out == '7\n-1'
	run_bad(v3_bin, 'if_none_branch_without_context_rejected',
		'fn main() {\n\tx := if true { none } else { 1 }\n\tprintln(x)\n}\n',
		'if-expression branch type mismatch')
	run_bad(v3_bin, 'if_none_branch_rejected_for_result_without_context',
		'fn fallible() !int {\n\treturn 2\n}\n\nfn main() {\n\tflag := true\n\tx := if flag { none } else { fallible() }\n\tprintln(int_str(x or { -1 }))\n}\n',
		'if-expression branch type mismatch')
	option_error_out := run_good(v3_bin, 'if_error_branch_infers_option',
		"fn f(ok bool) ?int {\n\treturn if ok { error('bad') } else { 1 }\n}\n\nfn main() {\n\tprintln(int_str(f(false) or { -1 }))\n\t_ := f(true) or {\n\t\tprintln(err.msg())\n\t\treturn\n\t}\n}\n")
	assert option_error_out == '1\nbad'
	run_bad(v3_bin, 'if_none_branch_rejected_for_result_payload',
		'fn g(ok bool) !int {\n\treturn if ok { none } else { 1 }\n}\n\nfn main() {\n\t_ := g(false) or { 0 }\n}\n',
		'if-expression branch type mismatch')
	match_option_error_out := run_good(v3_bin, 'match_error_branch_infers_option',
		"fn f(n int) ?int {\n\treturn match n {\n\t\t0 { error('bad') }\n\t\telse { 1 }\n\t}\n}\n\nfn main() {\n\tprintln(int_str(f(1) or { -1 }))\n\t_ := f(0) or {\n\t\tprintln(err.msg())\n\t\treturn\n\t}\n}\n")
	assert match_option_error_out == '1\nbad'
	run_bad(v3_bin, 'match_none_branch_rejected_for_result_payload',
		'fn g(n int) !int {\n\treturn match n {\n\t\t0 { none }\n\t\telse { 1 }\n\t}\n}\n\nfn main() {\n\t_ := g(1) or { 0 }\n}\n',
		'cannot return')
	run_bad(v3_bin, 'if_option_void_branch_rejected_for_payload',
		'fn maybe_void() ? {\n\treturn\n}\n\nfn f(ok bool) ?int {\n\treturn if ok { maybe_void() } else { 1 }\n}\n\nfn main() {\n\t_ := f(true) or { 0 }\n}\n',
		'if-expression branch type mismatch')
	run_bad(v3_bin, 'if_result_void_branch_rejected_for_payload',
		'fn maybe_void() ! {\n\treturn\n}\n\nfn f(ok bool) !int {\n\treturn if ok { maybe_void() } else { 1 }\n}\n\nfn main() {\n\t_ := f(true) or { 0 }\n}\n',
		'if-expression branch type mismatch')
	run_bad(v3_bin, 'match_option_void_branch_rejected_for_payload',
		'fn maybe_void() ? {\n\treturn\n}\n\nfn f(n int) ?int {\n\treturn match n {\n\t\t0 { maybe_void() }\n\t\telse { 1 }\n\t}\n}\n\nfn main() {\n\t_ := f(0) or { 0 }\n}\n',
		'cannot return')
}

fn test_assoc_return_runs_defers() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'assoc_return_runs_defers',
		'struct Point {\n\tx int\n\ty int\n}\n\n__global hit int\n\nfn make_point() Point {\n\tbase := Point{\n\t\tx: 1\n\t\ty: 2\n\t}\n\tdefer {\n\t\thit = 7\n\t}\n\treturn Point{\n\t\t...base\n\t\tx: 5\n\t}\n}\n\nfn main() {\n\tp := make_point()\n\tprintln(int_str(p.x))\n\tprintln(int_str(hit))\n}\n')
	assert out == '5\n7'
}

fn test_pointer_arithmetic_deref_keeps_pointer_type() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'pointer_arithmetic_deref',
		'fn main() {\n\tmut nums := [1, 2]!\n\tp := unsafe { &nums[0] }\n\tv := unsafe { *(p + 1) }\n\tprintln(int_str(v))\n}\n')
	assert out == '2'
}

fn test_builtin_addr_requires_unsafe_and_addresses_pointer_variables() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'builtin_addr_requires_unsafe', 'fn main() {
	x := 1
	_ := __addr(x)
}
',
		'`__addr` can only be used in unsafe blocks')
	source := 'fn main() {
	mut a := 1
	mut b := 2
	mut p := &a
	q := unsafe { __addr(p) }
	unsafe {
		*q = &b
		*p = 7
	}
	println(int_str(a))
	println(int_str(b))
}
'
	c_source := gen_c(v3_bin, 'builtin_addr_pointer_variable_c', source)
	assert c_source.contains('&p'), c_source
	out := run_good(v3_bin, 'builtin_addr_pointer_variable', source)
	assert out == '1\n7'
}

fn test_builtin_addr_overloaded_index_materializes_getter_result() {
	v3_bin := build_v3()
	source := 'struct Item {
	n int
}

struct Dict {
	values map[string]Item
}

fn (d Dict) [] (key string) Item {
	return d.values[key]
}

fn main() {
	d := Dict{
		values: {
			"a": Item{
				n: 7
			}
		}
	}
	p := unsafe { __addr(d["a"]) }
	println(int_str((*p).n))
}
'
	c_source := gen_c(v3_bin, 'builtin_addr_overloaded_index_materializes_getter_result_c', source)
	assert c_source.contains('addr'), c_source
	assert !c_source.contains('&Dict__index('), c_source
	out := run_good(v3_bin, 'builtin_addr_overloaded_index_materializes_getter_result', source)
	assert out == '7'
}

fn test_array_alias_free_uses_array_builtin_inside_alias_method() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'array_alias_free_builtin',
		'import strings\n\nfn main() {\n\tmut b := strings.new_builder(4)\n\tb.write_string("ok")\n\tunsafe { b.free() }\n\tprintln("ok")\n}\n')
	assert out == 'ok'
}

fn test_dynamic_enum_array_literal_keeps_enum_element_width() {
	v3_bin := build_v3()
	c_source := gen_c(v3_bin, 'dynamic_enum_array_literal_width',
		'enum Tiny as u8 {\n\tzero\n\tone\n}\n\nfn main() {\n\tvalues := [Tiny.zero, Tiny.one]\n\tprintln(int_str(int(values[0])))\n\tprintln(int_str(int(values[1])))\n}\n')
	assert c_source.contains('array_new(\tsizeof(Tiny), 0, 2)'), c_source
	assert !c_source.contains('Array values = array_new(\tsizeof(int), 0, 2)'), c_source
	out := run_good(v3_bin, 'dynamic_enum_array_literal_width_run',
		'enum Tiny as u8 {\n\tzero\n\tone\n}\n\nfn main() {\n\tvalues := [Tiny.zero, Tiny.one]\n\tprintln(int_str(int(values[0])))\n\tprintln(int_str(int(values[1])))\n}\n')
	assert out == '0\n1'
}

fn test_nested_string_plus_releases_intermediate_storage() {
	v3_bin := build_v3()
	source := "fn concat_path(dir string, name string) string {\n\treturn '\${dir}/\${name}'\n}\n\nfn main() {\n\tname := 'file'\n\tprintln(concat_path('root', name))\n}\n"
	c_source := gen_c(v3_bin, 'nested_string_plus_owned_intermediate', source)
	assert !c_source.contains('string__plus(string__plus(dir,'), c_source
	assert c_source.contains('string__free(&__str_plus_acc_'), c_source
	out := run_good(v3_bin, 'nested_string_plus_owned_intermediate_run', source)
	assert out == 'root/file'
}

fn test_for_mut_pointer_storage_receivers_do_not_get_extra_address() {
	v3_bin := build_v3()
	item_src := 'struct Item {
mut:
	n int
}

fn (mut item Item) bump() {
	item.n++
}

fn bump_item(mut item Item) {
	item.bump()
}

struct Counter {
mut:
	n int
}

fn (mut c Counter) inc() {
	c.n++
}

fn inc_counter(mut c Counter) {
	c.inc()
}

fn main() {
	mut items := [Item{n: 1}, Item{n: 2}]
	for mut item in items {
		item.bump()
		bump_item(mut item)
	}
	mut first := &Item{n: 3}
	mut pointer_items := [first]
	for mut pointer_item in pointer_items {
		pointer_item.bump()
		bump_item(mut pointer_item)
	}
	{
		mut item := Counter{}
		inc_counter(mut item)
		item.inc()
		assert item.n == 2
	}
	mut c := Counter{}
	inc_counter(mut c)
	c.inc()
	println(int_str(items[0].n))
	println(int_str(items[1].n))
	println(int_str(first.n))
	println(int_str(c.n))
}
'
	out := run_good(v3_bin, 'for_mut_item_receiver_run', item_src)
	assert out == '3\n4\n5\n2'
	item_c := gen_c(v3_bin, 'for_mut_item_receiver_c', item_src)
	item_main := c_fn_body(item_c, 'int main(')
	assert item_main.len > 0, item_c
	assert item_main.contains('Item* item ='), item_main
	assert item_main.contains('__bump(item);'), item_main
	assert !item_main.contains('__bump(&item);'), item_main
	assert item_main.contains('bump_item(item);'), item_main
	assert !item_main.contains('bump_item(&item);'), item_main
	assert item_main.contains('Item** pointer_item ='), item_main
	assert item_main.contains('__bump(*pointer_item);'), item_main
	assert item_main.contains('bump_item(*pointer_item);'), item_main
	assert !item_main.contains('bump_item(pointer_item);'), item_main
	assert !item_main.contains('__bump(pointer_item);'), item_main
	assert item_main.contains('inc_counter(&item);'), item_main
	assert !item_main.contains('inc_counter(item);'), item_main
	assert item_main.contains('inc_counter(&c);'), item_main
	assert item_main.contains('__inc(&item);'), item_main
	assert item_main.contains('__inc(&c);'), item_main
	assert !item_main.contains('__inc(c);'), item_main
	assert item_main.contains('Item* item ='), item_main
	assert item_main.contains('__bump(item);'), item_main
	assert !item_main.contains('__bump(&item);'), item_main
	assert item_main.contains('bump_item(item);'), item_main
	assert !item_main.contains('bump_item(&item);'), item_main
	assert item_main.contains('inc_counter(&item);'), item_main
	assert !item_main.contains('inc_counter(item);'), item_main
	assert item_main.contains('inc_counter(&c);'), item_main
	assert item_main.contains('__inc(&item);'), item_main
	assert item_main.contains('__inc(&c);'), item_main
	assert !item_main.contains('__inc(c);'), item_main

	string_c := gen_c(v3_bin, 'for_mut_string_free_receiver', "fn main() {
	mut values := ['alpha', 'beta']
	for mut s in values {
		unsafe { s.free() }
	}
}
")
	string_main := c_fn_body(string_c, 'int main(')
	assert string_main.len > 0, string_c
	assert string_main.contains('string* s ='), string_main
	assert string_main.contains('string__free(s);'), string_main
	assert !string_main.contains('string__free(&s);'), string_main
}

fn test_assert_capture_preserves_inferred_wide_const_type() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'assert_capture_inferred_wide_const_type', 'type Duration = i64

const nanosecond = Duration(1)
const microsecond = 1000 * nanosecond
const millisecond = 1000 * microsecond
const timeout = 10000 * millisecond

struct Config {
	value Duration = timeout
}

fn main() {
	config := Config{}
	assert config.value == timeout
	println(config.value.str())
}
')
	assert out == '10000000000'
}

fn test_c_pointer_receiver_str_is_used_for_reference_field() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'c_pointer_receiver_str_for_reference_field', "struct C.Opaque {}

struct Holder {
	ptr &C.Opaque
}

fn (p &C.Opaque) str() string {
	return 'C.Opaque(0x\${voidptr(p)})'
}

fn main() {
	holder := unsafe { Holder{&C.Opaque(123)} }
	println(holder.str().contains('&C.Opaque(0x7b)'))
}
")
	assert out == 'true'
}

fn test_channel_alias_close_method_wins_over_builtin() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'channel_alias_close_method_before_builtin',
		'type MyChan = chan int\n\nfn (c MyChan) close() int {\n\treturn 71\n}\n\nfn main() {\n\tch := MyChan(unsafe { nil })\n\tprintln(int_str(ch.close()))\n}\n')
	assert out == '71'
	pointer_c := gen_c(v3_bin, 'pointer_channel_close_lowers_to_runtime',
		'fn main() {\n\tmut ch := chan bool{cap: 1}\n\tp := &ch\n\tp.close()\n}\n')
	assert pointer_c.contains('sync__Channel__close(*p,')
}

fn test_channel_reference_auto_str_reads_channel_value() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'channel_reference_auto_str', 'fn main() {
	ch := chan int{cap: 2}
	println(&ch)
}
')
	assert out == 'chan int{\n    cap: 2, closed: false\n}'
}

fn test_channel_alias_reference_auto_str_reads_channel_value() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'channel_alias_reference_auto_str', 'type MyChan = chan int

fn main() {
	ch := MyChan(chan int{cap: 2})
	println(&ch)
}
')
	assert out == 'MyChan(chan int{\n    cap: 2, closed: false\n})'
}

fn test_channel_auto_str_helpers_are_rooted_for_aggregates() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'channel_aggregate_auto_str_helpers', 'struct Holder {
	ch chan int
}

fn main() {
	ch := chan int{cap: 2}
	println([ch])
	println(Holder{
		ch: ch
	})
	println(Holder{})
}
')
	assert out.contains('chan int{\n    cap: 2, closed: false\n}')
	assert out.contains('Holder{')
	assert out.contains('chan int{\n        cap: 0, closed: false\n    }')
}

fn test_explicit_return_semicolon_keeps_unreachable_check() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'explicit_return_semicolon_unreachable', 'fn stop() {
	return;
	println("unreachable")
}
fn main() {}
',
		'unreachable code')
	run_bad(v3_bin, 'nested_explicit_return_semicolon_unreachable', 'fn stop(ok bool) {
	if ok {
		return;
		println("unreachable")
	}
}
fn main() {}
',
		'unreachable code')
	run_bad(v3_bin, 'nested_return_semicolon_unreachable', 'fn stop(ok bool) {
	if ok {
		return;
	}
	return
	println("unreachable")
}
fn main() {}
',
		'unreachable code')
}

fn test_qualified_enum_str_requires_exact_receiver() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'qualified_enum_str_exact_receiver', {
		'main.v':      'module main\n\nimport moda\nimport modb\n\nfn main() {\n\tprintln(moda.Color.red.str())\n\tprintln(modb.Color.blue.str())\n}\n'
		'moda/moda.v': 'module moda\n\npub enum Color {\n\tred\n}\n'
		'modb/modb.v': "module modb\n\npub enum Color {\n\tblue\n}\n\npub fn (c Color) str() string {\n\treturn 'custom'\n}\n"
	}, 'main.v')
	assert out == 'red\ncustom'
}

fn test_array_builtin_method_fallback_keeps_return_type() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'array_builtin_method_fallback',
		'fn main() {\n\tmut nums := []int{}\n\tnums << 1\n\tnums << 2\n\tnums << 3\n\tptrs := unsafe { nums.pointers() }\n\tprintln(int_str(ptrs.len))\n}\n')
	assert out == '3'
	ptr_out := run_good(v3_bin, 'array_pointers_pointer_receiver',
		'fn main() {\n\tmut nums := []int{}\n\tnums << 1\n\tp := &nums\n\tptrs := unsafe { p.pointers() }\n\tprintln(int_str(ptrs.len))\n}\n')
	assert ptr_out == '1'
	reverse_out := run_good(v3_bin, 'array_reverse_pointer_receiver',
		'fn main() {\n\tmut nums := []int{}\n\tnums << 1\n\tnums << 2\n\tp := &nums\n\tp.reverse()\n\tprintln("ok")\n}\n')
	assert reverse_out == 'ok'
	exact_out := run_good(v3_bin, 'exact_array_receiver_method_before_builtin',
		'fn (a []int) pointers() []int {\n\treturn a\n}\n\nfn main() {\n\tnums := [9]\n\tptrs := nums.pointers()\n\tprintln(int_str(ptrs[0]))\n}\n')
	assert exact_out == '9'
	exact_clear_out := run_good(v3_bin, 'exact_array_clear_method_before_cgen',
		'fn (a []int) clear() int {\n\treturn 5\n}\n\nfn main() {\n\tmut nums := []int{}\n\tnums << 1\n\tprintln(int_str(nums.clear()))\n\tprintln(int_str(nums.len))\n}\n')
	assert exact_clear_out == '5\n1'
	exact_clone_out := run_good(v3_bin, 'exact_array_clone_method_before_builtin',
		'fn (a []int) clone() int {\n\treturn 12\n}\n\nfn main() {\n\tmut nums := []int{}\n\tnums << 1\n\tprintln(int_str(nums.clone()))\n}\n')
	assert exact_clone_out == '12'
	exact_reverse_out := run_good(v3_bin, 'exact_array_reverse_method_before_builtin',
		'fn (a []int) reverse() int {\n\treturn 13\n}\n\nfn main() {\n\tmut nums := []int{}\n\tnums << 1\n\tprintln(int_str(nums.reverse()))\n}\n')
	assert exact_reverse_out == '13'
	module_array_prefix_out := run_good_project(v3_bin, 'array_prefix_module_receiver_method', {
		'main.v':            'module main\n\nimport array_utils\n\nfn main() {\n\tprintln(array_utils.run())\n}\n'
		'array_utils/mod.v': 'module array_utils\n\nfn (a []int) reverse() int {\n\treturn 73\n}\n\npub fn run() string {\n\tmut nums := []int{}\n\tnums << 1\n\treturn int_str(nums.reverse())\n}\n'
	}, 'main.v')
	assert module_array_prefix_out == '73'
	module_array_runtime_prefix_out := run_good_project(v3_bin,
		'array_runtime_prefix_module_receiver_method', {
		'main.v':             'module main\n\nimport array__utils\n\nfn main() {\n\tprintln(array__utils.run())\n}\n'
		'array__utils/mod.v': 'module array__utils\n\nfn (a []int) reverse() int {\n\treturn 83\n}\n\npub fn run() string {\n\tmut nums := []int{}\n\tnums << 1\n\treturn int_str(nums.reverse())\n}\n'
	}, 'main.v')
	assert module_array_runtime_prefix_out == '83'
	module_array_move_out := run_good_project(v3_bin, 'module_array_move_receiver_method', {
		'main.v':      'module main\n\nimport thing\n\nfn main() {\n\tprintln(thing.run())\n}\n'
		'thing/mod.v': 'module thing\n\nfn (a []int) move() int {\n\treturn 91\n}\n\npub fn run() string {\n\tmut nums := []int{}\n\tnums << 1\n\treturn int_str(nums.move())\n}\n'
	}, 'main.v')
	assert module_array_move_out == '91'
	exact_prepend_out := run_good(v3_bin, 'exact_array_prepend_method_before_builtin',
		'fn (a []int) prepend(x int) int {\n\treturn x + 1\n}\n\nfn main() {\n\tmut nums := []int{}\n\tnums << 1\n\tprintln(int_str(nums.prepend(4)))\n}\n')
	assert exact_prepend_out == '5'
	run_bad(v3_bin, 'exact_array_first_method_checked_before_builtin',
		'fn (a []int) first() string {\n\treturn "bad"\n}\n\nfn take_int(x int) {}\n\nfn main() {\n\tmut nums := []int{}\n\tnums << 1\n\ttake_int(nums.first())\n}\n',
		'cannot use `string` as argument 1 to `take_int`; expected `int`')
	fixed_dynamic_out := run_good(v3_bin, 'fixed_array_dynamic_receiver_method_before_builtin',
		'fn (a []int) pointers() int {\n\treturn 41\n}\n\nfn main() {\n\tfixed := [3]int{}\n\tprintln(int_str(fixed.pointers()))\n}\n')
	assert fixed_dynamic_out == '41'
	nested_fixed_dynamic_out := run_good(v3_bin, 'nested_fixed_array_dynamic_receiver_method',
		'fn (a [][2]int) pointers() int {\n\treturn 82\n}\n\nfn main() {\n\tfixed := [3][2]int{}\n\tprintln(int_str(fixed.pointers()))\n}\n')
	assert nested_fixed_dynamic_out == '82'
	fixed_alias_shape_out := run_good(v3_bin, 'fixed_array_builtin_not_alias_method',
		'type F = [2]int\n\nfn (f F) pointers() int {\n\treturn 66\n}\n\nfn main() {\n\tmut fixed := [2]int{}\n\tptrs := unsafe { fixed.pointers() }\n\tprintln(int_str(ptrs.len))\n}\n')
	assert fixed_alias_shape_out == '2'
	plain_array_contains_out := run_good(v3_bin, 'plain_array_contains_not_alias_method',
		'type A = []int\n\nfn (a A) contains(x int) int {\n\treturn 0\n}\n\nfn main() {\n\tnums := [1, 2, 3]\n\tif nums.contains(2) {\n\t\tprintln("builtin")\n\t} else {\n\t\tprintln("alias")\n\t}\n\talias := A(nums)\n\tprintln(int_str(alias.contains(2)))\n}\n')
	assert plain_array_contains_out == 'builtin\n0'
	module_primitive_out := run_good_project(v3_bin, 'module_primitive_array_receiver_method', {
		'main.v':      'module main\n\nimport thing\n\nfn main() {\n\tprintln(thing.run())\n}\n'
		'thing/mod.v': 'module thing\n\nfn (a []int) pointers() int {\n\treturn 64\n}\n\npub fn run() string {\n\tmut nums := []int{}\n\tnums << 1\n\treturn int_str(nums.pointers())\n}\n'
	}, 'main.v')
	assert module_primitive_out == '64'
	fixed_out := run_good(v3_bin, 'fixed_array_pointers_original_storage',
		'fn main() {\n\tmut fixed := [3]int{}\n\tfixed[0] = 1\n\tptrs := unsafe { fixed.pointers() }\n\tunsafe {\n\t\tp0 := &int(ptrs[0])\n\t\t*p0 = 9\n\t}\n\tprintln(int_str(fixed[0]))\n}\n')
	assert fixed_out == '9'
	fixed_expr_out := run_good(v3_bin, 'fixed_array_pointers_evaluates_receiver_once',
		'__global calls int\n\nfn next() int {\n\tcalls = calls + 1\n\treturn 0\n}\n\nfn main() {\n\tmut rows := [1][2]int{}\n\trows[0][0] = 5\n\tptrs := unsafe { rows[next()].pointers() }\n\tunsafe {\n\t\tp0 := &int(ptrs[0])\n\t\t*p0 = 8\n\t}\n\tprintln(int_str(calls))\n\tprintln(int_str(rows[0][0]))\n}\n')
	assert fixed_expr_out == '1\n8'
	run_bad(v3_bin, 'fixed_array_pointers_rejects_rvalue_receiver',
		'fn make_fixed() [2]int {\n\treturn [7, 8]!\n}\n\nfn main() {\n\t_ := unsafe { make_fixed().pointers() }\n}\n',
		'fixed array receiver for `pointers` must be addressable')
	run_bad(v3_bin, 'fixed_array_pointers_rejects_map_index_receiver',
		'fn main() {\n\tmut m := map[string][2]int{}\n\tm["x"] = [1, 2]!\n\t_ := unsafe { m["x"].pointers() }\n}\n',
		'fixed array receiver for `pointers` must be addressable')
	fixed_len_expr_out := run_good(v3_bin, 'fixed_array_pointers_folds_len_expr',
		'const segs = 2\n\nfn main() {\n\tmut const_len := [segs + 1]int{}\n\tconst_ptrs := unsafe { const_len.pointers() }\n\tmut shift_len := [8 >>> 1]int{}\n\tshift_ptrs := unsafe { shift_len.pointers() }\n\tprintln(int_str(const_ptrs.len))\n\tprintln(int_str(shift_ptrs.len))\n}\n')
	assert fixed_len_expr_out == '3\n4'
	run_bad(v3_bin, 'fixed_array_pointers_rejects_extra_arg',
		'fn extra_arg() int {\n\treturn 1\n}\n\nfn main() {\n\tmut fixed := [3]int{}\n\t_ := unsafe { fixed.pointers(extra_arg()) }\n}\n',
		'argument count mismatch for `fixed.pointers`: expected 1, got 2')
}

fn test_alias_receiver_method_value_escape_is_supported() {
	v3_bin := build_v3()
	underlying := run_good(v3_bin, 'alias_receiver_underlying_method_value_escape', 'struct Runner {
	n int
}

type RAlias = Runner

fn (r Runner) run() int {
	return r.n
}

fn make_callback(r RAlias) fn () int {
	return r.run
}

fn main() {
	r := RAlias(Runner{
		n: 41
	})
	cb := make_callback(r)
	println(int_str(cb()))
}
')
	assert underlying == '41'
	own := run_good(v3_bin, 'alias_receiver_own_method_value_escape', 'struct Runner {
	n int
}

type RAlias = Runner

fn (r RAlias) alias_run() int {
	return r.n
}

fn make_callback(r RAlias) fn () int {
	return r.alias_run
}

fn main() {
	r := RAlias(Runner{
		n: 42
	})
	cb := make_callback(r)
	println(int_str(cb()))
}
')
	assert own == '42'
}

fn test_interface_method_value_escape_is_supported() {
	v3_bin := build_v3()
	interface_method := run_good(v3_bin, 'review_interface_method_value_escape', 'interface Runner {
	run() int
}

struct Job {
	n int
}

fn (j Job) run() int {
	return j.n
}

struct Holder {
	cb fn () int
}

fn main() {
	r := Runner(Job{
		n: 1
	})
	h := Holder{
		cb: r.run
	}
	println(int_str(h.cb()))
}
')
	assert interface_method == '1'
}

fn test_map_builtin_method_fallback_checks_arguments() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'map_keys_rejects_extra_arg',
		'fn extra_arg() int {\n\treturn 1\n}\n\nfn main() {\n\tmut m := map[string]int{}\n\t_ := m.keys(extra_arg())\n}\n',
		'argument count mismatch for `m.keys`: expected 0, got 1')
	run_bad(v3_bin, 'map_delete_rejects_bad_key_type',
		'fn main() {\n\tmut m := map[string]int{}\n\tm.delete(123)\n}\n',
		'cannot use `int` as argument 2 to `m.delete`; expected `string`')
	run_bad(v3_bin, 'map_reserve_rejects_bad_count_type',
		'fn main() {\n\tmut m := map[string]int{}\n\tm.reserve("bad")\n}\n',
		'cannot use `string` as argument 2 to `m.reserve`; expected `u32`')
	out := run_good(v3_bin, 'map_builtin_method_fallback',
		'fn main() {\n\tmut m := map[string]int{}\n\tm["abc"] = 42\n\tmut moved := m.move()\n\tprintln(int_str(m.len))\n\tmoved.clear()\n\tmoved.reserve(6)\n\tmoved.delete("x")\n\tkeys := moved.keys()\n\tvalues := moved.values()\n\tcloned := moved.clone()\n\tprintln(int_str(keys.len + values.len + cloned.len))\n}\n')
	assert out == '0\n0'
	empty_arrays_out := run_good(v3_bin, 'map_empty_keys_values_keep_elem_size',
		"struct State {\n\tlabels map[string]string\n}\n\nfn main() {\n\ts := State{}\n\tmut keys := s.labels.keys()\n\tkeys << 'abc'\n\tprintln(keys[0])\n\tmut values := s.labels.values()\n\tvalues << 'def'\n\tprintln(values[0])\n}\n")
	assert empty_arrays_out == 'abc\ndef'
	pointer_out := run_good(v3_bin, 'map_move_pointer_receiver_returns_map',
		'fn take(m map[string]int) int {\n\treturn m.len\n}\n\nfn main() {\n\tmut m := map[string]int{}\n\tm["abc"] = 42\n\tp := &m\n\tprintln(int_str(take(p.move())))\n\tprintln(int_str(m.len))\n}\n')
	assert pointer_out == '1\n0'
	exact_out := run_good(v3_bin, 'exact_map_receiver_method_before_builtin',
		'fn (m map[string]int) keys() int {\n\treturn 77\n}\n\nfn main() {\n\tmut m := map[string]int{}\n\tm["x"] = 1\n\tn := m.keys()\n\tprintln(int_str(n))\n}\n')
	assert exact_out == '77'
	alias_rvalue_out := run_good(v3_bin, 'map_alias_rvalue_receiver_method_before_builtin',
		'type M = map[string]int\n\nfn (m M) delete(k string) int {\n\treturn 66\n}\n\nfn make_m() M {\n\tmut m := M(map[string]int{})\n\tm["x"] = 1\n\treturn m\n}\n\nfn main() {\n\tprintln(int_str(make_m().delete("x")))\n}\n')
	assert alias_rvalue_out == '66'
	plain_map_out := run_good(v3_bin, 'plain_map_builtin_not_alias_method',
		'type M = map[string]int\n\nfn (m M) keys() int {\n\treturn 66\n}\n\nfn main() {\n\tmut m := map[string]int{}\n\tm["x"] = 1\n\tkeys := m.keys()\n\tprintln(int_str(keys.len))\n}\n')
	assert plain_map_out == '1'
	module_map_runtime_prefix_out := run_good_project(v3_bin,
		'map_runtime_prefix_module_receiver_method', {
		'main.v':           'module main\n\nimport map__utils\n\nfn main() {\n\tprintln(map__utils.run())\n}\n'
		'map__utils/mod.v': 'module map__utils\n\nfn (m map[string]int) keys() int {\n\treturn 84\n}\n\npub fn run() string {\n\tmut m := map[string]int{}\n\tm["x"] = 1\n\treturn int_str(m.keys())\n}\n'
	}, 'main.v')
	assert module_map_runtime_prefix_out == '84'
	module_map_out := run_good_project(v3_bin, 'map_module_receiver_method', {
		'main.v':    'module main\n\nimport map\n\nfn main() {\n\tprintln(map.run())\n}\n'
		'map/mod.v': 'module map\n\nfn (m map[string]int) keys() int {\n\treturn 85\n}\n\npub fn run() string {\n\tmut m := map[string]int{}\n\tm["x"] = 1\n\treturn int_str(m.keys())\n}\n'
	}, 'main.v')
	assert module_map_out == '85'
	fixed_key_out := run_good(v3_bin, 'fixed_array_key_map_receiver_method_before_builtin',
		'fn (m map[[2]string]int) keys() int {\n\treturn 88\n}\n\nfn main() {\n\tmut m := map[[2]string]int{}\n\tkey := ["a", "b"]!\n\tm[key] = 1\n\tprintln(int_str(m.keys()))\n}\n')
	assert fixed_key_out == '88'
	nested_fixed_key_out := run_good(v3_bin, 'nested_fixed_array_key_map_receiver_method',
		'fn (m map[[3][2]int]int) keys() int {\n\treturn 99\n}\n\nfn main() {\n\tmut m := map[[3][2]int]int{}\n\tkey := [3][2]int{}\n\tm[key] = 1\n\tprintln(int_str(m.keys()))\n}\n')
	assert nested_fixed_key_out == '99'
	module_collection_out := run_good_project(v3_bin, 'module_collection_receiver_methods', {
		'main.v':      'module main\n\nimport thing\n\nfn main() {\n\tprintln(thing.run())\n}\n'
		'thing/mod.v': 'module thing\n\nstruct Foo {}\nstruct Key {}\n\nfn (m map[string]Foo) keys() int {\n\treturn 31\n}\n\nfn (a []Foo) pointers() int {\n\treturn 42\n}\n\nfn (m map[Key]int) keys() int {\n\treturn 53\n}\n\npub fn run() string {\n\tmut m := map[string]Foo{}\n\tm["x"] = Foo{}\n\titems := [Foo{}]\n\tkeyed := map[Key]int{}\n\treturn int_str(m.keys()) + "\\n" + int_str(items.pointers()) + "\\n" + int_str(keyed.keys())\n}\n'
	}, 'main.v')
	assert module_collection_out == '31\n42\n53'
}

fn test_arm64_string_roundtrip_preserves_literal_flag() {
	$if macos && arm64 {
		v3_bin := build_v3()
		out := run_good_backend(v3_bin, 'arm64_string_roundtrip_preserves_literal_flag', 'arm64',
			"fn literal_local() string {\n\ts := 'literal-static'\n\treturn s\n}\n\nfn arg_local(s string) string {\n\tlocal := s\n\treturn local\n}\n\nfn main() {\n\ta := literal_local()\n\tb := arg_local('argument-static')\n\tunsafe {\n\t\ta.free()\n\t\tb.free()\n\t}\n\tprintln('ok')\n}\n")
		assert out == 'ok'
		map_out := run_good_backend(v3_bin, 'arm64_map_empty_arrays_keep_elem_size', 'arm64',
			"struct State {\n\tlabels map[string]string\n}\n\nfn main() {\n\ts := State{}\n\tmut keys := s.labels.keys()\n\tkeys << 'abc'\n\tprintln(keys[0])\n\tmut values := s.labels.values()\n\tvalues << 'def'\n\tprintln(values[0])\n}\n")
		assert map_out == 'abc\ndef'
	} $else {
		assert true
	}
}

fn test_runtime_inits_run_before_module_init() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'runtime_inits_before_module_init', {
		'main.v':      'module main\n\nimport moda\n\nfn main() {\n\tprintln(int_str(moda.const_seen()))\n\tprintln(int_str(moda.global_seen()))\n}\n'
		'moda/moda.v': "module moda\n\nconst const_map = map[string]int{\n\t'const': 5\n}\n\n__global (\n\tglobal_map = map[string]int{\n\t\t'global': 7\n\t}\n\tseen_const int\n\tseen_global int\n)\n\nfn init() {\n\tseen_const = const_map['const']\n\tseen_global = global_map['global']\n}\n\npub fn const_seen() int {\n\treturn seen_const\n}\n\npub fn global_seen() int {\n\treturn seen_global\n}\n"
	}, 'main.v')
	assert out == '5\n7'
}

fn test_const_dependencies_follow_receiver_method() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'const_deps_receiver_method', 'struct B {}

fn (b B) value() int {
	return 7
}

struct A {}

fn (a A) value() int {
	return dep + 1
}

fn seed() int {
	return 41
}

const result = A{}.value()
const dep = seed()

fn main() {
	println(int_str(result))
}
')
	assert out == '42'
}

fn test_json_decode_generic_struct_preserves_field_default() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'json_decode_generic_struct_default', 'import json

struct Box[T] {
	n int = 5
}

struct GenericChild {
	n int
}

struct PointerBox[T] {
	p     &GenericChild = &GenericChild{n: 7}
	value T
}

fn main() {
	box := json.decode(Box[int], "{}") or { Box[int]{n: 5} }
	println(int_str(box.n))
	pointer_box := json.decode(PointerBox[int], "{\\"value\\":3}") or {
		PointerBox[int]{value: 3}
	}
	println(int_str(pointer_box.p.n))
	println(int_str(pointer_box.value))
}
')
	assert out == '5\n7\n3'
}

fn test_json_decode_fast_path_validates_arrays_and_preserves_defaults() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'json_decode_fast_path_nested_values', 'import json

struct Inner {
	value int
}

struct Outer {
	inner Inner
}

struct BoolList {
	values []bool
}

struct I64List {
	values []i64
}

struct WideInts {
	min             i64
	max             u64
	signed_values   []i64
	unsigned_values []u64
}

struct StrictChild {
	ok bool
}

struct ChildList {
	values []StrictChild
}

struct PointerDefault {
	value &Inner = &Inner{value: 7}
}

struct NestedPointerDefaults {
	nested PointerDefault
	values []PointerDefault
}

fn main() {
	mut array_failed := false
	_ := json.decode(BoolList, "{\\"values\\":[1]}") or {
		array_failed = true
		BoolList{}
	}
	println(array_failed)
	i64_values := json.decode(I64List, "{\\"values\\":[9007199254740993]}")!
	println(i64_values.values[0].str())
	mut struct_array_failed := false
	_ := json.decode(ChildList, "{\\"values\\":[{\\"ok\\":1}]}") or {
		struct_array_failed = true
		ChildList{}
	}
	println(struct_array_failed)

	mut nested_failed := false
	outer := json.decode(Outer, "{}") or {
		nested_failed = true
		Outer{}
	}
	println(!nested_failed)
	println(int_str(outer.inner.value))

	pointer_default := json.decode(PointerDefault, "{}")!
	println(int_str(pointer_default.value.value))

	nested_defaults := json.decode(NestedPointerDefaults, "{\\"values\\":[{}]}")!
	println(int_str(nested_defaults.nested.value.value))
	println(int_str(nested_defaults.values[0].value.value))

	wide := json.decode(WideInts, "{\\"min\\":-9223372036854775808,\\"max\\":18446744073709551615,\\"signed_values\\":[9007199254740993],\\"unsigned_values\\":[9007199254740993]}")!
	println(wide.min.str())
	println(wide.max.str())
	println(wide.signed_values[0].str())
	println(wide.unsigned_values[0].str())
}
')
	assert out == 'true\n9007199254740993\ntrue\ntrue\n0\n7\n7\n7\n-9223372036854775808\n18446744073709551615\n9007199254740993\n9007199254740993'
}

fn test_json_decode_fast_path_uses_renamed_fields_recursively() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'json_decode_renamed_fields', 'import json

struct Item {
	id int @[json: \'itemId\']
}

struct Payload {
	group_name string @[json: \'groupName\']
	items      []Item  @[json: \'testItems\']
}

fn main() {
	payload := json.decode(Payload, "{\\"groupName\\":\\"A\\",\\"testItems\\":[{\\"itemId\\":7}]}")!
	println(payload.group_name)
	println(int_str(payload.items[0].id))
}
')
	assert out == 'A\n7'
}

fn test_json_decode_aligned_pointer_fields_use_aligned_memdup() {
	v3_bin := build_v3()
	source := 'import json

@[aligned: 64]
struct Aligned {
	x int
}

struct Box {
	p &Aligned
}

fn main() {
	box := json.decode(Box, "{\\"p\\":{\\"x\\":7}}")!
	println(int_str(box.p.x))
	unsafe {
		free(box.p)
	}
}
'
	c_source := gen_c(v3_bin, 'json_decode_aligned_pointer_field', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv)')
	assert main_body.contains('v3_aligned_memdup('), main_body
	assert !main_body.contains('(Aligned*)memdup('), main_body
	assert main_body.contains('v3_aligned_free(box.p)'), main_body
	out := run_good(v3_bin, 'json_decode_aligned_pointer_field_run', source)
	assert out == '7'
}

fn test_aligned_alias_heap_cast_uses_aligned_memdup() {
	v3_bin := build_v3()
	source := '@[aligned: 64]
struct Aligned {
	x int
}

type A = Aligned

fn main() {
	p := &A(Aligned{
		x: 7
	})
	println(int_str(p.x))
	unsafe {
		free(p)
	}
}
'
	c_source := gen_c(v3_bin, 'aligned_alias_heap_cast', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv)')
	assert main_body.contains('(main__Aligned*)v3_aligned_memdup('), main_body
	assert !main_body.contains('(main__Aligned*)memdup('), main_body
	assert main_body.contains('v3_aligned_free(p)'), main_body
	out := run_good(v3_bin, 'aligned_alias_heap_cast_run', source)
	assert out == '7'
}

fn test_unimported_main_types_are_not_visible_in_modules() {
	v3_bin := build_v3()
	run_bad_project(v3_bin, 'unimported_plain_main_type', {
		'main.v':      'module main\n\nimport moda\n\nstruct Foo {}\n\nfn main() {\n\t_ = moda.make()\n}\n'
		'moda/moda.v': 'module moda\n\npub struct Holder {\n\tvalue Foo\n}\n\npub fn make() Holder {\n\treturn Holder{}\n}\n'
	}, ['main.v'], 'unknown type `Foo`')
	run_bad_project(v3_bin, 'unimported_generic_main_type', {
		'main.v':      'module main\n\nimport moda\n\nstruct Box[T] {}\n\nfn main() {\n\t_ = moda.make()\n}\n'
		'moda/moda.v': 'module moda\n\npub struct Holder {\n\tvalue Box[int]\n}\n\npub fn make() Holder {\n\treturn Holder{}\n}\n'
	}, ['main.v'], 'unknown type `Box`')
}

fn test_json_fast_paths_handle_primitives_and_stringified_composites() {
	v3_bin := build_v3()
	bool_source := 'import json

struct Flag {
	ok bool
}

fn main() {
	println(json.encode(Flag{ok: true}))
	println(json.encode(Flag{ok: false}))
}
'
	bool_encoded := run_good(v3_bin, 'json_encode_bool_without_str_helper', bool_source)
	assert bool_encoded == '{"ok":true}\n{"ok":false}'
	bool_c := gen_c(v3_bin, 'json_encode_bool_without_str_helper_c', bool_source)
	main_body := c_fn_body(bool_c, 'int main(int argc, char** argv)')
	assert !main_body.contains('bool__str(')

	encoded := run_good(v3_bin, 'json_encode_primitive_struct_fields', 'import json

struct User {
	age int
	ok bool
	score f64
}

fn main() {
	println(json.encode(User{
		age: 1
		ok: true
		score: 1.5
	}))
}
	')
	assert encoded == '{"age":1,"ok":true,"score":1.5}'

	omitempty_c := gen_c(v3_bin, 'json_encode_omitempty_field_falls_back', 'import json

struct Payload {
	keep int
	omit int @[omitempty]
}

fn main() {
	println(json.encode(Payload{
		keep: 1
	}))
}
')
	omitempty_main := c_fn_body(omitempty_c, 'int main(int argc, char** argv)')
	assert !omitempty_main.contains('json__encode(&(Payload)')
	assert omitempty_main.contains('.omit')

	decoded := run_good(v3_bin, 'json_decode_composites_to_strings', 'import json

struct Payload {
	object string
	array string
}

fn main() {
	payload := json.decode(Payload, "{\\"object\\":{},\\"array\\":[1,2]}")!
	println(payload.object)
	println(payload.array)
}
')
	assert decoded == '{}\n[1,2]'
}

fn test_json_fast_paths_accept_null_strings_and_encode_non_finite_floats() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'json_null_string_and_non_finite_floats', 'import json
import math

struct Payload {
	name string
	nan  f64
	pos  f64
	neg  f32
}

fn main() {
	decoded := json.decode(Payload, "{\\"name\\":null}")!
	println(decoded.name.len)
	println(json.encode(Payload{
		nan: math.nan()
		pos: math.inf(1)
		neg: f32(math.inf(-1))
	}))
}
')
	assert out == '0\n{"name":"","nan":null,"pos":null,"neg":null}'
}

fn test_json_encode_embedded_structs_use_fast_path_flattening() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'json_encode_embedded_struct_flattening', 'import json

struct Json3 {
	embed f64
}

struct Json2 {
	Json3
	inner []f64
}

struct Json {
	Json2
	test f64
}

fn main() {
	data := Json{
		Json2: Json2{
			Json3: Json3{
				embed: 2.0
			}
			inner: [1.0, 2.0]
		}
		test: 1.0
	}
	println(json.encode(data))
}
')
	assert out == '{"embed":2,"inner":[1,2],"test":1}'
	qualified := run_good_project(v3_bin, 'json_qualified_embedded_struct_flattening', {
		'other/other.v': 'module other\n\npub struct Inner {\npub:\n\tembed f64\n\tname string\n}\n'
		'main.v':        'module main\n\nimport json\nimport other\n\nstruct Outer {\n\tother.Inner\n\tn int\n}\n\nfn main() {\n\tdata := Outer{\n\t\tother.Inner{\n\t\t\tembed: 2.0\n\t\t\tname:  "Ada"\n\t\t}\n\t\tn: 3\n\t}\n\tprintln(json.encode(data))\n\tdecoded := json.decode(Outer, "{\\"embed\\":4.0,\\"name\\":\\"Bea\\",\\"n\\":5}")!\n\tprintln(decoded.name)\n\tprintln(int_str(int(decoded.embed)) + ":" + int_str(decoded.n))\n}\n'
	}, 'main.v')
	assert qualified == '{"embed":2,"name":"Ada","n":3}\nBea\n4:5'
}

fn test_json_encode_omitempty_field_attr_preserves_omission() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'json_encode_omitempty_field_attr', 'import json

struct User {
	name string @[omitempty]
	age int
}

fn main() {
	println(json.encode(User{
		age: 3
	}))
	println(json.encode(User{
		name: "Ada"
		age:  4
	}))
}
')
	assert out == '{"age":3}\n{"name":"Ada","age":4}'
}

fn test_json_encode_sum_types_and_composite_omitempty_fields_use_fast_path() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'json_encode_sum_types_and_composite_omitempty', 'import json

type Value = Payload | string

struct Style {
	width f64 = 2.0 @[omitempty]
	dash string = "solid" @[omitempty]
}

struct Payload {
	values []f64            @[omitempty]
	lookup map[string]string @[omitempty]
	style  Style            @[omitempty]
}

struct Envelope {
	items  []Value
	lookup map[string]Value
}

struct Holder {
	value Value @[omitempty]
}

fn main() {
	empty := Payload{}
	filled := Payload{
		values: [1.0, 2.0]
		lookup: {"kind": "line"}
		style: Style{
			width: 4.0
		}
	}
	println(json.encode(empty))
	println(json.encode(filled))
	println(json.encode([Value(filled), Value("trace")]))
	println(json.encode(Envelope{
		items: [Value(filled), Value("trace")]
		lookup: {"trace": Value(filled)}
	}))
	println(json.encode(Holder{}))
	println(json.encode(Holder{
		value: Value(filled)
	}))
}
')
	assert out == '{}\n{"values":[1,2],"lookup":{"kind":"line"},"style":{"width":4,"dash":"solid"}}\n[{"values":[1,2],"lookup":{"kind":"line"},"style":{"width":4,"dash":"solid"},"_type":"Payload"},"trace"]\n{"items":[{"values":[1,2],"lookup":{"kind":"line"},"style":{"width":4,"dash":"solid"},"_type":"Payload"},"trace"],"lookup":{"trace":{"values":[1,2],"lookup":{"kind":"line"},"style":{"width":4,"dash":"solid"},"_type":"Payload"}}}\n{}\n{"value":{"values":[1,2],"lookup":{"kind":"line"},"style":{"width":4,"dash":"solid"},"_type":"Payload"}}'
}

fn test_json_encode_json_dash_label_skips_fast_path_field() {
	v3_bin := build_v3()
	source := 'import json

struct User {
	name   string @[json: \'-\']
	secret int    @[json: \'-\']
	age    int
}

fn main() {
	println(json.encode(User{
		name:   "Ada"
		secret: 9
		age:    4
	}))
}
'
	out := run_good(v3_bin, 'json_encode_json_dash_label_skips_fast_path_field', source)
	assert out == '{"age":4}'
	c_source := gen_c(v3_bin, 'json_encode_json_dash_label_skips_fast_path_field_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv)')
	assert !main_body.contains('json__encode(&')
	assert !main_body.contains('"-":')
}

fn test_json_encode_escapes_struct_field_labels_on_fast_path() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'json_encode_escaped_struct_field_labels', 'import json

struct Packet {
	text  string @[json: \'a"b\']
	line  int    @[json: \'line\\nbreak\']
	slash bool   @[json: \'c\\\\d\']
}

fn main() {
	println(json.encode(Packet{
		text:  "ok"
		line:  2
		slash: true
	}))
}
')
	assert out == '{"a\\"b":"ok","line\\nbreak":2,"c\\\\d":true}'
}

fn test_json_encode_accepts_required_field_attr() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'json_encode_required_field_attr', 'import json

struct User {
	name string @[required]
	age int
}

fn main() {
	println(json.encode(User{
		name: "Ada"
		age:  4
	}))
}
')
	assert out == '{"name":"Ada","age":4}'
}

fn test_enum_helper_prefers_exact_free_function_over_method_suffix() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'enum_helper_exact_free_function', 'struct Maker {}

fn (m Maker) make() int {
	return 99
}

fn make() int {
	return 4
}

enum HelperKind {
	a = make()
}

fn main() {
	println(int_str(int(HelperKind.a)))
}
')
	assert out == '4'
}

fn test_enum_helper_resolves_module_const() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'enum_helper_module_const', 'const base = 4

fn make() int {
	return base
}

fn from_param(base int) int {
	return base
}

enum HelperKind {
	a = make()
	b = from_param(7)
	c
}

fn main() {
	println(int_str(int(HelperKind.a)))
	println(int_str(int(HelperKind.b)))
	println(int_str(int(HelperKind.c)))
}
')
	assert out == '4\n7\n8'
}

fn test_backed_enum_cast_qualifies_member_reference() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'backed_enum_cast_member_reference', 'const a = 1

enum BackedKind as u64 {
	a = 1
	b = u64(a) + 1
}

fn main() {
	println(int_str(int(BackedKind.b)))
}
')
	assert out == '2'
}

fn test_backed_enum_helper_initializer_is_folded() {
	v3_bin := build_v3()
	source := 'fn make() int {
	return 4
}

fn make_wide() u64 {
	return u64(1) << 40
}

enum FoldedKind as u64 {
	a = make()
	b
	wide = make_wide()
	max = 18446744073709551615
}

fn main() {
	println(int_str(int(FoldedKind.a)))
	println(int_str(int(FoldedKind.b)))
	println(u64(FoldedKind.wide))
	match FoldedKind.a {
		.a { println("a") }
		else { println("other") }
	}
}
'
	out := run_good(v3_bin, 'backed_enum_helper_initializer', source)
	assert out == '4\n5\n1099511627776\na'
	c_source := gen_c(v3_bin, 'backed_enum_helper_initializer_c', source)
	macro := c_source.split_into_lines().filter(it.starts_with('#define FoldedKind__a '))
	assert macro == ['#define FoldedKind__a ((FoldedKind)(4))']
	shift_macro := c_source.split_into_lines().filter(it.starts_with('#define FoldedKind__wide '))
	assert shift_macro == ['#define FoldedKind__wide ((FoldedKind)(1099511627776))']
	wide_macro := c_source.split_into_lines().filter(it.starts_with('#define FoldedKind__max '))
	assert wide_macro == [
		'#define FoldedKind__max ((FoldedKind)(18446744073709551615))',
	]
}

fn test_enum_helper_folding_tracks_local_declarations() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'enum_helper_immutable_local_assignment', 'fn value() int {
	x := 1
	x = 2
	return x
}

enum HelperKind {
	item = value()
}

fn main() {}
',
		'immutable, declare it with `mut`')
	source := 'fn make_local() int {
	x := 4
	mut y := x + 2
	y = y + 1
	return y
}

enum Plain {
	zero
	local = make_local()
	next
}

enum Backed as u64 {
	local = make_local()
}

fn main() {
	println(int_str(int(Plain.local)))
	println(int_str(int(Plain.next)))
	println(u64(Backed.local))
}
'
	out := run_good(v3_bin, 'enum_helper_local_declarations', source)
	assert out == '7\n8\n7'
	c_source := gen_c(v3_bin, 'enum_helper_local_declarations_c', source)
	macro := c_source.split_into_lines().filter(it.starts_with('#define Backed__local '))
	assert macro == ['#define Backed__local ((Backed)(7))']
}

fn test_enum_initializer_helper_cannot_redefine_builtin() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'enum_helper_builtin_redefinition', 'fn exit() int {
	return 9
}

enum HelperKind {
	a = exit()
	b
}

fn main() {
	println(HelperKind.a)
}
',
		'cannot redefine builtin public function `exit`')
}

fn test_enum_initializer_helper_keeps_noreturn_validation() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'enum_helper_noreturn_validation', '@[noreturn]
fn value() int {
	return 1
}

enum HelperKind {
	item = value()
}

fn main() {}
',
		'[noreturn] functions cannot use return statements')
}

fn test_json_decode_enum_accepts_name_and_label() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'json_decode_enum_name_and_label', 'import json

enum Kind {
	unknown
	field_name @[json: "wire"]
}

struct Packet {
	kind Kind
}

fn main() {
	by_name := json.decode(Packet, "{\\"kind\\":\\"field_name\\"}")!
	by_label := json.decode(Packet, "{\\"kind\\":\\"wire\\"}")!
	println(by_name.kind == .field_name)
	println(by_label.kind == .field_name)
}
')
	assert out == 'true\ntrue'
}

fn test_json_encode_escapes_enum_label() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'json_encode_escaped_enum_label', 'import json

enum Kind {
	quoted @[json: \'a"b\']
}

fn main() {
	println(json.encode(Kind.quoted))
}
')
	assert out == '"a\\"b"'
}

fn test_json_enum_label_preserves_edge_quote() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'json_enum_edge_quote_label', 'import json

enum Kind {
	fallback
	trailing @[json: \'a"\']
}

struct Packet {
	kind Kind
}

fn main() {
	encoded := json.encode(Kind.trailing)
	println(encoded)
	packet := json.decode(Packet, "{\\"kind\\":" + encoded + "}")!
	println(packet.kind == .trailing)
}
')
	assert out == '"a\\""\ntrue'
}

fn test_flag_enum_autostr_deduplicates_member_references() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'flag_enum_autostr_member_reference', '@[flag]
@[_allow_multiple_values]
enum Permission {
	a = 1
	b = .a
}

fn main() {
	println(Permission.b.str())
}
')
	assert out == 'Permission{.a}'
}

fn test_string_index_type_is_u8() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'string_index_type_is_u8',
		"fn main() {\n\ts := 'ABC'\n\tprintln(typeof(s[0]).name)\n\tprintln('\${s[2]}')\n}\n")
	assert out == 'u8\n67'
}

fn test_f32_map_and_fixed_array_stringification() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'f32_map_stringification',
		"fn main() {\n\tm := {\n\t\t'a': f32(1.5)\n\t}\n\tprintln(m)\n\tfixed := [f32(1.5), f32(2.25)]!\n\tmf := {\n\t\t'x': fixed\n\t}\n\tprintln(mf)\n}\n")
	assert out == "{'a': 1.5}\n{'x': [1.5, 2.25]}"
}

fn test_u8_map_stringification_is_numeric() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'u8_map_stringification',
		"fn main() {\n\tkeys := {\n\t\tu8(23): 'x'\n\t}\n\tvals := {\n\t\t'x': u8(23)\n\t}\n\tboth := {\n\t\tu8(65): u8(10)\n\t}\n\tprintln(keys)\n\tprintln(vals)\n\tprintln(both)\n}\n")
	assert out == "{23: 'x'}\n{'x': 23}\n{65: 10}"
}

fn test_map_equality_uses_semantic_value_comparison() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'map_semantic_value_equality',
		"struct Item {\n\tname string\n\tparts []string\n}\n\nfn join(a string, b string) string {\n\treturn a + b\n}\n\nfn main() {\n\tleft := {\n\t\t'x': Item{\n\t\t\tname: 'hello'.clone()\n\t\t\tparts: ['ab'.clone()]\n\t\t}\n\t}\n\tright := {\n\t\t'x': Item{\n\t\t\tname: join('he', 'llo')\n\t\t\tparts: [join('a', 'b')]\n\t\t}\n\t}\n\tarr_left := {\n\t\t'y': ['cd'.clone()]\n\t}\n\tarr_right := {\n\t\t'y': [join('c', 'd')]\n\t}\n\tprintln(left == right)\n\tprintln(left != right)\n\tprintln(arr_left == arr_right)\n}\n")
	assert out == 'true\nfalse\ntrue'
}

fn test_array_equality_marks_struct_operator_used() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'array_eq_struct_operator_used',
		'struct Item {\n\tvalue int\n}\n\nfn (a Item) == (b Item) bool {\n\treturn a.value % 10 == b.value % 10\n}\n\nfn main() {\n\tleft := [Item{value: 12}]\n\tright := [Item{value: 2}]\n\tprintln(left == right)\n}\n')
	assert out == 'true'
}

fn test_zero_padded_interpolation_preserves_wide_integers() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'wide_zero_padded_interpolation',
		"fn main() {\n\tbig := i64(5000000000)\n\tubig := u64(18446744073709551615)\n\tsmall := u64(42)\n\tprintln('\${big:012d}')\n\tprintln('\${ubig:020d}')\n\tprintln('\${small:08d}')\n}\n")
	assert out == '005000000000\n18446744073709551615\n00000042'
}

fn test_formatted_interpolation_rune_and_long_float() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'formatted_interpolation_rune_and_long_float',
		"fn main() {\n\tr := '\${rune(0x20ac):c}'\n\tprintln(int_str(r.len))\n\tprintln(int_str(int(r[0])) + ',' + int_str(int(r[1])) + ',' + int_str(int(r[2])))\n\tlong := '\${1.0:.200f}'\n\tprintln(int_str(long.len))\n\tprintln(int_str(int(long[0])) + ',' + int_str(int(long[1])) + ',' + int_str(int(long[2])) + ',' + int_str(int(long[long.len - 1])))\n\tprintln('\${238.5:0.0f}')\n\tprintln('\${239.5555555:0.6f}')\n}\n")
	assert out == '3\n226,130,172\n202\n49,46,48,48\n239\n239.555556'
}

fn test_formatted_interpolation_integer_alias_character_code() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'formatted_interpolation_integer_alias_character_code',
		"type Code = u8\ntype SignedCode = i16\ntype NestedCode = Code\n\nfn main() {\n\tprintln('\${Code(65):c}\${SignedCode(66):c}\${NestedCode(67):c}')\n}\n")
	assert out == 'ABC'
}

fn test_formatted_interpolation_alias_uses_string_representation() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'formatted_interpolation_alias_string',
		'import time\n\nfn main() {\n\tduration := time.Duration(10)\n\tprintln("|\${duration:10s}|")\n}\n')
	assert out == '|      10ns|'
}

fn test_callback_pointer_return_is_compatible_with_voidptr_return() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'callback_pointer_return_to_voidptr',
		'struct Item {\n\tvalue int\n}\n\nstruct Config {\n\tcallback fn () voidptr\n}\n\nfn make_item() &Item {\n\treturn &Item{value: 42}\n}\n\nfn main() {\n\tconfig := Config{callback: make_item}\n\titem := unsafe { &Item(config.callback()) }\n\tprintln(item.value)\n}\n')
	assert out == '42'
}

fn test_stats_reports_failed_test_status_and_passed_total() {
	v3_bin := build_v3()
	source := '${tmp_test_path('stats_failed_test_status')}_test.v'
	os.write_file(source,
		'fn test_fails() {\n\tassert false\n}\n\nfn test_passes() {\n\tassert true\n}\n') or {
		panic(err)
	}
	outer_run_only := os.getenv_opt('VTEST_ONLY_FN')
	os.unsetenv('VTEST_ONLY_FN')
	defer {
		if value := outer_run_only {
			os.setenv('VTEST_ONLY_FN', value, true)
		} else {
			os.unsetenv('VTEST_ONLY_FN')
		}
	}
	result := cmdexec.run(v3_bin, ['-nocache', '-no-memory-limit', '-stats', 'test', source])
	assert result.exit_code != 0
	assert result.output.contains('     FAIL  [1/2]'), result.output
	assert result.output.contains('     OK    [2/2]'), result.output
	assert result.output.contains('1 failed, 1 passed, 2 total'), result.output
	assert !result.output.contains('2 passed, 2 total'), result.output
}

fn test_driver_accepts_cdebug_alias() {
	v3_bin := build_v3()
	out := run_good_with_flags(v3_bin, 'cdebug_alias', '-nocache -cdebug',
		"fn main() {\n\t\$if debug {\n\t\tprintln('debug')\n\t} \$else {\n\t\tprintln('release')\n\t}\n}\n")
	assert out == 'debug'
}

fn test_alias_interface_str_dispatch_marks_alias_method_used() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'alias_interface_str_dispatch',
		"interface Printer {\n\tstr() string\n}\n\ntype Label = int\n\nfn (l Label) str() string {\n\treturn 'label:' + int_str(int(l))\n}\n\nfn make() Printer {\n\tl := Label(7)\n\treturn l\n}\n\nfn main() {\n\tp := make()\n\tprintln('\${p}')\n}\n")
	assert out == 'label:7'
}

fn test_implicit_interface_str_dispatch_uses_boxed_receiver() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_receiver', 'interface Printable {
	str() string
}

struct Foo {
	x int
}

fn main() {
	value := Printable(Foo{
		x: 7
	})
	println(value.str())
}
')
	assert out == 'Foo{\n    x: 7\n}'
}

fn test_implicit_interface_str_dispatch_accepts_generic_struct() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_generic_struct', 'interface Printable {
	str() string
}

struct Box[T] {
	value T
}

fn main() {
	value := Printable(Box[int]{
		value: 7
	})
	println(value.str())
}
')
	assert out == 'Box[int]{\n    value: 7\n}'
}

fn test_implicit_interface_str_dispatch_stringifies_enum() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_enum', 'interface Printable {
	str() string
}

enum Color {
	red
	blue
}

fn main() {
	value := Printable(Color.red)
	println(value.str())
}
')
	assert out == 'red'
}

fn test_implicit_interface_str_dispatch_stringifies_struct_alias() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_struct_alias', 'interface Printable {
	str() string
}

struct Foo {
	x int
}

type AliasFoo = Foo

fn main() {
	aliased := AliasFoo(Foo{
		x: 7
	})
	value := Printable(aliased)
	println(value.str())
}
')
	assert out.contains('x: 7')
	assert !out.contains('Foo{}')
}

fn test_implicit_interface_str_dispatch_stringifies_collection_aliases() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_collection_aliases', 'interface Printable {
	str() string
}

type Items = []int
type Counts = map[string]int
type Pair = [2]int
type Words = []string

fn main() {
	items := Printable(Items([1, 2]))
	counts := Printable(Counts({
		"a": 3
	}))
	pair := Printable(Pair([4, 5]!))
	words := Printable(Words(["x", "y"]))
	println(items.str())
	println(counts.str())
	println(pair.str())
	println(words.str())
}
')
	assert out == "[1, 2]\n{'a': 3}\n[4, 5]\n['x', 'y']"
}

fn test_empty_interface_str_dispatch_stringifies_boxed_map() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'empty_interface_str_dispatch_boxed_map', 'interface Any {}

fn show(value Any) string {
	return "\${value}"
}

fn main() {
	value := Any({
		"answer": 42
	})
	println(show(value))
}
')
	assert out == "Any({'answer': 42})"
}

fn test_implicit_interface_str_dispatch_rejects_sum_without_dispatch_id() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'implicit_interface_str_dispatch_rejects_sum', 'interface Printable {
	str() string
}

type Value = int | string

fn main() {
	value := Value(1)
	_ := Printable(value)
}
',
		'does not implement interface')
}

fn test_implicit_interface_str_dispatch_stringifies_nested_struct_fields() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_nested_struct', 'interface Printable {
	str() string
}

struct Bar {
	x int
}

struct Foo {
	bar Bar
}

fn main() {
	value := Printable(Foo{
		bar: Bar{
			x: 7
		}
	})
	println(value.str())
}
')
	assert out == 'Foo{\n    bar: Bar{\n        x: 7\n    }\n}'
}

fn test_implicit_interface_str_dispatch_stringifies_collection_fields() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_collections', 'interface Printable {
	str() string
}

struct Foo {
	nums []int
	labels map[string]int
	fixed [2]int
	words []string
}

fn main() {
	value := Printable(Foo{
		nums: [1, 2]
		labels: {
			"a": 3
		}
		fixed: [4, 5]!
		words: ["x", "y"]
	})
	println(value.str())
}
')
	assert out == "Foo{\n    nums: [1, 2]\n    labels: {'a': 3}\n    fixed: [4, 5]\n    words: ['x', 'y']\n}"
}

fn test_implicit_interface_str_dispatch_stringifies_typed_map_fields() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_typed_map_fields', 'interface Printable {
	str() string
}

struct Bar {
	x int
}

struct Foo {
	m map[string]Bar
}

fn main() {
	value := Printable(Foo{
		m: {
			"one": Bar{
				x: 7
			}
		}
	})
	println(value.str())
}
')
	assert out.contains("'one': Bar{"), out
	assert out.contains('x: 7'), out
	assert !out.contains('<map value>'), out
	assert !out.contains('Bar{}'), out
}

fn test_implicit_interface_str_dispatch_stringifies_optional_fields() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_optional_fields', 'interface Printable {
	str() string
}

struct Foo {
	present ?int
	missing ?int
	text    ?string
}

fn main() {
	value := Printable(Foo{
		present: ?int(7)
		text: ?string("hi")
	})
	println(value.str())
}
')
	assert out.contains('present: Option(7)'), out
	assert out.contains('missing: Option(none)'), out
	assert out.contains("text: Option('hi')"), out
	assert !out.contains('?int{}'), out
}

fn test_implicit_interface_str_dispatch_unaliases_field_types() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_aliased_fields', 'interface Printable {
	str() string
}

struct Bar {
	x int
}

type MyBar = Bar
type MyNums = []int
type MyLabels = map[string]int
type MyFixed = [2]int

struct Foo {
	bar MyBar
	nums MyNums
	labels MyLabels
	fixed MyFixed
}

fn main() {
	value := Printable(Foo{
		bar: MyBar(Bar{
			x: 7
		})
		nums: MyNums([1, 2])
		labels: MyLabels({
			"a": 3
		})
		fixed: MyFixed([4, 5]!)
	})
	println(value.str())
}
')
	assert out == "Foo{\n    bar: Bar{\n        x: 7\n    }\n    nums: [1, 2]\n    labels: {'a': 3}\n    fixed: [4, 5]\n}"
}

fn test_implicit_interface_str_dispatch_preserves_pointer_field_custom_str() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_pointer_custom_str', 'interface Printable {
	str() string
}

struct Bar {
	x int
}

fn (b Bar) str() string {
	return "value:" + int_str(b.x)
}

struct Baz {
	x int
}

fn (b &Baz) str() string {
	return "ptr:" + int_str(b.x)
}

struct Foo {
	bar &Bar
	baz &Baz
}

fn main() {
	bar := &Bar{
		x: 7
	}
	baz := &Baz{
		x: 9
	}
	value := Printable(Foo{
		bar: bar
		baz: baz
	})
	println(value.str())
}
')
	assert out == 'Foo{\n    bar: value:7\n    baz: ptr:9\n}'
}

fn test_implicit_interface_str_dispatch_preserves_pointer_alias_custom_str() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_pointer_alias_custom_str', 'interface Printable {
	str() string
}

type Name = string

fn (n Name) str() string {
	return "name:" + string(n)
}

type Code = int

fn (c &Code) str() string {
	return "code:" + int_str(int(*c))
}

struct Foo {
	name    &Name
	missing &Name
	code    &Code
}

fn main() {
	name := Name("Ada")
	code := Code(7)
	value := Printable(Foo{
		name: &name
		missing: unsafe { nil }
		code: &code
	})
	println(value.str())
}
')
	assert out == 'Foo{\n    name: name:Ada\n    missing: &nil\n    code: code:7\n}'
}

fn test_implicit_interface_str_dispatch_preserves_pointer_alias_receiver_custom_str() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_pointer_alias_receiver_custom_str', 'interface Printable {
	str() string
}

struct Bar {
	x int
}

type Ref = &Bar

fn (r Ref) str() string {
	return "ref:" + int_str(r.x)
}

type RefBox = &Bar

fn (r &RefBox) str() string {
	value := *r
	return "refbox:" + int_str(value.x)
}

struct Foo {
	ref    Ref
	refbox RefBox
}

fn main() {
	bar := &Bar{
		x: 7
	}
	other := &Bar{
		x: 9
	}
	value := Printable(Foo{
		ref: bar
		refbox: other
	})
	println(value.str())
}
')
	assert out == 'Foo{\n    ref: ref:7\n    refbox: refbox:9\n}'
}

fn test_implicit_interface_str_dispatch_dereferences_pointer_struct_fields() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_pointer_struct_fields', 'interface Printable {
	str() string
}

struct Bar {
	x int
}

struct Foo {
	bar   &Bar
	empty &Bar
}

fn main() {
	bar := &Bar{
		x: 7
	}
	value := Printable(Foo{
		bar: bar
		empty: unsafe { nil }
	})
	println(value.str())
}
')
	assert out == 'Foo{\n    bar: Bar{\n        x: 7\n    }\n    empty: &nil\n}'
}

fn test_implicit_interface_str_dispatch_treats_pointer_alias_fields_as_pointers() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_pointer_alias_struct_fields', 'interface Printable {
	str() string
}

struct Bar {
	x int
}

type BarRef = &Bar

struct Foo {
	bar   BarRef
	empty BarRef
}

fn main() {
	bar := &Bar{
		x: 7
	}
	value := Printable(Foo{
		bar: bar
		empty: unsafe { nil }
	})
	println(value.str())
}
')
	assert out == 'Foo{\n    bar: Bar{\n        x: 7\n    }\n    empty: &nil\n}'
}

fn test_bare_aligned_attribute_metadata_and_cgen() {
	v3_bin := build_v3()
	source := '@[aligned]
struct Bare {
	x int
}

@[aligned; markused]
struct Marked {
	y int
}

fn make_alias() &Bare {
	x := Bare{
		x: 4
	}
	p := &x
	return p
}

	fn make_direct() &Bare {
		x := Bare{
			x: 5
		}
		return &x
	}

	fn make_param_alias(x Bare) &Bare {
		p := &x
		return p
	}

	fn main() {
		b := Bare{
		x: 1
	}
	m := Marked{
		y: 2
	}
		h := &Bare{
			x: 3
		}
		a := make_alias()
		d := make_direct()
		pa := make_param_alias(Bare{
			x: 6
		})
		base := Bare{
			x: 7
		}
		ha := &Bare{
			...base
			x: 8
		}
		println(int_str(b.x + m.y + h.x + a.x + d.x + pa.x + ha.x))
		unsafe {
			free(h)
			free(a)
			free(d)
			free(pa)
			free(ha)
		}
	}
	'
	c_source := gen_c(v3_bin, 'bare_aligned_attribute_metadata', source)
	assert c_source.contains('__attribute__((aligned))')
	assert !c_source.contains('aligned(aligned)')
	assert c_source.contains('_aligned_malloc((size_t)sz, alignment)')
	assert c_source.contains('static inline void v3_aligned_free(void* p)')
	assert !c_source.contains('uintptr_t raw = (uintptr_t)malloc')
	assert c_source.contains('v3_aligned_free(h)')
	assert c_source.contains('v3_aligned_free(a)')
	assert c_source.contains('v3_aligned_free(d)')
	assert c_source.contains('v3_aligned_free(pa)')
	assert c_source.contains('v3_aligned_free(ha)')
	assert c_source.contains('v3_aligned_memdup(&x, sizeof(main__Bare), __alignof__(main__Bare))')
	make_direct_body := c_fn_body(c_source, 'main__Bare* make_direct(void) {')
	assert make_direct_body.contains('v3_aligned_memdup(&(main__Bare){.x = 5}, sizeof(main__Bare), __alignof__(main__Bare))'), make_direct_body
	assert !c_source.contains('__alignof__(Bare)')
	assert c_source.contains('v3_aligned_memdup(&__assoc_')
	out := run_good(v3_bin, 'bare_aligned_attribute_cgen', source)
	assert out == '29'
}

fn test_implicit_interface_str_dispatch_dereferences_pointer_scalar_fields() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_pointer_scalar_fields', 'interface Printable {
	str() string
}

struct Foo {
	p &int
	s &string
}

fn main() {
	n := 7
	text := "hi"
	value := Printable(Foo{
		p: &n
		s: &text
	})
	println(value.str())
}
')
	assert out == "Foo{\n    p: 7\n    s: 'hi'\n}"
}

fn test_implicit_interface_str_dispatch_dereferences_pointer_collection_fields() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'implicit_interface_str_dispatch_pointer_collection_fields', 'interface Printable {
	str() string
}

struct Foo {
	nums &[]int
	labels &map[string]int
	fixed &[2]int
	words &[]string
}

fn main() {
	nums := [1, 2]
	labels := {
		"a": 3
	}
	fixed := [4, 5]!
	words := ["x", "y"]
	value := Printable(Foo{
		nums: &nums
		labels: &labels
		fixed: unsafe { &fixed }
		words: &words
	})
	println(value.str())
}
')
	assert out == "Foo{\n    nums: [1, 2]\n    labels: {'a': 3}\n    fixed: [4, 5]\n    words: ['x', 'y']\n}"
}

fn test_map_pointer_alias_cast_preserves_existing_pointer() {
	v3_bin := build_v3()
	source := 'type M = map[string]int

fn keep(p &M) &M {
	return &M(p)
}

fn from_void(p voidptr) &M {
	return &M(p)
}

fn main() {
	mut m := {
		"a": 1
	}
	p := keep(unsafe { &M(&m) })
	q := from_void(voidptr(p))
	unsafe {
		(*q)["b"] = 2
	}
	println(int_str(m["b"]))
}
'
	c_source := gen_c(v3_bin, 'map_pointer_alias_cast_preserves_existing_pointer_c', source)
	body := c_fn_body(c_source, 'map* keep(map* p) {')
	assert body.contains('return (map*)(p);'), body
	assert !body.contains('map _t') && !body.contains('&_t') && !body.contains('&p'), body
	void_body := c_fn_body(c_source, 'map* from_void(void* p) {')
	assert void_body.contains('return (map*)(p);'), void_body
	assert !void_body.contains('&p'), void_body
	out := run_good(v3_bin, 'map_pointer_alias_cast_preserves_existing_pointer', source)
	assert out == '2'
}

fn test_empty_interface_is_matches_alias_equivalent_type_ids() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'empty_interface_alias_type_id',
		'interface Any {}\n\ntype MyInt = int\n\nfn main() {\n\tvalue := MyInt(1)\n\ta := Any(value)\n\tprintln((a is MyInt).str())\n\tprintln((a is int).str())\n\tplain := int(2)\n\tb := Any(plain)\n\tprintln((b is MyInt).str())\n\tprintln((b is int).str())\n}\n')
	assert out == 'true\ntrue\ntrue\ntrue'
}

fn test_empty_interface_box_preserves_enum_type_id() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'empty_interface_enum_type_id',
		'interface Any {}\n\nenum Color {\n\tred\n\tblue\n}\n\nfn main() {\n\tx := Any(Color.red)\n\tprintln((x is Color).str())\n\tprintln((x is int).str())\n}\n')
	assert out == 'true\nfalse'
}

fn test_interface_cast_rejects_pointer_shape_mismatch() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'interface_pointer_shape_mismatch',
		'interface Sink {\n\tput(x &int)\n}\n\nstruct Bad {}\n\nfn (b Bad) put(x int) {}\n\nfn main() {\n\t_ := Sink(Bad{})\n}\n',
		'does not implement interface')
	run_bad(v3_bin, 'interface_voidptr_cast_rejected',
		'interface Sink {\n\tput()\n}\n\nfn main() {\n\tx := 1\n\tp := voidptr(&x)\n\t_ := Sink(p)\n}\n',
		'does not implement interface')
	pointer_escape_out := run_good(v3_bin, 'interface_pointer_voidptr_cast_escape_hatch',
		'interface Sink {\n\tput()\n}\n\nfn main() {\n\tp := unsafe { voidptr(0) }\n\t_ := &Sink(p)\n\tprintln("ok")\n}\n')
	assert pointer_escape_out == 'ok'
	run_bad(v3_bin, 'interface_alias_cast_non_implementer',
		'interface Sink {\n\tput()\n}\n\ntype SinkAlias = Sink\n\nstruct Bad {}\n\nfn main() {\n\t_ := SinkAlias(Bad{})\n}\n',
		'does not implement interface')
	nil_out := run_good(v3_bin, 'interface_pointer_nil_cast',
		"interface Sink {\n\tput()\n}\n\ntype SinkAlias = Sink\n\nfn main() {\n\t_ := Sink(nil)\n\t_ := &Sink(nil)\n\t_ := &SinkAlias(nil)\n\tprintln('ok')\n}\n")
	assert nil_out == 'ok'
	nil_arg_out := run_good(v3_bin, 'interface_pointer_nil_argument',
		"interface Item {\n\tname string\n}\n\nfn take(item &Item) {\n\tassert item == unsafe { nil }\n}\n\nfn main() {\n\tvalue := unsafe { nil }\n\ttake(value)\n\ttake(unsafe { nil })\n\tprintln('ok')\n}\n")
	assert nil_arg_out == 'ok'
}

fn test_interface_is_unqualified_local_uses_exact_impl_id() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'interface_is_local_exact_impl_id', {
		'v.mod':           "Module { name: 'interface_is_local_exact_impl_id' }\n"
		'common/common.v': 'module common\n\npub interface Actor {\n\ttag() int\n}\n'
		'a/a.v':           'module a\n\npub struct Foo {}\n\npub fn (f Foo) tag() int {\n\treturn 1\n}\n'
		'b/b.v':           'module b\n\nimport a\nimport common\n\npub struct Foo {}\n\npub fn (f Foo) tag() int {\n\treturn 2\n}\n\npub fn make_local() common.Actor {\n\treturn Foo{}\n}\n\npub fn make_a() common.Actor {\n\treturn a.Foo{}\n}\n\npub fn is_local_actor(actor common.Actor) bool {\n\treturn actor is Foo\n}\n'
		'main.v':          'module main\n\nimport b\n\nfn main() {\n\tprintln(b.is_local_actor(b.make_local()).str())\n\tprintln(b.is_local_actor(b.make_a()).str())\n}\n'
	}, 'main.v')
	assert out == 'true\nfalse'
}

fn test_callback_lambda_lift_preserves_outer_captures() {
	v3_bin := build_v3()
	no_arg_out := run_good(v3_bin, 'callback_no_arg_lambda_lift_preserves_capture',
		'fn apply(cb fn () int) int {\n\treturn cb()\n}\n\nfn main() {\n\tvalue := 41\n\tprintln(int_str(apply(|| value + 1)))\n}\n')
	assert no_arg_out == '42'
	out := run_good(v3_bin, 'callback_lambda_lift_preserves_capture',
		'fn apply(cb fn (int) int, n int) int {\n\treturn cb(n)\n}\n\nfn main() {\n\toffset := 7\n\tprintln(int_str(apply(|n| n + offset, 5)))\n}\n')
	assert out == '12'
	callee_out := run_good(v3_bin, 'callback_lambda_lift_preserves_fn_callee_capture',
		'fn apply(cb fn (int) int, n int) int {\n\treturn cb(n)\n}\n\nfn double(n int) int {\n\treturn n * 2\n}\n\nfn main() {\n\tcb := double\n\tprintln(int_str(apply(|n| cb(n), 6)))\n}\n')
	assert callee_out == '12'
}

fn test_callback_lambda_lift_forwards_optional_void_failures() {
	v3_bin := build_v3()
	result_out := run_good(v3_bin, 'callback_lambda_result_void_forward',
		'fn takes(cb fn () !void) {\n\tcb() or {\n\t\tprintln(err.msg())\n\t\treturn\n\t}\n\tprintln("success")\n}\n\nfn maybe_fails() !void {\n\treturn error("fail")\n}\n\nfn main() {\n\ttakes(|| maybe_fails())\n}\n')
	assert result_out == 'fail'
	option_out := run_good(v3_bin, 'callback_lambda_option_void_forward',
		'fn takes(cb fn () ?) {\n\tcb() or {\n\t\tprintln("none")\n\t\treturn\n\t}\n\tprintln("some")\n}\n\nfn maybe_none() ? {\n\treturn none\n}\n\nfn main() {\n\ttakes(|| maybe_none())\n}\n')
	assert option_out == 'none'
}

fn test_user_new_map_call_with_args_uses_renamed_symbol() {
	v3_bin := build_v3()
	source := 'fn new_map(x int) int {\n\treturn x + 1\n}\n\nfn main() {\n\tprintln(int_str(new_map(41)))\n}\n'
	c_source := gen_c(v3_bin, 'user_new_map_call_with_args', source)
	assert c_source.contains('main__new_map(41)'), c_source
	assert !c_source.contains('(new_map(41)'), c_source
	out := run_good(v3_bin, 'user_new_map_call_with_args_run', source)
	assert out == '42'
}

fn test_amp_interface_cast_heap_copies_concrete_source() {
	v3_bin := build_v3()
	source := 'interface Reader {\n\tvalue() int\n}\n\nstruct Box {\n\tn int\n}\n\nfn (b Box) value() int {\n\treturn b.n\n}\n\nfn make() &Reader {\n\tb := Box{\n\t\tn: 5\n\t}\n\treturn &Reader(b)\n}\n\nfn main() {\n\tr := make()\n\tprintln(int_str(r.value()))\n}\n'
	c_source := gen_c(v3_bin, 'amp_interface_cast_heap_copy', source)
	make_body := c_fn_body(c_source, '\nReader* make(void) {')
	assert make_body.contains('._object = (main__Box*)(memdup(&b, sizeof(main__Box)))')
	assert make_body.contains('memdup(&(Reader){')
	assert !make_body.contains('__iface_box_')
	out := run_good(v3_bin, 'amp_interface_cast_heap_copy_run', source)
	assert out == '5'
}

fn test_interface_cast_from_local_address_preserves_pointer_identity() {
	v3_bin := build_v3()
	source := 'interface Reader {\n\tget() int\n}\n\nstruct Box {\nmut:\n\tn int\n}\n\nfn (b &Box) get() int {\n\treturn b.n\n}\n\nfn main() {\n\tmut b := Box{\n\t\tn: 1\n\t}\n\tr := Reader(&b)\n\tb.n = 2\n\tprintln(int_str(r.get()))\n}\n'
	c_source := gen_c(v3_bin, 'interface_local_address_identity', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv)')
	assert main_body.contains('._object = __iface_src_'), main_body
	assert !main_body.contains('memdup(&b, sizeof(Box))'), main_body
	out := run_good(v3_bin, 'interface_local_address_identity_run', source)
	assert out == '2'
	mut_source := 'interface Writer {\nmut:\n\tset(n int)\n\tget() int\n}\n\nstruct Box {\nmut:\n\tn int\n}\n\nfn (mut b Box) set(n int) {\n\tb.n = n\n}\n\nfn (b Box) get() int {\n\treturn b.n\n}\n\nfn main() {\n\tmut b := Box{\n\t\tn: 1\n\t}\n\tmut w := Writer(&b)\n\tw.set(3)\n\tprintln(int_str(b.n))\n\tprintln(int_str(w.get()))\n}\n'
	mut_out := run_good(v3_bin, 'interface_local_address_identity_mut_run', mut_source)
	assert mut_out == '3\n3'
	escape_source := 'interface Reader {\n\tget() int\n}\n\nstruct Box {\nmut:\n\tn int\n}\n\nfn (b &Box) get() int {\n\treturn b.n\n}\n\nfn make_reader() Reader {\n\tmut b := Box{\n\t\tn: 5\n\t}\n\tr := Reader(&b)\n\tb.n = 6\n\treturn r\n}\n\nfn main() {\n\tr := make_reader()\n\tprintln(int_str(r.get()))\n}\n'
	escape_c := gen_c(v3_bin, 'interface_local_address_escape_box', escape_source)
	make_reader_body := c_fn_body(escape_c, '\nReader make_reader(void) {')
	assert make_reader_body.contains('Box* b ='), make_reader_body
	assert make_reader_body.contains('memdup'), make_reader_body
	assert !make_reader_body.contains('Box b ='), make_reader_body
	escape_out := run_good(v3_bin, 'interface_local_address_escape_box_run', escape_source)
	assert escape_out == '6'
	escape_variants := run_good(v3_bin, 'interface_local_address_escape_variants', 'interface Reader {
	get() int
}

struct Box {
mut:
	n int
}

struct Holder {
mut:
	inner Box
}

struct StaticReader {
	n int
}

__global fallback Box
__global global_box Box

fn (b &Box) get() int {
	return b.n
}

fn (s StaticReader) get() int {
	return s.n
}

fn make_aggregate_reader() []Reader {
	mut b := Box{
		n: 5
	}
	r := Reader(&b)
	b.n = 6
	return [r]
}

fn make_assigned_reader() Reader {
	mut b := Box{
		n: 7
	}
	mut r := Reader(StaticReader{
		n: 0
	})
	r = Reader(&b)
	b.n = 8
	return r
}

fn make_pointer_alias_reader() Reader {
	mut b := Box{
		n: 9
	}
	p := &b
	q := p
	r := Reader(q)
	b.n = 10
	return r
}

fn make_field_reader() Reader {
	mut holder := Holder{
		inner: Box{
			n: 11
		}
	}
	r := Reader(&holder.inner)
	holder.inner.n = 12
	return r
}

fn make_pointer_alias_field_reader() Reader {
	mut holder := Holder{
		inner: Box{
			n: 18
		}
	}
	p := &holder
	q := p
	r := Reader(&q.inner)
	holder.inner.n = 19
	return r
}

fn make_conditional_reader(use_global bool) Reader {
	mut b := Box{
		n: 13
	}
	mut r := Reader(&b)
	if use_global {
		r = Reader(&fallback)
	}
	b.n = 14
	return r
}

fn make_global_reader() Reader {
	r := Reader(&global_box)
	return r
}

fn main() {
	fallback = Box{
		n: 15
	}
	global_box = Box{
		n: 16
	}
	global_reader := make_global_reader()
	global_box.n = 17
	println(int_str(make_aggregate_reader()[0].get()))
	println(int_str(make_assigned_reader().get()))
	println(int_str(make_pointer_alias_reader().get()))
	println(int_str(make_field_reader().get()))
	println(int_str(make_pointer_alias_field_reader().get()))
	println(int_str(make_conditional_reader(false).get()))
	println(int_str(make_conditional_reader(true).get()))
	println(int_str(global_reader.get()))
}
')
	assert escape_variants == '6\n8\n10\n12\n19\n14\n15\n17'
}

fn test_mut_interface_argument_borrows_existing_interface_box() {
	v3_bin := build_v3()
	source := 'interface Visitor {\n\tvalue() int\nmut:\n\tvisit()\n}\n\nstruct Counter {\nmut:\n\tn int\n}\n\nfn (c Counter) value() int {\n\treturn c.n\n}\n\nfn (mut c Counter) visit() {\n\tc.n++\n}\n\nfn call(mut visitor Visitor) {\n\tvisitor.visit()\n}\n\nfn main() {\n\tmut visitor := Visitor(Counter{})\n\tcall(mut visitor)\n\tprintln(int_str(visitor.value()))\n}\n'
	c_source := gen_c(v3_bin, 'mut_interface_arg_borrows_existing_box', source)
	assert c_source.contains('call(&visitor);')
	assert !c_source.contains('call((Visitor*)(memdup(&__iface_box_')
	out := run_good(v3_bin, 'mut_interface_arg_borrows_existing_box_run', source)
	assert out == '1'

	assign_source := 'interface Base {
	get() int
}

struct Item {
	n int
}

fn (i Item) get() int {
	return i.n
}

fn update(mut x Base) {
	x = Base(Item{
		n: 7
	})
}

fn main() {
	mut b := Base(Item{
		n: 1
	})
	update(mut b)
	println(int_str(b.get()))
}
'
	assign_c := gen_c(v3_bin, 'mut_interface_arg_assignment_keeps_storage', assign_source)
	assert assign_c.contains('update(&b);')
	assert !assign_c.contains('update(&((Base[]){b})[0]);')
	assign_out := run_good(v3_bin, 'mut_interface_arg_assignment_keeps_storage_run', assign_source)
	assert assign_out == '7'
}

fn test_pointer_interface_arg_heap_copies_rvalue_interface_sources() {
	v3_bin := build_v3()
	source := 'interface Value {\n\tget() int\n}\n\nstruct Item {\n\tn int\n}\n\nfn (i Item) get() int {\n\treturn i.n\n}\n\nstruct Holder {\n\titem Value\n}\n\nfn make_holder() Holder {\n\treturn Holder{\n\t\titem: Value(Item{\n\t\t\tn: 7\n\t\t})\n\t}\n}\n\nfn make_items() []Value {\n\treturn [Value(Item{\n\t\tn: 9\n\t})]\n}\n\nfn use(value &Value) int {\n\treturn value.get()\n}\n\nfn main() {\n\tprintln(int_str(use(make_holder().item)))\n\tprintln(int_str(use(make_items()[0])))\n}\n'
	c_source := gen_c(v3_bin, 'pointer_interface_rvalue_sources', source)
	assert c_source.contains('memdup(&__iface_box_')
	out := run_good(v3_bin, 'pointer_interface_rvalue_sources_run', source)
	assert out == '7\n9'
}

fn test_pointer_interface_cast_heap_copies_converted_interface_source() {
	v3_bin := build_v3()
	source := 'interface Base {\n\tget() int\n}\n\ninterface Narrow {\n\tBase\n\textra() int\n}\n\nstruct Item {\n\tn int\n}\n\nfn (i Item) get() int {\n\treturn i.n\n}\n\nfn (i Item) extra() int {\n\treturn i.n + 1\n}\n\nfn make_narrow() Narrow {\n\treturn Item{\n\t\tn: 11\n\t}\n}\n\nfn use(value &Base) int {\n\treturn value.get()\n}\n\nfn make_base() &Base {\n\tnarrow := make_narrow()\n\treturn &Base(narrow)\n}\n\nfn main() {\n\tprintln(int_str(use(make_base())))\n}\n'
	c_source := gen_c(v3_bin, 'pointer_interface_converted_source', source)
	assert c_source.contains('Base __iface_cast_')
	assert c_source.contains('return (Base*)(memdup(&__iface_box_')
	out := run_good(v3_bin, 'pointer_interface_converted_source_run', source)
	assert out == '11'
}

fn test_c_atomic_pointer_load_store_preserves_pointer_width() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'c_atomic_pointer_load_store',
		'fn C.atomic_load_ptr(voidptr) voidptr\nfn C.atomic_store_ptr(voidptr, voidptr)\n\nfn main() {\n\tvalue := 9\n\tmut slot := unsafe { nil }\n\tC.atomic_store_ptr(voidptr(&slot), voidptr(&value))\n\tprintln((C.atomic_load_ptr(voidptr(&slot)) == voidptr(&value)).str())\n}\n')
	assert out == 'true'
}

fn test_native_arm64_atomic_pointer_fetch_add_sub() {
	$if macos && arm64 {
		v3_bin := build_v3()
		out := run_good_backend(v3_bin, 'native_atomic_pointer_fetch_add_sub', 'arm64',
			'fn C.atomic_fetch_add_ptr(voidptr, voidptr) voidptr\nfn C.atomic_fetch_sub_ptr(voidptr, voidptr) voidptr\n\nfn main() {\n\tmut vals := [10, 20, 30]!\n\tmut p := voidptr(unsafe { &vals[0] })\n\told := C.atomic_fetch_add_ptr(voidptr(&p), voidptr(sizeof(int)))\n\tprintln(old == voidptr(unsafe { &vals[0] }))\n\tprintln(p == voidptr(unsafe { &vals[1] }))\n\told2 := C.atomic_fetch_sub_ptr(voidptr(&p), voidptr(sizeof(int)))\n\tprintln(old2 == voidptr(unsafe { &vals[1] }))\n\tprintln(p == voidptr(unsafe { &vals[0] }))\n}\n')
		assert out == 'true\ntrue\ntrue\ntrue'
	}
}

fn test_anonymous_struct_literals_use_typed_shape() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'anonymous_struct_literal_typed_shape', 'fn take_int(value struct {
	x int
}) int {
	return value.x
}

fn take_string(value struct {
	x string
}) string {
	return value.x
}

fn take_i64(value struct {
	x i64
}) i64 {
	return value.x
}

fn take_grouped(value struct {
	x, y int
}) int {
	return value.x * 10 + value.y
}

fn call_late_i64() i64 {
	return take_late_i64(struct { x: 11 })
}

fn take_late_i64(value struct {
	x i64
}) i64 {
	return value.x
}

fn main() {
	println(int_str(take_int(struct { x: 7 })))
	println(take_string(struct { x: "right" }))
	println(take_i64(struct { x: i64(9) }))
	println(int_str(take_grouped(struct { x: 2, y: 3 })))
	println(call_late_i64().str())
	mut values := []struct {
		x int
	}{}
	values << struct { x: 13 }
	println(int_str(values[0].x))
}
	')
	assert out == '7\nright\n9\n23\n11\n13'
	inferred_out := run_good(v3_bin, 'anonymous_struct_inferred_literal_typed_shape',
		'fn main() {\n\ta := struct { x: 1 }\n\tb := struct { x: "typed" }\n\tprintln(int_str(a.x))\n\tprintln(b.x)\n}\n')
	assert inferred_out == '1\ntyped'
}

fn test_anonymous_struct_type_allows_volatile_field_name() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'anonymous_struct_volatile_field_name', 'fn read(value struct {
	volatile u8
}) u8 {
	return value.volatile
}

fn main() {
	value := struct { volatile: u8(73) }
	println(read(value))
}
')
	assert out == '73'
}

fn test_latest_pr_review_codegen_regressions() {
	v3_bin := build_v3()
	small_int_comparison := run_good(v3_bin, 'parenthesized_small_int_comparison', 'fn main() {
	println(((u8(255) + u8(1)) == 0).str())
	println((((u8(255) + u8(1))) == 0).str())
}
')
	assert small_int_comparison == 'true\ntrue'

	c_strings := run_good(v3_bin, 'single_char_c_string_pointer_context', "fn C.strlen(charptr) usize

fn main() {
	println(C.strlen(c'\\n'))
	println(C.strlen((c'\\n')))
}
")
	assert c_strings == '1\n1'

	ierror_selector := run_good(v3_bin, 'temporary_ierror_selector_equality', "struct ErrorHolder {
	err IError
}

fn make_error_holder() ErrorHolder {
	return ErrorHolder{
		err: error('boom')
	}
}

fn main() {
	other := error('boom')
	println((make_error_holder().err == other).str())
}
")
	assert ierror_selector == 'true'

	ierror_concrete_types := run_good(v3_bin, 'ierror_equality_concrete_type', 'struct ErrorA {}

fn (err ErrorA) msg() string {
	return "same"
}

fn (err ErrorA) code() int {
	return 7
}

struct ErrorB {}

fn (err ErrorB) msg() string {
	return "same"
}

fn (err ErrorB) code() int {
	return 7
}

fn make_error_a() IError {
	return ErrorA{}
}

fn make_error_b() IError {
	return ErrorB{}
}

fn main() {
	println((make_error_a() == make_error_b()).str())
	println((make_error_a() != make_error_b()).str())
	println((make_error_a() == make_error_a()).str())
}
')
	assert ierror_concrete_types == 'false\ntrue\ntrue'

	ierror_sum_field := run_good(v3_bin, 'ierror_shared_sum_field_equality', 'struct ErrorHolderA {
	err IError
}

struct ErrorHolderB {
	err IError
}

type ErrorHolder = ErrorHolderA | ErrorHolderB

fn equal_error(holder ErrorHolder, other IError) bool {
	return holder.err == other
}

fn main() {
	left := ErrorHolder(ErrorHolderA{
		err: error("boom")
	})
	right := ErrorHolder(ErrorHolderB{
		err: error("other")
	})
	println(equal_error(left, error("boom")).str())
	println(equal_error(right, error("boom")).str())
}
')
	assert ierror_sum_field == 'true\nfalse'

	shift_once := run_good(v3_bin, 'unsigned_shift_assign_lvalue_once', 'struct Counter {
mut:
	value int
}

fn next(mut calls Counter) int {
	calls.value++
	return 0
}

fn main() {
	mut calls := Counter{}
	mut values := [8, 16]
	values[next(mut calls)] >>>= 1
	println(int_str(calls.value))
	println(int_str(values[0]))
	println(int_str(values[1]))
	mut signed_values := [i8(-5)]
	signed_values[0] >>>= 1
	println(int_str(signed_values[0]))
	mut shifted_map := map[int]i8{}
	shifted_map[0] = i8(-5)
	shifted_map[next(mut calls)] >>>= 1
	println(int_str(calls.value))
	println(int_str(shifted_map[0]))
}
')
	assert shift_once == '1\n4\n16\n125\n2\n125'

	logical_shifts := run_good(v3_bin, 'signed_logical_shift_results', 'type MyInt = int
type MyIntAlias = MyInt
type Small = i8

const shifted = i64(-5) >>> 1

fn shift_lhs(mut order []int) i64 {
	order << 1
	return -5
}

fn shift_rhs(mut order []int) int {
	order << 2
	return 64
}

fn main() {
	println((i8(-1) >>> 0 == u8(255)).str())
	println(shifted.str())
	value := i64(-5) >>> 1
	println(value.str())
	println(typeof(value).name)
	narrow := i8(-1) >>> 0
	println(narrow.str())
	println(typeof(narrow).name)
	println((i64(-5) >>> 1).str())
	println("\${i64(-5) >>> 1}")
	mut order := []int{}
	oversized := shift_lhs(mut order) >>> shift_rhs(mut order)
	println(int_str(order[0] * 10 + order[1]))
	println(oversized.str())
	aliased := MyIntAlias(-5) >>> 1
	println(aliased.str())
	println(typeof(aliased).name)
	small_last_bit := Small(-1) >>> 7
	println(small_last_bit.str())
	println(typeof(small_last_bit).name)
	println((Small(-1) >>> 8).str())
	mut small_assign := Small(-1)
	small_assign >>>= 7
	println(int_str(small_assign))
	mut small_oversized := Small(-1)
	small_oversized >>>= 8
	println(int_str(small_oversized))
}
')
	assert logical_shifts == 'true\n9223372036854775805\n9223372036854775805\nu64\n255\nu8\n9223372036854775805\n9223372036854775805\n12\n0\n2147483645\nu32\n1\nu8\n0\n1\n0'

	widened_left_shifts := run_good(v3_bin, 'const_count_left_shift_widening', 'const shift_count = 50 + 1
const named_shift = u64(1 << shift_count)
const parenthesized_shift = u64(1 << (51))

fn main() {
	println(named_shift.str())
	println(parenthesized_shift.str())
}
')
	assert widened_left_shifts == '2251799813685248\n2251799813685248'

	shared_sum_field := run_good(v3_bin, 'nested_sum_shared_field_diamond', 'struct Sub1 {
	id int
}

struct Sub2 {
	id int
}

struct Sub3 {
	id int
}

type Master = Sub1 | Sub2
type Master2 = Master | Sub3
type Outer = Master | Master2

fn main() {
	value := Outer(Master2(Sub3{
		id: 7
	}))
	println(int_str(value.id))
}
')
	assert shared_sum_field == '7'

	comptime_types := run_good(v3_bin, 'comptime_pointer_and_alias_identity', "type MyAlias = string

fn pointer_kind(p &int) string {
	$if p is $pointer {
		return 'pointer'
	} $else $if p is $int {
		return 'int'
	} $else {
		return 'other'
	}
}

fn alias_kind[T](value T) string {
	$if T.typ is string {
		return 'string'
	} $else {
		return 'alias'
	}
}

fn main() {
	value := 1
	println(pointer_kind(&value))
	println(alias_kind(MyAlias('x')))
}
")
	assert comptime_types == 'pointer\nalias'
}

fn test_selected_compile_error_in_void_fn_has_clean_diagnostic() {
	v3_bin := build_v3()
	bad_src := '${tmp_test_path('selected_compile_error_void_fn')}.v'
	os.write_file(bad_src, "fn main() {\n\t\$compile_error('bad')\n}\n") or { panic(err) }
	bad_bin := tmp_test_path('selected_compile_error_void_fn')
	compile := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('compile-time error: bad'), compile.output
	assert !compile.output.contains('void function should not return a value'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_comptime_flags_are_not_shadowed_by_cached_values() {
	v3_bin := build_v3()
	platform_flag := if os.user_os() == 'windows' { 'windows' } else { 'unix' }
	out := run_good_with_flags(v3_bin, 'comptime_flags_shadow_cached_values', '-d myflag', "const myflag = false

fn main() {
	${platform_flag} := false
	mut rows := []string{}
	\$if ${platform_flag} {
		rows << 'platform'
	} \$else {
		rows << 'wrong-platform'
	}
	\$if myflag ? {
		rows << 'custom'
	} \$else {
		rows << 'wrong-custom'
	}
	println(rows.join('|'))
}
")
	assert out == 'platform|custom'
}

fn test_overloaded_index_compound_assignment_caches_operands() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'overloaded_index_compound_assignment_caches_operands', 'struct IntList {
mut:
	values []int
}

fn (l IntList) [] (index int) int {
	return l.values[index]
}

fn (mut l IntList) []= (index int, value int) {
	l.values[index] = value
}

struct Env {
mut:
	list_index_calls int
	index_calls      int
}

fn (mut e Env) list_idx() int {
	e.list_index_calls++
	return 0
}

fn (mut e Env) idx() int {
	n := e.index_calls
	e.index_calls++
	return n
}

fn main() {
	mut env := Env{}
	mut lists := [IntList{
			values: [1, 20]
		}]
	lists[env.list_idx()][env.idx()] += 10
	println(int_str(env.list_index_calls) + "," + int_str(env.index_calls))
	println(int_str(lists[0].values[0]) + "," + int_str(lists[0].values[1]))
}
')
	assert out == '1,1\n11,20'
}

fn test_overloaded_index_compound_assignment_uses_v_operators() {
	v3_bin := build_v3()
	string_source := 'struct Dict {\nmut:\n\tvalues map[string]string\n}\n\nfn (d Dict) [] (key string) string {\n\treturn d.values[key]\n}\n\nfn (mut d Dict) []= (key string, value string) {\n\td.values[key] = value\n}\n\nfn main() {\n\tmut d := Dict{\n\t\tvalues: {\n\t\t\t"name": "a"\n\t\t}\n\t}\n\td["name"] += "x"\n\tprintln(d.values["name"])\n}\n'
	string_c := gen_c(v3_bin, 'overloaded_index_compound_string_operator_c', string_source)
	assert string_c.contains('string__plus('), string_c
	string_out := run_good(v3_bin, 'overloaded_index_compound_string_operator', string_source)
	assert string_out == 'ax'

	struct_source := 'struct Num {\n\tn int\n}\n\nfn (a Num) + (b Num) Num {\n\treturn Num{\n\t\tn: a.n + b.n\n\t}\n}\n\nstruct Slot {\nmut:\n\tvalue Num\n}\n\nfn (s Slot) [] (key string) Num {\n\t_ := key\n\treturn s.value\n}\n\nfn (mut s Slot) []= (key string, value Num) {\n\t_ := key\n\ts.value = value\n}\n\nfn main() {\n\tmut s := Slot{\n\t\tvalue: Num{\n\t\t\tn: 3\n\t\t}\n\t}\n\ts["value"] += Num{\n\t\tn: 4\n\t}\n\tprintln(int_str(s.value.n))\n}\n'
	struct_c := gen_c(v3_bin, 'overloaded_index_compound_struct_operator_c', struct_source)
	assert struct_c.contains('Num__plus('), struct_c
	struct_out := run_good(v3_bin, 'overloaded_index_compound_struct_operator', struct_source)
	assert struct_out == '7'
}

fn test_overloaded_index_ref_arg_materializes_getter_result() {
	v3_bin := build_v3()
	source := 'struct Item {
	n int
}

struct Dict {
	values map[string]Item
}

fn (d Dict) [] (key string) Item {
	return d.values[key]
}

fn read(item &Item) int {
	return item.n
}

fn main() {
	d := Dict{
		values: {
			"a": Item{
				n: 7
			}
		}
	}
	println(int_str(read(d["a"])))
}
'
	c_source := gen_c(v3_bin, 'overloaded_index_ref_arg_materializes_getter_result_c', source)
	assert c_source.contains('ref_arg'), c_source
	assert !c_source.contains('&Dict__index('), c_source
	out := run_good(v3_bin, 'overloaded_index_ref_arg_materializes_getter_result', source)
	assert out == '7'
}

fn test_overloaded_index_sum_ref_arg_materializes_getter_result() {
	v3_bin := build_v3()
	source := 'struct Item {
	n int
}

struct Other {}

type Value = Item | Other

struct Dict {
	values map[string]Item
}

fn (d Dict) [] (key string) Item {
	return d.values[key]
}

fn read(value &Value) string {
	_ := value
	return "ok"
}

fn main() {
	d := Dict{
		values: {
			"a": Item{
				n: 7
			}
		}
	}
	println(read(d["a"]))
}
'
	c_source := gen_c(v3_bin, 'overloaded_index_sum_ref_arg_materializes_getter_result_c', source)
	assert c_source.contains('sum_ref_arg'), c_source
	assert !c_source.contains('&Dict__index('), c_source
	out := run_good(v3_bin, 'overloaded_index_sum_ref_arg_materializes_getter_result', source)
	assert out == 'ok'
}

fn test_overloaded_index_accepts_declared_key_type() {
	v3_bin := build_v3()
	dict_src := 'struct Dict {
	values map[string]int
}

fn (d Dict) [] (key string) int {
	return d.values[key]
}
'
	out := run_good(v3_bin, 'overloaded_index_accepts_declared_key_type', dict_src +
		'

fn main() {
	d := Dict{
		values: {
			"name": 7
		}
	}
	println(int_str(d["name"]))
}
')
	assert out == '7'
	run_bad(v3_bin, 'overloaded_index_rejects_wrong_key_type', dict_src +
		'

fn main() {
	d := Dict{}
	println(int_str(d[1]))
}
',
		'cannot use `int` as overloaded index; expected `string`')
	run_bad(v3_bin, 'overloaded_index_assignment_requires_setter', dict_src +
		'

fn main() {
	mut d := Dict{}
	d["name"] = 1
}
',
		'index assignment requires a `[]=` overload on `Dict`')
	run_bad(v3_bin, 'overloaded_index_compound_assignment_requires_setter', dict_src +
		'

fn main() {
	mut d := Dict{}
	d["name"] += 1
}
',
		'index assignment requires a `[]=` overload on `Dict`')
}

fn test_overloaded_index_assignment_uses_setter_signature() {
	v3_bin := build_v3()
	setter_only_src := 'struct Dict {
mut:
	values map[string]int
}

fn (mut d Dict) []= (key string, value int) {
	d.values[key] = value
}
'
	setter_only := run_good(v3_bin, 'overloaded_index_assignment_write_only_setter',
		setter_only_src +
		'

fn main() {
	mut d := Dict{
		values: map[string]int{}
	}
	d["name"] = 7
	println(int_str(d.values["name"]))
}
')
	assert setter_only == '7'
	run_bad(v3_bin, 'overloaded_index_compound_assignment_requires_getter', setter_only_src +
		'

fn main() {
	mut d := Dict{
		values: map[string]int{}
	}
	d["name"] += 1
}
',
		'compound index assignment requires a `[]` overload on `Dict`')
	mismatched_getter_src := 'struct Tensor {}

fn (t Tensor) [] (index int) int {
	return 0
}

fn (mut t Tensor) []= (parts []SliceIndex, value int) {
}
'
	run_bad(v3_bin, 'overloaded_index_compound_assignment_checks_getter_index',
		mismatched_getter_src + '

fn main() {
	mut t := Tensor{}
	t[1, 2] += 3
}
',
		'multi-index expressions on overloaded `[]` require a `[]SliceIndex` parameter')
	range_mismatched_getter_src := 'struct Window {}

fn (w Window) [] (part SliceIndex) int {
	return 0
}

fn (mut w Window) []= (parts []SliceIndex, value int) {
}
'
	run_bad(v3_bin, 'overloaded_index_compound_assignment_rejects_mismatched_index_temps',
		range_mismatched_getter_src + '

fn main() {
	mut w := Window{}
	w[1..2] += 3
}
',
		'compound index assignment requires matching `[]` and `[]=` index parameter types')
	run_bad(v3_bin, 'overloaded_index_assignment_rejects_wrong_setter_key', setter_only_src +
		'

fn main() {
	mut d := Dict{
		values: map[string]int{}
	}
	d[1] = 7
}
',
		'cannot use `int` as overloaded index; expected `string`')
	getter_and_setter_src := 'struct Dict {
mut:
	values map[string]int
}

fn (d Dict) [] (key string) string {
	return "getter:" + key
}

fn (mut d Dict) []= (key string, value int) {
	d.values[key] = value
}
'
	both := run_good(v3_bin, 'overloaded_index_assignment_prefers_setter_value_type',
		getter_and_setter_src +
		'

fn main() {
	mut d := Dict{
		values: map[string]int{}
	}
	d["name"] = 9
	println(int_str(d.values["name"]))
}
')
	assert both == '9'
	run_bad(v3_bin, 'overloaded_index_assignment_rejects_getter_value_type',
		getter_and_setter_src +
		'

fn main() {
	mut d := Dict{
		values: map[string]int{}
	}
	d["name"] = "bad"
}
',
		'expected `int`, not `string`')
	run_bad(v3_bin, 'overloaded_index_compound_assignment_rejects_getter_value_type',
		getter_and_setter_src +
		'

fn main() {
	mut d := Dict{
		values: map[string]int{}
	}
	d["name"] += 1
}
',
		'compound index assignment getter returns `string`, which cannot be used as setter value `int`')
	run_bad(v3_bin, 'overloaded_index_postfix_mutation_rejected', getter_and_setter_src +
		'

fn main() {
	mut d := Dict{
		values: map[string]int{}
	}
	d["name"]++
}
',
		'postfix mutation is not supported for overloaded index expressions')
}

fn test_generic_overloaded_index_uses_specialized_methods() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'generic_overloaded_index_specialized_methods', 'struct Box[T] {
mut:
	items []T
}

fn (b Box[T]) [] (index int) T {
	return b.items[index]
}

fn (mut b Box[T]) []= (index int, value T) {
	b.items[index] = value
}

fn main() {
	mut b := Box[int]{
		items: [1, 2]
	}
	println(int_str(b[0]))
	b[1] = 7
	b[0] += 3
	println(int_str(b[0]) + "," + int_str(b[1]))
}
')
	assert out == '1\n4,7'
}

fn test_unused_generic_receiver_method_is_not_instantiated() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'unused_generic_receiver_method', 'struct Item {}

struct Box[T] {
	items []T
}

fn (box Box[T]) len() int {
	return box.items.len
}

fn (box Box[T]) ordered() bool {
	return box.items[0] < box.items[1]
}

fn main() {
	_ = Box[Item]{}.len()
}
')
	assert out == ''
}

fn test_explicit_generic_method_index_callee_codegen() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'explicit_generic_method_index_callee', 'struct Tool {}

struct Config {
	x int
}

fn (t Tool) pick[T](value T) T {
	return value
}

fn main() {
	tool := Tool{}
	println(int_str(tool.pick[int](7)))
	cfg := tool.pick[Config](x: 9)
	println(int_str(cfg.x))
}
')
	assert out == '7\n9'
}

fn test_isreftype_parenthesized_type_args() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'isreftype_parenthesized_type_args', 'struct Foo {
	n int
}

fn main() {
	foo := Foo{}
	if isreftype(foo) {
		println("bad expr")
	} else {
		println("value expr")
	}
	if isreftype(&foo) {
		println("ptr expr")
	}
	if isreftype(&Foo) {
		println("ptr type")
	}
	if isreftype(fn () int) {
		println("fn type")
	}
	if isreftype([]int) {
		println("array type")
	}
	if isreftype(chan int) {
		println("chan type")
	}
	_ := isreftype(thread Foo)
	println("thread type parsed")
}
')
	assert out == 'value expr\nptr expr\nptr type\nfn type\narray type\nchan type\nthread type parsed'
	qualified_out := run_good_project(v3_bin, 'isreftype_qualified_type_args', {
		'v.mod':     "Module { name: 'isreftype_qualified_type_args' }\n"
		'foo/foo.v': 'module foo\n\npub struct Bar {}\n'
		'main.v':    'module main\n\nimport foo\n\nfn main() {\n\tif !isreftype(foo.Bar) {\n\t\tprintln("qualified type")\n\t}\n\tif isreftype(&foo.Bar) {\n\t\tprintln("qualified ptr type")\n\t}\n\tbar := foo.Bar{}\n\tif isreftype(bar) {\n\t\tprintln("bad expr")\n\t} else {\n\t\tprintln("qualified value expr")\n\t}\n}\n'
	}, 'main.v')
	assert qualified_out == 'qualified type\nqualified ptr type\nqualified value expr'
	run_bad(v3_bin, 'isreftype_unknown_type_arg', 'fn main() {\n\t_ := isreftype(NoSuchType)\n}\n',
		'unknown type `NoSuchType`')
	run_bad(v3_bin, 'isreftype_unknown_array_elem_type_arg',
		'fn main() {\n\t_ := isreftype([]MissingElem)\n}\n', 'unknown type `MissingElem`')
	run_bad(v3_bin, 'isreftype_unknown_bracket_type_arg',
		'fn main() {\n\t_ := isreftype[OtherMissing]()\n}\n', 'unknown type `OtherMissing`')
}

fn test_shadowed_global_local_rename_is_scoped_to_binding() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'shadowed_global_local_rename_scoped', '__global foo int

fn main() {
	foo = 1
	if true {
		foo := 3
		println(int_str(foo))
	}
	println(int_str(foo))
}
')
	assert out == '3\n1'
}

fn test_capturing_fn_literal_aliases_are_scoped_to_lambda() {
	check_good('capturing_fn_literal_aliases_scoped_to_lambda', 'fn call(cb fn ()) {
	_ = cb
}

fn plain() int {
	return 3
}

fn make() fn () int {
	cb := plain
	x := 7
	call(|| {
		cb := fn [x] () int {
			return x
		}
	})
	return cb
}

fn main() {}
')
}

fn test_capturing_fn_literal_aliases_are_scoped_to_shadowing_block() {
	check_good('capturing_fn_literal_aliases_scoped_to_shadowing_block', 'fn plain() int {
	return 3
}

fn make(cond bool) fn () int {
	cb := plain
	x := 7
	if cond {
		cb := fn [x] () int {
			return x
		}
		_ = cb
	}
	return cb
}

fn main() {}
')
}

fn test_for_in_uppercase_const_body_not_struct_init() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'for_in_uppercase_const_body_not_struct_init', "@[translated]
module main

const Foo = [1, 2]

fn main() {
	mut sum := 0
	for x in Foo {
		label := 'x:'
		_ = label
		y := x
		sum += y
	}
	println(int_str(sum))
}
")
	assert out == '3'
}

fn test_amp_uppercase_index_operand_preserves_postfix() {
	v3_bin := build_v3()
	source := '@[translated]
module main

const Foo = [1, 2]

fn main() {
	mut p := &Foo[0]
	p = &Foo[1]
	println(int_str(*p))
}
'
	c_source := gen_c(v3_bin, 'amp_uppercase_index_operand_preserves_postfix', source)
	assert c_source.contains('int* p ='), c_source
	assert !c_source.contains('int p = (*(int*)array_get(*&'), c_source
	out := run_good(v3_bin, 'amp_uppercase_index_operand_preserves_postfix_run', source)
	assert out == '2'
}

fn test_interface_rvalue_upcast_to_embedded_base_argument() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_rvalue_upcast_to_embedded_base_argument', 'interface Base {
	value() int
}

interface Child {
	Base
	extra() int
}

struct Item {
	n int
}

fn (i Item) value() int {
	return i.n
}

fn (i Item) extra() int {
	return i.n + 1
}

fn make_child(n int) Child {
	return Child(Item{
		n: n
	})
}

fn take_base(b Base) int {
	return b.value()
}

fn main() {
	println(int_str(take_base(make_child(7))))
	println(int_str(take_base(if true { make_child(8) } else { make_child(0) })))
	children := [make_child(9)]
	println(int_str(take_base(children[0])))
}
	')
	assert out == '7\n8\n9'
}

fn test_interface_upcast_copies_promoted_struct_fields() {
	v3_bin := build_v3()
	source := 'interface Base {
	name string
}

interface Child {
	Base
	value() int
}

struct Inner {
	name string
}

struct User {
	Inner
	id int
}

fn (u User) value() int {
	return u.id
}

fn make_child(name string, id int) Child {
	return Child(User{
		Inner: Inner{
			name: name
		}
		id: id
	})
}

fn take_base(b Base) string {
	return b.name
}

fn main() {
	child := make_child("Ada", 5)
	println(take_base(child))
	println(take_base(if true { make_child("Grace", 7) } else { child }))
}
'
	c_source := gen_c(v3_bin, 'interface_upcast_promoted_struct_field', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv)')
	assert main_body.contains('.name = child.name'), main_body
	assert !main_body.contains('->name'), main_body
	out := run_good(v3_bin, 'interface_upcast_promoted_struct_field_run', source)
	assert out == 'Ada\nGrace'
}

fn test_selector_interface_upcast_caches_side_effectful_base() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'selector_interface_upcast_caches_side_effectful_base', 'interface Base {
	value() int
}

interface Child {
	Base
	extra() int
}

struct Item {
	n int
}

struct Holder {
	child Child
}

__global (
	calls     int
	idx_calls int
)

fn (i Item) value() int {
	return i.n
}

fn (i Item) extra() int {
	return i.n + 100
}

fn next_holder() Holder {
	calls = calls + 1
	return Holder{
		child: Child(Item{
			n: calls
		})
	}
}

fn next_index() int {
	idx_calls = idx_calls + 1
	return 0
}

fn take_base(b Base) int {
	return b.value()
}

fn main() {
	println(int_str(take_base(next_holder().child)))
	println(int_str(calls))
	holders := [Holder{
		child: Child(Item{
			n: 7
		})
	}]
	println(int_str(take_base(holders[next_index()].child)))
	println(int_str(idx_calls))
}
')
	assert out == '1\n1\n7\n1'
}

fn test_non_generic_reflection_compile_error_waits_for_selected_branch() {
	v3_bin := build_v3()
	good := run_good(v3_bin, 'non_generic_reflection_unselected_compile_error', "struct App {}

fn (app App) present() {
	_ = app
}

fn main() {
	\$for method in App.methods {
		\$if method.name == 'missing' {
			\$compile_error('missing method selected')
		}
	}
	println('ok')
}
")
	assert good == 'ok'
	run_bad(v3_bin, 'non_generic_reflection_selected_compile_error', "struct App {}

fn (app App) present() {
	_ = app
}

fn main() {
	\$for method in App.methods {
		\$if method.name == 'present' {
			\$compile_error('present method selected')
		}
	}
}
",
		'compile-time error: present method selected')
}

fn test_review_index_overload_and_interface_regressions() {
	v3_bin := build_v3()
	overload_out := run_good(v3_bin, 'review_index_overload_regressions', "__global (
	hits int
)

fn next() int {
	hits++
	return 0
}

type Key = string
type Label = string

fn next_key() Key {
	hits++
	return Key('name')
}

struct Dict {
mut:
	values map[string]int
}

fn (d Dict) [] (key string) int {
	return d.values[key]
}

fn (mut d Dict) []= (key string, value int) {
	d.values[key] = value
}

struct AliasDict {
mut:
	values map[string]int
}

fn (d AliasDict) [] (key Key) int {
	return d.values[string(key)]
}

fn (mut d AliasDict) []= (key string, value int) {
	d.values[key] = value
}

struct TextDict {
mut:
	values map[string]Label
}

fn (d TextDict) [] (key string) Label {
	return d.values[key]
}

fn (mut d TextDict) []= (key string, value Label) {
	d.values[key] = value
}

struct Amount {
	n int
}

fn (a Amount) + (b Amount) Amount {
	return Amount{
		n: a.n + b.n
	}
}

struct AmountDict {
mut:
	values map[string]Amount
}

fn (d AmountDict) [] (key string) Amount {
	return d.values[key]
}

fn (mut d AmountDict) []= (key string, value Amount) {
	d.values[key] = value
}

struct Bag {
mut:
	values []int
}

fn (b Bag) [] (i int) int {
	return b.values[i]
}

fn (mut b Bag) []= (i int, value int) {
	b.values[i] = value
}

fn main() {
	mut d := Dict{
		values: {
			'name': 7
		}
	}
	println(d['name'].str())
	d['name'] += 5
	println(d.values['name'].str())
	mut alias_d := AliasDict{
		values: {
			'name': 4
		}
	}
	alias_d[next_key()] += 6
	println(hits.str())
	println(alias_d.values['name'].str())
	mut text_d := TextDict{
		values: {
			'name': Label('a')
		}
	}
	text_d['name'] += Label('x')
	println(text_d.values['name'])
	mut amount_d := AmountDict{
		values: {
			'sum': Amount{
				n: 2
			}
		}
	}
	amount_d['sum'] += Amount{
		n: 3
	}
	println(amount_d.values['sum'].n.str())
	mut b := Bag{
		values: [1]
	}
	b[next()] += 4
	println(hits.str())
	println(b.values[0].str())
}
")
	assert overload_out == '7\n12\n1\n10\nax\n5\n2\n5'
	generic_index_out := run_good(v3_bin, 'review_generic_index_overload_specializes',
		'struct Box[T] {\nmut:\n\tvalues []T\n}\n\nfn (b Box[T]) [] (i int) T {\n\treturn b.values[i]\n}\n\nfn (mut b Box[T]) []= (i int, value T) {\n\tb.values[i] = value\n}\n\nfn main() {\n\tmut b := Box[int]{\n\t\tvalues: [1, 2]\n\t}\n\tprintln(b[1].str())\n\tb[1] = 9\n\tprintln(b[1].str())\n}\n')
	assert generic_index_out == '2\n9'
	explicit_method_out := run_good(v3_bin, 'review_explicit_generic_method_callee',
		'interface Named {\n\tname() string\n}\n\nstruct Config {\n\tx int\n}\n\nstruct User {\n\tname string\n}\n\nfn (u User) name() string {\n\treturn u.name\n}\n\nstruct Runner {}\n\nfn (r Runner) type_name[T]() string {\n\t_ = r\n\treturn typeof[T]().name\n}\n\nfn (r Runner) make[T](cfg T) T {\n\t_ = r\n\treturn cfg\n}\n\nfn (r Runner) pass[T](value T) T {\n\t_ = r\n\treturn value\n}\n\nfn main() {\n\tr := Runner{}\n\tprintln(r.type_name[int]())\n\tcfg := r.make[Config](x: 7)\n\tprintln(int_str(cfg.x))\n\tnamed := r.pass[Named](User{\n\t\tname: "Ada"\n\t})\n\tprintln(named.name())\n}\n')
	assert explicit_method_out == 'int\n7\nAda'
	str_out := run_good(v3_bin, 'review_pointer_fields_implicit_str', "interface Printable {
	str() string
}

struct Bar {
	x int
}

struct Foo {
	nums &[]int
	m    &map[string]int
	bar  &Bar
}

fn main() {
	nums := [1, 2]
	m := {
		'a': 3
	}
	f := Printable(Foo{
		nums: &nums
		m:    &m
		bar:  &Bar{
			x: 7
		}
	})
	println(f.str())
}
")
	assert str_out.contains('nums: [1, 2]'), str_out
	assert str_out.contains("m: {'a': 3}"), str_out
	assert str_out.contains('bar: Bar'), str_out
	assert str_out.contains('x: 7'), str_out
	run_bad(v3_bin, 'review_voidptr_interface_cast', 'interface Sink {
	sink()
}

struct S {}

fn (s S) sink() {}

fn main() {
	x := 1
	p := voidptr(&x)
	_ := Sink(p)
}
	',
		'does not implement interface')
	rvalue_upcast_out := run_good(v3_bin, 'review_interface_rvalue_upcasts',
		'interface Base {\n\tname string\n}\n\ninterface Child {\n\tBase\n\tchild() int\n}\n\nstruct User {\n\tname string\n}\n\nfn (u User) child() int {\n\treturn u.name.len\n}\n\nfn make_child(name string) Child {\n\treturn User{\n\t\tname: name\n\t}\n}\n\nfn take_base(b Base) string {\n\treturn b.name\n}\n\nfn main() {\n\tprintln(take_base(make_child("call")))\n\tcond := true\n\tprintln(take_base(if cond { make_child("if") } else { make_child("else") }))\n\titems := [make_child("index")]\n\tprintln(take_base(items[0]))\n}\n')
	assert rvalue_upcast_out == 'call\nif\nindex'
	embedded_interface_out := run_good(v3_bin, 'review_embedded_interface_fields_and_ptr_upcast',
		'interface Base {\n\tname string\n\tlabel() string\n}\n\ninterface Child {\n\tBase\n\tchild() int\n}\n\nstruct User {\n\tname string\n}\n\nfn (u User) label() string {\n\treturn u.name + ":label"\n}\n\nfn (u User) child() int {\n\treturn u.name.len\n}\n\nfn use_ptr(b &Base) string {\n\treturn b.name + ":" + b.label()\n}\n\nfn describe(base Base) string {\n\treturn match base {\n\t\tChild { base.name + ":" + base.child().str() }\n\t\telse { "else" }\n\t}\n}\n\nfn main() {\n\tchild := Child(User{\n\t\tname: "Ada"\n\t})\n\tbase := Base(User{\n\t\tname: "Bea"\n\t})\n\tprintln(child.name)\n\tprintln(use_ptr(child))\n\tprintln(describe(base))\n}\n')
	assert embedded_interface_out == 'Ada\nAda:Ada:label\nBea:3'
}

fn test_review_shadowed_global_pointer_str_and_setter_only_compound() {
	v3_bin := build_v3()
	shadow_out := run_good(v3_bin, 'review_shadowed_global_nested_scope',
		'__global score int\n\nfn main() {\n\tscore = 10\n\tif true {\n\t\tscore := 3\n\t\tprintln(int_str(score))\n\t}\n\tscore += 2\n\tprintln(int_str(score))\n}\n')
	assert shadow_out == '3\n12'
	pointer_str_out := run_good(v3_bin, 'review_pointer_value_receiver_str',
		"struct Foo {\n\tx int\n}\n\nfn (f Foo) str() string {\n\treturn 'custom:' + int_str(f.x)\n}\n\nfn main() {\n\tfoo := Foo{\n\t\tx: 7\n\t}\n\tp := &foo\n\tprintln(p.str())\n}\n")
	assert pointer_str_out == '&custom:7'
	interface_smartcast_str_out := run_good(v3_bin, 'review_interface_smartcast_pointer_str',
		"interface Named {\n\tname() string\n}\n\nstruct Item {}\n\nfn (i Item) name() string {\n\treturn 'item'\n}\n\nfn (i Item) str() string {\n\treturn i.name()\n}\n\nfn describe(value Named) string {\n\treturn match value {\n\t\tItem { value.str() }\n\t\telse { 'unknown' }\n\t}\n}\n\nfn main() {\n\tvalue := Named(&Item{})\n\tprintln(describe(value))\n\tboxed := Named(Item{})\n\tprintln(describe(boxed))\n}\n")
	assert interface_smartcast_str_out == '&item\nitem'
	run_bad(v3_bin, 'review_setter_only_compound_index_assignment',
		"struct Dict {}\n\nfn (mut d Dict) []= (key string, value int) {\n\t_ = key\n\t_ = value\n}\n\nfn main() {\n\tmut d := Dict{}\n\td['x'] += 1\n}\n",
		'compound index assignment requires a `[]` overload')
	run_bad(v3_bin, 'review_getter_only_index_assignment',
		"struct Dict {}\n\nfn (d Dict) [] (key string) int {\n\t_ = key\n\treturn 0\n}\n\nfn main() {\n\tmut d := Dict{}\n\td['x'] = 1\n}\n",
		'index assignment requires a `[]=` overload')
	run_bad(v3_bin, 'review_getter_only_compound_index_assignment',
		"struct Dict {}\n\nfn (d Dict) [] (key string) int {\n\t_ = key\n\treturn 0\n}\n\nfn main() {\n\tmut d := Dict{}\n\td['x'] += 1\n}\n",
		'index assignment requires a `[]=` overload')
	run_bad(v3_bin, 'review_compound_index_getter_key_mismatch',
		"struct Dict {}\n\nfn (mut d Dict) []= (key string, value int) {\n\t_ = key\n\t_ = value\n}\n\nfn (d Dict) [] (key int) int {\n\t_ = key\n\treturn 0\n}\n\nfn main() {\n\tmut d := Dict{}\n\td['x'] += 1\n}\n",
		'cannot use `string` as overloaded index; expected `int`')
	run_bad(v3_bin, 'review_compound_index_getter_value_mismatch',
		"struct Dict {}\n\nfn (mut d Dict) []= (key string, value int) {\n\t_ = key\n\t_ = value\n}\n\nfn (d Dict) [] (key string) string {\n\t_ = key\n\treturn 'bad'\n}\n\nfn main() {\n\tmut d := Dict{}\n\td['x'] += 1\n}\n",
		'compound index assignment getter returns `string`, which cannot be used as setter value `int`')
	pointer_depth_out := run_good(v3_bin, 'review_one_level_implicit_address',
		'fn take(p &int) int {\n\treturn *p\n}\n\nfn main() {\n\tmut n := 3\n\tprintln(int_str(take(n)))\n}\n')
	assert pointer_depth_out == '3'
	alias_str_out := run_good(v3_bin, 'review_alias_struct_implicit_interface_str',
		"interface Printable {\n\tstr() string\n}\n\nstruct Foo {\n\tx int\n}\n\ntype AliasFoo = Foo\n\nfn main() {\n\tvalue := Printable(AliasFoo(Foo{\n\t\tx: 7\n\t}))\n\ttext := value.str()\n\tprintln(text.contains('Foo'))\n\tprintln(text.contains('x: 7'))\n}\n")
	assert alias_str_out == 'true\ntrue'
	alias_field_str_out := run_good(v3_bin, 'review_alias_fields_implicit_interface_str',
		'interface Printable {\n\tstr() string\n}\n\nstruct Bar {\n\tx int\n}\n\ntype MyBar = Bar\ntype MyNums = []int\ntype MyFixed = [2]int\ntype MyName = string\n\nstruct Foo {\n\tbar   MyBar\n\tnums  MyNums\n\tfixed MyFixed\n\tname  MyName\n}\n\nfn main() {\n\tvalue := Printable(Foo{\n\t\tbar: MyBar(Bar{\n\t\t\tx: 7\n\t\t})\n\t\tnums: MyNums([1, 2])\n\t\tfixed: MyFixed([3, 4]!)\n\t\tname: MyName(\'Ada\')\n\t})\n\ttext := value.str()\n\tprintln(text.contains(\'x: 7\'))\n\tprintln(text.contains(\'[1, 2]\'))\n\tprintln(text.contains(\'[3, 4]\'))\n\tprintln(text.contains("\'Ada\'"))\n}\n')
	assert alias_field_str_out == 'true\ntrue\ntrue\ntrue'
	call_ptr_out := run_good(v3_bin, 'review_call_return_pointer_not_arg_alias',
		'fn choose(a &int, b &int) &int {\n\t_ = a\n\treturn b\n}\n\nfn make() &int {\n\tx := 10\n\ty := 20\n\tp := choose(&x, &y)\n\treturn p\n}\n\nfn main() {\n\tprintln(int_str(*make()))\n}\n')
	assert call_ptr_out == '20'
	mut_param_alias_out := run_good(v3_bin, 'review_mut_param_pointer_alias_return',
		'fn keep[T](mut x T) &T {\n\tp := &x\n\treturn p\n}\n\nfn keep_chain[T](mut x T) &T {\n\tp := &x\n\tq := p\n\treturn q\n}\n\nfn main() {\n\tmut a := 1\n\tp := keep[int](mut a)\n\tunsafe {\n\t\t*p = 7\n\t}\n\tprintln(a.str())\n\tprintln((*p).str())\n\tmut b := 2\n\tq := keep_chain[int](mut b)\n\tunsafe {\n\t\t*q = 8\n\t}\n\tprintln(b.str())\n\tprintln((*q).str())\n}\n')
	assert mut_param_alias_out == '7\n7\n8\n8'
	fixed_field_out := run_good(v3_bin, 'review_capital_field_const_fixed_array',
		'@[translated]\nmodule main\n\nconst n = 2\n\nstruct S {\n\tFoo [n]int\n}\n\nfn main() {\n\ts := S{\n\t\tFoo: [3, 4]!\n\t}\n\tprintln(int_str(s.Foo[0] + s.Foo[1]))\n}\n')
	assert fixed_field_out == '7'
}

fn test_imported_private_free_function_is_rejected() {
	v3_bin := build_v3()
	run_bad_project(v3_bin, 'review_imported_private_free_function', {
		'v.mod':         "Module { name: 'review_imported_private_free_function' }\n"
		'other/other.v': 'module other\n\nfn hidden() int {\n\treturn 7\n}\n'
		'main.v':        'module main\n\nimport other\n\nfn main() {\n\tprintln(int_str(other.hidden()))\n}\n'
	}, ['main.v'], 'function `other.hidden` is private')
}

fn test_private_declarations_in_main_module_accept_empty_module_alias() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'review_main_module_private_alias', {
		'v.mod':  "Module { name: 'review_main_module_private_alias' }\n"
		'app.v':  'module main

struct App {
	value int
}

fn (app App) hidden() int {
	return app.value
}
'
		'main.v': 'module main

fn main() {
	app := App{
		value: 7
	}
	println(app.hidden())
}
'
	}, '')
	assert out == '7'
}

fn test_map_index_value_can_be_implicit_non_mut_reference_argument() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'review_map_index_implicit_ref_arg', 'struct Image {
	value int
}

fn draw(image &Image) int {
	return image.value
}

fn main() {
	images := {
		"avatar": Image{value: 9}
	}
	println(draw(images["avatar"]))
}
')
	assert out == '9'
}

fn test_cross_module_mut_receiver_checks_visible_mutation() {
	v3_bin := build_v3()
	run_bad_project(v3_bin, 'review_cross_module_public_mut_receiver', {
		'v.mod':          "Module { name: 'review_cross_module_public_mut_receiver' }\n"
		'other/config.v': 'module other\n\npub struct Config {\npub mut:\n\tvalue int\n}\n\npub fn (mut cfg Config) reset() {\n\tcfg.value = 0\n}\n'
		'main.v':         'module main\n\nimport other\n\nfn main() {\n\tcfg := other.Config{\n\t\tvalue: 1\n\t}\n\tcfg.reset()\n}\n'
	}, ['main.v'], 'method `reset` requires a mutable receiver')
	private_out := run_good_project(v3_bin, 'review_cross_module_private_mut_receiver', {
		'v.mod':         "Module { name: 'review_cross_module_private_mut_receiver' }\n"
		'other/state.v': 'module other\n\npub struct State {\nmut:\n\thidden int\n}\n\npub fn (mut state State) bump() {\n\tstate.hidden++\n}\n\npub fn (state State) value() int {\n\treturn state.hidden\n}\n'
		'main.v':        'module main\n\nimport other\n\nfn main() {\n\tstate := other.State{}\n\tstate.bump()\n\tprintln(int_str(state.value()))\n}\n'
	}, 'main.v')
	assert private_out == '1'
}

fn test_implicit_reference_materializes_required_pointer_levels() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'review_multi_level_implicit_addresses',
		'fn set_double(pp &&int) {\n\tunsafe {\n\t\t**pp = 5\n\t}\n}\n\nfn set_triple(pp &&&int) {\n\tunsafe {\n\t\t***pp = 7\n\t}\n}\n\nfn main() {\n\tmut x := 1\n\tset_double(x)\n\tmut y := 2\n\tp := &y\n\tset_triple(p)\n\tprintln(int_str(x))\n\tprintln(int_str(y))\n}\n')
	assert out == '5\n7'
}

fn test_discard_assignment_preserves_array_return_type() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'discard_array_return_no_context',
		"fn values() []string {\n\treturn ['a', 'b']\n}\n\nfn main() {\n\t_ = values()\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_late_resolution_and_promoted_init_regressions() {
	v3_bin := build_v3()
	typeof_out := run_good(v3_bin, 'runtime_sum_typeof', '__global typeof_calls int

type Value = int | string

fn make_typeof_value() Value {
	typeof_calls++
	return Value(9)
}

fn main() {
	a := Value(7)
	b := Value("v3")
	p := &Value("pointer")
	nil_pointer := unsafe { &Value(nil) }
	println(unsafe { typeof(a) })
	println(unsafe { typeof(b) })
	println(unsafe { typeof(p) })
	println(unsafe { typeof(nil_pointer) })
	println(typeof(nil_pointer).name)
	println(typeof(make_typeof_value()).name)
	println(int_str(typeof_calls))
}
')
	assert typeof_out == 'int\nstring\nstring\nunknown Value\n&Value\nValue\n0'
	promoted_out := run_good(v3_bin, 'promoted_embed_struct_init', 'struct Inner {
	count int = 3
	items []int
	value int
}

struct Outer {
	Inner
}

fn main() {
	value := Outer{
		value: 7
	}
	heap := &Outer{
		value: 8
	}
	println(int_str(value.count))
	println(int_str(value.value))
	println(int_str(value.items.len))
	println(int_str(heap.count))
	println(int_str(heap.value))
	println(int_str(heap.items.len))
}
')
	assert promoted_out == '3\n7\n0\n3\n8\n0'
	promoted_declared_default_out := run_good(v3_bin, 'promoted_embed_declared_struct_default', 'struct DefaultInner {
	a int
	b int
}

struct DefaultOuter {
	DefaultInner = DefaultInner{
		a: 3
		b: 4
	}
}

struct NestedDefaultInner {
	x int
	y int
}

struct NestedDefaultMid {
	NestedDefaultInner
}

struct NestedDefaultOuter {
	NestedDefaultMid = NestedDefaultMid{
		NestedDefaultInner: NestedDefaultInner{
			x: 1
			y: 2
		}
	}
}

fn main() {
	value := DefaultOuter{
		b: 7
	}
	heap := &DefaultOuter{
		b: 8
	}
	println(int_str(value.a))
	println(int_str(value.b))
	println(int_str(heap.a))
	println(int_str(heap.b))
	nested := NestedDefaultOuter{
		x: 9
	}
	nested_heap := &NestedDefaultOuter{
		x: 10
	}
	println(int_str(nested.x))
	println(int_str(nested.y))
	println(int_str(nested_heap.x))
	println(int_str(nested_heap.y))
}
')
	assert promoted_declared_default_out == '3\n7\n3\n8\n9\n2\n10\n2'
	promoted_cross_module_default_out := run_good_project(v3_bin,
		'promoted_cross_module_struct_default', {
		'v.mod':            "Module { name: 'promoted_cross_module_struct_default' }\n"
		'defaults/types.v': 'module defaults\n\npub const default_a = 3\n\npub struct Inner {\npub:\n\ta int\n\tb int\n}\n\npub struct Outer {\npub:\n\tInner = Inner{\n\t\ta: default_a\n\t\tb: 4\n\t}\n}\n'
		'main.v':           'module main\n\nimport defaults\n\nconst default_a = 99\n\nfn main() {\n\tvalue := defaults.Outer{\n\t\tb: 7\n\t}\n\tprintln(int_str(value.Inner.a))\n\tprintln(int_str(value.Inner.b))\n}\n'
	}, 'main.v')
	assert promoted_cross_module_default_out == '3\n7'
	promoted_import_alias_call_default_out := run_good_project(v3_bin,
		'promoted_import_alias_call_default', {
		'v.mod':             "Module { name: 'promoted_import_alias_call_default' }\n"
		'helpers/helpers.v': 'module helpers\n\npub fn default_a() int {\n\treturn 3\n}\n'
		'defaults/types.v':  'module defaults\n\nimport helpers as h\n\npub struct Inner {\npub:\n\ta int\n\tb int\n}\n\npub struct Outer {\npub:\n\tInner = make_inner(h.default_a())\n}\n\nfn make_inner(a int) Inner {\n\treturn Inner{\n\t\ta: a\n\t\tb: 4\n\t}\n}\n'
		'main.v':            'module main\n\nimport defaults\n\nfn main() {\n\tvalue := defaults.Outer{\n\t\tb: 7\n\t}\n\tprintln(int_str(value.Inner.a))\n\tprintln(int_str(value.Inner.b))\n}\n'
	}, 'main.v')
	assert promoted_import_alias_call_default_out == '3\n7'
	promoted_value_prelude_out := run_good(v3_bin, 'promoted_default_value_prelude', 'struct PromotedArrayInner {
	promoted_values []int
}

fn make_promoted_array_inner() PromotedArrayInner {
	return PromotedArrayInner{
		promoted_values: [0]
	}
}

struct PromotedArrayOuter {
	PromotedArrayInner = make_promoted_array_inner()
}

fn main() {
	value := PromotedArrayOuter{
		promoted_values: [2, 3]
	}
	println(int_str(value.PromotedArrayInner.promoted_values.len))
	println(int_str(value.PromotedArrayInner.promoted_values[0]))
}
')
	assert promoted_value_prelude_out == '2\n2'
	promoted_positional_default_out := run_good(v3_bin, 'promoted_embed_positional_default', 'struct PositionalInner {
	a int
	b int
}

struct PositionalOuter {
	PositionalInner = PositionalInner{3, 4}
}

fn main() {
	value := PositionalOuter{
		a: 7
	}
	heap := &PositionalOuter{
		a: 8
	}
	println(int_str(value.a))
	println(int_str(value.b))
	println(int_str(heap.a))
	println(int_str(heap.b))
}
')
	assert promoted_positional_default_out == '7\n4\n8\n4'
	promoted_call_default_out := run_good(v3_bin, 'promoted_embed_call_default', '__global calls int

struct CallInner {
	a int
	b int
}

struct CallOuter {
	CallInner = make_call_inner()
}

fn make_call_inner() CallInner {
	calls++
	return CallInner{
		a: 5
		b: 6
	}
}

fn main() {
	value := CallOuter{
		b: 7
	}
	heap := &CallOuter{
		b: 8
	}
	println(int_str(value.a))
	println(int_str(value.b))
	println(int_str(heap.a))
	println(int_str(heap.b))
	println(int_str(calls))
}
')
	assert promoted_call_default_out == '5\n7\n5\n8\n2'
	fixed_promoted_out := run_good(v3_bin, 'promoted_fixed_array_struct_init', 'struct FixedInner {
	values [2]int
}

struct FixedOuter {
	FixedInner
}

fn main() {
	values := [1, 2]!
	value := FixedOuter{
		values: values
	}
	heap := &FixedOuter{
		values: [3, 4]!
	}
	println(int_str(value.FixedInner.values[0] + value.FixedInner.values[1]))
	println(int_str(heap.FixedInner.values[0] + heap.FixedInner.values[1]))
}
')
	assert fixed_promoted_out == '3\n7'
	fixed_c_struct_eq_out := run_good_project(v3_bin, 'fixed_array_c_struct_equality', {
		'v.mod':  "Module { name: 'fixed_array_c_struct_equality' }\n"
		'shim.h': 'struct v3_lowercase_tag { int value; };\n'
		'main.v': 'module main\n\n#include "shim.h"\n\nfn main() {\n\tleft := [2]C.v3_lowercase_tag{}\n\tright := [2]C.v3_lowercase_tag{}\n\tprintln(left == right)\n}\n'
	}, 'main.v')
	assert fixed_c_struct_eq_out == 'true'
	pointer_promoted_out := run_good(v3_bin, 'promoted_pointer_embed_struct_init', 'struct PointerInner {
	count int = 4
	items []int
	value int
}

type PointerInnerRef = &PointerInner

struct PointerOuter {
	PointerInnerRef
}

fn main() {
	value := PointerOuter{
		value: 9
	}
	heap := &PointerOuter{
		value: 10
	}
	println(int_str(value.PointerInnerRef.count))
	println(int_str(value.PointerInnerRef.value))
	println(int_str(value.PointerInnerRef.items.len))
	println(int_str(heap.PointerInnerRef.count))
	println(int_str(heap.PointerInnerRef.value))
	println(int_str(heap.PointerInnerRef.items.len))
}
')
	assert pointer_promoted_out == '4\n9\n0\n4\n10\n0'
	pointer_promoted_default_out := run_good(v3_bin, 'promoted_pointer_embed_call_default_escape', 'struct EscapingPointerInner {
	a int
	b int
}

type EscapingPointerInnerRef = &EscapingPointerInner

struct EscapingPointerOuter {
	EscapingPointerInnerRef = make_escaping_pointer_inner()
}

fn make_escaping_pointer_inner() EscapingPointerInnerRef {
	return &EscapingPointerInner{
		a: 5
		b: 6
	}
}

fn make_escaping_pointer_outer(b int) EscapingPointerOuter {
	return EscapingPointerOuter{
		b: b
	}
}

fn main() {
	first := make_escaping_pointer_outer(7)
	second := make_escaping_pointer_outer(8)
	println(int_str(first.EscapingPointerInnerRef.a))
	println(int_str(first.EscapingPointerInnerRef.b))
	println(int_str(second.EscapingPointerInnerRef.a))
	println(int_str(second.EscapingPointerInnerRef.b))
}
')
	assert pointer_promoted_default_out == '5\n7\n5\n8'
	alias_typeof_out := run_good(v3_bin, 'runtime_sum_pointer_alias_typeof', 'type AliasValue = int | string
type AliasValueRef = &AliasValue

fn main() {
	value := AliasValue("alias")
	ref := AliasValueRef(&value)
	println(unsafe { typeof(ref) })
	println(typeof(ref).name)
}
')
	assert alias_typeof_out == 'string\nAliasValueRef'
	include_out := run_good_project(v3_bin, 'quoted_source_include_from_include_dir', {
		'v.mod':          "Module { name: 'quoted_source_include_from_include_dir' }\n"
		'include/shim.c': 'int answer_from_shim(void) { return 42; }\n'
		'main.v':         'module main\n\n#flag -I @DIR/include\n#include "shim.c"\n\nfn C.answer_from_shim() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_shim()))\n}\n'
	}, 'main.v')
	assert include_out == '42'
	guarded_include_out := run_good_project(v3_bin, 'guarded_quoted_source_include', {
		'v.mod':         "Module { name: 'guarded_quoted_source_include' }\n"
		'packed.c':      'typedef struct { char first; int second; } V3ReviewPacked;\nint v3_review_packed_size(void) { return sizeof(V3ReviewPacked); }\n'
		'shim.c':        'int answer_from_guarded_shim(void) { return V3_GUARDED_SHIM_VALUE; }\n'
		'toplevel.c':    'int answer_from_toplevel_shim(void) { return V3_TOPLEVEL_SHIM_VALUE; }\n'
		'specialized.c': '#if V3_SOURCE_VARIANT == 1\nint answer_from_source_variant_one(void) { return 1; }\n#elif V3_SOURCE_VARIANT == 2\nint answer_from_source_variant_two(void) { return 2; }\n#endif\n'
		'main.v':        'module main\n\n#define V3_TOPLEVEL_SHIM_VALUE 46\n#include "toplevel.c"\n#undef V3_TOPLEVEL_SHIM_VALUE\n\n#define V3_GUARDED_SOURCE\n#ifdef V3_GUARDED_SOURCE\n#define V3_GUARDED_SHIM_VALUE 45\n#include "shim.c"\n#undef V3_GUARDED_SHIM_VALUE\n#endif\n\n#define V3_SOURCE_VARIANT 1\n#include "specialized.c"\n#undef V3_SOURCE_VARIANT\n#define V3_SOURCE_VARIANT 2\n#include "specialized.c"\n#undef V3_SOURCE_VARIANT\n\n#pragma pack(push, 1)\n#include "packed.c"\n#pragma pack(pop)\n\nfn C.answer_from_guarded_shim() int\nfn C.answer_from_toplevel_shim() int\nfn C.answer_from_source_variant_one() int\nfn C.answer_from_source_variant_two() int\nfn C.v3_review_packed_size() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_guarded_shim() + C.answer_from_toplevel_shim() + C.answer_from_source_variant_one() + C.answer_from_source_variant_two()))\n\tprintln(int_str(C.v3_review_packed_size()))\n}\n'
	}, 'main.v')
	assert guarded_include_out == '94\n5'
	guarded_header_before_source_out := run_good_project(v3_bin,
		'guarded_header_before_source_include', {
		'v.mod':   "Module { name: 'guarded_header_before_source_include' }\n"
		'types.h': 'typedef struct { int value; } V3GuardedHeaderType;\n'
		'impl.c':  'V3GuardedHeaderType v3_make_guarded_header_type(void) { return (V3GuardedHeaderType){50}; }\nint v3_guarded_header_type_value(V3GuardedHeaderType value) { return value.value; }\n'
		'main.v':  'module main\n\n#define V3_USE_GUARDED_HEADER\n#ifdef V3_USE_GUARDED_HEADER\n#include "types.h"\n#include "impl.c"\n#endif\n\nstruct GuardedHeaderHolder {\n\tvalue C.V3GuardedHeaderType\n}\n\nfn C.v3_make_guarded_header_type() C.V3GuardedHeaderType\nfn C.v3_guarded_header_type_value(C.V3GuardedHeaderType) int\n\nfn main() {\n\tholder := GuardedHeaderHolder{\n\t\tvalue: C.v3_make_guarded_header_type()\n\t}\n\tprintln(int_str(C.v3_guarded_header_type_value(holder.value)))\n}\n'
	}, 'main.v')
	assert guarded_header_before_source_out == '50'
	source_type_out := run_good_project(v3_bin, 'source_include_type_before_v_declaration', {
		'v.mod':  "Module { name: 'source_include_type_before_v_declaration' }\n"
		'shim.c': 'typedef struct { int value; } V3SourceType;\nint v3_source_type_value(V3SourceType value) { return value.value; }\n'
		'main.v': 'module main\n\n#include "shim.c"\n\nstruct SourceTypeHolder {\n\tvalue C.V3SourceType\n}\n\nfn C.v3_source_type_value(C.V3SourceType) int\n\nfn main() {\n\tholder := SourceTypeHolder{\n\t\tvalue: C.V3SourceType{\n\t\t\tvalue: 51\n\t\t}\n\t}\n\tprintln(int_str(C.v3_source_type_value(holder.value)))\n}\n'
	}, 'main.v')
	assert source_type_out == '51'
	objective_c_type_out := run_good_project_with_flags(v3_bin, 'objective_c_type_provider',
		'-cc clang', {
		'v.mod':  "Module { name: 'objective_c_type_provider' }\n"
		'shim.m': 'typedef struct { int value; } V3ObjectiveCType;\nint v3_objective_c_type_value(V3ObjectiveCType value) { return value.value; }\n'
		'main.v': 'module main\n\n#include "shim.m"\n\nstruct ObjectiveCTypeHolder {\n\tvalue C.V3ObjectiveCType\n}\n\nfn C.v3_objective_c_type_value(C.V3ObjectiveCType) int\n\nfn main() {\n\tholder := ObjectiveCTypeHolder{\n\t\tvalue: C.V3ObjectiveCType{\n\t\t\tvalue: 56\n\t\t}\n\t}\n\tprintln(int_str(C.v3_objective_c_type_value(holder.value)))\n}\n'
	}, 'main.v')
	assert objective_c_type_out == '56'
	objective_c_typedef_out := run_good_project_with_flags(v3_bin, 'objective_c_typedef_provider',
		'-cc clang', {
		'v.mod':  "Module { name: 'objective_c_typedef_provider' }\n"
		'shim.m': 'typedef enum { V3_KIND_ZERO = 0 } V3Kind;\ntypedef unsigned long V3Plain;\n'
		'main.v': 'module main\n\n#include "shim.m"\n\nstruct ObjectiveCTypedefHolder {\n\tkind C.V3Kind\n\tvalue C.V3Plain\n}\n\nfn main() {\n\t_ := ObjectiveCTypedefHolder{}\n\tprintln(int_str(66))\n}\n'
	}, 'main.v')
	assert objective_c_typedef_out == '66'
	objective_c_sum_typedef_out := run_good_project_with_flags(v3_bin,
		'objective_c_sum_typedef_provider', '-cc clang', {
		'v.mod':  "Module { name: 'objective_c_sum_typedef_provider' }\n"
		'shim.m': 'typedef struct { int value; } V3Obj;\n'
		'main.v': 'module main\n\n#include "shim.m"\n\ntype ObjectiveCSumValue = C.V3Obj | int\n\nfn main() {\n\t_ := ObjectiveCSumValue(67)\n\tprintln(int_str(67))\n}\n'
	}, 'main.v')
	assert objective_c_sum_typedef_out == '67'
	objective_cpp_out := run_good_project_with_flags(v3_bin, 'objective_cpp_source_include',
		'-cc clang', {
		'v.mod':   "Module { name: 'objective_cpp_source_include' }\n"
		'shim.cc': '#include <string>\nextern "C" int answer_from_cpp(void) { std::string answer(2, \'x\'); return int(answer.size()); }\n'
		'shim.m':  'int answer_from_objective_c(void) { return 1; }\n'
		'shim.mm': 'extern "C" int answer_from_objective_cpp(void) { auto answer = []() { return new int(43); }; auto value = answer(); int result = *value; delete value; return result; }\n'
		'main.v':  'module main\n\n#flag @VMODROOT/shim.o\n#include "shim.m"\n#include "shim.mm"\n\nfn C.answer_from_cpp() int\nfn C.answer_from_objective_c() int\nfn C.answer_from_objective_cpp() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_cpp() + C.answer_from_objective_c() + C.answer_from_objective_cpp()))\n}\n'
	}, 'main.v')
	assert objective_cpp_out == '46'
	objective_cpp_object_fallback_out := run_good_project_with_flags(v3_bin,
		'objective_cpp_object_fallback', '-cc clang', {
		'v.mod':   "Module { name: 'objective_cpp_object_fallback' }\n"
		'shim.mm': '#include <string>\nextern "C" int answer_from_objective_cpp_object(void) { std::string answer(49, \'x\'); return int(answer.size()); }\n'
		'main.v':  'module main\n\n#flag @VMODROOT/shim.o\n\nfn C.answer_from_objective_cpp_object() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_objective_cpp_object()))\n}\n'
	}, 'main.v')
	assert objective_cpp_object_fallback_out == '49'
	objective_c_object_fallback_out := run_good_project_with_flags(v3_bin,
		'objective_c_object_fallback', '-cc clang', {
		'v.mod':  "Module { name: 'objective_c_object_fallback' }\n"
		'shim.m': 'int answer_from_objective_c_object(void) { return 64; }\n'
		'main.v': 'module main\n\n#flag @VMODROOT/shim.o\n\nfn C.answer_from_objective_c_object() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_objective_c_object()))\n}\n'
	}, 'main.v')
	assert objective_c_object_fallback_out == '64'
	objective_cpp_after_guarded_header_out := run_good_project_with_flags(v3_bin,
		'objective_cpp_after_guarded_header', '-cc clang', {
		'v.mod':   "Module { name: 'objective_cpp_after_guarded_header' }\n"
		'shim.h':  '#ifndef V3_REVIEW_SHIM_H\n#define V3_REVIEW_SHIM_H\ntypedef int v3_review_header_int;\n#endif\n'
		'shim.mm': 'extern "C" int answer_after_guarded_header(void) { v3_review_header_int value = 48; auto answer = [value]() { return value; }; return answer(); }\n'
		'main.v':  'module main\n\n#include "shim.h"\n#include "shim.mm"\n\nfn C.answer_after_guarded_header() int\n\nfn main() {\n\tprintln(int_str(C.answer_after_guarded_header()))\n}\n'
	}, 'main.v')
	assert objective_cpp_after_guarded_header_out == '48'
	guarded_objective_cpp_out := run_good_project_with_flags(v3_bin,
		'guarded_objective_cpp_source_include', '-cc clang', {
		'v.mod':          "Module { name: 'guarded_objective_cpp_source_include' }\n"
		'disabled.m':     '#error disabled Objective-C source must not be compiled\n'
		'disabled.mm':    '#error disabled Objective-C++ source must not be compiled\n'
		'defs.h':         'typedef int v3_intervening_header_type;\n'
		'macro_value.mm': '#ifndef V3_OBJECTIVE_CPP_VALUE\n#error missing include macro context\n#endif\nextern "C" int answer_from_macro_objective_cpp(void) { auto answer = []() { return V3_OBJECTIVE_CPP_VALUE; }; return answer(); }\n'
		'main.v':         'module main\n\n#ifdef V3_NEVER_DEFINED\n#include "disabled.mm"\n#endif\n\n#define V3_OBJECTIVE_CPP_VALUE 47\n#include "defs.h"\n#include "macro_value.mm"\n#undef V3_OBJECTIVE_CPP_VALUE\n\n#undef V3_NEVER_DEFINED\n#ifdef V3_NEVER_DEFINED\n#include "disabled.m"\n#endif\n\n#ifdef __OBJC__\n#error generated V translation unit must remain C\n#endif\n\nfn C.answer_from_macro_objective_cpp() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_macro_objective_cpp()))\n}\n'
	}, 'main.v')
	assert guarded_objective_cpp_out == '47'
	inactive_objective_c_out := run_good_project(v3_bin, 'inactive_objective_c_source', {
		'v.mod':      "Module { name: 'inactive_objective_c_source' }\n"
		'disabled.m': '#error inactive Objective-C source must not be compiled\n'
		'main.v':     'module main\n\n#ifdef V3_NEVER_DEFINED\n#include "disabled.m"\n#endif\n\nfn main() {\n\tprintln(int_str(60))\n}\n'
	}, 'main.v')
	assert inactive_objective_c_out == '60'
	inactive_objective_cpp := run_good_project_result(v3_bin, 'inactive_objective_cpp_source', '', {
		'v.mod':        "Module { name: 'inactive_objective_cpp_source' }\n"
		'disabled.mm':  '#error inactive Objective-C++ source must not be compiled\n'
		'later_defs.c': '#define V3_NEVER_DEFINED_OBJECTIVE_CPP 1\n'
		'main.v':       'module main\n\n#if 0\n#include "disabled.mm"\n#endif\n\n#ifdef V3_NEVER_DEFINED_OBJECTIVE_CPP\n#include "disabled.mm"\n#endif\n\n#include "later_defs.c"\n\nfn main() {\n\tprintln(int_str(65))\n}\n'
	}, 'main.v')
	assert inactive_objective_cpp.run_output == '65'
	assert !inactive_objective_cpp.compile_output.contains('v3_native_source_context_'), inactive_objective_cpp.compile_output

	guarded_objective_c_static_out := run_good_project_with_flags(v3_bin,
		'guarded_objective_c_static', '-cc clang', {
		'v.mod':  "Module { name: 'guarded_objective_c_static' }\n"
		'shim.m': '#ifndef V3_OBJECTIVE_C_STATIC_VALUE\n#error missing guarded Objective-C context\n#endif\nstatic int answer_from_guarded_objective_c_static(void) { return V3_OBJECTIVE_C_STATIC_VALUE; }\n'
		'main.v': 'module main\n\n#define V3_OBJECTIVE_C_STATIC_VALUE 55\n#ifdef V3_OBJECTIVE_C_STATIC_VALUE\n#include "shim.m"\n#endif\n\nfn C.answer_from_guarded_objective_c_static() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_guarded_objective_c_static()))\n}\n'
	}, 'main.v')
	assert guarded_objective_c_static_out == '55'
	delayed_objective_c_macro_out := run_good_project_with_flags(v3_bin,
		'delayed_objective_c_macro', '-cc clang', {
		'v.mod':  "Module { name: 'delayed_objective_c_macro' }\n"
		'shim.m': '#ifndef V3_DELAYED_OBJECTIVE_C_VALUE\n#error missing delayed Objective-C macro context\n#endif\nstatic int answer_from_delayed_objective_c_macro(void) { return V3_DELAYED_OBJECTIVE_C_VALUE; }\n'
		'main.v': 'module main\n\n#define V3_DELAYED_OBJECTIVE_C_VALUE 57\n#ifdef V3_DELAYED_OBJECTIVE_C_VALUE\n#include "shim.m"\n#endif\n#undef V3_DELAYED_OBJECTIVE_C_VALUE\n\nfn C.answer_from_delayed_objective_c_macro() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_delayed_objective_c_macro()))\n}\n'
	}, 'main.v')
	assert delayed_objective_c_macro_out == '57'
	inactive_undef_context_out := run_good_project_with_flags(v3_bin, 'inactive_undef_context',
		'-cc clang', {
		'v.mod':  "Module { name: 'inactive_undef_context' }\n"
		'shim.m': '#ifndef V3_ACTIVE_THROUGH_INACTIVE_UNDEF\n#error active macro was lost through inactive undef\n#endif\nstatic int answer_after_inactive_undef(void) { return V3_ACTIVE_THROUGH_INACTIVE_UNDEF; }\n'
		'main.v': 'module main\n\n#define V3_ACTIVE_THROUGH_INACTIVE_UNDEF 62\n#if 0\n#undef V3_ACTIVE_THROUGH_INACTIVE_UNDEF\n#endif\n#include "shim.m"\n#undef V3_ACTIVE_THROUGH_INACTIVE_UNDEF\n\nfn C.answer_after_inactive_undef() int\n\nfn main() {\n\tprintln(int_str(C.answer_after_inactive_undef()))\n}\n'
	}, 'main.v')
	assert inactive_undef_context_out == '62'
	inactive_defined_guard_out := run_good_project(v3_bin, 'inactive_defined_objective_c_guard', {
		'v.mod':      "Module { name: 'inactive_defined_objective_c_guard' }\n"
		'disabled.m': '#error defined() guarded Objective-C source must not be compiled\n'
		'main.v':     'module main\n\n#if defined(V3_NEVER_DEFINED_FOR_OBJECTIVE_C)\n#include "disabled.m"\n#endif\n\n#if 0\n#elif 0\n#include "disabled.m"\n#endif\n\n#if 1\n#elif 0\n#else\n#include "disabled.m"\n#endif\n\n#ifdef __OBJC__\n#error inactive guards must not enable Objective-C\n#endif\n\nfn main() {\n\tprintln(int_str(63))\n}\n'
	}, 'main.v')
	assert inactive_defined_guard_out == '63'
	noncontiguous_source_context_out := run_good_project_with_flags(v3_bin,
		'noncontiguous_source_context', '-cc clang', {
		'v.mod':  "Module { name: 'noncontiguous_source_context' }\n"
		'defs.h': 'typedef int v3_noncontiguous_context_header_type;\n'
		'shim.m': '#ifndef V3_NONCONTIGUOUS_CONTEXT_VALUE\n#error missing non-contiguous macro context\n#endif\nstatic int answer_from_noncontiguous_context(void) { return V3_NONCONTIGUOUS_CONTEXT_VALUE; }\n'
		'main.v': 'module main\n\n#define V3_NONCONTIGUOUS_CONTEXT_VALUE 61\n#pragma pack(push, 1)\n#include "defs.h"\n#include "shim.m"\n#pragma pack(pop)\n#undef V3_NONCONTIGUOUS_CONTEXT_VALUE\n\nstruct V3DelayedContextLayout {\n\tfirst u8\n\tsecond u64\n}\n\nfn C.answer_from_noncontiguous_context() int\n\nfn main() {\n\tprintln(int_str(int(sizeof(V3DelayedContextLayout))))\n\tprintln(int_str(C.answer_from_noncontiguous_context()))\n}\n'
	}, 'main.v')
	assert noncontiguous_source_context_out == '16\n61'
	relative_source_include_out := run_good_project_relative_input(v3_bin, 'relative_source_input',
		'-cc clang', {
		'v.mod':  "Module { name: 'relative_source_input' }\n"
		'shim.c': 'int answer_from_relative_c(void) { return 58; }\n'
		'shim.m': 'int answer_from_relative_objective_c(void) { return 1; }\n'
		'main.v': 'module main\n\n#include "shim.c"\n#include "shim.m"\n\nfn C.answer_from_relative_c() int\nfn C.answer_from_relative_objective_c() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_relative_c() + C.answer_from_relative_objective_c()))\n}\n'
	}, 'main.v')
	assert relative_source_include_out == '59'
	cpp_runtime_out := run_good_project_with_flags(v3_bin, 'cpp_source_runtime', '-cc clang', {
		'v.mod':    "Module { name: 'cpp_source_runtime' }\n"
		'shim.cpp': '#include <string>\nextern "C" int answer_from_cpp_runtime(void) { std::string answer(44, \'x\'); return int(answer.size()); }\n'
		'main.v':   'module main\n\n#flag @VMODROOT/shim.cpp\n\nfn C.answer_from_cpp_runtime() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_cpp_runtime()))\n}\n'
	}, 'main.v')
	assert cpp_runtime_out == '44'
	explicit_language_out := run_good_project_with_flags(v3_bin, 'explicit_language_source_flag',
		'-cc clang', {
		'v.mod':  "Module { name: 'explicit_language_source_flag' }\n"
		'shim.c': '#include <string>\nextern "C" int answer_from_explicit_cpp(void) { std::string answer(44, \'x\'); return int(answer.size()); }\n'
		'main.v': 'module main\n\n#flag -x c++\n#flag @VMODROOT/shim.c\n\nfn C.answer_from_explicit_cpp() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_explicit_cpp()))\n}\n'
	}, 'main.v')
	assert explicit_language_out == '44'
	explicit_object_language_out := run_good_project_with_flags(v3_bin,
		'explicit_object_fallback_language', '-cc clang', {
		'v.mod':  "Module { name: 'explicit_object_fallback_language' }\n"
		'shim.c': '#include <string>\nextern "C" int answer_from_explicit_object_cpp(void) { std::string answer(52, \'x\'); return int(answer.size()); }\n'
		'main.v': 'module main\n\n#flag -x c++\n#flag @VMODROOT/shim.o\n\nfn C.answer_from_explicit_object_cpp() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_explicit_object_cpp()))\n}\n'
	}, 'main.v')
	assert explicit_object_language_out == '52'
	extensionless_language_out := run_good_project_with_flags(v3_bin,
		'extensionless_explicit_language', '-cc clang', {
		'v.mod':  "Module { name: 'extensionless_explicit_language' }\n"
		'shim':   '#include <string>\nextern "C" int answer_from_extensionless_cpp(void) { std::string answer(53, \'x\'); return int(answer.size()); }\n'
		'main.v': 'module main\n\n#flag -x c++\n#flag @VMODROOT/shim\n\nfn C.answer_from_extensionless_cpp() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_extensionless_cpp()))\n}\n'
	}, 'main.v')
	assert extensionless_language_out == '53'
	objective_cpp_c_override_out := run_good_project_with_flags(v3_bin, 'objective_cpp_c_override',
		'-cc clang', {
		'v.mod':   "Module { name: 'objective_cpp_c_override' }\n"
		'shim.mm': 'int answer_from_mm_compiled_as_c(void) { void* raw = 0; int* typed = raw; return typed == 0 ? 54 : 0; }\n'
		'main.v':  'module main\n\n#flag -x c\n#flag @VMODROOT/shim.mm\n\nfn C.answer_from_mm_compiled_as_c() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_mm_compiled_as_c()))\n}\n'
	}, 'main.v')
	assert objective_cpp_c_override_out == '54'
}

fn test_imported_objective_cpp_wrapper_context() {
	v3_bin := build_v3()
	out := run_good_project_with_flags(v3_bin, 'imported_objective_cpp_wrapper_context',
		'-cc clang', {
		'v.mod':                   "Module { name: 'imported_objective_cpp_wrapper_context' }\n"
		'nativecontext/context.v': 'module nativecontext\n\n#define V3_IMPORTED_OBJECTIVE_CPP_VALUE 68\n#include "types.h"\n\npub fn keep_context_module() {}\n'
		'nativecontext/types.h':   'typedef int v3_imported_objective_cpp_int;\n'
		'consumer/consumer.v':     'module consumer\n\nimport nativecontext\n\n#include "shim.mm"\n\nfn C.answer_from_imported_objective_cpp_context() int\n\npub fn answer() int {\n\tnativecontext.keep_context_module()\n\treturn C.answer_from_imported_objective_cpp_context()\n}\n'
		'consumer/shim.mm':        '#ifndef V3_IMPORTED_OBJECTIVE_CPP_VALUE\n#error missing imported Objective-C++ macro context\n#endif\nextern "C" int answer_from_imported_objective_cpp_context(void) { v3_imported_objective_cpp_int value = V3_IMPORTED_OBJECTIVE_CPP_VALUE; auto answer = [value]() { return value; }; return answer(); }\n'
		'main.v':                  'module main\n\nimport consumer\n\nfn main() {\n\tprintln(int_str(consumer.answer()))\n}\n'
	}, 'main.v')
	assert out == '68'
}

fn test_cached_native_root_preserves_preceding_header_macro_mutations() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'cached_native_header_macro_mutation', {
		'v.mod':                         "Module { name: 'cached_native_header_macro_mutation' }\n"
		'nativeanswer/config.h':         '#undef V3_NATIVE_FEATURE\n#define V3_NATIVE_HEADER_VALUE 73\n'
		'nativeanswer/implementation.h': '#ifdef V3_NATIVE_FEATURE\nint v3_native_header_answer(void) { return 1; }\n#else\nint v3_native_header_answer(void) { return V3_NATIVE_HEADER_VALUE; }\n#endif\n'
		'nativeanswer/nativeanswer.v':   'module nativeanswer\n\n#define V3_NATIVE_FEATURE 1\n#include "config.h"\n#insert "implementation.h"\n\nfn C.v3_native_header_answer() int\n\npub fn answer() int {\n\treturn C.v3_native_header_answer()\n}\n'
		'main.v':                        'module main\n\nimport nativeanswer\n\nfn main() {\n\tprintln(int_str(nativeanswer.answer()))\n}\n'
	}, 'main.v')
	assert out == '73'
}

fn test_cached_native_public_replay_does_not_repeat_preceding_header() {
	v3_bin := build_v3()
	out := run_good_cached_project(v3_bin, 'cached_native_single_preceding_header', {
		'v.mod':                         "Module { name: 'cached_native_single_preceding_header' }\n"
		'nativeanswer/context.h':        '#pragma once\nstruct V3CacheContextType { int value; };\n#define V3_CACHE_CONTEXT_VALUE 76\n'
		'nativeanswer/implementation.h': '#ifdef V3_CACHE_CONTEXT_IMPLEMENTATION\nint v3_cache_context_answer(void) { struct V3CacheContextType value = { V3_CACHE_CONTEXT_VALUE }; return value.value; }\n#else\nint v3_cache_context_answer(void);\n#endif\n'
		'nativeanswer/nativeanswer.v':   'module nativeanswer\n\n#define V3_CACHE_CONTEXT_IMPLEMENTATION\n#include "context.h"\n#insert "implementation.h"\n#undef V3_CACHE_CONTEXT_IMPLEMENTATION\n\nfn C.v3_cache_context_answer() int\n\npub fn answer() int {\n\treturn C.v3_cache_context_answer()\n}\n'
		'main.v':                        'module main\n\nimport nativeanswer\n\nfn main() {\n\tprintln(int_str(nativeanswer.answer()))\n}\n'
	}, 'main.v')
	assert out == '76'
}

fn test_cached_native_root_uses_generated_pre_and_postinclude_order() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'cached_native_placed_includes', {
		'v.mod':                         "Module { name: 'cached_native_placed_includes' }\n"
		'nativeanswer/pre.h':            '#define V3_NATIVE_PRE_READY 1\n'
		'nativeanswer/post.h':           '#define V3_NATIVE_POST_LATE 1\n'
		'nativeanswer/implementation.h': '#ifndef V3_NATIVE_PRE_READY\n#error missing generated preinclude context\n#endif\n#ifdef V3_NATIVE_POST_LATE\n#error postinclude replayed before native root\n#endif\nint v3_placed_include_answer(void) { return 74; }\n'
		'nativeanswer/nativeanswer.v':   'module nativeanswer\n\n#postinclude "@DIR/post.h"\n#insert "@DIR/implementation.h"\n#preinclude "@DIR/pre.h"\n\nfn C.v3_placed_include_answer() int\n\npub fn answer() int {\n\treturn C.v3_placed_include_answer()\n}\n'
		'main.v':                        'module main\n\nimport nativeanswer\n\nfn main() {\n\tprintln(int_str(nativeanswer.answer()))\n}\n'
	}, 'main.v')
	assert out == '74'
}

fn test_cached_native_parameter_name_does_not_suppress_c_type() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'cached_native_parameter_type_name', {
		'v.mod':                       "Module { name: 'cached_native_parameter_type_name' }\n"
		'nativeanswer/native.h':       'int v3_parameter_name_only(int Unrelated);\n'
		'nativeanswer/nativeanswer.v': 'module nativeanswer\n\n#insert "native.h"\n\nstruct C.Unrelated {}\n\nfn accepts_unrelated(value &C.Unrelated) int {\n\treturn if isnil(value) { 75 } else { 0 }\n}\n\npub fn answer() int {\n\treturn accepts_unrelated(unsafe { nil })\n}\n'
		'main.v':                      'module main\n\nimport nativeanswer\n\nfn main() {\n\tprintln(int_str(nativeanswer.answer()))\n}\n'
	}, 'main.v')
	assert out == '75'
}

fn test_bare_macro_objective_c_guards_stay_inactive() {
	v3_bin := build_v3()
	result := run_good_project_result(v3_bin, 'bare_macro_objective_c_guards', '', {
		'v.mod':           "Module { name: 'bare_macro_objective_c_guards' }\n"
		'disabled.m':      '#error inactive Objective-C source must not be compiled\n'
		'disabled.mm':     '#error inactive Objective-C++ source must not be compiled\n'
		'inactive_defs.c': '#define V3_INACTIVE_SOURCE_FEATURE 1\n'
		'main.v':          'module main\n\n#if V3_NEVER_DEFINED_OBJECTIVE_C\n#include "disabled.m"\n#endif\n\n#if 0\n#include "inactive_defs.c"\n#endif\n\n#if V3_INACTIVE_SOURCE_FEATURE\n#include "disabled.mm"\n#endif\n\nfn main() {\n\tprintln(int_str(70))\n}\n'
	}, 'main.v')
	assert result.run_output == '70'
	assert result.compile_output.contains('tcc.exe'), result.compile_output
	assert !result.compile_output.contains('v3_native_source_context_'), result.compile_output
}

fn test_valued_bare_macro_objective_c_guards_remain_possible() {
	v3_bin := build_v3()
	out := run_good_project_with_flags(v3_bin, 'valued_bare_macro_objective_c_guards', '-cc clang', {
		'v.mod':     "Module { name: 'valued_bare_macro_objective_c_guards' }\n"
		'active.m':  'static int answer_from_valued_m_guard(void) { return 1; }\n'
		'active.mm': 'extern "C" int answer_from_valued_mm_guard(void) { auto answer = []() { return 70; }; return answer(); }\n'
		'main.v':    'module main\n\n#flag -DV3_MM_FEATURE=0\n\n#define V3_M_FEATURE 0\n#if !V3_M_FEATURE\n#include "active.m"\n#endif\n\n#if !V3_MM_FEATURE\n#include "active.mm"\n#endif\n\nfn C.answer_from_valued_m_guard() int\nfn C.answer_from_valued_mm_guard() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_valued_m_guard() + C.answer_from_valued_mm_guard()))\n}\n'
	}, 'main.v')
	assert out == '71'
}

fn test_external_bare_macro_objective_c_guards_remain_possible() {
	v3_bin := build_v3()
	out := run_good_project_with_flags(v3_bin, 'external_bare_macro_objective_c_guards',
		'-cc clang', {
		'v.mod':           "Module { name: 'external_bare_macro_objective_c_guards' }\n"
		'config.h':        '#define V3_HEADER_FEATURE 1\n'
		'forced.h':        '#define V3_FORCED_FEATURE 1\n'
		'source_defs.c':   '#define V3_SOURCE_FEATURE 1\n'
		'active.m':        'static int answer_from_header_macro_guard(void) { return 2; }\n'
		'active.mm':       'extern "C" int answer_from_forced_macro_guard(void) { auto answer = []() { return 70; }; return answer(); }\n'
		'source_active.m': 'static int answer_from_source_macro_guard(void) { return 3; }\n'
		'main.v':          'module main\n\n#flag -UV3_FORCED_FEATURE\n#flag -include @VMODROOT/forced.h\n\n#undef V3_HEADER_FEATURE\n#include "config.h"\n#if V3_HEADER_FEATURE\n#include "active.m"\n#endif\n\n#include "source_defs.c"\n#if V3_SOURCE_FEATURE\n#include "source_active.m"\n#endif\n\n#if V3_FORCED_FEATURE\n#include "active.mm"\n#endif\n\nfn C.answer_from_header_macro_guard() int\nfn C.answer_from_source_macro_guard() int\nfn C.answer_from_forced_macro_guard() int\n\nfn main() {\n\tprintln(int_str(C.answer_from_header_macro_guard() + C.answer_from_source_macro_guard() + C.answer_from_forced_macro_guard()))\n}\n'
	}, 'main.v')
	assert out == '75'
}

fn test_review_fixed_array_alias_clone_dispatch() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'fixed_array_alias_clone_dispatch', 'type FixedClone = [2]int

fn (value FixedClone) clone() FixedClone {
	return FixedClone([value[1], value[0]]!)
}

fn make_fixed_clone() FixedClone {
	return FixedClone([1, 2]!)
}

fn main() {
	value := make_fixed_clone()
	cloned := value.clone()
	println(int_str(cloned[0]))
	println(int_str(cloned[1]))
}
')
	assert out == '2\n1'
}

fn test_review_generic_pointer_sizeof() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'generic_pointer_sizeof', 'struct SizeBox[T] {
	value T
}

fn main() {
	first := &SizeBox[int]{
		value: 1
	}
	lhs := [2]&SizeBox[int]{init: first}
	rhs := [2]&SizeBox[int]{init: first}
	println(sizeof(&SizeBox[int]) == sizeof(voidptr))
	println(sizeof([2]&SizeBox[int]) == 2 * sizeof(voidptr))
	println(lhs == rhs)
}
')
	assert out == 'true\ntrue\ntrue'
}

fn test_followup_review_pointer_call_and_equality_semantics() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'c_voidptr_does_not_auto_address', 'fn C.take(voidptr)

fn main() {
	value := 7
	C.take(value)
}
',
		'cannot use `int` as argument')
	run_bad(v3_bin, 'v_voidptr_enum_value_does_not_auto_address', 'enum Color {
	red
}

fn take(value voidptr) {
	_ = value
}

fn main() {
	take(Color.red)
}
',
		'cannot use `Color` as argument')
	v_call_out := run_good(v3_bin, 'v_voidptr_auto_address', 'const voidptr_const_value = 9

fn take(value voidptr) int {
	return unsafe { *(&int(value)) }
}

fn main() {
	value := 7
	println(int_str(take(value)))
	println(int_str(take(voidptr_const_value)))
}
')
	assert v_call_out == '7\n9'
	pointer_for_out := run_good(v3_bin, 'mut_pointer_for_in_arg', 'struct PointerItem {
mut:
	value int
}

fn increment(mut item PointerItem) {
	item.value++
}

fn main() {
	mut first := PointerItem{
		value: 1
	}
	mut second := PointerItem{
		value: 2
	}
	mut items := [&first, &second]
	for mut item in items {
		increment(mut item)
	}
	println(int_str(first.value))
	println(int_str(second.value))
}
')
	assert pointer_for_out == '2\n3'
	optional_pointer_out := run_good(v3_bin, 'optional_pointer_equality_semantics', 'struct Item {
	value int
}

struct Bag {
	items []int
}

type BagRef = &Bag

fn main() {
	a := 7
	b := 7
	item_a := Item{
		value: 7
	}
	item_b := Item{
		value: 7
	}
	opt_a := unsafe { ?&int(&a) }
	opt_b := unsafe { ?&int(&b) }
	opt_item_a := unsafe { ?&Item(&item_a) }
	opt_item_b := unsafe { ?&Item(&item_b) }
	bag_a := &Bag{
		items: [1]
	}
	bag_b := &Bag{
		items: [1]
	}
	nil_bag := &Bag(unsafe { nil })
	opt_bag_a := unsafe { ?&Bag(bag_a) }
	opt_bag_b := unsafe { ?&Bag(bag_b) }
	opt_nil_bag := unsafe { ?&Bag(nil_bag) }
	opt_alias_bag_a := unsafe { ?BagRef(BagRef(bag_a)) }
	opt_alias_bag_b := unsafe { ?BagRef(BagRef(bag_b)) }
	opt_alias_nil_bag := unsafe { ?BagRef(BagRef(nil_bag)) }
	println(opt_a == opt_b)
	println(opt_a == opt_a)
	println(opt_item_a == opt_item_b)
	println(opt_nil_bag == opt_bag_a)
	println(opt_nil_bag == opt_nil_bag)
	println(opt_bag_a == opt_bag_b)
	println(opt_alias_bag_a == opt_alias_bag_b)
	println(opt_alias_nil_bag == opt_alias_bag_a)
	println(opt_alias_nil_bag == opt_alias_nil_bag)
}
')
	assert optional_pointer_out == 'false\ntrue\ntrue\nfalse\ntrue\ntrue\ntrue\nfalse\ntrue'
	mut_pointer_iteration_out := run_good(v3_bin, 'mut_pointer_iteration_rebinds_slots', 'struct Item {
mut:
	value int
}

fn increment(mut item Item) {
	item.value++
}

fn main() {
	first := &Item{
		value: 1
	}
	mut second := &Item{
		value: 2
	}
	mut dynamic := [first]
	for mut item in dynamic {
		item = second
		increment(mut item)
	}
	mut fixed := [first]!
	for mut item in fixed {
		item = second
		increment(mut *item)
	}
	second.value = 9
	println(int_str(dynamic[0].value))
	println(int_str(fixed[0].value))
}
')
	assert mut_pointer_iteration_out == '9\n9'
	ordinary_pointer_out := run_good(v3_bin, 'ordinary_pointer_value_param', 'struct PointerCallItem {
	value int
}

fn take_value(item PointerCallItem) int {
	return item.value
}

fn main() {
	item := &PointerCallItem{
		value: 7
	}
	println(int_str(take_value(item)))
}
')
	assert ordinary_pointer_out == '7'
}

fn test_map_retains_address_of_local_after_return() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'map_retains_address_of_local_after_return', 'struct Item {
	value int
}

fn make_cache() map[string]&Item {
	mut cache := map[string]&Item{}
	mut local := Item{
		value: 7
	}
	ptr := &local
	cache["alias"] = ptr
	cache["direct"] = &local
	return cache
}

fn main() {
	cache := make_cache()
	println(int_str(cache["alias"].value))
	println(int_str(cache["direct"].value))
}
')
	assert out == '7\n7'
}

fn test_map_retains_addresses_of_same_named_branch_locals() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'map_retains_addresses_of_same_named_branch_locals', 'struct Item {
mut:
	value int
}

fn make_cache() map[string]&Item {
	mut cache := map[string]&Item{}
	mut outer := Item{
		value: 3
	}
	cache["outer"] = &outer
	if true {
		mut local := Item{
			value: 1
		}
		cache["first"] = &local
		local.value = 11
	}
	if true {
		mut local := Item{
			value: 2
		}
		cache["second"] = &local
		local.value = 22
	}
	outer.value = 33
	return cache
}

fn main() {
	cache := make_cache()
	println(int_str(cache["first"].value))
	println(int_str(cache["second"].value))
	println(int_str(cache["outer"].value))
}
')
	assert out == '11\n22\n33'
}

fn test_lambda_capture_counts_as_local_use_without_counting_shadowed_parameters() {
	check_src := '${tmp_test_path('lambda_capture_local_usage')}.v'
	os.write_file(check_src, 'fn apply(f fn (int) int, value int) int {
	return f(value)
}

fn captured() int {
	offset := 7
	return apply(|n| n + offset, 5)
}

fn shadowed() int {
	shadowed_offset := 7
	return apply(|shadowed_offset| shadowed_offset + 1, 5)
}
') or {
		panic(err)
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(check_src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	assert !tc.notices.any(it.msg == 'unused variable: `offset`'), tc.notices.str()
	assert tc.notices.any(it.msg == 'unused variable: `shadowed_offset`'), tc.notices.str()

	mut fixture_parser := parser.Parser.new(prefs)
	mut fixture_ast := fixture_parser.parse_file(check_src)
	mut fixture_tc := types.TypeChecker.new(fixture_ast)
	fixture_tc.checker_fixture_mode = true
	fixture_tc.collect(fixture_ast)
	fixture_tc.check_semantics()
	assert fixture_tc.errors.any(it.msg == 'undefined variable `offset`'), fixture_tc.errors.str()
	assert fixture_tc.errors.any(it.msg == '`offset` used as value'), fixture_tc.errors.str()
	assert !fixture_tc.errors.any(it.msg.contains('shadowed_offset')), fixture_tc.errors.str()
	assert fixture_tc.notices.any(it.msg == 'unused variable: `offset`'), fixture_tc.notices.str()
}

fn test_duplicate_function_diagnostics_survive_unrelated_semantic_errors() {
	check_src := '${tmp_test_path('duplicate_fn_with_unrelated_error')}.v'
	os.write_file(check_src, "fn duplicate() {}

fn duplicate(value int) {}

fn unrelated() int {
	return 'bad'
}
") or {
		panic(err)
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(check_src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.any(it.severity == 'builder error:'
		&& it.msg == 'redefinition of function `duplicate`'), tc.errors.str()
	assert tc.errors.filter(it.severity == 'conflicting declaration:'
		&& it.node_value == 'duplicate').len == 2, tc.errors.str()
	assert tc.errors.any(it.msg.contains('cannot use `string` as type `int` in return argument')), tc.errors.str()
}

fn test_repeated_template_lines_keep_distinct_diagnostic_positions() {
	v3_bin := build_v3()
	root := '${tmp_test_path('repeated_template_line_diagnostics')}_project'
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_project_file(root, 'main.v', "module main

fn main() {
	\$tmpl('repeated.txt')
}
")
	write_project_file(root, 'repeated.txt', '@unknown_var
middle
@unknown_var
')
	output := tmp_test_path('repeated_template_line_diagnostics')
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(output)}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('repeated.txt:1:3: error: undefined ident: `unknown_var`'), compile.output

	assert compile.output.contains('repeated.txt:3:3: error: undefined ident: `unknown_var`'), compile.output
}

fn test_template_interpolations_keep_distinct_columns() {
	v3_bin := build_v3()
	root := '${tmp_test_path('template_interpolation_columns')}_project'
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_project_file(root, 'main.v', "module main

fn main() {
	\$tmpl('columns.txt')
}
")
	write_project_file(root, 'columns.txt', '@unknown @unknown
')
	output := tmp_test_path('template_interpolation_columns')
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(output)}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('columns.txt:1:3: error: undefined ident: `unknown`'), compile.output
	assert compile.output.contains('columns.txt:1:12: error: undefined ident: `unknown`'), compile.output
	assert compile.output.contains('    1 | @unknown @unknown\n      |  ~~~~~~~'), compile.output
}

fn test_explicit_template_interpolations_use_expression_columns() {
	v3_bin := build_v3()
	root := '${tmp_test_path('explicit_template_interpolation_columns')}_project'
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_project_file(root, 'main.v', "module main

fn main() {
	\$tmpl('explicit_columns.txt')
}
")
	write_project_file(root, 'explicit_columns.txt', '@{missing} @(absent)
')
	output := tmp_test_path('explicit_template_interpolation_columns')
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(output)}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('explicit_columns.txt:1:4: error: undefined ident: `missing`'), compile.output

	assert compile.output.contains('explicit_columns.txt:1:15: error: undefined ident: `absent`'), compile.output
	assert compile.output.contains('    1 | @{missing} @(absent)\n      |   ~~~~~~~'), compile.output
}

fn test_dollar_template_interpolations_use_expression_columns() {
	v3_bin := build_v3()
	root := '${tmp_test_path('dollar_template_interpolation_columns')}_project'
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_project_file(root, 'main.v', "module main

fn main() {
	\$tmpl('dollar_columns.txt')
}
")
	write_project_file(root, 'dollar_columns.txt', '\${first} @second
')
	output := tmp_test_path('dollar_template_interpolation_columns')
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(output)}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('dollar_columns.txt:1:4: error: undefined ident: `first`'), compile.output
	assert compile.output.contains('dollar_columns.txt:1:12: error: undefined ident: `second`'), compile.output
	assert compile.output.contains('    1 | \${first} @second\n      |   ~~~~~'), compile.output
}

fn test_template_translation_shorthand_diagnostics_use_template_source() {
	v3_bin := build_v3()
	root := '${tmp_test_path('template_translation_shorthand_diagnostics')}_project'
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_project_file(root, 'main.v', "module main

fn main() {
	\$tmpl('translation.html')
}
")
	write_project_file(root, 'translation.html', '%title
')
	output := tmp_test_path('template_translation_shorthand_diagnostics')
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(output)}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('translation.html:1:'), compile.output
	assert compile.output.contains('undefined ident: `ctx`'), compile.output
	assert compile.output.contains('called from ') && compile.output.contains('/main.v:4:2'), compile.output
	assert !compile.output.contains('<veb-template>'), compile.output
}

fn test_template_control_diagnostics_use_template_source() {
	v3_bin := build_v3()
	root := '${tmp_test_path('template_control_diagnostics')}_project'
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_project_file(root, 'main.v', "module main

fn main() {
	\$tmpl('control.txt')
}
")
	write_project_file(root, 'control.txt', '@if missing {
value
@end
@for item in missing_items {
@item
@end
')
	output := tmp_test_path('template_control_diagnostics')
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(output)}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('control.txt:1:5: error: undefined ident: `missing`'), compile.output
	assert compile.output.contains('control.txt:4:14: error: undefined ident: `missing_items`'), compile.output
	assert compile.output.contains('called from ') && compile.output.contains('/main.v:4:2'), compile.output
}

fn test_inline_template_control_bodies_use_template_source() {
	v3_bin := build_v3()
	root := '${tmp_test_path('inline_template_control_diagnostics')}_project'
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_project_file(root, 'main.v', "module main

fn main() {
	\$tmpl('inline_control.txt')
}
")
	write_project_file(root, 'inline_control.txt', '@if true { @missing_if }
@for item in [1] { @missing_for }
')
	output := tmp_test_path('inline_template_control_diagnostics')
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(output)}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('inline_control.txt:1:14: error: undefined ident: `missing_if`'), compile.output

	assert compile.output.contains('inline_control.txt:2:22: error: undefined ident: `missing_for`'), compile.output

	assert compile.output.contains('called from ') && compile.output.contains('/main.v:4:2'), compile.output
	assert !compile.output.contains('<veb-template>'), compile.output
}

fn test_qualified_struct_literal_in_select_send_condition() {
	v3_bin := build_v3()
	result := run_good_project_result(v3_bin, 'qualified_struct_literal_select_send', '', {
		'v.mod':               "Module { name: 'qualified_struct_literal_select_send' }\n"
		'messages/messages.v': 'module messages

pub struct Msg {
pub:
	x int
}
'
		'main.v':              'module main

import messages

fn main() {
	ch := chan messages.Msg{cap: 2}
	select {
		ch <- messages.Msg{
			x: 7
		} {
			println(7)
		}
	}
	select {
		ch <- messages.Msg {
			x: 8
		} {
			println(8)
		}
	}
}
'
	}, 'main.v')
	assert !result.compile_output.contains('unexpected token'), result.compile_output
	assert result.run_output == '7\n8', result.run_output
}

fn test_imported_lowercase_selector_with_attached_block_is_not_struct_literal() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'imported_lowercase_selector_attached_block', {
		'v.mod':         "Module { name: 'imported_lowercase_selector_attached_block' }\n"
		'flags/flags.v': 'module flags

pub const enabled = true
'
		'main.v':        'module main

import flags

fn main() {
	mut seen := 0
	if flags.enabled{
		seen = 1
	}
	println(seen)
}
'
	}, 'main.v')
	assert out == '1'
}

fn test_number_prefixed_identifier_suppression_stays_in_declaration_scope() {
	path := '${tmp_test_path('number_prefixed_identifier_scopes')}.v'
	os.write_file(path, 'fn declares() {
	mut 3a := 1
}

fn uses() {
	println(3a)
}

fn compares() {
	if 3a == 0 {}
}

fn same_scope() {
	mut 4b := 2
	println(4b)
}
') or {
		panic(err)
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_file(path)
	three_a := p.diagnostics.filter(it.message == 'identifier name `3a` cannot start with a number')
	assert three_a.len == 3, p.diagnostics.str()
	assert three_a.map(it.line) == [2, 6, 10], p.diagnostics.str()
	four_b := p.diagnostics.filter(it.message == 'identifier name `4b` cannot start with a number')
	assert four_b.len == 1, p.diagnostics.str()
	assert four_b[0].line == 14, p.diagnostics.str()
}

fn test_recursive_str_helper_progress_must_cover_early_return_paths() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_helper_early_return', 'struct Item {
mut:
	remaining int
}

fn maybe_decrement(mut item Item, stop bool) {
	if stop {
		return
	}
	item.remaining--
}

fn (item Item) str() string {
	mut next := item
	maybe_decrement(mut next, true)
	return next.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_unknown_struct_suppression_stays_with_related_generic_declaration() {
	v3_bin := build_v3()
	src := 'fn broken[U](value T) {
	_ := value
}

fn unrelated() {
	_ := T{}
}

fn main() {}
'
	bad_src := '${tmp_test_path('unrelated_unknown_struct')}.v'
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := tmp_test_path('unrelated_unknown_struct')
	compile := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('generic type name `T` is not mentioned in fn `broken[U]`'), compile.output
	assert compile.output.contains('unknown struct `T`'), compile.output
}

fn test_generic_array_suppression_keeps_unrelated_unknown_type_errors() {
	v3_bin := build_v3()
	src := 'struct Example[T] {}

fn main() {
	_ = T(0)
	_ = []Example[T]{}
}
'
	bad_src := '${tmp_test_path('generic_array_unrelated_unknown_type')}.v'
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := tmp_test_path('generic_array_unrelated_unknown_type')
	compile := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert compile.exit_code != 0, compile.output
	unknown_lines :=
		compile.output.split_into_lines().filter(it.contains('error: unknown type `T`'))
	assert unknown_lines.len > 0, compile.output
	assert unknown_lines.all(it.contains(':4:')), compile.output
	assert compile.output.contains('generic struct cannot be used in non-generic function'), compile.output
}

fn test_recursive_str_loop_progress_retains_zero_iteration_path() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_zero_iteration_loop', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	mut next := item
	for _ in []int{} {
		next.remaining--
	}
	return next.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_or_fallback_progress_is_conditional() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_conditional_or_fallback', 'struct Item {
mut:
	remaining int
}

fn maybe() ?int {
	return 1
}

fn (item Item) str() string {
	mut copy := item
	_ := maybe() or {
		copy.remaining--
		0
	}
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_short_circuit_progress_is_conditional() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_logical_and_rhs_progress', 'struct Item {
mut:
	remaining int
}

fn advance(mut item Item) bool {
	item.remaining--
	return true
}

fn (item Item) str() string {
	mut copy := item
	_ = false && advance(mut copy)
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_logical_or_rhs_progress', 'struct Item {
mut:
	remaining int
}

fn advance(mut item Item) bool {
	item.remaining--
	return true
}

fn (item Item) str() string {
	mut copy := item
	_ = true || advance(mut copy)
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_unreachable_short_circuit_call', 'struct Item {}

fn (item Item) str() string {
	if false && item.str() == "never" {
		return "unreachable"
	}
	if true || item.str() == "never" {
		return "ok"
	}
	return "unreachable"
}

fn main() {
	println(Item{}.str())
}
')
	assert out == 'ok'
}

fn test_recursive_str_comptime_if_uses_selected_branch() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_inactive_comptime_progress', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	mut copy := item
	$if threads {
		copy.remaining--
	}
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_selected_comptime_progress', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return ""
	}
	mut copy := item
	$if threads {
	} $else {
		copy.remaining--
	}
	return copy.str()
}

fn main() {
	println(Item{
		remaining: 2
	}.str())
}
')
	assert out == ''
	inactive_call_out := run_good(v3_bin, 'recursive_str_inactive_comptime_call', 'struct Item {}

fn (item Item) str() string {
	$if threads {
		return item.str()
	} $else {
		return "ok"
	}
}

fn main() {
	println(Item{}.str())
}
')
	assert inactive_call_out == 'ok'
}

fn test_recursive_str_helper_return_preserves_receiver_provenance() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_helper_return_alias', 'struct Item {}

fn same(item Item) Item {
	return item
}

fn (item Item) str() string {
	return same(item).str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_invoked_helper_calls_are_analyzed() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_invoked_helper_call', 'struct Item {}

fn recurse(item Item) string {
	return item.str()
}

fn (item Item) str() string {
	return recurse(item)
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_progressed_invoked_helper_call', 'struct Item {
mut:
	remaining int
}

fn recurse(item Item) string {
	return item.str()
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return "done"
	}
	mut copy := item
	copy.remaining--
	return recurse(copy)
}

fn main() {
	println(Item{
		remaining: 1
	}.str())
}
')
	assert out == 'done'
	cycle_out := run_good(v3_bin, 'recursive_str_helper_analysis_cycle_guard', 'struct Item {}

fn ping(item Item, remaining int) string {
	if remaining <= 0 {
		return "done"
	}
	return pong(item, remaining - 1)
}

fn pong(item Item, remaining int) string {
	return ping(item, remaining)
}

fn (item Item) str() string {
	return ping(item, 1)
}

fn main() {}
')
	assert cycle_out == ''
}

fn test_recursive_str_helper_multiple_returns_merge_provenance() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_helper_multiple_return_aliases', 'struct Item {
mut:
	remaining int
}

fn choose(original Item, changed Item, use_original bool) Item {
	if use_original {
		return original
	}
	return changed
}

fn (item Item) str() string {
	mut changed := item
	changed.remaining--
	return choose(item, changed, true).str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_constant_true_branch_has_no_fallthrough() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'recursive_str_constant_true_branch', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return ""
	}
	mut copy := item
	if true {
		copy.remaining--
	}
	return copy.str()
}

	fn main() {}
')
	assert out == ''
	helper_out := run_good(v3_bin, 'recursive_str_helper_constant_true_branch', 'struct Item {
mut:
	remaining int
}

fn advance(mut item Item) {
	if true {
		item.remaining--
	}
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return ""
	}
	mut copy := item
	advance(mut copy)
	return copy.str()
}

fn main() {}
')
	assert helper_out == ''
}

fn test_recursive_str_helper_rebind_transfers_receiver_provenance() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_helper_rebind_source', 'struct Item {
mut:
	remaining int
}

fn copy_from(mut destination Item, source Item) {
	destination = source
}

fn (item Item) str() string {
	mut copy := item
	copy.remaining--
	copy_from(mut copy, item)
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_spawn_mutation_is_not_synchronous_progress() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_spawned_mutation', 'struct Item {
mut:
	remaining int
}

fn advance(mut item Item) {
	item.remaining--
}

fn (item Item) str() string {
	mut copy := item
	spawn advance(mut copy)
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_spawned_call', 'struct Item {}

fn (item Item) str() string {
	t := spawn item.str()
	return t.wait()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_assertion_mutation_is_not_guaranteed_progress() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_assertion_mutation', 'struct Item {
mut:
	remaining int
}

fn advance(mut item Item) bool {
	item.remaining--
	return true
}

fn (item Item) str() string {
	mut copy := item
	assert advance(mut copy)
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_unreachable_assert_message', 'struct Item {}

fn (item Item) str() string {
	assert true, item.str()
	return "ok"
}

fn main() {
	println(Item{}.str())
}
')
	assert out == 'ok'
	run_bad(v3_bin, 'recursive_str_reachable_assert_message', 'struct Item {}

fn (item Item) str() string {
	assert false, item.str()
	return "unreachable"
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_select_branches_have_isolated_progress() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_select_branch_progress', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	mut copy := item
	ch := chan int{cap: 1}
	select {
		ch <- 1 {
			copy.remaining--
		}
		else {
			return copy.str()
		}
	}
	return ""
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_deferred_mutation_does_not_count_as_progress() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_deferred_mutation', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	mut copy := item
	defer {
		copy.remaining--
	}
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_deferred_calls_use_scope_exit_state_in_lifo_order() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_deferred_call_exit_state', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	mut next := item
	next.remaining--
	defer {
		_ := next.str()
	}
	next = item
	return ""
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_deferred_call_before_mutation', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	mut next := item
	defer {
		next.remaining--
	}
	defer {
		_ := next.str()
	}
	return ""
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_deferred_mutation_before_call', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	mut next := item
	defer {
		_ := next.str()
	}
	defer {
		next.remaining--
	}
	return ""
}

fn main() {}
')
	assert out == ''
}

fn test_recursive_str_does_not_execute_stored_closure_bodies() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'recursive_str_stored_closure_bodies', 'struct LambdaItem {}

fn (item LambdaItem) str() string {
	callback := || item.str()
	_ = callback
	return "lambda"
}

struct LiteralItem {}

fn (item LiteralItem) str() string {
	callback := fn [item] () string {
		return item.str()
	}
	_ = callback
	return "literal"
}

fn main() {
	println(LambdaItem{}.str())
	println(LiteralItem{}.str())
}
')
	assert out == 'lambda\nliteral'
}

fn test_recursive_str_invoked_closure_preserves_receiver_provenance() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_invoked_lambda', 'struct LambdaItem {}

fn (item LambdaItem) str() string {
	callback := || item.str()
	return callback()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_invoked_fn_literal', 'struct LiteralItem {}

fn (item LiteralItem) str() string {
	callback := fn [item] () string {
		return item.str()
	}
	return callback()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_unbacked_enum_field_keeps_integer_overflow_diagnostic() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'unbacked_enum_integer_overflow', 'enum Huge {
	value = 18446744073709551616
}

fn main() {}
',
		'integer literal 18446744073709551616 overflows int')
}

fn test_diagnostic_footer_uses_deduplicated_error_count() {
	v3_bin := build_v3()
	mut source := ''
	for index in 0 .. 15 {
		source += 'fn broken_${index}[U](value T) {
	_ := T{}
}

'
	}
	source += 'fn main() {}
'
	bad_src := '${tmp_test_path('deduplicated_error_footer')}.v'
	os.write_file(bad_src, source) or { panic(err) }
	bad_bin := tmp_test_path('deduplicated_error_footer')
	compile := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.count('generic type name `T` is not mentioned in fn') == 15, compile.output
	assert !compile.output.contains('unknown struct `T`'), compile.output
	assert !compile.output.contains('more errors'), compile.output
}

fn test_parameter_redefinition_only_suppresses_related_unused_notices() {
	v3_bin := build_v3()
	src := 'fn broken(value int, value string) {
	same_function_unused := 1
}

fn unrelated() {
	unused := 1
}

fn main() {}
'
	bad_src := '${tmp_test_path('parameter_redefinition_unrelated_notice')}.vv'
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := tmp_test_path('parameter_redefinition_unrelated_notice')
	compile := os.execute('${v3_bin} -checker-fixture ${bad_src} -b c -o ${bad_bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('redefinition of parameter `value`'), compile.output
	assert compile.output.contains('unused variable: `same_function_unused`'), compile.output
	assert compile.output.contains('unused variable: `unused`'), compile.output
}

fn test_malformed_function_call_keeps_unrelated_unused_notice() {
	v3_bin := build_v3()
	src := 'fn bad(value int, value int) int {
	return value
}

fn caller() {
	unused := bad(1, 2)
}

fn main() {}
'
	bad_src := '${tmp_test_path('malformed_function_call_unused_notice')}.vv'
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := tmp_test_path('malformed_function_call_unused_notice')
	compile := os.execute('${v3_bin} -checker-fixture ${bad_src} -b c -o ${bad_bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('redefinition of parameter `value`'), compile.output
	assert compile.output.contains('unused variable: `unused`'), compile.output
}

fn test_optional_typed_map_rejects_populated_braces() {
	path := '${tmp_test_path('optional_typed_map_populated')}.v'
	os.write_file(path, "fn main() {\n\t_ := ?map[string]int{'x': 1}\n}\n") or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_file(path)
	assert p.diagnostics.any(it.message == '`}` expected; explicit `map` initialization does not support parameters'), p.diagnostics.str()

	empty_path := '${tmp_test_path('optional_typed_map_empty')}.v'
	os.write_file(empty_path, 'fn main() {
	_ := ?map[string]int{}
}
') or { panic(err) }
	mut empty_parser := parser.Parser.new(prefs)
	empty_parser.parse_file(empty_path)
	assert !empty_parser.diagnostics.any(it.message.contains('explicit `map` initialization does not support parameters')), empty_parser.diagnostics.str()
}

fn test_undefined_variable_preserves_unrelated_unused_import() {
	check_src := '${tmp_test_path('undefined_variable_unrelated_import')}.v'
	os.write_file(check_src, 'import os

fn main() {
	value := value
	println(value)
}
') or {
		panic(err)
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(check_src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.any(it.msg.starts_with('undefined variable') && it.node_value == 'value'), tc.errors.str()

	assert tc.notices.any(it.msg.contains("module 'os' is imported but never used")), tc.notices.str()
}

fn test_recursive_str_helper_merges_incompatible_branch_effects_conservatively() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_incompatible_helper_effects', 'struct Item {
	rebind bool
mut:
	values []int
}

fn change(mut item Item) {
	if item.rebind {
		item.values[0]--
	} else {
		item = Item{
			values: [1]
		}
	}
}

fn (item Item) str() string {
	mut copy := item
	change(mut copy)
	return item.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_array_append_counts_as_progress() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'recursive_str_array_append_progress', 'struct Item {
mut:
	items []int
}

fn (item Item) str() string {
	if item.items.len == 2 {
		return "done"
	}
	mut copy := item
	copy.items << 1
	return copy.str()
}

fn main() {
	println(Item{}.str())
}
')
	assert out == 'done'
}

fn test_recursive_str_exhaustive_enum_and_sum_matches_count_as_progress() {
	v3_bin := build_v3()
	enum_out := run_good(v3_bin, 'recursive_str_exhaustive_enum_match_progress', 'enum Mode {
	one
	two
}

struct Item {
	mode Mode
mut:
	remaining int
}

fn decrement(mut item Item) {
	match item.mode {
		.one { item.remaining-- }
		.two { item.remaining-- }
	}
}

fn (item Item) str() string {
	if item.remaining == 0 {
		return "done"
	}
	mut copy := item
	decrement(mut copy)
	return copy.str()
}

fn main() {
	println(Item{
		remaining: 1
	}.str())
}
')
	assert enum_out == 'done'

	sum_out := run_good(v3_bin, 'recursive_str_exhaustive_sum_match_progress', 'struct First {}
struct Second {}
type Mode = First | Second

struct Item {
	mode Mode = First{}
mut:
	remaining int
}

fn decrement(mut item Item) {
	match item.mode {
		First { item.remaining-- }
		Second { item.remaining-- }
	}
}

fn (item Item) str() string {
	if item.remaining == 0 {
		return "done"
	}
	mut copy := item
	decrement(mut copy)
	return copy.str()
}

fn main() {
	println(Item{
		remaining: 1
	}.str())
}
')
	assert sum_out == 'done'
}

fn test_recursive_str_direct_exhaustive_matches_count_as_progress() {
	v3_bin := build_v3()
	enum_out := run_good(v3_bin, 'recursive_str_direct_exhaustive_enum_match', 'enum Mode {
	one
	two
}

struct Item {
	mode Mode
mut:
	remaining int
}

fn (item Item) str() string {
	if item.remaining == 0 {
		return "done"
	}
	mut copy := item
	match copy.mode {
		.one { copy.remaining-- }
		.two { copy.remaining-- }
	}
	return copy.str()
}

fn main() {
	println(Item{
		remaining: 1
	}.str())
}
')
	assert enum_out == 'done'

	sum_out := run_good(v3_bin, 'recursive_str_direct_exhaustive_sum_match', 'struct First {}
struct Second {}
type Mode = First | Second

struct Item {
	mode Mode = First{}
mut:
	remaining int
}

fn (item Item) str() string {
	if item.remaining == 0 {
		return "done"
	}
	mut copy := item
	match copy.mode {
		First { copy.remaining-- }
		Second { copy.remaining-- }
	}
	return copy.str()
}

fn main() {
	println(Item{
		remaining: 1
	}.str())
}
')
	assert sum_out == 'done'
}

fn test_recursive_str_noop_mutations_do_not_count_as_progress() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_add_zero_noop', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	mut copy := item
	copy.remaining += 0
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_multiply_one_noop', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	mut copy := item
	copy.remaining *= 1
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_helper_add_zero_noop', 'struct Item {
mut:
	remaining int
}

fn unchanged(mut item Item) {
	item.remaining += 0
}

fn (item Item) str() string {
	mut copy := item
	unchanged(mut copy)
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_reversed_mutations_do_not_count_as_progress() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_reversed_increment', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	mut copy := item
	copy.remaining--
	copy.remaining++
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_reversed_compound_assignment', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	mut copy := item
	copy.remaining += 2
	copy.remaining -= 2
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_nested_helper_mutations_count_as_progress() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'recursive_str_nested_helper_progress', 'struct Item {
mut:
	remaining int
}

fn decrement(mut item Item) {
	item.remaining--
}

fn advance(mut item Item) {
	decrement(mut item)
}

fn (item Item) str() string {
	if item.remaining == 0 {
		return "done"
	}
	mut copy := item
	advance(mut copy)
	return copy.str()
}

fn main() {
	println(Item{
		remaining: 1
	}.str())
}
')
	assert out == 'done'
}

fn test_recursive_str_helper_terminal_rebind_is_not_progress() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_helper_terminal_rebind', 'struct Item {
mut:
	remaining int
}

fn advance(mut copy Item, original Item, reset bool) {
	copy.remaining--
	if reset {
		copy = original
		return
	}
}

fn (item Item) str() string {
	mut copy := item
	advance(mut copy, item, true)
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_duplicate_function_diagnostics_survive_body_errors() {
	check_src := '${tmp_test_path('duplicate_fn_with_body_error')}.v'
	os.write_file(check_src, "fn duplicate() int {
	return 'bad'
}

fn duplicate(value int) {}
") or {
		panic(err)
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(check_src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.any(it.severity == 'builder error:'
		&& it.msg == 'redefinition of function `duplicate`'), tc.errors.str()
	assert tc.errors.filter(it.severity == 'conflicting declaration:'
		&& it.node_value == 'duplicate').len == 2, tc.errors.str()
	assert tc.errors.any(it.msg.contains('cannot use `string` as type `int` in return argument')), tc.errors.str()
}

fn test_bare_generic_inference_suppression_stays_with_return_declaration() {
	check_src := '${tmp_test_path('bare_generic_inference_scope')}.v'
	source := 'struct GenericChannelStruct[T] {
	ch chan T
}

struct Simple {
	msg string
}

fn main() {
	new_channel_struct[Simple]()
}

pub fn new_channel_struct[T]() GenericChannelStruct {
	d := GenericChannelStruct{
		ch: chan T{}
	}
	return d
}

fn unrelated() {
	_ := GenericChannelStruct{}
}
'
	os.write_file(check_src, source) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(check_src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.any(it.msg.starts_with('return generic struct `GenericChannelStruct` in fn declaration must specify the generic type names')), tc.errors.str()

	inference_errors :=
		tc.errors.filter(it.msg == 'could not infer generic type `T` in generic struct `GenericChannelStruct[T]`')
	assert inference_errors.len == 1, tc.errors.str()
	unrelated_start := source.index('fn unrelated') or { panic('missing unrelated function') }
	assert inference_errors[0].pos.offset > unrelated_start, tc.errors.str()
}

fn test_nested_generic_receiver_call_waits_for_receiver_type() {
	check_good('nested_generic_receiver_inference', 'struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

fn (tree Tree[T]) min[T]() T {
	return match tree {
		Empty { panic("empty tree") }
		Node[T] { tree.value }
	}
}

fn (tree Tree[T]) delete[T](value T) Tree[T] {
	return match tree {
		Empty { tree }
		Node[T] {
			Node[T]{
				...tree
				value: tree.right.min()
				right: tree.right.delete(tree.right.min())
			}
		}
	}
}
')
}

fn test_template_include_diagnostics_use_partial_source() {
	v3_bin := build_v3()
	root := '${tmp_test_path('template_include_diagnostic_source')}_project'
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_project_file(root, 'main.v', "module main

fn main() {
	\$tmpl('root.txt')
}
")
	write_project_file(root, 'root.txt', "before
@include 'partial.txt'
after
")
	write_project_file(root, 'partial.txt', 'partial first
@missing_from_partial
partial last
')
	output := tmp_test_path('template_include_diagnostic_source')
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(output)}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('partial.txt:2:3: error: undefined ident: `missing_from_partial`'), compile.output
	assert compile.output.contains('called from ') && compile.output.contains('/main.v:4:2'), compile.output
	assert !compile.output.contains('<veb-template>'), compile.output
}

fn test_template_import_diagnostics_preserve_each_line() {
	v3_bin := build_v3()
	root := '${tmp_test_path('template_import_diagnostic_lines')}_project'
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	write_project_file(root, 'main.v', "module main

fn main() {
	\$tmpl('imports.txt')
}
")
	write_project_file(root, 'imports.txt', '@import os
middle
@import json
')
	output := tmp_test_path('template_import_diagnostic_lines')
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(output)}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.count('invalid expression: unexpected keyword `import`') == 2, compile.output
	assert compile.output.count('expression does not return a value (veb action: main__main)') == 2, compile.output

	assert compile.output.count('imports.txt:1:30: error:') == 2, compile.output
	assert compile.output.count('imports.txt:3:30: error:') == 2, compile.output
}

fn test_template_css_import_is_emitted_literally() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'template_css_import', {
		'main.v':     "module main

fn main() {
	print(\$tmpl('style.html'))
}
"
		'style.html': "<style>
@import url('theme.css');
body { color: red; }
</style>
"
	}, 'main.v')
	assert out == "<style>
@import url('theme.css');
body { color: red; }
</style>"
}

fn test_recursive_str_bound_method_value_preserves_receiver_provenance() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_bound_method_value', 'struct Item {}

fn (item Item) str() string {
	recurse := item.str
	return recurse()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_indexed_bound_method_value', 'struct Item {}

fn (item Item) str() string {
	recurse := item.str
	callbacks := [recurse]
	return callbacks[0]()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_interface_bound_method_value', 'interface Printable {
	str() string
}

struct Item {}

fn (item Item) str() string {
	printable := Printable(item)
	recurse := printable.str
	return recurse()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_function_field_bound_method_value', 'struct Holder {
	cb fn () string
}

struct Item {}

fn (item Item) str() string {
	holder := Holder{
		cb: item.str
	}
	return holder.cb()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_helper_summary_keeps_later_rebind() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_helper_later_rebind', 'struct Item {
mut:
	remaining int
}

fn advance(mut copy Item, original Item) {
	copy.remaining--
	copy = original
}

fn (item Item) str() string {
	mut copy := item
	advance(mut copy, item)
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_user_c_string_function_is_not_inferred_unsafe() {
	v3_bin := build_v3()
	src_path := '${tmp_test_path('user_c_strlen_not_unsafe')}.v'
	bin_path := tmp_test_path('user_c_strlen_not_unsafe')
	os.write_file(src_path, "fn C.strlen(charptr) usize

fn main() {
	println(C.strlen(c'x'))
	_ = C.strerror(0)
}
") or {
		panic(err)
	}
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(src_path)} -b c -o ${os.quoted_path(bin_path)}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('must be called from an `unsafe` block'), compile.output
}

fn test_recursive_str_forward_goto_skips_progress() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_forward_goto', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	mut copy := item
	unsafe {
		goto recurse
	}
	copy.remaining--
recurse:
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_allows_recursing_into_child_values() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'recursive_str_child_value', 'struct Tree {
	children []Tree
}

fn (tree Tree) str() string {
	if tree.children.len == 0 {
		return "leaf"
	}
	return tree.children[0].str()
}

fn main() {
	println(Tree{
		children: [Tree{}]
	}.str())
}
')
	assert out == 'leaf'
}

fn test_recursive_str_helper_returned_descendant_is_distinct() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_helper_returned_aggregate_alias', 'struct Item {}

fn first(items []Item) Item {
	return items[0]
}

fn (item Item) str() string {
	items := [item]
	return first(items).str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_helper_returned_child', 'struct Tree {
	children []Tree
}

fn first(tree Tree) Tree {
	return tree.children[0]
}

fn (tree Tree) str() string {
	if tree.children.len == 0 {
		return "leaf"
	}
	return first(tree).str()
}

fn main() {
	println(Tree{
		children: [Tree{}]
	}.str())
}
')
	assert out == 'leaf'
}

fn test_recursive_str_preserves_provenance_through_array_elements() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_array_element_provenance', 'struct Item {}

fn (item Item) str() string {
	items := [item]
	return items[0].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_map_element_provenance', 'struct Item {}

fn (item Item) str() string {
	values := {"self": item}
	return values["self"].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_array_indexed_progress', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return ""
	}
	mut items := [item]
	items[0].remaining--
	return items[0].str()
}

fn main() {
	print(Item{
		remaining: 2
	}.str())
}
')
	assert out == ''
}

fn test_recursive_str_analyzes_array_map_callbacks() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_array_map_callback', 'struct Item {}

fn (item Item) str() string {
	return [item].map(fn (copy Item) string {
		return copy.str()
	}).join("")
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_preserves_multi_return_slots_and_aggregate_clones() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_multi_return_slot_provenance', 'struct Item {}

fn carry(item Item) (Item, int) {
	return item, 0
}

fn (item Item) str() string {
	copy, _ := carry(item)
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_multi_return_assign_slot_provenance', 'struct Item {}

fn carry(item Item) (Item, int) {
	return item, 0
}

fn (item Item) str() string {
	mut copy := Item{}
	copy, _ = carry(item)
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_array_clone_element_provenance', 'struct Item {}

fn (item Item) str() string {
	copies := [item].clone()
	return copies[0].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_map_clone_element_provenance', 'struct Item {}

fn (item Item) str() string {
	copies := {"self": item}.clone()
	return copies["self"].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_preserves_wrapper_append_and_helper_aggregate_provenance() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_wrapper_field_provenance', 'struct Item {}

struct Wrapper {
	value Item
}

fn (item Item) str() string {
	wrapped := Wrapper{
		value: item
	}
	return wrapped.value.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_array_append_provenance', 'struct Item {}

fn (item Item) str() string {
	mut items := []Item{}
	items << item
	return items[0].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_helper_array_provenance', 'struct Item {}

fn wrap(item Item) []Item {
	return [item]
}

fn (item Item) str() string {
	return wrap(item)[0].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_helper_map_provenance', 'struct Item {}

fn wrap(item Item) map[string]Item {
	return {"self": item}
}

fn (item Item) str() string {
	return wrap(item)["self"].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_helper_wrapper_provenance', 'struct Item {}

struct Wrapper {
	value Item
}

fn wrap(item Item) Wrapper {
	return Wrapper{
		value: item
	}
}

fn (item Item) str() string {
	return wrap(item).value.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_preserves_qualified_helper_args_and_slot_replacements() {
	v3_bin := build_v3()
	run_bad_project(v3_bin, 'recursive_str_qualified_helper_args', {
		'v.mod':             "Module { name: 'recursive_str_qualified_helper_args' }\n"
		'helpers/helpers.v': 'module helpers\n\npub interface Stringer {\n\tstr() string\n}\n\npub fn render(value Stringer) string {\n\treturn value.str()\n}\n'
		'main.v':            'module main\n\nimport helpers\n\nstruct Item {}\n\nfn (item Item) str() string {\n\treturn helpers.render(item)\n}\n\nfn main() {}\n'
	}, ['main.v'], 'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_static_helper_args', 'struct Helpers {}

struct Item {}

fn Helpers.render(item Item) string {
	return item.str()
}

fn (item Item) str() string {
	return Helpers.render(item)
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_array_slot_replacement', 'struct Item {
	remaining int
}

fn (item Item) str() string {
	mut items := [Item{
		remaining: 0
	}]
	items[0] = item
	return items[0].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_map_slot_replacement', 'struct Item {
	remaining int
}

fn (item Item) str() string {
	mut items := {"self": Item{
		remaining: 0
	}}
	items["self"] = item
	return items["self"].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_wrapper_field_replacement', 'struct Item {
	remaining int
}

struct Wrapper {
mut:
	value Item
}

fn (item Item) str() string {
	mut wrapped := Wrapper{
		value: Item{
			remaining: 0
		}
	}
	wrapped.value = item
	return wrapped.value.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_noreturn_branch_does_not_fall_through() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'recursive_str_noreturn_branch', '@[noreturn]
fn stop() {
	panic("done")
}

struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	mut copy := item
	if copy.remaining == 0 {
		stop()
	} else {
		copy.remaining--
	}
	return copy.str()
}

fn main() {}
')
	assert out == ''
}

fn test_map_rebind_clears_unsafe_alias_provenance() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'map_rebind_clears_unsafe_alias', 'fn main() {
	mut original := {
		"value": 1
	}
	mut alias := unsafe { original }
	alias = map[string]int{}
	copy := alias
	println(copy.len)
}
',
		'cannot copy map: call `move` or `clone` method (or use a reference)')
}

fn test_unsafe_map_alias_provenance_isolates_assert_messages() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'unsafe_map_alias_assert_message_assignment', 'fn main() {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	assert true, unsafe {
		alias = unsafe { original }
		"failed"
	}
	copy := alias
	println(copy.len)
}
',
		'cannot copy map: call `move` or `clone` method (or use a reference)')
	out := run_good(v3_bin, 'unsafe_map_alias_assert_message_rebind', 'fn main() {
	mut original := {
		"value": 1
	}
	mut alias := unsafe { original }
	assert true, unsafe {
		alias = map[string]int{}
		"failed"
	}
	copy := alias
	println(copy.len)
}
')
	assert out == '1'
	run_bad(v3_bin, 'unsafe_map_alias_assert_condition_assignment', 'fn main() {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	assert unsafe {
		alias = unsafe { original }
		true
	}
	copy := alias
	println(copy.len)
}
',
		'cannot copy map: call `move` or `clone` method (or use a reference)')
}

fn test_fresh_unsafe_map_is_not_reference_alias() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'fresh_unsafe_map_is_not_reference_alias', 'fn main() {
	mut alias := unsafe { map[string]int{} }
	copy := alias
	println(copy.len)
}
',
		'cannot copy map: call `move` or `clone` method (or use a reference)')
	out := run_good(v3_bin, 'unsafe_map_reference_alias', 'fn main() {
	mut original := {
		"value": 1
	}
	mut alias := unsafe { original }
	copy := alias
	println(copy.len)
}
')
	assert out == '1'
}

fn test_unsafe_map_alias_provenance_merges_conditional_expressions() {
	v3_bin := build_v3()
	if_out := run_good(v3_bin, 'unsafe_map_alias_if_expression', 'fn choose(cond bool) {
	mut left := {
		"value": 1
	}
	mut right := {
		"value": 2
	}
	alias := if cond { (unsafe { left }) } else { (unsafe { right }) }
	copy := alias
	println(copy["value"])
}

fn main() {
	choose(true)
	choose(false)
}
')
	assert if_out == '1\n2'
	match_out := run_good(v3_bin, 'unsafe_map_alias_match_expression', 'fn choose(value int) {
	mut left := {
		"value": 1
	}
	mut right := {
		"value": 2
	}
	alias := match value {
		0 { (unsafe { left }) }
		else { (unsafe { right }) }
	}
	copy := alias
	println(copy["value"])
}

fn main() {
	choose(0)
	choose(1)
}
')
	assert match_out == '1\n2'
}

fn test_unsafe_map_alias_unconditional_loop_has_no_zero_iteration_path() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'unsafe_map_alias_unconditional_loop', 'fn main() {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	for {
		alias = unsafe { original }
		break
	}
	copy := alias
	println(copy.len)
}
')
	assert out == '1'
	post_out := run_good(v3_bin, 'unsafe_map_alias_skipped_loop_post', 'fn main() {
	mut original := {
		"value": 1
	}
	mut alias := unsafe { original }
	for ;; alias = map[string]int{} {
		break
	}
	copy := alias
	println(copy.len)
}
')
	assert post_out == '1'
	run_bad(v3_bin, 'unsafe_map_alias_conditional_loop_zero_path', 'fn branch(cond bool) {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	for cond {
		alias = unsafe { original }
		break
	}
	copy := alias
	println(copy.len)
}

fn main() {
	branch(false)
}
',
		'cannot copy map: call `move` or `clone` method (or use a reference)')
}

fn test_unsafe_map_alias_provenance_tracks_each_loop_break() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'unsafe_map_alias_break_before_assignment', 'fn branch(cond bool) {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	for {
		if cond {
			break
		}
		alias = unsafe { original }
	}
	copy := alias
	println(copy.len)
}

fn main() {
	branch(true)
}
',
		'cannot copy map: call `move` or `clone` method (or use a reference)')
	out := run_good(v3_bin, 'unsafe_map_alias_assignment_before_break', 'fn main() {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	for {
		alias = unsafe { original }
		break
	}
	copy := alias
	println(copy.len)
}
')
	assert out == '1'
}

fn test_unsafe_map_alias_provenance_merges_short_circuit_operands() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'unsafe_map_alias_skipped_logical_and_rhs', 'fn main() {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	if false && unsafe {
		alias = unsafe { original }
		true
	} {}
	copy := alias
	println(copy.len)
}
',
		'cannot copy map: call `move` or `clone` method (or use a reference)')
	run_bad(v3_bin, 'unsafe_map_alias_skipped_logical_or_rhs', 'fn main() {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	if true || unsafe {
		alias = unsafe { original }
		true
	} {}
	copy := alias
	println(copy.len)
}
',
		'cannot copy map: call `move` or `clone` method (or use a reference)')
	out := run_good(v3_bin, 'unsafe_map_alias_required_logical_rhs', 'fn main() {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	if true && unsafe {
		alias = unsafe { original }
		true
	} {}
	copy := alias
	println(copy.len)
}
')
	assert out == '1'
}

fn test_unsafe_map_alias_provenance_merges_control_flow_paths() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'unsafe_map_alias_if_return_path', 'fn branch(cond bool) {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	if cond {
		alias = unsafe { original }
		return
	}
	copy := alias
	println(copy.len)
}

fn main() {
	branch(false)
}
',
		'cannot copy map: call `move` or `clone` method (or use a reference)')
	run_bad(v3_bin, 'unsafe_map_alias_match_return_path', 'fn branch(value int) {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	match value {
		0 {
			alias = unsafe { original }
			return
		}
		else {}
	}
	copy := alias
	println(copy.len)
}

fn main() {
	branch(1)
}
',
		'cannot copy map: call `move` or `clone` method (or use a reference)')
	run_bad(v3_bin, 'unsafe_map_alias_loop_zero_path', 'fn branch(values []int) {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	for _ in values {
		alias = unsafe { original }
	}
	copy := alias
	println(copy.len)
}

fn main() {
	branch([]int{})
}
',
		'cannot copy map: call `move` or `clone` method (or use a reference)')
	out := run_good(v3_bin, 'unsafe_map_alias_all_if_paths', 'fn branch(cond bool) {
	mut first := {
		"value": 1
	}
	mut second := {
		"value": 2
	}
	mut alias := map[string]int{}
	if cond {
		alias = unsafe { first }
	} else {
		alias = unsafe { second }
	}
	copy := alias
	println(copy.len)
}

fn main() {
	branch(false)
}
')
	assert out == '1'
	loop_out := run_good(v3_bin, 'unsafe_map_alias_loop_return_path', 'fn branch(values []int) {
	mut original := {
		"value": 1
	}
	mut alias := unsafe { original }
	for _ in values {
		alias = map[string]int{}
		return
	}
	copy := alias
	println(copy.len)
}

fn main() {
	branch([]int{})
}
')
	assert loop_out == '1'
}

fn test_unsafe_map_alias_provenance_isolates_or_fallback() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'unsafe_map_alias_or_fallback_success_path', 'fn maybe(ok bool) ?int {
	if ok {
		return 1
	}
	return none
}

fn branch(ok bool) {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	_ := maybe(ok) or {
		alias = unsafe { original }
		0
	}
	copy := alias
	println(copy.len)
}

fn main() {
	branch(true)
}
',
		'cannot copy map: call `move` or `clone` method (or use a reference)')
	out := run_good(v3_bin, 'unsafe_map_alias_all_or_paths', 'fn maybe() ?int {
	return none
}

fn main() {
	mut first := {
		"value": 1
	}
	mut second := {
		"value": 2
	}
	mut alias := unsafe { first }
	_ := maybe() or {
		alias = unsafe { second }
		0
	}
	copy := alias
	println(copy.len)
}
')
	assert out == '1'
}

fn test_unsafe_map_alias_provenance_delays_defer_effects() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'unsafe_map_alias_deferred_assignment', 'fn main() {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	defer {
		alias = unsafe { original }
	}
	copy := alias
	println(copy.len)
}
',
		'cannot copy map: call `move` or `clone` method (or use a reference)')
	out := run_good(v3_bin, 'unsafe_map_alias_deferred_rebind', 'fn main() {
	mut original := {
		"value": 1
	}
	mut alias := unsafe { original }
	defer {
		alias = map[string]int{}
	}
	copy := alias
	println(copy.len)
}
')
	assert out == '1'
}

fn test_unsafe_map_alias_provenance_isolates_select_branches() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'unsafe_map_alias_select_branch_isolation', 'fn main() {
	mut original := {
		"value": 1
	}
	mut alias := map[string]int{}
	ch := chan int{cap: 1}
	select {
		ch <- 1 {
			alias = unsafe { original }
		}
		else {
			copy := alias
			println(copy.len)
		}
	}
}
',
		'cannot copy map: call `move` or `clone` method (or use a reference)')
	out := run_good(v3_bin, 'unsafe_map_alias_all_select_paths', 'fn main() {
	mut first := {
		"value": 1
	}
	mut second := {
		"value": 2
	}
	mut alias := map[string]int{}
	ch := chan int{cap: 1}
	select {
		ch <- 1 {
			alias = unsafe { first }
		}
		else {
			alias = unsafe { second }
		}
	}
	copy := alias
	println(copy.len)
}
')
	assert out == '1'
}

fn test_recursive_str_struct_literal_preserves_receiver_provenance() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_struct_literal_provenance', 'struct Item {
	remaining int
}

fn (item Item) str() string {
	return Item{
		remaining: item.remaining
	}.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_empty_struct_literal_provenance', 'struct Item {}

fn (item Item) str() string {
	return Item{}.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_changed_struct_literal', 'struct Item {
	remaining int
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return ""
	}
	return Item{
		remaining: item.remaining - 1
	}.str()
}

fn main() {
	println(Item{
		remaining: 2
	}.str())
}
')
	assert out == ''
}

fn test_recursive_str_struct_update_preserves_receiver_provenance() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_struct_update_provenance', 'struct Item {
	value int
}

fn (item Item) str() string {
	return Item{
		...item
	}.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_struct_update_noop_field', 'struct Item {
	remaining int
}

fn (item Item) str() string {
	return Item{
		...item
		remaining: item.remaining + 0
	}.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_struct_update_helper_provenance', 'struct Item {
	value int
}

fn same(value Item) Item {
	return value
}

fn (item Item) str() string {
	return same(Item{
		...item
	}).str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_helper_returned_struct_update', 'struct Item {
	value int
}

fn same(item Item) Item {
	return Item{
		...item
	}
}

fn (item Item) str() string {
	return same(item).str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_progressed_struct_update', 'struct Item {
	remaining int
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return ""
	}
	return Item{
		...item
		remaining: item.remaining - 1
	}.str()
}

fn main() {
	println(Item{
		remaining: 2
	}.str())
}
')
	assert out == ''
	run_bad(v3_bin, 'recursive_str_helper_unconditional_loop_early_break', 'struct Item {
mut:
	remaining int
}

fn advance(mut item Item) {
	for {
		if item.remaining == 0 {
			break
		}
		item.remaining--
	}
}

fn (item Item) str() string {
	mut copy := item
	advance(mut copy)
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_guarded_nonnumeric_struct_update_progress() {
	v3_bin := build_v3()
	bool_out := run_good(v3_bin, 'recursive_str_guarded_bool_struct_update', 'struct Item {
	done bool
}

fn (item Item) str() string {
	if item.done {
		return "done"
	}
	return Item{
		...item
		done: true
	}.str()
}

fn main() {
	println(Item{}.str())
}
')
	assert bool_out == 'done'
	enum_out := run_good(v3_bin, 'recursive_str_guarded_enum_struct_update', 'enum State {
	active
	done
}

struct Item {
	state State
}

fn (item Item) str() string {
	if item.state == .done {
		return "done"
	}
	return Item{
		...item
		state: .done
	}.str()
}

fn main() {
	println(Item{}.str())
}
')
	assert enum_out == 'done'
	run_bad(v3_bin, 'recursive_str_guarded_bool_noop_struct_update', 'struct Item {
	done bool
}

fn (item Item) str() string {
	if !item.done {
		return "done"
	}
	return Item{
		...item
		done: true
	}.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_helper_unconditional_loop_progress() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'recursive_str_helper_unconditional_loop_progress', 'struct Item {
mut:
	remaining int
}

fn advance(mut item Item) {
	for {
		item.remaining--
		break
	}
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return ""
	}
	mut copy := item
	advance(mut copy)
	return copy.str()
}

fn main() {
	println(Item{
		remaining: 2
	}.str())
}
')
	assert out == ''
}

fn test_recursive_str_unconditional_loop_has_no_zero_iteration_path() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'recursive_str_unconditional_loop_progress', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return ""
	}
	mut copy := item
	for {
		copy.remaining--
		break
	}
	return copy.str()
}

fn main() {
	println(Item{
		remaining: 2
	}.str())
}
')
	assert out == ''
	run_bad(v3_bin, 'recursive_str_unconditional_loop_break_path', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	mut copy := item
	for {
		if copy.remaining == 0 {
			break
		}
		copy.remaining--
	}
	return copy.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_preserves_provenance_through_buffered_channels() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_buffered_channel_provenance', 'struct Item {}

fn (item Item) str() string {
	ch := chan Item{cap: 1}
	ch <- item
	return (<-ch).str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_buffered_channel_progress', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return ""
	}
	mut copy := item
	copy.remaining--
	ch := chan Item{cap: 1}
	ch <- copy
	return (<-ch).str()
}

fn main() {
	println(Item{
		remaining: 2
	}.str())
}
')
	assert out == ''
}

fn test_recursive_str_updates_array_provenance_after_mutators() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_array_delete_shift', 'struct Item {
	remaining int
}

fn (item Item) str() string {
	mut items := [Item{
		remaining: item.remaining - 1
	}, item]
	items.delete(0)
	return items[0].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_array_reverse_in_place', 'struct Item {
	remaining int
}

fn (item Item) str() string {
	mut items := [item, Item{
		remaining: item.remaining - 1
	}]
	items.reverse_in_place()
	return items[1].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_array_prepend_shift', 'struct Item {
	remaining int
}

fn (item Item) str() string {
	mut items := [item]
	items.prepend(Item{
		remaining: item.remaining - 1
	})
	return items[1].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_array_delete_progress', 'struct Item {
	remaining int
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return ""
	}
	mut items := [item, Item{
		remaining: item.remaining - 1
	}]
	items.delete(0)
	return items[0].str()
}

fn main() {
	print(Item{
		remaining: 2
	}.str())
}
')
	assert out == ''
}

fn test_recursive_str_detects_implicit_print_formatting() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_print_receiver', 'struct Item {}

fn (item Item) str() string {
	println(item)
	return ""
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_print_aggregate_receiver', 'struct Item {}

fn (item Item) str() string {
	eprintln([item])
	return ""
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_print_progressed_receiver', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return ""
	}
	mut next := item
	next.remaining--
	print(next)
	return ""
}

fn main() {
	print(Item{
		remaining: 2
	}.str())
}
')
	assert out == ''
}

fn test_recursive_str_resolves_constant_local_array_indexes() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'recursive_str_constant_local_terminal_index', 'struct Item {
	done bool
}

fn (item Item) str() string {
	if item.done {
		return ""
	}
	items := [item, Item{
		done: true
	}]
	index := 1
	return items[index].str()
}

fn main() {
	print(Item{}.str())
}
')
	assert out == ''
	run_bad(v3_bin, 'recursive_str_constant_local_receiver_index', 'struct Item {}

fn (item Item) str() string {
	items := [item, Item{}]
	index := 0
	return items[index].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_mutated_local_receiver_index', 'struct Item {}

fn (item Item) str() string {
	items := [item, Item{}]
	mut index := 1
	index--
	return items[index].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_detects_string_interpolation_formatting() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_interpolated_receiver', 'struct Item {}

fn (item Item) str() string {
	return "\${item}"
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_interpolated_aggregate_receiver', 'struct Item {}

fn (item Item) str() string {
	return "\${[item]}"
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_formatted_receiver', 'struct Item {}

fn (item Item) str() string {
	return "\${item:10}"
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_interpolated_progressed_receiver', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return ""
	}
	mut next := item
	next.remaining--
	return "\${next}"
}

fn main() {
	print(Item{
		remaining: 2
	}.str())
}
')
	assert out == ''
}

fn test_recursive_str_detects_explicit_aggregate_stringification() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_explicit_array_str', 'struct Item {}

fn (item Item) str() string {
	values := [item]
	return values.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_explicit_map_str', 'struct Item {}

fn (item Item) str() string {
	values := {
		"item": item
	}
	return values.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_explicit_array_str_progress', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return ""
	}
	mut next := item
	next.remaining--
	values := [next]
	return values.str()
}

fn main() {
	_ = Item{
		remaining: 2
	}.str()
}
')
	assert out == ''
	custom_out := run_good(v3_bin, 'recursive_str_custom_array_alias_str', 'struct Item {}

type Items = []Item

fn (items Items) str() string {
	return "safe"
}

fn (item Item) str() string {
	values := Items([item])
	return values.str()
}

fn main() {
	print(Item{}.str())
}
')
	assert custom_out == 'safe'
}

fn test_recursive_str_skips_zero_length_repeated_array_provenance() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'recursive_str_zero_length_repeated_array', 'struct Item {}

fn (item Item) str() string {
	values := []Item{len: 0, init: item}
	return values.str()
}

fn main() {
	print(Item{}.str())
}
')
	assert out == '[]'
	helper_out := run_good(v3_bin, 'recursive_str_helper_zero_length_repeated_array', 'struct Item {}

fn repeat(item Item) []Item {
	return []Item{len: 0, init: item}
}

fn (item Item) str() string {
	return repeat(item).str()
}

fn main() {
	print(Item{}.str())
}
')
	assert helper_out == '[]'
	run_bad(v3_bin, 'recursive_str_nonzero_repeated_array', 'struct Item {}

fn (item Item) str() string {
	values := []Item{len: 1, init: item}
	return values.str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_helper_nonzero_repeated_array', 'struct Item {}

fn repeat(item Item) []Item {
	return []Item{len: 1, init: item}
}

fn (item Item) str() string {
	return repeat(item).str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
}

fn test_recursive_str_tracks_for_in_values_and_array_slices() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'recursive_str_for_in_value', 'struct Item {}

fn (item Item) str() string {
	for copy in [item] {
		return copy.str()
	}
	return ""
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_for_in_index_and_value', 'struct Item {}

fn (item Item) str() string {
	for _, copy in [item] {
		return copy.str()
	}
	return ""
}

fn main() {}
',
		'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_array_slice_receiver', 'struct Item {}

fn (item Item) str() string {
	items := [item]
	slice := items[0..1]
	return slice[0].str()
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good(v3_bin, 'recursive_str_array_slice_terminal_index', 'struct Item {
	done bool
}

fn (item Item) str() string {
	if item.done {
		return ""
	}
	items := [item, Item{
		done: true
	}]
	start := 1
	end := 2
	slice := items[start..end]
	return slice[0].str()
}

fn main() {
	_ = Item{}.str()
}
')
	assert out == ''
	empty_slice_out := run_good(v3_bin, 'recursive_str_repeated_empty_slice', 'struct Item {}

fn (item Item) str() string {
	items := [3]Item{init: item}
	empty := items[0..0]
	return empty.str()
}

fn main() {
	_ = Item{}.str()
}
')
	assert empty_slice_out == ''
}

fn test_recursive_str_detects_dump_formatting() {
	v3_bin := build_v3()
	source := 'struct Item {}

fn (item Item) str() string {
	_ = dump(item)
	return ""
}

fn main() {
	_ = Item{}.str()
}
'
	run_bad(v3_bin, 'recursive_str_dump_receiver', source, 'cannot call `str()` method recursively')
	run_bad(v3_bin, 'recursive_str_dump_aggregate_receiver', 'struct Item {}

fn (item Item) str() string {
	_ = dump([item])
	return ""
}

fn main() {}
',
		'cannot call `str()` method recursively')
	out := run_good_with_flags(v3_bin, 'recursive_str_dump_nop_dump', '-d nop_dump', source)
	assert out == ''
	progressed_out := run_good(v3_bin, 'recursive_str_dump_progressed_receiver', 'struct Item {
mut:
	remaining int
}

fn (item Item) str() string {
	if item.remaining <= 0 {
		return ""
	}
	mut next := item
	next.remaining--
	_ = dump(next)
	return ""
}

fn main() {
	_ = Item{
		remaining: 1
	}.str()
}
')
	assert progressed_out.len > 0
}

fn test_capturing_fn_literal_cast_keeps_declared_parameters() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'capturing_fn_literal_cast_parameters', 'type Callback = fn (int)

fn main() {
	mut total := 0
callback := fn [mut total] (value int) {
	total += value
	println(int_str(total))
}
typed := Callback(callback)
typed(4)
println(int_str(total))
}
')
	assert out == '4\n0'
}

fn test_return_control_expression_forwards_matching_result() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'return_control_expression_result_forward', "fn bytes(ok bool) ![]u8 {
	if !ok {
		return error('bad')
	}
	return [u8(7)]
}

fn via_if(ok bool) ![]u8 {
	return if ok { bytes(true) } else { bytes(false) }
}

fn via_match(ok bool) ![]u8 {
	return match ok {
		true { bytes(true) }
		else { bytes(false) }
	}
}

fn main() {
	println(via_if(true)!.str())
	via_if(false) or { println(err.msg()) }
	println(via_match(true)!.str())
	via_match(false) or { println(err.msg()) }
}
")
	assert out == '[7]\nbad\n[7]\nbad'
}

fn test_generic_array_interpolation_resolves_main_struct() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'generic_array_interpolation_main_struct', 'struct Item {
	value int
}

fn render[T](items []T) string {
	return "\${items}"
}

fn main() {
	println(render([Item{
		value: 7
	}]))
}
')
	assert out.contains('Item{')
	assert out.contains('value: 7')
}

fn test_interface_match_smartcast_prefers_concrete_str_method() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_match_concrete_str', 'interface Value {
	number() int
}

struct Item {}

fn (_ &Item) number() int {
	return 7
}

fn (_ &Item) str() string {
	return "item"
}

fn (value &Value) str() string {
	match value {
		Item { return value.str() }
		else { return "unknown" }
	}
}

fn main() {
	value := Value(&Item{})
	println(value.str())
}
')
	assert out == '&item'
}

fn test_imported_struct_default_wraps_scalar_sum_variant() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'imported_struct_default_scalar_sum', {
		'v.mod':         "Module { name: 'imported_struct_default_scalar_sum' }\n"
		'model/model.v': 'module model\n\ntype Choice = Item | bool\n\npub struct Item {}\n\npub struct Settings {\npub:\n\tchoice Choice = true\n}\n\npub fn choice_is_true(settings Settings) bool {\n\treturn settings.choice is bool && settings.choice\n}\n'
		'main.v':        'module main\n\nimport model\n\nfn main() {\n\tprintln(model.choice_is_true(model.Settings{}))\n}\n'
	}, 'main.v')
	assert out == 'true'
}

fn test_imported_generic_receiver_alias_methods_on_struct_field() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'imported_generic_receiver_alias_field', {
		'v.mod':             "Module { name: 'imported_generic_receiver_alias_field' }\n"
		'vectors/vectors.v': 'module vectors\n\npub struct Vec[T] {\npub mut:\n\tx T\n}\n\npub fn (left Vec[T]) + (right Vec[T]) Vec[T] {\n\treturn Vec[T]{\n\t\tx: left.x + right.x\n\t}\n}\n\npub fn (value Vec[T]) divide[U](scalar U) Vec[T] {\n\treturn Vec[T]{\n\t\tx: value.x / T(scalar)\n\t}\n}\n\npub fn (value Vec[T]) difference(other Vec[T]) T {\n\treturn value.x - other.x\n}\n'
		'main.v':            'module main\n\nimport vectors\n\ntype V2 = vectors.Vec[f32]\n\nstruct Holder {\nmut:\n\tvalue V2\n}\n\nfn main() {\n\tmut holder := Holder{\n\t\tvalue: V2{\n\t\t\tx: 8\n\t\t}\n\t}\n\tholder.value += V2{\n\t\tx: 2\n\t}\n\tscaled := holder.value.divide(2)\n\tassert scaled.x == 5\n\tassert holder.value.difference(V2{\n\t\tx: 3\n\t}) == 7\n\tprintln("ok")\n}\n'
	}, 'main.v')
	assert out == 'ok'
}

fn test_imported_generic_receiver_alias_method_return_is_concrete() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'imported_generic_receiver_alias_return', {
		'v.mod':             "Module { name: 'imported_generic_receiver_alias_return' }\n"
		'vectors/vectors.v': 'module vectors\n\npub struct Vec3[T] {\npub:\n\tx T\n\ty T\n\tz T\n}\n\npub fn (left Vec3[T]) + (right Vec3[T]) Vec3[T] {\n\treturn Vec3[T]{left.x + right.x, left.y + right.y, left.z + right.z}\n}\n\npub fn (value Vec3[T]) mul_scalar[U](scalar U) Vec3[T] {\n\treturn Vec3[T]{value.x * T(scalar), value.y * T(scalar), value.z * T(scalar)}\n}\n\npub fn (left Vec3[T]) cross(right Vec3[T]) Vec3[T] {\n\treturn Vec3[T]{\n\t\tx: left.y * right.z - left.z * right.y\n\t\ty: left.z * right.x - left.x * right.z\n\t\tz: left.x * right.y - left.y * right.x\n\t}\n}\n'
		'main.v':            'module main\n\nimport vectors\n\ntype Vec = vectors.Vec3[f64]\n\nfn main() {\n\tleft := Vec{\n\t\tx: 1\n\t\ty: 0\n\t\tz: 0\n\t}\n\tright := Vec{\n\t\tx: 0\n\t\ty: 1\n\t\tz: 0\n\t}\n\tresult := Vec(left.cross(right).mul_scalar(2) + left)\n\tprintln(result.z)\n}\n'
	}, 'main.v')
	assert out == '2.0'
}

fn test_top_level_statements_with_postinclude_generate_main() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'top_level_postinclude_main',
		'#postinclude <limits.h>\n\n@[export: "v3_exported_helper"]\nfn exported_helper() {}\n\nprintln("ok")\n')
	assert out == 'ok'
}

fn test_composite_string_format_accepts_width_and_alignment() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'composite_string_format_width',
		'fn main() {\n\tprintln("|\${[1, 2]:-12s}|")\n}\n')
	assert out == '|[1, 2]      |'
}

fn test_comptime_define_field_default_is_not_fixed_array_initializer() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'comptime_define_field_default',
		'struct Job {\n\tid string = \$d("id", "Job")\n}\n\nstruct App {\n\tjobs [\$d("jobs", 2)]Job\n}\n\nfn main() {\n\tapp := App{}\n\tprintln(app.jobs[0].id)\n}\n')
	assert out == 'Job'
	run_bad(v3_bin, 'comptime_define_fixed_array_initializer',
		'struct App {\n\tjobs [\$d("jobs", 2)]int = [1, 2]!\n}\n\nfn main() {}\n',
		'cannot initialize a fixed size array field that uses `$d()` as size quantifier')
}

fn test_comptime_define_call_is_not_parenthesized_condition_warning() {
	v3_bin := build_v3()
	out := run_good_with_flags(v3_bin, 'comptime_define_if_warning', '-W',
		'fn main() {\n\tif \$d("enabled", true) {\n\t\tprintln("ok")\n\t}\n}\n')
	assert out == 'ok'
}

fn test_pointer_map_assignment_does_not_require_or_block() {
	v3_bin := build_v3()
	out := run_good_with_flags(v3_bin, 'pointer_map_assignment_warning', '-W',
		'struct Item {}\n\nfn main() {\n\tmut items := map[string]&Item{}\n\titems["one"] = &Item{}\n\tprintln(items.len)\n}\n')
	assert out == '1'
}

fn test_params_struct_fields_use_callback_and_userdata_compatibility() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'params_struct_callback_userdata_compatibility', 'type Callback = fn (voidptr)

@[params]
struct Config {
	callback Callback
	user_data voidptr
}

struct App {
mut:
	called bool
}

fn run(config Config) {
	config.callback(config.user_data)
}

fn (mut app App) callback() {
	app.called = true
}

mut app := &App{}
run(callback: app.callback, user_data: app)
println(app.called)
')
	assert out == 'true'
}

fn test_voidptr_function_value_argument_skips_pointer_depth_mismatch() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'voidptr_function_value_argument', 'type Mapper = fn (f64) f64

fn apply(mapper Mapper) f64 {
	return mapper(2)
}

fn double(value f64) f64 {
	return value * 2
}

fn main() {
	value := voidptr(double)
	println(apply(value))
}
')
	assert out == '4.0'
}

fn test_interface_mut_array_argument_uses_pointer_storage() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'interface_mut_array_argument', 'interface Reader {
	read(mut buf []u8)
}

struct Source {}

fn (mut source Source) read(mut buf []u8) {
	buf[0] = 7
}

fn main() {
	mut reader := Reader(Source{})
	mut buf := []u8{len: 1}
	reader.read(mut buf)
	println(buf[0])
}
')
	assert out == '7'
}

fn test_array_generic_specialization_is_recovered_from_lowered_callee() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'array_generic_specialization_recovery', 'import json2

struct Item {
	value int
}

fn main() {
	items := json2.decode[[]Item]("[{\\"value\\":7}]")!
	println(items[0].value)
}
')
	assert out == '7'
}

fn test_imported_generic_preserves_main_embedded_context_type() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'imported_generic_main_embedded_context', {
		'v.mod':                 "Module { name: 'imported_generic_main_embedded_context' }\n"
		'ctxhelper/ctxhelper.v': 'module ctxhelper\n\npub struct Context {\npub:\n\tvalue int\n}\n\npub fn read[X]() int {\n\tctx := Context{\n\t\tvalue: 7\n\t}\n\tuser_context := X{\n\t\tContext: ctx\n\t}\n\treturn user_context.Context.value + user_context.value\n}\n'
		'main.v':                'module main\n\nimport ctxhelper\n\nstruct Context {\n\tctxhelper.Context\n}\n\nfn main() {\n\tprintln(ctxhelper.read[Context]())\n}\n'
	}, 'main.v')
	assert out == '14'
}

fn test_imported_generic_closure_preserves_main_embedded_context_type() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'imported_generic_closure_main_embedded_context', {
		'v.mod':                 "Module { name: 'imported_generic_closure_main_embedded_context' }\n"
		'ctxhelper/ctxhelper.v': 'module ctxhelper

pub struct Context {
pub:
	value int
}

pub struct Options[T] {
pub:
	handler fn (mut T) bool
}

pub fn make[T]() Options[T] {
	return Options[T]{
		handler: fn [T](mut ctx T) bool {
			return ctx.Context.value == 7
		}
	}
}
'
		'main.v':                'module main

import ctxhelper

struct Context {
	ctxhelper.Context
}

fn main() {
	mut ctx := Context{
		Context: ctxhelper.Context{
			value: 7
		}
	}
	options := ctxhelper.make[Context]()
	println(options.handler(mut ctx))
}
'
	}, 'main.v')
	assert out == 'true'
}
