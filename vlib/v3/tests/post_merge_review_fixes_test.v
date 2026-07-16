import os
import v3.parser
import v3.pref
import v3.types

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const repo_dir = os.dir(vlib_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')
const compiler_src_dir = os.join_path(repo_dir, 'cmd', 'v')

fn tmp_test_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
}

fn build_v3() string {
	v3_bin := tmp_test_path('post_merge_review_fixes_test')
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

fn test_amp_array_literal_uses_scanned_heap_header() {
	v3_bin := build_v3()
	c_source := gen_c(v3_bin, 'amp_array_literal_scanned_header', 'struct Holder {
	values &[]int
}

fn make_holder() Holder {
	return Holder{
		values: &[1, 2, 3]
	}
}

fn main() {
	holder := make_holder()
	println(holder.values[1])
}
')
	assert c_source.contains('void* memdup(void* src, ptrdiff_t sz);\nstatic inline Array* v3_heap_array(Array value) { return (Array*)memdup(&value, sizeof(Array)); }'), c_source

	assert c_source.count('v3_heap_array(') >= 2, c_source
	assert !c_source.contains('malloc_noscan(sizeof(Array))'), c_source
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
	compile := os.execute('${v3_bin} ${input_path} -b c -o ${good_bin}')
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
	c_out := os.join_path(os.temp_dir(), 'v3_review_vexe.c')
	os.rm(c_out) or {}
	gen := os.execute('${v3_bin} -o ${c_out} ${compiler_src_dir}')
	assert gen.exit_code == 0, gen.output
	assert os.exists(c_out)
	c_source := os.read_file(c_out) or { panic(err) }
	assert !c_source.contains('v3_vexe_target')
	assert !c_source.contains('fopen(v3_src')
	assert !c_source.contains('v3_checkout_vexe')
	assert !c_source.contains('v3_arg0')
	assert !c_source.contains('v3_src_real_result')
	assert c_source.contains('const char* v3_vexe = "')
	assert c_source.contains('_putenv_s("VEXE", v3_vexe);')
	assert c_source.contains('setenv("VEXE", v3_vexe, 1);')
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

fn test_is_check_preserves_pointer_sum_variants() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'is_pointer_sum_variant', 'struct Foo {
	value int
}

type Item = &Foo | int

fn main() {
	item := Item(7)
	if item is &Foo {
		println("wrong")
	} else {
		println("ok")
	}
}
')
	assert out == 'ok'
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

fn consume_result(value !IValue) bool {
	payload := value or { return false }
	return same(payload)
}

fn main() {
	mut option_value := ?IValue(none)
	option_value = make_option()
	option_payload := option_value or { panic("missing option") }
	println(same(option_payload).str())
	println(consume_result(make_result()).str())
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
	out := run_good(v3_bin, 'interface_eq_veb_handler_call_box', 'import veb

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
	_ = app.index()
}
')
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
	mut aliased_option_value := MaybeValue(none)
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

	result_ch := chan !Value{cap: 1}
	result_ch <- make_result()
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
	run_bad(v3_bin, 'if_error_branch_rejected_for_option_payload',
		"fn f(ok bool) ?int {\n\treturn if ok { error('bad') } else { 1 }\n}\n\nfn main() {\n\t_ := f(false) or { 0 }\n}\n",
		'if-expression branch type mismatch')
	run_bad(v3_bin, 'if_none_branch_rejected_for_result_payload',
		'fn g(ok bool) !int {\n\treturn if ok { none } else { 1 }\n}\n\nfn main() {\n\t_ := g(false) or { 0 }\n}\n',
		'if-expression branch type mismatch')
	run_bad(v3_bin, 'match_error_branch_rejected_for_option_payload',
		"fn f(n int) ?int {\n\treturn match n {\n\t\t0 { error('bad') }\n\t\telse { 1 }\n\t}\n}\n\nfn main() {\n\t_ := f(1) or { 0 }\n}\n",
		'cannot return')
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
	source := "fn concat_path(dir string, name &string) string {\n\treturn '\${dir}/\${name}'\n}\n\nfn main() {\n\tname := 'file'\n\tprintln(concat_path('root', &name))\n}\n"
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
	println(int_str(c.n))
}
'
	out := run_good(v3_bin, 'for_mut_item_receiver_run', item_src)
	assert out == '3\n4\n2'
	item_c := gen_c(v3_bin, 'for_mut_item_receiver_c', item_src)
	item_main := c_fn_body(item_c, 'int main(')
	assert item_main.len > 0, item_c
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
	assert out.contains('chan int(nil)')
}

fn test_explicit_return_semicolon_ends_void_return() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'explicit_return_semicolon_boundary', 'fn stop() {
	return;
		println("unreachable")
}

fn main() {
	stop()
	println("done")
}
')
	assert out == 'done'
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
	assert main_body.contains('(Aligned*)v3_aligned_memdup('), main_body
	assert !main_body.contains('(Aligned*)memdup('), main_body
	assert main_body.contains('v3_aligned_free(p)'), main_body
	out := run_good(v3_bin, 'aligned_alias_heap_cast_run', source)
	assert out == '7'
}

fn test_unimported_main_types_are_not_visible_in_modules() {
	v3_bin := build_v3()
	run_bad_project(v3_bin, 'unimported_plain_main_type', {
		'main.v':      'module main\n\nimport moda\n\nstruct Foo {}\n\nfn main() {\n\t_ = moda.make()\n}\n'
		'moda/moda.v': 'module moda\n\npub struct Holder {\n\tvalue Foo\n}\n\npub fn make() Holder {\n\treturn Holder{}\n}\n'
	}, ['main.v', 'moda/moda.v'], 'unknown type `Foo`')
	run_bad_project(v3_bin, 'unimported_generic_main_type', {
		'main.v':      'module main\n\nimport moda\n\nstruct Box[T] {}\n\nfn main() {\n\t_ = moda.make()\n}\n'
		'moda/moda.v': 'module moda\n\npub struct Holder {\n\tvalue Box[int]\n}\n\npub fn make() Holder {\n\treturn Holder{}\n}\n'
	}, ['main.v', 'moda/moda.v'], 'unknown type `Box`')
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

fn test_json_encode_declines_unsupported_field_attrs() {
	v3_bin := build_v3()
	c_source := gen_c(v3_bin, 'json_encode_unsupported_field_attr', 'import json

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
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv)')
	assert main_body.contains('json__encode(&')
	assert !main_body.contains('v3_json_encode_string(')
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

enum E {
	a = make()
}

fn main() {
	println(int_str(int(E.a)))
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

enum E {
	a = make()
	b = from_param(7)
	c
}

fn main() {
	println(int_str(int(E.a)))
	println(int_str(int(E.b)))
	println(int_str(int(E.c)))
}
')
	assert out == '4\n7\n8'
}

fn test_backed_enum_cast_qualifies_member_reference() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'backed_enum_cast_member_reference', 'const a = 1

enum E as u64 {
	a = 1
	b = u64(a) + 1
}

fn main() {
	println(int_str(int(E.b)))
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

enum E as u64 {
	a = make()
	b
	wide = make_wide()
	max = 18446744073709551615
}

fn main() {
	println(int_str(int(E.a)))
	println(int_str(int(E.b)))
	println(u64(E.wide))
	match E.a {
		.a { println("a") }
		else { println("other") }
	}
}
'
	out := run_good(v3_bin, 'backed_enum_helper_initializer', source)
	assert out == '4\n5\n1099511627776\na'
	c_source := gen_c(v3_bin, 'backed_enum_helper_initializer_c', source)
	macro := c_source.split_into_lines().filter(it.starts_with('#define E__a '))
	assert macro == ['#define E__a ((E)(4))']
	shift_macro := c_source.split_into_lines().filter(it.starts_with('#define E__wide '))
	assert shift_macro == ['#define E__wide ((E)(1099511627776))']
	wide_macro := c_source.split_into_lines().filter(it.starts_with('#define E__max '))
	assert wide_macro == ['#define E__max ((E)(18446744073709551615))']
}

fn test_enum_helper_folding_tracks_local_declarations() {
	v3_bin := build_v3()
	source := 'fn make_local() int {
	x := 4
	y := x + 2
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

fn test_enum_helper_scan_resets_module_at_file_boundary() {
	v3_bin := build_v3()
	source := 'fn exit() int {
	return 9
}

enum E {
	a = exit()
	b
}

fn main() {
	println(E.a.str())
	println(E.b.str())
}
'
	c_source := gen_c(v3_bin, 'enum_helper_main_file_module_reset_c', source)
	assert c_source.contains('\tE__a = 9,')
	assert c_source.contains('\tE__b = 10,')
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
	assert c_source.contains('v3_aligned_memdup(&x, sizeof(Bare), __alignof__(Bare))')
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
		fixed: &fixed
		words: &words
	})
	println(value.str())
}
')
	assert out == "Foo{\n    nums: [1, 2]\n    labels: {'a': 3}\n    fixed: [4, 5]\n    words: ['x', 'y']\n}"
}

fn test_empty_interface_box_preserves_alias_type_id() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'empty_interface_alias_type_id',
		'interface Any {}\n\ntype MyInt = int\n\nfn main() {\n\tvalue := MyInt(1)\n\ta := Any(value)\n\tprintln((a is MyInt).str())\n\tprintln((a is int).str())\n\tplain := int(2)\n\tb := Any(plain)\n\tprintln((b is MyInt).str())\n\tprintln((b is int).str())\n}\n')
	assert out == 'true\nfalse\nfalse\ntrue'
}

fn test_interface_cast_rejects_pointer_shape_mismatch() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'interface_pointer_shape_mismatch',
		'interface Sink {\n\tput(x &int)\n}\n\nstruct Bad {}\n\nfn (b Bad) put(x int) {}\n\nfn main() {\n\t_ := Sink(Bad{})\n}\n',
		'does not implement interface')
	run_bad(v3_bin, 'interface_voidptr_cast_rejected',
		'interface Sink {\n\tput()\n}\n\nfn main() {\n\tx := 1\n\tp := voidptr(&x)\n\t_ := Sink(p)\n}\n',
		'does not implement interface')
	run_bad(v3_bin, 'interface_pointer_voidptr_cast_rejected',
		'interface Sink {\n\tput()\n}\n\nfn main() {\n\tx := 1\n\tp := voidptr(&x)\n\t_ := &Sink(p)\n}\n',
		'does not implement interface')
	run_bad(v3_bin, 'interface_alias_cast_non_implementer',
		'interface Sink {\n\tput()\n}\n\ntype SinkAlias = Sink\n\nstruct Bad {}\n\nfn main() {\n\t_ := SinkAlias(Bad{})\n}\n',
		'does not implement interface')
	nil_out := run_good(v3_bin, 'interface_pointer_nil_cast',
		"interface Sink {\n\tput()\n}\n\ntype SinkAlias = Sink\n\nfn main() {\n\t_ := Sink(nil)\n\t_ := &Sink(nil)\n\t_ := &SinkAlias(nil)\n\tprintln('ok')\n}\n")
	assert nil_out == 'ok'
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
		'fn takes(cb fn () ?void) {\n\tcb() or {\n\t\tprintln("none")\n\t\treturn\n\t}\n\tprintln("some")\n}\n\nfn maybe_none() ?void {\n\treturn none\n}\n\nfn main() {\n\ttakes(|| maybe_none())\n}\n')
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
	assert c_source.contains('._object = (Box*)(memdup(&b, sizeof(Box)))')
	assert c_source.contains('memdup(&__iface_box_')
	out := run_good(v3_bin, 'amp_interface_cast_heap_copy_run', source)
	assert out == '5'
}

fn test_interface_boxed_local_address_preserves_pointer_identity() {
	v3_bin := build_v3()
	source := 'interface Reader {\n\tvalue() int\n}\n\nstruct Box {\nmut:\n\tn int\n}\n\nfn (b &Box) value() int {\n\treturn b.n\n}\n\nfn main() {\n\tmut b := Box{\n\t\tn: 1\n\t}\n\tr := Reader(&b)\n\tb.n = 2\n\tprintln(int_str(r.value()))\n}\n'
	c_source := gen_c(v3_bin, 'interface_box_local_address_identity', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv)')
	assert !main_body.contains('memdup(') && !main_body.contains('sizeof(Box)'), main_body
	out := run_good(v3_bin, 'interface_box_local_address_identity_run', source)
	assert out == '2'
}

fn test_by_value_interface_box_with_fields_heap_copies_addressable_source() {
	v3_bin := build_v3()
	source := 'interface HasName {\nmut:\n\tname string\n}\n\nstruct Item {\nmut:\n\tname string\n}\n\nfn save(value HasName) HasName {\n\treturn value\n}\n\nfn make() HasName {\n\tmut item := Item{\n\t\tname: "old"\n\t}\n\tboxed := save(item)\n\titem.name = "new"\n\treturn boxed\n}\n\nfn main() {\n\tprintln(make().name)\n}\n'
	c_source := gen_c(v3_bin, 'by_value_interface_box_with_fields_heap_copy', source)
	make_body := c_fn_body(c_source, 'HasName make(void) {')
	assert make_body.contains('memdup(&item') && make_body.contains('sizeof(Item)'), make_body
	out := run_good(v3_bin, 'by_value_interface_box_with_fields_heap_copy_run', source)
	assert out == 'old'
}

fn test_interface_boxed_returned_local_address_still_heap_copies() {
	v3_bin := build_v3()
	source := 'interface Reader {\n\tvalue() int\n}\n\nstruct Box {\n\tn int\n}\n\nfn (b &Box) value() int {\n\treturn b.n\n}\n\nfn make() Reader {\n\tb := Box{\n\t\tn: 5\n\t}\n\treturn Reader(&b)\n}\n\nfn main() {\n\tr := make()\n\tprintln(int_str(r.value()))\n}\n'
	c_source := gen_c(v3_bin, 'interface_box_returned_local_address_heap_copy', source)
	make_body := c_fn_body(c_source, 'Reader make(void) {')
	assert make_body.contains('memdup(') && make_body.contains('sizeof(Box)'), make_body
	out := run_good(v3_bin, 'interface_box_returned_local_address_heap_copy_run', source)
	assert out == '5'
}

fn test_returned_interface_local_boxed_from_local_address_heap_copies_on_return() {
	v3_bin := build_v3()
	source := 'interface Reader {\n\tvalue() int\n}\n\nstruct Box {\nmut:\n\tn int\n}\n\nfn (b &Box) value() int {\n\treturn b.n\n}\n\nfn make() Reader {\n\tmut b := Box{\n\t\tn: 1\n\t}\n\tr := Reader(&b)\n\tb.n = 2\n\treturn r\n}\n\nfn main() {\n\tr := make()\n\tprintln(int_str(r.value()))\n}\n'
	c_source := gen_c(v3_bin, 'interface_box_returned_local_alias_heap_copy', source)
	make_body := c_fn_body(c_source, 'Reader make(void) {')
	assert make_body.contains('memdup(') && make_body.contains('sizeof(Box)'), make_body
	out := run_good(v3_bin, 'interface_box_returned_local_alias_heap_copy_run', source)
	assert out == '2'
}

fn test_returned_interface_local_boxed_from_local_address_heap_copies_in_aggregates_and_assignments() {
	v3_bin := build_v3()
	source := 'interface Reader {\n\tvalue() int\n}\n\nstruct Box {\nmut:\n\tn int\n}\n\nfn (b &Box) value() int {\n\treturn b.n\n}\n\nstruct Holder {\n\treader Reader\n}\n\nfn make_array() []Reader {\n\tmut b := Box{\n\t\tn: 1\n\t}\n\tr := Reader(&b)\n\tb.n = 2\n\treturn [r]\n}\n\nfn make_holder() Holder {\n\tmut b := Box{\n\t\tn: 3\n\t}\n\tr := Reader(&b)\n\tb.n = 4\n\treturn Holder{\n\t\treader: r\n\t}\n}\n\nfn make_reassigned() Reader {\n\tmut r := Reader(&Box{\n\t\tn: 0\n\t})\n\tmut b := Box{\n\t\tn: 5\n\t}\n\tr = Reader(&b)\n\tb.n = 6\n\treturn r\n}\n\nfn main() {\n\tarr := make_array()\n\tprintln(int_str(arr[0].value()))\n\tholder := make_holder()\n\tprintln(int_str(holder.reader.value()))\n\treader := make_reassigned()\n\tprintln(int_str(reader.value()))\n}\n'
	c_source := gen_c(v3_bin, 'interface_box_returned_aggregate_heap_copy', source)
	for signature in ['Array make_array(void) {', 'Holder make_holder(void) {',
		'Reader make_reassigned(void) {'] {
		body := c_fn_body(c_source, signature)
		assert body.contains('memdup(') && body.contains('sizeof(Box)'), body
	}
	out := run_good(v3_bin, 'interface_box_returned_aggregate_heap_copy_run', source)
	assert out == '2\n4\n6'
}

fn test_returned_interface_boxed_from_global_address_preserves_identity() {
	v3_bin := build_v3()
	source := 'interface Reader {\n\tvalue() int\n}\n\nstruct Box {\nmut:\n\tn int\n}\n\n__global global_box Box\n\nfn (b &Box) value() int {\n\treturn b.n\n}\n\nfn make_direct() Reader {\n\tr := Reader(&global_box)\n\treturn r\n}\n\nfn make_alias() Reader {\n\tp := &global_box\n\tr := Reader(p)\n\treturn r\n}\n\nfn make_reassigned() Reader {\n\tb := Box{\n\t\tn: 3\n\t}\n\tmut r := Reader(&b)\n\tr = Reader(&global_box)\n\treturn r\n}\n\nfn make_conditional(use_global bool) Reader {\n\tmut b := Box{\n\t\tn: 21\n\t}\n\tmut r := Reader(&b)\n\tif use_global {\n\t\tr = Reader(&global_box)\n\t}\n\tb.n = 22\n\treturn r\n}\n\nfn main() {\n\tglobal_box.n = 1\n\tr := make_direct()\n\tglobal_box.n = 7\n\tprintln(int_str(r.value()))\n\talias := make_alias()\n\tglobal_box.n = 9\n\tprintln(int_str(alias.value()))\n\tglobal_box.n = 11\n\treassigned := make_reassigned()\n\tglobal_box.n = 13\n\tprintln(int_str(reassigned.value()))\n\tconditional := make_conditional(false)\n\tprintln(int_str(conditional.value()))\n}\n'
	c_source := gen_c(v3_bin, 'interface_box_global_address_identity', source)
	for signature in ['Reader make_direct(void) {', 'Reader make_alias(void) {',
		'Reader make_reassigned(void) {'] {
		body := c_fn_body(c_source, signature)
		assert !body.contains('memdup(') && !body.contains('sizeof(Box)'), body
	}
	conditional_body := c_fn_body(c_source, 'Reader make_conditional(bool use_global) {')
	assert conditional_body.contains('memdup(') && conditional_body.contains('sizeof(Box)'), conditional_body

	out := run_good(v3_bin, 'interface_box_global_address_identity_run', source)
	assert out == '7\n9\n13\n22'
}

fn test_embedded_interface_fields_lower_to_concrete_object() {
	v3_bin := build_v3()
	source := "interface Base {\nmut:\n\tname string\n}\n\ninterface Child {\n\tBase\n}\n\nstruct Item {\nmut:\n\tname string\n}\n\nfn main() {\n\tmut child := Child(Item{\n\t\tname: 'old'\n\t})\n\tprintln(child.name)\n\tchild.name = 'new'\n\tprintln(child.name)\n}\n"
	out := run_good(v3_bin, 'embedded_interface_field_lowering', source)
	assert out == 'old\nnew'
}

fn test_returned_interface_local_boxed_from_pointer_alias_heap_copies_on_return() {
	v3_bin := build_v3()
	source := 'interface Reader {\n\tvalue() int\n}\n\nstruct Box {\nmut:\n\tn int\n}\n\nfn (b &Box) value() int {\n\treturn b.n\n}\n\nfn make() Reader {\n\tmut b := Box{\n\t\tn: 1\n\t}\n\tp := &b\n\tq := p\n\tr := Reader(q)\n\tb.n = 2\n\treturn r\n}\n\nfn main() {\n\tprintln(int_str(make().value()))\n}\n'
	c_source := gen_c(v3_bin, 'interface_box_returned_pointer_alias_heap_copy', source)
	make_body := c_fn_body(c_source, 'Reader make(void) {')
	assert make_body.contains('memdup(') && make_body.contains('sizeof(Box)'), make_body
	out := run_good(v3_bin, 'interface_box_returned_pointer_alias_heap_copy_run', source)
	assert out == '2'
}

fn test_returned_interface_boxed_from_local_field_and_index_heap_copies() {
	v3_bin := build_v3()
	source := 'interface Reader {\n\tvalue() int\n}\n\nstruct Box {\nmut:\n\tn int\n}\n\nstruct Holder {\nmut:\n\tbox Box\n}\n\nfn (b &Box) value() int {\n\treturn b.n\n}\n\nfn make_direct_field() Reader {\n\tmut holder := Holder{\n\t\tbox: Box{\n\t\t\tn: 3\n\t\t}\n\t}\n\tholder.box.n = 4\n\treturn Reader(&holder.box)\n}\n\nfn make_later_field() Reader {\n\tmut holder := Holder{\n\t\tbox: Box{\n\t\t\tn: 5\n\t\t}\n\t}\n\tr := Reader(&holder.box)\n\tholder.box.n = 6\n\treturn r\n}\n\nfn make_direct_index() Reader {\n\tmut boxes := [Box{\n\t\tn: 7\n\t}, Box{\n\t\tn: 8\n\t}]!\n\tboxes[1].n = 9\n\treturn Reader(&boxes[1])\n}\n\nfn make_later_index() Reader {\n\tmut boxes := [Box{\n\t\tn: 10\n\t}, Box{\n\t\tn: 11\n\t}]!\n\tr := Reader(&boxes[1])\n\tboxes[1].n = 12\n\treturn r\n}\n\nfn main() {\n\tprintln(int_str(make_direct_field().value()))\n\tprintln(int_str(make_later_field().value()))\n\tprintln(int_str(make_direct_index().value()))\n\tprintln(int_str(make_later_index().value()))\n}\n'
	c_source := gen_c(v3_bin, 'interface_box_returned_field_index_heap_copy', source)
	for signature in ['Reader make_direct_field(void) {', 'Reader make_later_field(void) {',
		'Reader make_direct_index(void) {', 'Reader make_later_index(void) {'] {
		body := c_fn_body(c_source, signature)
		assert body.contains('memdup(') && body.contains('sizeof(Box)'), body
	}
	out := run_good(v3_bin, 'interface_box_returned_field_index_heap_copy_run', source)
	assert out == '4\n6\n9\n12'
}

fn test_returned_interface_boxed_from_pointer_alias_field_heap_copies() {
	v3_bin := build_v3()
	source := 'interface Reader {\n\tvalue() int\n}\n\nstruct Box {\nmut:\n\tn int\n}\n\nstruct Holder {\nmut:\n\tbox Box\n}\n\nfn (b &Box) value() int {\n\treturn b.n\n}\n\nfn make() Reader {\n\tmut holder := Holder{\n\t\tbox: Box{\n\t\t\tn: 13\n\t\t}\n\t}\n\tp := &holder\n\tr := Reader(&p.box)\n\tholder.box.n = 14\n\treturn r\n}\n\nfn main() {\n\tprintln(int_str(make().value()))\n}\n'
	c_source := gen_c(v3_bin, 'interface_box_returned_pointer_alias_field_heap_copy', source)
	make_body := c_fn_body(c_source, 'Reader make(void) {')
	assert make_body.contains('memdup(') && make_body.contains('sizeof(Box)'), make_body
	out := run_good(v3_bin, 'interface_box_returned_pointer_alias_field_heap_copy_run', source)
	assert out == '14'
}

fn test_mut_interface_argument_borrows_existing_interface_box() {
	v3_bin := build_v3()
	source := 'interface Visitor {\n\tvalue() int\nmut:\n\tvisit()\n}\n\nstruct Counter {\nmut:\n\tn int\n}\n\nfn (c Counter) value() int {\n\treturn c.n\n}\n\nfn (mut c Counter) visit() {\n\tc.n++\n}\n\nfn call(mut visitor Visitor) {\n\tvisitor.visit()\n}\n\nfn main() {\n\tmut visitor := Visitor(Counter{})\n\tcall(mut visitor)\n\tprintln(int_str(visitor.value()))\n}\n'
	c_source := gen_c(v3_bin, 'mut_interface_arg_borrows_existing_box', source)
	assert c_source.contains('call(&visitor);')
	assert !c_source.contains('call((Visitor*)(memdup(&__iface_box_')
	out := run_good(v3_bin, 'mut_interface_arg_borrows_existing_box_run', source)
	assert out == '1'
}

fn test_pointer_interface_arg_upcasts_interface_lvalue_source() {
	v3_bin := build_v3()
	source := 'interface Base {\n\tget() int\n}\n\ninterface Child {\n\tBase\n\textra() int\n}\n\nstruct Item {\n\tn int\n}\n\nfn (i Item) get() int {\n\treturn i.n\n}\n\nfn (i Item) extra() int {\n\treturn i.n + 1\n}\n\nfn use(value &Base) int {\n\treturn value.get()\n}\n\nfn main() {\n\tchild := Child(Item{\n\t\tn: 13\n\t})\n\tprintln(int_str(use(child)))\n}\n'
	c_source := gen_c(v3_bin, 'pointer_interface_lvalue_upcast_source', source)
	assert !c_source.contains('&((Base[]){child})[0]')
	assert c_source.contains('Base __iface_cast_')
	assert c_source.contains('use((Base*)(memdup(&__iface_box_')
	out := run_good(v3_bin, 'pointer_interface_lvalue_upcast_source_run', source)
	assert out == '13'
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
			'fn C.atomic_fetch_add_ptr(voidptr, voidptr) voidptr\nfn C.atomic_fetch_sub_ptr(voidptr, voidptr) voidptr\n\nfn main() {\n\tmut vals := [10, 20, 30]!\n\tmut p := voidptr(&vals[0])\n\told := C.atomic_fetch_add_ptr(voidptr(&p), voidptr(sizeof(int)))\n\tprintln(old == voidptr(&vals[0]))\n\tprintln(p == voidptr(&vals[1]))\n\told2 := C.atomic_fetch_sub_ptr(voidptr(&p), voidptr(sizeof(int)))\n\tprintln(old2 == voidptr(&vals[1]))\n\tprintln(p == voidptr(&vals[0]))\n}\n')
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

fn main() {
	println(int_str(take_int(struct { x: 7 })))
	println(take_string(struct { x: "right" }))
	println(take_i64(struct { x: i64(9) }))
	println(int_str(take_grouped(struct { x: 2, y: 3 })))
	mut values := []struct {
		x int
	}{}
	values << struct { x: 13 }
	println(int_str(values[0].x))
}
')
	assert out == '7\nright\n9\n23\n13'
	inferred := run_good(v3_bin, 'anonymous_struct_inferred_literal_typed_shape', 'fn main() {
	a := struct { x: 1 }
	b := struct { x: "typed" }
	println(int_str(a.x))
	println(b.x)
}
')
	assert inferred == '1\ntyped'
}

fn test_anonymous_struct_context_accepts_untyped_numeric_literal() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'anonymous_struct_context_untyped_numeric_literal', 'fn take_i64(value struct {
	n i64
}) i64 {
	return value.n
}

fn take_str(value struct {
	n string
}) string {
	return value.n
}

fn main() {
	println(take_i64(struct { n: 1 }).str())
	println(take_str(struct { n: "ok" }))
}
')
	assert out == '1\nok'
}

fn test_implicit_ref_arg_rejects_multiple_pointer_levels() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'implicit_ref_arg_rejects_multiple_pointer_levels', 'fn take(value &&int) {
	_ = value
}

fn main() {
	mut n := 1
	take(n)
}
',
		'expected `&&int`')
	run_bad(v3_bin, 'implicit_ref_arg_rejects_pointer_lvalue_depth_lift', 'fn take(value &&int) {
	_ = value
}

fn main() {
	mut n := 1
	p := &n
	take(p)
}
',
		'expected `&&int`')
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

	shift_once := run_good(v3_bin, 'unsigned_shift_assign_lvalue_once', 'fn next(mut calls int) int {
	calls++
	return 0
}

fn main() {
	mut calls := 0
	mut values := [8, 16]
	values[next(mut calls)] >>>= 1
	println(int_str(calls))
	println(int_str(values[0]))
	println(int_str(values[1]))
	mut signed_values := [i8(-5)]
	signed_values[0] >>>= 1
	println(int_str(signed_values[0]))
	mut shifted_map := map[int]i8{}
	shifted_map[0] = i8(-5)
	shifted_map[next(mut calls)] >>>= 1
	println(int_str(calls))
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
		'overloaded index assignment requires a matching `[]=` setter')
	run_bad(v3_bin, 'overloaded_index_compound_assignment_requires_setter', dict_src +
		'

fn main() {
	mut d := Dict{}
	d["name"] += 1
}
',
		'overloaded index assignment requires a matching `[]=` setter')
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
		'compound overloaded index assignment requires a matching `[]` getter')
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
		'compound overloaded index assignment requires matching `[]` and `[]=` index parameter types')
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
		'cannot assign `string` to `int`')
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
		'compound overloaded index assignment requires `[]` return type compatible with `[]=` value parameter type')
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
}
')
	assert out == 'value expr\nptr expr\nptr type\nfn type\narray type'
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
	out := run_good(v3_bin, 'for_in_uppercase_const_body_not_struct_init', "const Foo = [1, 2]

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
	source := 'const Foo = [1, 2]

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
	assert main_body.contains('->Inner.name'), main_body
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
