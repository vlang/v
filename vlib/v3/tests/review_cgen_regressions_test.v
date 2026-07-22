import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3_review_cgen() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_review_cgen_regressions_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn review_cgen_write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn review_cgen_run_good(v3_bin string, name string, src string) string {
	good_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn review_cgen_run_good_project(v3_bin string, name string, files map[string]string, input string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		review_cgen_write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${input_path} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn review_cgen_run_bad_project(v3_bin string, name string, files map[string]string, input string, expected string) {
	root := os.join_path(os.temp_dir(), 'v3_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		review_cgen_write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	result := os.execute('${v3_bin} ${input_path} -b c -o ${bad_bin}')
	assert result.exit_code != 0, '${name}: expected failure, got success\n${result.output}'
	assert result.output.contains(expected), '${name}: expected `${expected}` in\n${result.output}'
	assert !result.output.contains('C compilation failed'), '${name}: reached C compilation\n${result.output}'
}

fn test_power_consts_are_initialized_at_runtime() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'power_const_runtime_init', 'const byte_power = u8(2 ** 3)
const max_len = 2 ** 3
const address_power = 3 ** 2
const float_power = 9.0 ** 0.5
const array_len = 2 ** 3

type NamedPowerArray = [array_len]int
type DirectPowerArray = [2 ** 2]int

fn read_int(value &int) int {
	return *value
}

fn read_float(value &f64) f64 {
	return *value
}

fn main() {
	println(int_str(int(byte_power)))
	println(int_str(max_len))
	println(int_str(read_int(&address_power)))
	println(read_float(&float_power).str())
	println(int_str(NamedPowerArray{}.len))
	println(int_str(DirectPowerArray{}.len))
}
')
	assert out == '8\n8\n9\n3.0\n8\n4'
	c_code := os.read_file(os.join_path(os.temp_dir(), 'v3_power_const_runtime_init.c')) or {
		panic(err)
	}
	assert c_code.contains('u8 main__byte_power;'), c_code
	assert c_code.contains('int main__max_len;'), c_code
	assert c_code.contains('int main__address_power;'), c_code
	assert c_code.contains('double main__float_power;'), c_code
	assert c_code.contains('main__byte_power = '), c_code
	assert c_code.contains('main__max_len = '), c_code
	assert c_code.contains('main__address_power = '), c_code
	assert c_code.contains('main__float_power = '), c_code
}

fn test_array_power_assign_uses_power_helper() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'array_power_assign', 'struct IntList {
mut:
	values []int
}

fn (list IntList) [] (index int) int {
	return list.values[index]
}

fn (mut list IntList) []= (index int, value int) {
	list.values[index] = value
}

fn main() {
	mut values := [2, 3]
	values[0] **= 3
	println(int_str(values[0]))
	mut list := IntList{
		values: [3]
	}
	list[0] **= 2
	println(int_str(list.values[0]))
}
')
	assert out == '8\n9'
	c_code := os.read_file(os.join_path(os.temp_dir(), 'v3_array_power_assign.c')) or { panic(err) }
	compact := c_code.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('array__set(_a0,_i0,&(int[]){((int)__v_pow_i64('), c_code
	assert compact.contains('IntList__op_index_set('), c_code
	assert !c_code.contains(' ** '), c_code
}

fn test_struct_field_power_assign_uses_operator_overload() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'struct_field_power_assign', 'struct Exponent {
	value int
}

fn (a Exponent) ** (b Exponent) Exponent {
	return Exponent{
		value: a.value * 10 + b.value
	}
}

struct Box {
mut:
	value Exponent
}

fn main() {
	mut box := Box{
		value: Exponent{
			value: 2
		}
	}
	box.value **= Exponent{
		value: 3
	}
	println(int_str(box.value.value))
}
')
	assert out == '23'
	c_code := os.read_file(os.join_path(os.temp_dir(), 'v3_struct_field_power_assign.c')) or {
		panic(err)
	}
	compact := c_code.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('box.value=Exponent__mul_(box.value,(Exponent){.value=3});'), c_code
}

fn test_addressed_inferred_array_const_keeps_dynamic_storage() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'addressed_inferred_array_const', 'const vals = [1, 2, 3]

fn array_len(values &[]int) int {
	return values.len
}

fn main() {
	ptr := &vals
	copy := vals
	println(int_str(array_len(ptr) + ptr[0] + copy[2]))
}
')
	assert out == '7'
	c_code := os.read_file(os.join_path(os.temp_dir(), 'v3_addressed_inferred_array_const.c')) or {
		panic(err)
	}
	compact := c_code.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('Arraymain__vals;'), c_code
	assert compact.contains('main__vals=new_array_from_c_array(3,3,sizeof(int),'), c_code
	assert !compact.contains('intmain__vals[3]'), c_code
}

fn test_numbered_string_identifiers_do_not_collide_with_literal_symbols() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'numbered_string_identifier_collision', 'struct Named {
	_str_2 string
}

fn main() {
	_str_1 := "local"
	value := Named{
		_str_2: "field"
	}
	println(_str_1 + ":" + value._str_2)
}
')
	assert out == 'local:field'
	c_code := os.read_file(os.join_path(os.temp_dir(), 'v3_numbered_string_identifier_collision.c')) or {
		panic(err)
	}
	assert c_code.contains('string v__str_1 = '), c_code
	assert c_code.contains('.v__str_2 = '), c_code
	assert c_code.contains('value.v__str_2'), c_code
}

fn test_module_qualified_free_call_is_not_rewritten_to_array_intrinsic() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good_project(v3_bin, 'module_qualified_free_call', {
		'v.mod':  'Module { name: "module_qualified_free_call" }\n'
		'main.v': 'module main

import m

fn main() {
	values := [2, 3]
	println(int_str(m.free(values)))
}
'
		'm/m.v':  'module m

pub fn free(values []int) int {
	return values[0] + values.len
}
'
	}, 'main.v')
	assert out == '4'
}

fn test_selectively_imported_free_call_is_not_rewritten_to_array_intrinsic() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good_project(v3_bin, 'selective_import_free_call', {
		'v.mod':  'Module { name: "selective_import_free_call" }\n'
		'main.v': 'module main

import m { free }

fn main() {
	values := [3, 4]
	println(int_str(free(values)))
}
'
		'm/m.v':  'module m

pub fn free(values []int) int {
	return values[0] + values.len
}
'
	}, 'main.v')
	assert out == '5'
}

fn test_sum_container_variant_import_alias_collision_uses_exact_tag() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good_project(v3_bin, 'sum_container_variant_alias_collision', {
		'v.mod':            'Module { name: "sum_container_variant_alias_collision" }\n'
		'other/value.v':    'module other\n\npub struct Value {\npub:\n\tn int\n}\n'
		'sub/tast/value.v': 'module tast\n\npub struct Value {\npub:\n\tn int\n}\n'
		'main.v':           'module main\n\nimport other\nimport sub.tast as tast\n\ntype Mixed = map[string]other.Value | map[string]tast.Value\n\nfn score(value Mixed) int {\n\tif value is map[string]tast.Value {\n\t\titems := value as map[string]tast.Value\n\t\treturn items["x"].n\n\t}\n\treturn -1\n}\n\nfn main() {\n\tvalue := Mixed(map[string]tast.Value{\n\t\t"x": tast.Value{\n\t\t\tn: 71\n\t\t}\n\t})\n\tprintln(int_str(score(value)))\n}\n'
	}, 'main.v')
	assert out == '71'
}

fn test_sum_name_import_alias_collision_uses_exact_sum() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good_project(v3_bin, 'sum_name_import_alias_collision', {
		'v.mod':         'Module { name: "sum_name_import_alias_collision" }\n'
		'a/tast/tast.v': 'module tast\n\npub struct Target {}\npub struct Second {}\npub type Value = Target | Second\n'
		'b/tast/tast.v': 'module tast\n\npub struct First {}\npub struct Target {\npub:\n\tn int\n}\npub type Value = First | Target\n'
		'main.v':        'module main\n\nimport a.tast as other_tast\nimport b.tast as tast\n\nfn score(value tast.Value) int {\n\tif value is tast.Target {\n\t\ttarget := value as tast.Target\n\t\treturn target.n\n\t}\n\treturn -1\n}\n\nfn main() {\n\t_ := other_tast.Value(other_tast.Target{})\n\tvalue := tast.Value(tast.Target{\n\t\tn: 79\n\t})\n\tprintln(int_str(score(value)))\n}\n'
	}, 'main.v')
	assert out == '79'
}

fn test_user_defined_free_method_is_preserved() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'user_defined_free_method', 'struct Handle {
mut:
	freed bool
}

fn (mut h Handle) free() {
	h.freed = true
	println("freed")
}

fn main() {
	mut h := Handle{}
	h.free()
}
')
	assert out == 'freed'
}

fn test_pointer_receiver_on_pointer_parameter_passes_parameter_directly() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'pointer_receiver_on_pointer_param', 'struct S {
	n int
}

fn (s &S) value_plus(delta int) int {
	return s.n + delta
}

fn use_pointer_param(s &S) int {
	return s.value_plus(5)
}

fn main() {
	s := S{
		n: 37
	}
	println(int_str(use_pointer_param(&s)))
}
')
	assert out == '42'
}

fn test_mut_receiver_return_self_pointer_uses_receiver_pointer() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'mut_receiver_return_self_pointer', 'struct Command {
mut:
	args []string
}

fn (mut cmd Command) arg(arg string) &Command {
	cmd.args << arg
	return &cmd
}

fn (mut cmd Command) count() int {
	return cmd.args.len
}

fn main() {
	mut cmd := Command{}
	println(int_str(cmd.arg("a").arg("b").count()))
}
')
	assert out == '2'
}

fn test_pointer_value_compatibility_keeps_qualified_struct_names() {
	v3_bin := build_v3_review_cgen()
	files := {
		'v.mod': 'Module { name: "pointer_value_qualified_names" }\n'
		'a/a.v': 'module a

pub struct Foo {
pub:
	value int
}

pub fn make() &Foo {
	return &Foo{
		value: 7
	}
}
'
		'b/b.v': 'module b

pub struct Foo {
pub:
	value int
}
'
	}
	mut return_files := files.clone()
	return_files['main.v'] = 'module main

import a
import b

fn f() b.Foo {
	return a.make()
}

fn main() {}
'
	review_cgen_run_bad_project(v3_bin, 'bad_pointer_value_cross_module_return', return_files,
		'main.v', 'cannot return')
	mut field_files := files.clone()
	field_files['main.v'] = 'module main

import a
import b

struct Holder {
	item b.Foo
}

fn make_holder() Holder {
	return Holder{
		item: a.make()
	}
	}

fn main() {}
'
	review_cgen_run_bad_project(v3_bin, 'bad_pointer_value_cross_module_field', field_files,
		'main.v', 'cannot initialize field')
}

fn test_optional_single_letter_struct_keeps_concrete_payload_type() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'optional_single_letter_struct', 'struct T {
	value int
}

fn maybe_t(ok bool) ?T {
	if !ok {
		return none
	}
	return T{
		value: 7
	}
}

fn result_t() !T {
	return T{
		value: 8
	}
}

fn main() {
	a := maybe_t(true) or { T{} }
	b := result_t() or { T{} }
	println(int_str(a.value + b.value))
}
')
	assert out == '15'
}

fn test_enum_power_initializers_are_folded() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'enum_power_initializers', 'fn enum_power(base int, exponent int) int {
	return base ** exponent
}

enum Power {
	direct = 2 ** 3
	helper = enum_power(3, 2)
	next
}

@[flag]
enum FlagPower {
	direct = 2 ** 1
	next
}

enum BackedPower as u64 {
	direct = 2 ** 5
}

@[flag]
enum BackedFlagPower as u64 {
	direct = 2 ** 2
	next
}

fn main() {
	println(int_str(int(Power.direct)))
	println(int_str(int(Power.helper)))
	println(int_str(int(Power.next)))
	println(int_str(int(FlagPower.direct)))
	println(int_str(int(FlagPower.next)))
	println(u64(BackedPower.direct))
	println(u64(BackedFlagPower.direct))
	println(u64(BackedFlagPower.next))
}
')
	assert out == '8\n9\n10\n4\n8\n32\n16\n32'
}

fn test_zero_and_new_preserve_nested_array_type_arguments() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'nested_array_zero_new', 'struct NewDefaults {
mut:
	values []int
	lookup map[string]int
	count  int = 16
}

type NewInts = []int
type NewNested = [][]int
type NewOptional = []?int

fn zero_payload_array[T](value ?T) []T {
	return $zero([]typeof(value).payload_type{})
}

fn new_value[T]() T {
	return $new(T.pointee_type)
}

fn main() {
	nested := $zero([][]int{})
	optional := $zero([]?int{})
	mut direct_ptr := new_value[&NewInts]()
	mut nested_ptr := new_value[&NewNested]()
	mut optional_ptr := new_value[&NewOptional]()
	mut defaults := new_value[&NewDefaults]()
	payload := zero_payload_array[int](none)
	direct_ptr << 11
	nested_ptr << [12]
	optional_ptr << 13
	defaults.values << 14
	defaults.lookup["answer"] = 15
	println(int_str(nested.len))
	println(int_str(optional.len))
	println(int_str(payload.len))
	println(int_str(direct_ptr[0]))
	println(int_str(nested_ptr[0][0]))
	println(int_str(optional_ptr[0] or { -1 }))
	println(int_str(defaults.values[0]))
	println(int_str(defaults.lookup["answer"]))
	println(int_str(defaults.count))
}
')
	assert out == '0\n0\n0\n11\n12\n13\n14\n15\n16'
}
