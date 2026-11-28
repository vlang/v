import os
import v.cmdexec
import v.tempname

const review_cgen_fixture_dir = os.join_path(os.vtmp_dir(), 'v3_review_cgen_${tempname.unique_token()}')

fn testsuite_begin() {
	os.mkdir_all(review_cgen_fixture_dir) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(review_cgen_fixture_dir) or {}
}

fn review_cgen_compile(input string, output string) os.Result {
	return cmdexec.run_in_merged_with_env(@VEXE, ['-nocache', input, '-b', 'c', '-o', output], '', {
		'VFLAGS': ''
	})
}

fn review_cgen_write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn review_cgen_run_good_project(name string, files map[string]string, input string) string {
	root := os.join_path(review_cgen_fixture_dir, 'v3_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		review_cgen_write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	good_bin := os.join_path(review_cgen_fixture_dir, 'v3_${name}')
	compile := review_cgen_compile(input_path, good_bin)
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.exec([good_bin])
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

struct ReviewCgenCase {
	name     string
	source   string
	expected string
}

fn review_cgen_run_batch(name string, cases []ReviewCgenCase, files map[string]string) string {
	root := os.join_path(review_cgen_fixture_dir, 'v3_${name}_project')
	for rel, src in files {
		review_cgen_write_project_file(root, rel, src)
	}
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for case in cases {
		module_name := case.name
		imports << 'import ${module_name}'
		calls << "\t\t'${case.name}' { ${module_name}.exercise() }"
		source := if case.source.starts_with('module main\n') {
			case.source.replace_once('module main\n', 'module ${module_name}\n')
		} else {
			'module ${module_name}\n\n' + case.source
		}
		review_cgen_write_project_file(root, '${module_name}/${case.name}.v', source.replace_once('\nfn main()', '\npub fn exercise()'))
	}
	review_cgen_write_project_file(root, 'main.v', "module main\n\nimport os\n${imports.join('\n')}\n\nfn main() {\n\tmatch os.args[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n")
	return review_cgen_run_cases(root, name, cases)
}

fn review_cgen_run_cases(root string, name string, cases []ReviewCgenCase) string {
	good_bin := os.join_path(review_cgen_fixture_dir, 'v3_${name}')
	compile := review_cgen_compile(root, good_bin)
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	for case in cases {
		run := os.exec([good_bin, case.name])
		assert run.exit_code == 0, '${case.name}: run failed\n${run.output}'
		out := run.output.trim_space()
		assert out == case.expected, '${case.name}: expected `${case.expected}`, got `${out}`'
	}
	return good_bin
}

fn review_cgen_body(c_code string, signature string) string {
	for line in c_code.split_into_lines() {
		if line.starts_with(signature) && line.ends_with(' {') {
			body := c_code.all_after(line + '\n')
			end := body.index('\n}') or { panic('missing end of generated C function: ${signature}') }
			return body[..end]
		}
	}
	panic('missing generated C function: ${signature}')
}

fn test_main_power_and_optional_values() {
	cases := [
		ReviewCgenCase{'power_const_runtime_init', 'const byte_power = u8(2 ** 3)
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
', '8\n8\n9\n3.0\n8\n4'},
		ReviewCgenCase{'optional_single_letter_struct', 'struct T {
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
', '15'},
	]
	name := 'main_power_and_optional_values'
	root := os.join_path(review_cgen_fixture_dir, 'v3_${name}_project')
	mut calls := []string{cap: cases.len}
	for case in cases {
		calls << "\t\t'${case.name}' { ${case.name}() }"
		review_cgen_write_project_file(root, '${case.name}.v', case.source.replace_once('fn main() {',
			'fn ${case.name}() {'))
	}
	review_cgen_write_project_file(root, 'main.v', "module main\n\nfn main() {\n\tmatch arguments()[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n")
	good_bin := review_cgen_run_cases(root, name, cases)
	c_code := os.read_file(good_bin + '.c') or { panic(err) }
	assert c_code.contains('u8 main__byte_power;'), c_code
	assert c_code.contains('i64 main__max_len;'), c_code
	assert c_code.contains('i64 main__address_power;'), c_code
	assert c_code.contains('double main__float_power;'), c_code
	assert c_code.contains('main__byte_power = '), c_code
	assert c_code.contains('main__max_len = '), c_code
	assert c_code.contains('main__address_power = '), c_code
	assert c_code.contains('main__float_power = '), c_code
	// `max_len` also names a private builtin const, which sizes the SortedMap
	// nodes; the builtin consts derived from it must not use the main const.
	mut builtin_max_len_uses := 0
	for line in c_code.split_into_lines() {
		if line.contains('builtin__max_len') || line.contains('builtin__children_bytes') {
			builtin_max_len_uses++
			assert !line.contains('main__max_len'), line
		}
	}
	assert builtin_max_len_uses > 0, c_code
}

fn test_codegen_review_fixtures() {
	mut cases := constant_storage_enum_powers_and_nested_arrays_fixtures()
	cases << literal_enum_pointer_and_mut_receivers_fixtures()
	cases << c_output_arguments_and_power_assignments_fixtures()
	cases << numbered_identifiers_and_free_calls_fixtures()
	cases << sum_type_aliases_and_generic_new_defaults_fixtures()
	good_bin := review_cgen_run_batch('codegen_review_fixtures', cases, {
		'v.mod':            "Module { name: 'codegen_review_fixtures' }\n"
		'token/token.v':    "module token\n\npub enum Kind {\n\tword\n}\n\npub fn (kind Kind) str() string {\n\treturn 'word'\n}\n"
		'ast/ast.v':        'module ast\n\nimport token\n\npub fn render(kind token.Kind) string {\n\treturn kind.str()\n}\n'
		'm/m.v':            'module m

pub fn free(values []int) int {
	return values[0] + values.len
}
'
		'other/value.v':    'module other\n\npub struct Value {\npub:\n\tn int\n}\n'
		'sub/tast/value.v': 'module tast\n\npub struct Value {\npub:\n\tn int\n}\n'
		'a/tast/tast.v':    'module tast\n\npub struct Target {}\npub struct Second {}\npub type Value = Target | Second\n'
		'b/tast/tast.v':    'module tast\n\npub struct First {}\npub struct Target {\npub:\n\tn int\n}\npub type Value = First | Target\n'
	})
	c_code := os.read_file(good_bin + '.c') or { panic(err) }
	check_constant_storage_enum_powers_and_nested_arrays_c(c_code)
	check_c_output_arguments_and_power_assignments_c(c_code)
	check_numbered_identifiers_and_free_calls_c(c_code)
}

fn check_constant_storage_enum_powers_and_nested_arrays_c(c_code string) {
	assert c_code.contains('struct_const_sum_type_runtime_init__Holder struct_const_sum_type_runtime_init__holder;'), c_code
	assert c_code.contains('struct_const_sum_type_runtime_init__holder = (struct_const_sum_type_runtime_init__Holder){'), c_code
	assert c_code.contains('._object = (struct_const_sum_type_runtime_init__Empty*)(memdup('), c_code
	compact := c_code.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('Arrayaddressed_inferred_array_const__vals;'), c_code
	assert compact.contains('addressed_inferred_array_const__vals=new_array_from_c_array(3,3,sizeof(i64),'), c_code
	assert !compact.contains('i64addressed_inferred_array_const__vals[3]'), c_code
}

fn check_c_output_arguments_and_power_assignments_c(c_code string) {
	abi_body := review_cgen_body(c_code, 'void c_int_array_data_abi__exercise(')
	assert abi_body.contains('int* _cabi_out_values_'), abi_body
	assert abi_body.contains('free(_cabi_out_values_'), abi_body
	square_body := review_cgen_body(c_code, 'void mut_parameter_power_assign__square(')
	compact := square_body.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('__v_pow_i64('), square_body
	assert !compact.contains('(i64*)__v_pow_i64('), square_body
	array_body := review_cgen_body(c_code, 'void array_power_assign__exercise(')
	array_compact := array_body.replace('\t', '').replace(' ', '').replace('\n', '')
	assert array_compact.contains('array__set(_a0,_i0,&(i64[]){((i64)__v_pow_i64('), array_body
	assert array_compact.contains('array__set(_a1,_i1,&(array_power_assign__Exponent[]){__v3_internal_symbol_op_array_power_assign__Exponent__pow(*(array_power_assign__Exponent*)array_get(*_a1,_i1),'), array_body
	assert array_compact.contains('__v3_internal_symbol_op_array_power_assign__IntList__op_index_set('), array_body
	assert array_compact.contains('__v3_internal_symbol_op_array_power_assign__ExponentList__op_index_set('), array_body
	assert array_compact.contains('__v3_internal_symbol_op_array_power_assign__Exponent__pow('), array_body
	assert !c_code.contains(' ** '), c_code
	field_body := review_cgen_body(c_code, 'void struct_field_power_assign__exercise(')
	field_compact := field_body.replace('\t', '').replace(' ', '').replace('\n', '')
	assert field_compact.contains('box.value=__v3_internal_symbol_op_struct_field_power_assign__Exponent__pow(box.value,(struct_field_power_assign__Exponent){.value=3});'), field_body
}

fn check_numbered_identifiers_and_free_calls_c(c_code string) {
	body := review_cgen_body(c_code, 'void numbered_string_identifier_collision__exercise(')
	assert body.contains('string v__str_1 = '), body
	assert body.contains('.v__str_2 = '), body
	assert body.contains('value.v__str_2'), body
}

fn constant_storage_enum_powers_and_nested_arrays_fixtures() []ReviewCgenCase {
	return [
		ReviewCgenCase{'struct_const_sum_type_runtime_init', 'interface Value {}

struct Empty {}

struct Holder {
	value Value
}

const holder = Holder{
	value: Empty{}
}

fn main() {
	println(holder.value is Empty)
}
', 'true'},
		ReviewCgenCase{'enum_power_initializers', 'const helper_base = 3

enum Power {
	direct = 2 ** 3
	helper = helper_base ** 2
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
', '8\n9\n10\n4\n8\n32\n16\n32'},
		ReviewCgenCase{'addressed_inferred_array_const', 'const vals = [1, 2, 3]

fn array_len(values &[]int) int {
	return values.len
}

fn main() {
	ptr := &vals
	copy := vals
	println(int_str(array_len(ptr) + unsafe { ptr[0] } + copy[2]))
}
', '7'},
		ReviewCgenCase{'nested_array_zero_new', 'struct NewDefaults {
mut:
	values []int
	lookup map[string]int
	count  int = 16
}

struct GenericBox[T] {
	value T
}

type NewInts = []int
type NewNested = [][]int
type NewOptional = []?int

fn zero_payload_array[T](value ?T) []T {
	return $zero([]typeof(value).payload_type{})
}

fn zero_optional_array[T]() []?T {
	return $zero([]?T{})
}

fn zero_result_array[T]() []!T {
	return $zero([]!T{})
}

fn zero_pointer_array[T]() []&T {
	return $zero([]&T{})
}

fn new_optional_array[T]() &[]?T {
	return $new([]?T{})
}

fn new_value[T]() T {
	return $new(T.pointee_type)
}

fn main() {
	nested := $zero([][]int{})
	optional := $zero([]?int{})
	generic := $zero([]GenericBox[int]{})
	fixed := $zero([2]int{})
	fixed_ptr := $new([3]int{})
	mut generic_ptr := $new([]GenericBox[int]{})
	mut direct_ptr := new_value[&NewInts]()
	mut nested_ptr := new_value[&NewNested]()
	mut optional_ptr := new_value[&NewOptional]()
	mut defaults := new_value[&NewDefaults]()
	payload := zero_payload_array[int](none)
	optional_generic := zero_optional_array[int]()
	result_generic := zero_result_array[int]()
	pointer_generic := zero_pointer_array[int]()
	mut optional_generic_ptr := new_optional_array[int]()
	direct_ptr << 11
	nested_ptr << [12]
	optional_ptr << 13
	defaults.values << 14
	defaults.lookup["answer"] = 15
	optional_generic_ptr << 17
	generic_ptr << GenericBox[int]{
		value: 18
	}
	println(int_str(nested.len))
	println(int_str(optional.len))
	println(int_str(generic.len))
	println(int_str(fixed.len))
	println(int_str((*fixed_ptr).len))
	println(int_str(generic_ptr.len))
	println(int_str(payload.len))
	println(int_str(optional_generic.len))
	println(int_str(result_generic.len))
	println(int_str(pointer_generic.len))
	println(int_str(unsafe { optional_generic_ptr[0] or { -1 } }))
	println(int_str(unsafe { direct_ptr[0] }))
	println(int_str(unsafe { nested_ptr[0][0] }))
	println(int_str(unsafe { optional_ptr[0] or { -1 } }))
	println(int_str(defaults.values[0]))
	println(int_str(defaults.lookup["answer"]))
	println(int_str(defaults.count))
}
', '0\n0\n0\n2\n3\n1\n0\n0\n0\n0\n17\n11\n12\n13\n14\n15\n16'},
	]
}

fn literal_enum_pointer_and_mut_receivers_fixtures() []ReviewCgenCase {
	return [
		ReviewCgenCase{'int_literal_receiver', 'fn main() {
	println(255.hex_full())
	println(4294967296.hex_full())
}
', '00000000000000ff\n0000000100000000'},
		ReviewCgenCase{'imported_enum_method_call', 'module main\n\nimport ast\nimport token\n\nfn main() {\n\tprintln(ast.render(token.Kind.word))\n}\n', 'word'},
		ReviewCgenCase{'pointer_receiver_on_pointer_param', 'struct S {
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
', '42'},
		ReviewCgenCase{'mut_receiver_return_self_pointer', 'struct Command {
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
', '2'},
	]
}

fn c_output_arguments_and_power_assignments_fixtures() []ReviewCgenCase {
	return [
		ReviewCgenCase{'c_int_array_data_abi', "#include <stdio.h>

fn main() {
	mut first := []int{len: 1}
	mut second := []int{len: 1}
	C.sscanf(c'4 2', c'%d %d', first.data, second.data)
	println(int_str(first[0] + second[0]))
}
", '6'},
		ReviewCgenCase{'mut_parameter_power_assign', 'struct Number {
mut:
	value int
}

fn square(mut number Number) {
	number.value **= 2
}

fn main() {
	mut number := Number{
		value: 3
	}
	square(mut number)
	println(int_str(number.value))
}
', '9'},
		ReviewCgenCase{'array_power_assign', 'struct IntList {
mut:
	values []int
}

fn (list IntList) [] (index int) int {
	return list.values[index]
}

fn (mut list IntList) []= (index int, value int) {
	list.values[index] = value
}

struct Exponent {
	value int
}

fn (a Exponent) ** (b Exponent) Exponent {
	return Exponent{
		value: a.value * 10 + b.value
	}
}

struct ExponentList {
mut:
	values []Exponent
}

fn (list ExponentList) [] (index int) Exponent {
	return list.values[index]
}

fn (mut list ExponentList) []= (index int, value Exponent) {
	list.values[index] = value
}

fn main() {
	mut values := [2, 3]
	values[0] **= 3
	println(int_str(values[0]))
	mut direct := [Exponent{
		value: 4
	}]
	direct[0] **= Exponent{
		value: 5
	}
	println(int_str(direct[0].value))
	mut list := IntList{
		values: [3]
	}
	list[0] **= 2
	println(int_str(list.values[0]))
	mut exponents := ExponentList{
		values: [Exponent{
			value: 2
		}]
	}
	exponents[0] **= Exponent{
		value: 3
	}
	println(int_str(exponents.values[0].value))
}
', '8\n45\n9\n23'},
		ReviewCgenCase{'struct_field_power_assign', 'struct Exponent {
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
', '23'},
	]
}

fn numbered_identifiers_and_free_calls_fixtures() []ReviewCgenCase {
	return [
		ReviewCgenCase{'numbered_string_identifier_collision', 'struct Named {
	_str_2 string
}

fn main() {
	_str_1 := "local"
	value := Named{
		_str_2: "field"
	}
	println(_str_1 + ":" + value._str_2)
}
', 'local:field'},
		ReviewCgenCase{'user_defined_free_method', 'struct Handle {
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
', 'freed'},
		ReviewCgenCase{'module_qualified_free_call', 'module main

import m

fn main() {
	values := [2, 3]
	println(int_str(m.free(values)))
}
', '4'},
		ReviewCgenCase{'selective_import_free_call', 'module main

import m { free }

fn main() {
	values := [3, 4]
	println(int_str(free(values)))
}
', '5'},
	]
}

fn sum_type_aliases_and_generic_new_defaults_fixtures() []ReviewCgenCase {
	return [
		ReviewCgenCase{'sum_container_variant_alias_collision', 'module main\n\nimport other\nimport sub.tast\n\ntype Mixed = map[string]other.Value | map[string]tast.Value\n\nfn score(value Mixed) int {\n\tif value is map[string]tast.Value {\n\t\titems := value as map[string]tast.Value\n\t\treturn items["x"].n\n\t}\n\treturn -1\n}\n\nfn main() {\n\tvalue := Mixed(map[string]tast.Value{\n\t\t"x": tast.Value{\n\t\t\tn: 71\n\t\t}\n\t})\n\tprintln(int_str(score(value)))\n}\n', '71'},
		ReviewCgenCase{'sum_name_import_alias_collision', 'module main\n\nimport a.tast as other_tast\nimport b.tast\n\nfn score(value tast.Value) int {\n\tif value is tast.Target {\n\t\ttarget := value as tast.Target\n\t\treturn target.n\n\t}\n\treturn -1\n}\n\nfn main() {\n\t_ := other_tast.Value(other_tast.Target{})\n\tvalue := tast.Value(tast.Target{\n\t\tn: 79\n\t})\n\tprintln(int_str(score(value)))\n}\n', '79'},
		ReviewCgenCase{'direct_map_generic_zero_new', 'struct Box[T] {
mut:
	value T
}

fn main() {
	mut values := $zero(map[string]int{})
	mut box := $new(Box[int]{})
	values["answer"] = 19
	box.value = 20
	println(int_str(values["answer"]))
	println(int_str(box.value))
}
', '19\n20'},
		ReviewCgenCase{'generic_new_expression_scope', "fn initialized_value() int {
	println('initialized')
	return 1
}

struct NeedsInitialization {
	value int = initialized_value()
}

fn maybe_new[T](cond bool) {
	if cond {
		_ = \$new(T)
	}
}

fn maybe_short_new[T](cond bool) bool {
	return cond && !isnil(\$new(T))
}

fn main() {
	maybe_new[NeedsInitialization](false)
	println('after false')
	maybe_new[NeedsInitialization](true)
	println(maybe_short_new[NeedsInitialization](false))
	println(maybe_short_new[NeedsInitialization](true))
}
", 'after false\ninitialized\nfalse\ninitialized\ntrue'},
	]
}

fn test_pointer_value_compatibility_keeps_qualified_struct_names() {
	mut files := {
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
	files['return_value.v'] = 'module main

import a
import b

fn f() b.Foo {
	return a.make()
}
'
	files['field_value.v'] = 'module main

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
'

	files['main.v'] = 'module main\n\nfn main() {}\n'
	root := os.join_path(review_cgen_fixture_dir, 'v3_pointer_value_compatibility_project')
	for file, contents in files {
		review_cgen_write_project_file(root, file, contents)
	}
	output := os.join_path(review_cgen_fixture_dir, 'v3_pointer_value_compatibility_keeps_qualified_struct_names')
	result := review_cgen_compile(root, output)
	assert result.exit_code != 0, result.output
	assert !result.output.contains('C compilation failed'), result.output
	diagnostics := result.output.split_into_lines()
	assert diagnostics.any(it.contains('return_value.v:') && it.contains('cannot use `&a.Foo` as type `b.Foo` in return argument')), 'return_value: ' + result.output
	assert diagnostics.any(it.contains('field_value.v:') && it.contains('cannot assign to field `item`: expected `b.Foo`, not `&a.Foo`')), 'field_value: ' + result.output
}
