import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3_review_checker() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_review_checker_regressions_test_${os.getpid()}')
	if os.is_executable(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${vexe} -gc none -prealloc -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn run_bad(v3_bin string, name string, src string, expected string) {
	bad_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	result := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert result.exit_code != 0, '${name}: expected failure, got success\n${result.output}'
	assert result.output.contains(expected), '${name}: expected `${expected}` in\n${result.output}'
	assert !result.output.contains('C compilation failed'), '${name}: reached C compilation\n${result.output}'
}

fn run_good(v3_bin string, name string, src string) string {
	good_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} -enable-globals ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn run_good_project(v3_bin string, name string, files map[string]string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}_project_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }\n") or { panic(err) }
	for relative, source in files {
		path := os.join_path(root, relative)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, source) or { panic(err) }
	}
	bin_path := os.join_path(root, 'program')
	compile := os.execute('${v3_bin} -nocache -b c -o ${bin_path} ${os.join_path(root, 'main.v')}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	run := os.execute(bin_path)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn run_runtime_bad(v3_bin string, name string, src string) string {
	bad_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.execute(bad_bin)
	assert run.exit_code != 0, '${name}: expected runtime failure, got success\n${run.output}'
	return run.output.trim_space()
}

fn test_recursive_str_allows_smartcast_sum_variant_formatting() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'recursive_str_smartcast_sum_variant', 'type Scalar = bool | int | string

fn (value Scalar) str() string {
	return value.display()
}

fn (value Scalar) display() string {
	return match value {
		bool, int { value.str() }
		string { value }
	}
}

fn main() {
	println(Scalar(7).str())
}
')
	assert out == '7'
}

fn test_explicit_mut_sum_smartcasts_preserve_concrete_variant_fields() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'explicit_mut_sum_smartcasts', 'struct First {
mut:
	value int
}

struct Second {}

type Expr = First | Second

struct Inspector {}

fn (mut inspector Inspector) inspect(mut value Expr) {}

fn main() {
	mut inspector := Inspector{}
	mut expr := Expr(First{
		value: 7
	})
	if mut expr is First {
		expr.value++
		inspector.inspect(mut expr)
		println(expr.value)
	}
	mut values := [Expr(First{
		value: 11
	})]
	if mut values[0] is First {
		values[0].value++
		println(values[0].value)
	}
}
')
	assert out == '8
12'
}

fn test_forwarded_mut_sum_parameter_invalidates_smartcast() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'forwarded_mut_sum_argument_invalidates_smartcast', 'struct First {
	value int
}

struct Second {}

type Expr = First | Second

fn retag(mut value Expr) {
	value = Expr(Second{})
}

fn wrapper(mut value Expr) {
	retag(mut value)
}

fn main() {
	mut expr := Expr(First{
		value: 7
	})
	if mut expr is First {
		wrapper(mut expr)
		println(expr.value)
	}
}
',
		'field `value` does not exist')
	run_bad(v3_bin, 'forwarded_mut_sum_receiver_invalidates_smartcast', 'struct First {
	value int
}

struct Second {}

type Expr = First | Second

fn (mut value Expr) retag() {
	value = Expr(Second{})
}

fn wrapper(mut value Expr) {
	value.retag()
}

fn main() {
	mut expr := Expr(First{
		value: 7
	})
	if mut expr is First {
		wrapper(mut expr)
		println(expr.value)
	}
}
',
		'field `value` does not exist')
}

fn test_mut_index_alias_updates_original_storage() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'mut_index_alias', 'struct Item {
mut:
	value int
}

fn main() {
	mut items := [Item{
		value: 1
	}]
	mut item := mut items[0]
	item.value++
	println(items[0].value)
}
')
	assert out == '1'
}

fn test_shared_globals_keep_lockable_storage_metadata() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'shared_global_storage', '@[has_globals]
module main

struct Counter {
mut:
	value int
}

__global counter shared Counter

fn main() {
	lock counter {
		counter.value = 42
	}
	value := rlock counter {
		counter.value
	}
	println(value)
}
')
	assert out == '42'
}

fn test_source_field_names_require_snake_case() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'uppercase_struct_field', 'struct S {\n\tFoo int\n}\n\nfn main() {}\n',
		'field name `Foo` cannot contain uppercase letters, use snake_case instead')
}

fn test_generic_factory_can_initialize_its_own_noinit_struct_for_external_callers() {
	v3_bin := build_v3_review_checker()
	out := run_good_project(v3_bin, 'generic_noinit_factory', {
		'factory/factory.v': 'module factory

@[noinit]
pub struct Box[T] {
pub:
	value T
}

pub fn Box.create[T](value T) Box[T] {
	return Box[T]{
		value: value
	}
}
'
		'main.v':            'module main

import factory

fn main() {
	println(factory.Box.create[int](42).value)
}
'
	})
	assert out == '42'
}

fn test_orm_checker_resolves_module_qualified_table_fields() {
	v3_bin := build_v3_review_checker()
	out := run_good_project(v3_bin, 'qualified_orm_table', {
		'model/model.v': "module model

import db.sqlite

pub struct Entry {
pub:
	id int @[primary]
	name string
}

pub fn setup(mut db sqlite.DB) ! {
	sql db {
		create table Entry
	}!
	entry := Entry{
		name: 'ready'
	}
	sql db {
		insert entry into Entry
	}!
}
"
		'main.v':        "module main

import db.sqlite
import model

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	model.setup(mut db) or { panic(err) }
	rows := sql db {
		select from model.Entry where name == 'ready'
	} or { panic(err) }
	println(rows.len)
}
"
	})
	assert out == '1'
}

fn test_immutable_fields_and_result_storage_are_rejected() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'immutable_struct_field',
		'struct S {\n\tread_only int\nmut:\n\twritable int\n}\n\nfn main() {\n\tmut s := S{}\n\ts.read_only = 1\n}\n',
		'field `read_only` of struct `S` is immutable')
	run_bad(v3_bin, 'result_function_parameter', 'fn consume(value !int) {}\n\nfn main() {}\n',
		'result type arguments are not supported')
	run_bad(v3_bin, 'result_channel_element', 'fn main() {\n\t_ := chan !int{}\n}\n',
		'cannot use chan with Result type')
}

fn test_declaration_mutability_and_storage_restrictions_match_v1() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'immutable_reference_in_mut_field', 'struct Holder {
mut:
	ptr &int
}

fn main() {
	value := 1
	_ := Holder{
		ptr: &value
	}
}
',
		'`value` is immutable, cannot have a mutable reference to an immutable object')
	run_bad(v3_bin, 'result_struct_field', 'struct Holder {
	value !int
}

fn main() {}
',
		'struct field does not support storing Result')
	run_bad(v3_bin, 'mutable_primitive_parameter', 'fn update(mut value int) {
	value++
}

fn main() {}
',
		'mutable arguments are only allowed for arrays, interfaces, maps, pointers, structs or their aliases')
	run_bad(v3_bin, 'parameter_shadows_import', 'import arrays

fn update(arrays int) {
	_ = arrays
}

fn main() {}
',
		'duplicate of an import symbol `arrays`')
	run_bad(v3_bin, 'mutable_interface_smartcast', 'interface Value {}

struct Concrete {}

fn main() {
	mut value := Value(Concrete{})
	if value is Concrete {}
}
',
		'smart casting a mutable interface value requires `if mut value is ...`')
	run_bad(v3_bin, 'explicit_option_void_return', 'fn work() ?void {
	return none
}

fn main() {}
',
		'use `?` instead of `?void`')
	out := run_good(v3_bin, 'canonical_option_void_return', 'fn work() ? {
	return none
}

fn main() {
	work() or {
		println("none")
		return
	}
}
')
	assert out == 'none'
}

fn test_pointer_struct_and_spawn_safety_match_v1() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'unparenthesized_pointer_write', 'fn main() {
	value := 0
	p := &value
	*p = 1
}
',
		'modifying variables via dereferencing can only be done in `unsafe` blocks')
	run_bad(v3_bin, 'parenthesized_pointer_write', 'fn main() {
	value := 0
	p := &value
	(*p) = 1
}
',
		'modifying variables via dereferencing can only be done in `unsafe` blocks')
	run_bad(v3_bin, 'uninitialized_reference_field', 'struct Holder {
	value &int
}

fn main() {
	_ := Holder{}
}
',
		'reference field `Holder.value` must be initialized')
	run_bad(v3_bin, 'partial_positional_struct_literal', 'struct Pair {
	first int
	second int
}

fn main() {
	_ := Pair{1}
}
',
		'too few fields in `Pair` literal (expecting 2, got 1)')
	run_bad(v3_bin, 'local_shadows_import', 'import arrays

fn main() {
	arrays := 1
	_ = arrays
}
',
		'duplicate of an import symbol `arrays`')
	run_bad(v3_bin, 'spawn_mutable_value_argument', 'struct State {
mut:
	value int
}

fn change(mut state State) {
	state.value++
}

fn main() {
	mut state := State{}
	spawn change(mut state)
}
',
		'function in `spawn` statement cannot contain mutable non-reference arguments')
	run_bad(v3_bin, 'spawn_mutable_value_receiver', 'struct State {
mut:
	value int
}

fn (mut state State) change() {
	state.value++
}

fn main() {
	mut state := State{}
	spawn state.change()
}
',
		'method in `spawn` statement cannot have non-reference mutable receiver')
	out := run_good(v3_bin, 'unsafe_pointer_write', 'fn main() {
	mut value := 0
	p := &value
	unsafe {
		*p = 1
		(*p) = 2
	}
	println(value)
}
')
	assert out == '2'
}

fn test_mutable_references_and_enum_order_match_v1() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'mutable_reference_to_immutable_local_declaration', 'struct Foo {
mut:
	value int
}

fn main() {
	f := Foo{}
	mut pf := &f
	pf.value = 1
}
',
		'`f` is immutable, cannot have a mutable reference to it')
	run_bad(v3_bin, 'mutable_reference_to_immutable_local_reassignment', 'struct Foo {
mut:
	value int
}

fn main() {
	f := Foo{}
	mut other := Foo{}
	mut pf := &other
	pf = &f
}
',
		'`f` is immutable, cannot have a mutable reference to it')
	run_bad(v3_bin, 'duplicate_value_enum_forward_reference', '@[_allow_multiple_values]
enum ForwardValue {
	a = .c
	c = 2
}

fn main() {}
',
		'`ForwardValue.c` should be declared before using it')
	mut out := run_good(v3_bin, 'immutable_reference_and_previous_enum_value', '@[_allow_multiple_values]
enum Value {
	a = 1
	b = .a
}

struct Foo {
	value int
}

fn main() {
	f := Foo{
		value: 2
	}
	pf := &f
	println(pf.value + int(Value.b))
}
')
	assert out == '3'
	out = run_good(v3_bin, 'parenthesized_reference_keeps_pointer_binding_mutable', 'fn main() {
	n := 1
	mut p := (&n)
	unsafe {
		*p = 10
	}
	println(*p)
}
')
	assert out == '10'
}

fn test_method_value_option_alias_and_const_names_match_v1() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'non_heap_pointer_receiver_method_value', 'struct Foo {}

fn (foo &Foo) ref() int {
	return 1
}

fn make() fn () int {
	foo := Foo{}
	return foo.ref
}

fn main() {}
',
		'method `Foo.ref` cannot be used as a variable outside `unsafe` blocks')
	run_bad(v3_bin, 'stack_pointer_alias_receiver_method_value', 'struct Foo {}

fn (foo &Foo) ref() int {
	return 1
}

fn make() fn () int {
	foo := Foo{}
	p := &foo
	return p.ref
}

fn main() {}
',
		'method `Foo.ref` cannot be used as a variable outside `unsafe` blocks')
	heap_method_value := run_good(v3_bin, 'heap_pointer_receiver_method_value', 'struct Foo {}

fn (foo &Foo) ref() int {
	return 1
}

fn main() {
	foo := &Foo{}
	callback := foo.ref
	println(callback())
}
')
	assert heap_method_value == '1'
	run_bad(v3_bin, 'direct_option_alias_cast', 'type MaybeInt = ?int

fn main() {
	_ := MaybeInt(none)
}
',
		'alias to Option type requires to be used as Option type (?MaybeInt(...))')
	run_bad(v3_bin, 'uppercase_const_name', 'const Red = 1

fn main() {
	println(Red)
}
',
		'const names cannot contain uppercase letters, use snake_case instead')
	out := run_good(v3_bin, 'heap_method_value_and_translated_const', '@[translated]
module main

const Red = 2

@[heap]
struct Foo {}

fn (foo &Foo) ref() int {
	return 1
}

fn main() {
	foo := Foo{}
	callback := foo.ref
	println(callback() + Red)
}
')
	assert out == '3'
}

fn test_reject_pointer_expressions_for_value_returns() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_return_pointer_to_value',
		'fn f() int {\n\tx := 1\n\treturn &x\n}\nfn main() {}\n',
		'you are returning `&int` instead')
}

fn test_reject_fixed_array_decay_to_pointer() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_fixed_array_pointer_argument',
		'fn consume(value &int) {}\n\nfn main() {\n\tconsume([1, 2]!)\n}\n',
		'cannot use `[2]int` as `&int` in argument 1 to `consume`')
	run_bad(v3_bin, 'bad_fixed_array_pointer_return',
		'fn make_pointer() &int {\n\treturn [1, 2]!\n}\n\nfn main() {}\n',
		'you are returning `[2]int` instead')
	run_bad(v3_bin, 'bad_translated_fixed_array_pointer_return',
		'@[translated]\nmodule main\n\nfn make_pointer() &int {\n\treturn [1, 2]!\n}\n\nfn main() {}\n',
		'you are returning `[2]int` instead')
	run_bad(v3_bin, 'bad_translated_fixed_array_pointer_temporary_assignment',
		'@[translated]\nmodule main\n\nfn main() {\n\tmut ptr := &int(0)\n\tptr = [1, 2]!\n}\n',
		'cannot assign to `ptr`: expected `&int`, not `[2]int`')
	run_bad(v3_bin, 'bad_addressed_fixed_u8_array_pointer_argument',
		'fn consume(value &u8) {}\n\nfn main() {\n\tbuf := [u8(1), 2]!\n\tconsume(&buf)\n}\n',
		'cannot use `&[2]u8`')
	run_bad(v3_bin, 'bad_fixed_i32_array_byte_pointer_assignment',
		'fn main() {\n\tbuf := [i32(1), 2]!\n\tbyte := u8(0)\n\tmut ptr := &byte\n\tptr = &buf\n}\n',
		'cannot reference fixed array `buf` outside `unsafe` blocks as it is supposed to be stored on stack')
	byte_out := run_good(v3_bin, 'good_fixed_array_pointer_inside_unsafe',
		'type Fixed = [2]u8\n\nfn main() {\n\tbuf := Fixed([u8(65), 66]!)\n\tptr := unsafe { &buf }\n\tprintln(int_str(int((*ptr)[0])))\n}\n')
	assert byte_out == '65'
	out := run_good(v3_bin, 'good_translated_fixed_array_pointer_assignment',
		'@[translated]\nmodule main\n\nfn main() {\n\tmut values := [1, 2]!\n\tmut ptr := unsafe { &values[0] }\n\tprintln(int_str(*ptr))\n}\n')
	assert out == '1'
}

fn test_enum_in_list_and_nested_array_address_match_v1() {
	v3_bin := build_v3_review_checker()
	insert_header := os.join_path(os.temp_dir(), 'v3_review_checker_insert.h')
	os.write_file(insert_header,
		'static inline int v3_review_checker_inserted(void) { return 6; }\n') or { panic(err) }
	out := run_good(v3_bin, 'good_enum_in_list_and_nested_array_address', '
#insert "@DIR/v3_review_checker_insert.h"

fn C.v3_review_checker_inserted() int

const max_bytes = 32 * 1024 * 1024

enum FormulaKind {
	text
	number
	date_time
}

struct Sheet {
	value int
}

struct Workbook {
	sheets []Sheet
}

struct Evaluator {
	workbook &Workbook
}

struct LocalHolder {
	evaluator &Evaluator
}

struct CopyValue {
	value int
}

fn pair() (int, int) {
	return 4, 5
}

fn number_value(value f64) f64 {
	return value
}

fn parse_number(text string) !f64 {
	return match text {
		"42" { text.f64() }
		else { error("not a number") }
	}
}

fn clone_value(original &CopyValue) CopyValue {
	return CopyValue{
		...original
	}
}

fn contains_value(value string, mut values []string) bool {
	return value in values
}

fn item_count(values []int) int {
	return values.len
}

fn shifted_value(base string) ?string {
	shifted := {
		"a": "A"
	}
	return shifted[base] or { none }
}

fn (mut evaluator Evaluator) keep_pointer_locally() {
	holder := LocalHolder{
		evaluator: evaluator
	}
	println(holder.evaluator.workbook.sheets[0].value)
}

fn inspect(evaluator &Evaluator, kind FormulaKind) {
	if kind in [.number, .date_time] {
		println("enum")
	}
	sheet := &evaluator.workbook.sheets[0]
	println(sheet.value)
	ch := "a"[0]
	assert ch >= `a` && ch <= `z`
	assert ch in [`a`, `b`]
	quote := `a`
	assert ch == quote
	assert u64(10) < max_bytes
	first, second := if ch == `a` {
		pair()
	} else {
		1, 2
	}
	println(first + second)
}

fn main() {
	workbook := &Workbook{
		sheets: [Sheet{
			value: 42
		}]
	}
	mut evaluator := Evaluator{
		workbook: workbook
	}
	mut values := ["value"]
	inspect(&evaluator, .number)
	evaluator.keep_pointer_locally()
	println(number_value(values.len))
	println(parse_number("42") or { 0 })
	println(clone_value(&CopyValue{
		value: 8
	}).value)
	println(contains_value("value", mut values))
	println(item_count([]))
	println(shifted_value("a") or { "none" })
	println(shifted_value("b") or { "none" })
	println(C.v3_review_checker_inserted())
}
')
	assert out == 'enum\n42\n9\n42\n1.0\n42.0\n8\ntrue\n0\nA\nnone\n6'
	run_bad(v3_bin, 'bad_address_mutable_array_element',
		'fn main() {\n\tmut values := [1]\n\t_ := &values[0]\n}\n',
		'cannot take the address of mutable array elements outside unsafe blocks')
}

fn test_reject_address_of_mutable_array_alias_element() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_address_mutable_array_alias_element',
		'type Numbers = []int\n\nfn main() {\n\tmut values := Numbers([1])\n\t_ := &values[0]\n}\n',
		'cannot take the address of mutable array elements outside unsafe blocks')
	run_bad(v3_bin, 'bad_address_nested_mutable_array_alias_element',
		'type Numbers = []int\ntype NumbersRef = &Numbers\n\nfn main() {\n\tmut values := Numbers([1])\n\tvalues_ref := NumbersRef(&values)\n\t_ := &values_ref[0]\n}\n',
		'cannot take the address of mutable array elements outside unsafe blocks')
	run_bad(v3_bin, 'bad_address_nested_map_alias_value',
		"type Scores = map[string]int\ntype ScoresRef = &Scores\n\nfn main() {\n\tmut scores := Scores({'key': 1})\n\tscores_ref := ScoresRef(&scores)\n\t_ := &scores_ref['key']\n}\n",
		'cannot take the address of map values outside `unsafe`')
}

fn test_cross_wrapper_option_result_returns_match_v1() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_result_value_in_option_return',
		'fn make_result() !int {\n\treturn 7\n}\n\nfn make_option() ?int {\n\treturn make_result()\n}\n\nfn main() {}\n',
		'cannot use `!int` as type `?int` in return argument')
	run_bad(v3_bin, 'bad_result_value_in_optional_pointer_return',
		'struct Item {}\n\nfn convert(res !Item) ?&Item {\n\treturn res\n}\n\nfn main() {}\n',
		'cannot use `!Item` as type `?&Item` in return argument')
	run_bad(v3_bin, 'bad_option_value_in_result_pointer_return',
		'struct Item {}\n\nfn convert(opt ?Item) !&Item {\n\treturn opt\n}\n\nfn main() {}\n',
		'cannot use `?Item` as type `!&Item` in return argument')
	option_out := run_good(v3_bin, 'good_error_branch_in_option_return',
		"fn make_option(ok bool) ?int {\n\treturn if ok { error('bad') } else { 1 }\n}\n\nfn main() {\n\tprintln(int_str(make_option(false) or { -1 }))\n\t_ := make_option(true) or {\n\t\tprintln(err.msg())\n\t\treturn\n\t}\n}\n")
	assert option_out == '1\nbad'
	constant_option_out := run_good(v3_bin, 'good_constant_error_branch_in_option_return',
		"fn make_option() ?int {\n\treturn if true { error('bad') } else { 1 }\n}\n\nfn main() {\n\t_ := make_option() or {\n\t\tprintln(err.msg())\n\t\treturn\n\t}\n}\n")
	assert constant_option_out == 'bad'
	out := run_good(v3_bin, 'good_error_branch_in_result_return',
		"fn make_result(ok bool) !int {\n\treturn if ok { error('bad') } else { 1 }\n}\n\nfn main() {\n\tprintln(int_str(make_result(false) or { -1 }))\n\tprintln(int_str(make_result(true) or { -1 }))\n}\n")
	assert out == '1\n-1'
}

fn test_option_void_is_not_compatible_with_payload_options() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_unsafe_none_option_default', 'struct Holder {
	value ?int = unsafe { none }
}

fn main() {
	holder := Holder{}
	println((holder.value == none).str())
}
')
	assert out == 'true'
	run_bad(v3_bin, 'bad_option_void_returned_as_option_int',
		'fn empty() ? {\n\treturn\n}\n\nfn value() ?int {\n\treturn empty()\n}\n\nfn main() {}\n',
		'cannot use `?void` as type `?int` in return argument')
	run_bad(v3_bin, 'bad_option_void_assigned_to_option_int',
		'fn empty() ? {\n\treturn\n}\n\nfn main() {\n\tmut value := ?int(1)\n\tvalue = empty()\n}\n',
		'cannot assign to `value`: expected `?int`, not `?void`')
}

fn test_assignment_selector_diagnostic_accepts_wide_columns() {
	v3_bin := build_v3_review_checker()
	indent := ' '.repeat(32768)
	run_bad(v3_bin, 'wide_assignment_selector_diagnostic', 'struct Holder {
	item ?string
}

fn main() {
	holder := Holder{}
	mut value := 0
${indent}value = holder.item or { "" }
}
',
		'cannot assign to `value`: expected `int`, not `string`')
}

fn test_generic_alias_substitutes_channel_element() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'generic_channel_alias_element', 'type Ch[T] = chan T

fn receive(ch Ch[int]) int {
	return <-ch
}

fn main() {
	ch := chan int{cap: 1}
	ch <- 42
	println(int_str(receive(ch)))
}
')
	assert out == '42'
	run_bad(v3_bin, 'bad_generic_channel_alias_element', 'type Ch[T] = chan T

fn receive(ch Ch[int]) int {
	return <-ch
}

fn main() {
	ch := chan string{cap: 1}
	_ := receive(ch)
}
',
		'cannot use `chan string`')
}

fn test_trailing_optional_parameters_are_lowered_to_none() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_omitted_optional_parameter',
		'fn consume(value ?int) int {\n\treturn value or { -1 }\n}\n\nfn main() {\n\tprintln(int_str(consume()))\n\tprintln(int_str(consume(7)))\n}\n')
	assert out == '-1\n7'
}

fn test_multi_return_arguments_must_consume_the_parameter_tail() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_variadic_multi_return_argument',
		'fn pair() (int, []int) {\n\treturn 1, [2, 3]\n}\n\nfn consume(a int, rest ...int) {}\n\nfn main() {\n\tconsume(pair())\n}\n',
		'cannot use `(int, []int)` as argument 1 to `consume`; expected `int`')
}

fn test_receiver_method_tail_multi_return_arguments_are_expanded() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'receiver_method_tail_multi_return',
		'struct Receiver {}\n\nfn pair() (int, int) {\n\treturn 2, 3\n}\n\nfn (receiver Receiver) use(a int, b int) int {\n\treturn a * 10 + b\n}\n\nfn (receiver Receiver) use_with_prefix(prefix int, a int, b int) int {\n\treturn prefix * 100 + a * 10 + b\n}\n\nfn main() {\n\tprintln(int_str(Receiver{}.use(pair())))\n\tprintln(int_str(Receiver{}.use_with_prefix(1, pair())))\n}\n')
	assert out == '23\n123'
}

fn test_power_requires_numeric_operands_or_an_overload() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_bool_power', 'fn main() {\n\t_ := true ** 2\n}\n',
		'operator `**` requires numeric operands; got `bool` and `int`')
	run_bad(v3_bin, 'bad_array_power', 'fn main() {\n\t_ := [1] ** 2\n}\n',
		'operator `**` requires numeric operands; got `[]int` and `int`')
	run_bad(v3_bin, 'bad_string_power', "fn main() {\n\t_ := 'x' ** 2\n}\n",
		'operator `**` requires numeric operands; got `string` and `int`')
	run_bad(v3_bin, 'bad_bool_power_assign', 'fn main() {\n\tmut ok := true\n\tok **= false\n}\n',
		'operator `**=` requires numeric operands; got `bool` and `bool`')
	run_bad(v3_bin, 'bad_array_power_assign',
		'fn main() {\n\tmut values := [1]\n\tvalues **= [2]\n}\n',
		'operator `**=` requires numeric operands; got `[]int` and `[]int`')
	run_bad(v3_bin, 'bad_string_power_assign',
		"fn main() {\n\tmut value := 'x'\n\tvalue **= 'y'\n}\n",
		'operator `**=` requires numeric operands; got `string` and `string`')
	out := run_good(v3_bin, 'good_overloaded_power', 'struct Exponent {
	value int
}

fn (a Exponent) ** (b Exponent) Exponent {
	return Exponent{
		value: a.value * b.value
	}
}

fn main() {
	value := Exponent{
		value: 2
	} ** Exponent{
		value: 3
	}
	println(int_str(value.value))
}
')
	assert out == '6'
}

fn test_if_expr_pointer_and_value_branches_are_incompatible() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_if_expr_pointer_value_branch',
		'struct Foo {}\n\nfn main() {\n\t_ := if true {\n\t\tFoo{}\n\t} else {\n\t\t&Foo{}\n\t}\n}\n',
		'mismatched types `Foo` and `&Foo`')
}

fn test_reject_narrowed_interface_method_parameters() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_exact_interface_method_param',
		'interface Base {\n\tbase() int\n}\n\ninterface Handler {\n\thandle(value Base) int\n}\n\nstruct Value {}\n\nfn (v Value) base() int {\n\treturn 7\n}\n\nstruct Service {}\n\nfn (s Service) handle(value Base) int {\n\treturn value.base()\n}\n\nfn main() {\n\tprintln(Handler(Service{}).handle(Base(Value{})))\n}\n')
	assert out == '7'
}

fn test_implicit_str_sum_does_not_satisfy_interface() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_implicit_str_sum_interface',
		'interface Printable {\n\tstr() string\n}\ntype Value = int | string\nfn main() {\n\t_ := Printable(Value(1))\n}\n',
		'does not implement interface')
}

fn test_implicit_str_unsupported_alias_does_not_satisfy_interface() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_implicit_str_fn_alias_interface',
		'interface Printable {\n\tstr() string\n}\ntype Callback = fn ()\nfn noop() {}\nfn main() {\n\tcb := Callback(noop)\n\t_ := Printable(cb)\n}\n',
		'cannot implement interface `Printable` using function')
}

fn test_multi_return_tail_slots_use_return_compatibility() {
	v3_bin := build_v3_review_checker()
	if_out := run_good(v3_bin, 'good_multi_return_if_pointer_value_tail',
		'struct S {\n\tn int\n}\nfn pick(ok bool) (S, int) {\n\ts := S{\n\t\tn: 5\n\t}\n\treturn if ok {\n\t\t&s\n\t\t1\n\t} else {\n\t\t&s\n\t\t2\n\t}\n}\nfn main() {\n\ta, b := pick(false)\n\tprintln(int_str(a.n) + "," + int_str(b))\n}\n')
	assert if_out == '5,2'
}

fn test_none_ierror_values_lower_to_builtin_none() {
	v3_bin := build_v3_review_checker()
	ierror_out := run_good(v3_bin, 'good_none_ierror_contexts',
		'struct Holder {\n\terr IError = none\n}\n\nfn take(e IError) int {\n\tif e is none {\n\t\treturn 1\n\t}\n\treturn 0\n}\n\nfn make() IError {\n\treturn none\n}\n\nfn main() {\n\tdefault := Holder{}\n\texplicit := Holder{\n\t\terr: none\n\t}\n\tprintln(int_str(take(none) + take(default.err) + take(explicit.err) + take(make())))\n}\n')
	assert ierror_out == '4'
	out := run_good(v3_bin, 'good_none_option_context',
		'fn maybe() ?int {\n\treturn none\n}\n\nfn main() {\n\tif maybe() == none {\n\t\tprintln("option")\n\t}\n}\n')
	assert out == 'option'
}

fn test_reject_non_optional_or_and_wrapped_string_concat() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_non_optional_literal_or_block', 'fn main() {\n\t_ := 1 or { 2 }\n}\n',
		'unexpected `or` block')
	run_bad(v3_bin, 'bad_non_optional_call_or_block',
		'fn value() int {\n\treturn 1\n}\n\nfn main() {\n\tvalue() or { panic(err) }\n}\n',
		'unexpected `or` block')
	run_bad(v3_bin, 'bad_non_optional_infix_or_block',
		'fn maybe() ?int {\n\treturn 1\n}\n\nfn main() {\n\t_ := maybe() == none or { false }\n}\n',
		'unexpected `or` block, expression of type `bool` is not an Option or a Result')
	run_bad(v3_bin, 'bad_optional_string_concat',
		"fn maybe_name() ?string {\n\treturn 'Ada'\n}\n\nfn main() {\n\t_ := 'hello ' + maybe_name()\n}\n",
		'`?string` cannot be used as `string`, unwrap the option first')
	run_bad(v3_bin, 'bad_result_string_concat',
		"fn result_name() !string {\n\treturn 'Ada'\n}\n\nfn main() {\n\t_ := result_name() + '!'\n}\n",
		'unwrapped Result cannot be used in an infix expression')
	out := run_good(v3_bin, 'good_map_or_and_unwrapped_string_concat',
		"fn maybe_name() ?string {\n\treturn 'Ada'\n}\n\nfn main() {\n\tnames := {\n\t\t'first': 'Grace'\n\t}\n\tprintln(names['first'] or { 'unknown' })\n\tprintln('hello ' + (maybe_name() or { 'unknown' }))\n}\n")
	assert out == 'Grace\nhello Ada'
}

fn test_rune_receiver_methods_resolve() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_rune_receiver_methods',
		'fn main() {\n\tr := `★`\n\tprintln(int_str(`A`.length_in_bytes()))\n\tprintln(int_str(r.bytes().len))\n\tprintln(`c`.to_upper().str())\n}\n')
	assert out == '1\n3\nC'
}

fn test_numeric_alias_returns_preserve_integer_float_direction() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_int_alias_float_variable_return',
		'type Id = int\n\nfn f(x f64) Id {\n\treturn x\n}\n\nfn main() {}\n',
		'cannot use `f64` as type `Id` in return argument')
	run_bad(v3_bin, 'bad_int_alias_float_expression_return',
		'type Id = int\n\nfn f(x f64) Id {\n\treturn x + 1.0\n}\n\nfn main() {}\n',
		'cannot use `f64` as type `Id` in return argument')
	out := run_good(v3_bin, 'good_float_alias_int_return',
		'type Amount = f64\n\nfn f() Amount {\n\treturn 1\n}\n\nfn main() {\n\tprintln(f().str())\n}\n')
	assert out == '1.0'
	explicit_out := run_good(v3_bin, 'good_explicit_float_to_int_alias_return',
		'type Id = int\n\nfn f(x f64) Id {\n\treturn Id(x)\n}\n\nfn main() {\n\tprintln(int_str(f(1.5)))\n}\n')
	assert explicit_out == '1'
}

fn test_fn_value_integer_returns_require_matching_c_abi() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_fn_value_integer_return_abi',
		'fn wide() u64 {\n\treturn 257\n}\n\nfn invoke(callback fn () u8) u8 {\n\treturn callback()\n}\n\nfn main() {\n\t_ := invoke(wide)\n}\n',
		'cannot use `fn () u64`')
	out := run_good(v3_bin, 'good_fn_value_matching_integer_return_abi',
		'fn letter() rune {\n\treturn `A`\n}\n\nfn invoke(callback fn () u32) u32 {\n\treturn callback()\n}\n\nfn main() {\n\tprintln(int_str(int(invoke(letter))))\n}\n')
	assert out == '65'
}

fn test_fn_value_aggregate_returns_require_compatible_payloads() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_fn_value_array_return_payload',
		'fn numbers() []int {\n\treturn [1, 2]\n}\n\nfn invoke(callback fn () []string) []string {\n\treturn callback()\n}\n\nfn main() {\n\t_ := invoke(numbers)\n}\n',
		'cannot use `fn () []int`')
}

fn test_alias_with_nested_type_separator_stays_alias() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_alias_nested_type_separator',
		'type Bits = [1 | 2]int\n\nfn values() Bits {\n\treturn [1, 2, 3]!\n}\n\nfn main() {\n\tbits := values()\n\tprintln(int_str(bits[0] + bits[1] + bits[2]))\n}\n')
	assert out == '6'
}

fn test_voidptr_params_reject_non_pointer_values() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_voidptr_scalar_arg', 'fn f(p voidptr) {}\n\nfn main() {\n\tf(1)\n}\n',
		'expression cannot be passed as `voidptr`')
	out := run_good(v3_bin, 'good_voidptr_pointer_arg',
		'fn f(p voidptr) int {\n\t_ = p\n\treturn 7\n}\n\nfn main() {\n\tx := 1\n\tprintln(int_str(f(&x)))\n}\n')
	assert out == '7'
}

fn test_shared_receiver_and_arg_require_shared_bindings() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_mut_receiver_immutable_value',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\ts := St{}\n\ts.bump()\n}\n',
		'`s` is immutable, declare it with `mut` to make it mutable')
	run_bad(v3_bin, 'bad_generic_mut_receiver_immutable_value',
		'struct Box[T] {\nmut:\n\tvalue T\n}\n\nfn (mut b Box[T]) set(value T) {\n\tb.value = value\n}\n\nfn main() {\n\tb := Box[int]{\n\t\tvalue: 1\n\t}\n\tb.set(2)\n}\n',
		'`b` is immutable, declare it with `mut` to make it mutable')
	run_bad(v3_bin, 'bad_mut_receiver_address_of_immutable_value',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\ts := St{}\n\t(&s).bump()\n}\n',
		'cannot pass expression as `mut`')
	immutable_pointer_out := run_good(v3_bin, 'good_mut_receiver_immutable_pointer_binding',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\tp := &s\n\tp.bump()\n\tprintln(int_str(s.value))\n}\n')
	assert immutable_pointer_out == '1'
	run_bad(v3_bin, 'bad_shared_receiver_plain_value',
		'struct St {}\n\nfn (shared s St) f() {}\n\nfn main() {\n\ts := St{}\n\ts.f()\n}\n',
		'cannot use shared method `f` as `s` is not a shared var')
	run_bad(v3_bin, 'bad_shared_arg_shadowed_local',
		'struct St {}\n\nfn take(shared s St) {}\n\nfn main() {\n\tshared s := St{}\n\tif true {\n\t\ts := St{}\n\t\ttake(s)\n\t}\n}\n',
		'parameter `s` is `shared`, so use `shared s` instead')
	run_bad(v3_bin, 'bad_explicit_shared_arg_plain_local',
		'struct St {}\n\nfn take(shared s St) {}\n\nfn main() {\n\ts := St{}\n\ttake(shared s)\n}\n',
		'cannot use non-shared `St` as argument 1')
	out := run_good(v3_bin, 'good_shared_arg_and_receiver',
		'struct St {}\n\nfn take(shared s St) int {\n\treturn 1\n}\n\nfn (shared s St) f() int {\n\treturn 2\n}\n\nfn main() {\n\tshared s := St{}\n\tprintln(int_str(take(shared s) + s.f()))\n}\n')
	assert out == '3'
	mut_out := run_good(v3_bin, 'good_mut_receiver_mutable_value',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\ts.bump()\n\tprintln(int_str(s.value))\n}\n')
	assert mut_out == '1'
	mut_address_out := run_good(v3_bin, 'good_mut_receiver_address_of_mutable_value',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\t(&s).bump()\n\tprintln(int_str(s.value))\n}\n')
	assert mut_address_out == '1'
	mut_pointer_out := run_good(v3_bin, 'good_mut_receiver_mutable_pointer_binding',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\tmut p := &s\n\tp.bump()\n\tprintln(int_str(s.value))\n}\n')
	assert mut_pointer_out == '1'
	global_pointer_out := run_good(v3_bin, 'good_mut_receiver_global_pointer',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\n__global global_st = &St{}\n\nfn main() {\n\tglobal_st.bump()\n\tprintln(int_str(global_st.value))\n}\n')
	assert global_pointer_out == '1'
	field_pointer_out := run_good(v3_bin, 'good_mut_receiver_pointer_field',
		'struct St {\nmut:\n\tvalue int\n}\n\nstruct Holder {\n\tst &St\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\tholder := Holder{\n\t\tst: &s\n\t}\n\tholder.st.bump()\n\tprintln(int_str(s.value))\n}\n')
	assert field_pointer_out == '1'
	if_guard_out := run_good(v3_bin, 'good_mut_receiver_if_guard_binding',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn get() ?St {\n\treturn St{}\n}\n\nfn main() {\n\tif mut s := get() {\n\t\ts.bump()\n\t\tprintln(int_str(s.value))\n\t}\n}\n')
	assert if_guard_out == '1'
	capture_out := run_good(v3_bin, 'good_mut_receiver_explicit_mut_capture',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\tfn [mut s] () {\n\t\ts.bump()\n\t\tprintln(int_str(s.value))\n\t}()\n}\n')
	assert capture_out == '1'
	ptr_out := run_good(v3_bin, 'good_pointer_receiver_immutable_value',
		'struct St {\n\tvalue int\n}\n\nfn (s &St) get() int {\n\treturn s.value\n}\n\nfn main() {\n\ts := St{\n\t\tvalue: 2\n\t}\n\tprintln(int_str(s.get()))\n}\n')
	assert ptr_out == '2'
}

fn test_method_receiver_rejects_extra_pointer_layers() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_value_receiver_extra_pointer_layers', 'struct S {}

fn (s S) value() int {
	return 1
}

fn main() {
	s := S{}
	p := &s
	pp := &p
	_ := pp.value()
}
',
		'cannot use receiver `&&S` as `S`')
	run_bad(v3_bin, 'bad_pointer_receiver_extra_pointer_layers', 'struct S {}

fn (s &S) value() int {
	return 1
}

fn main() {
	s := S{}
	p := &s
	pp := &p
	ppp := &pp
	_ := ppp.value()
}
',
		'cannot use receiver `&&&S` as `&S`')
	out := run_good(v3_bin, 'good_receiver_single_pointer_adjustment', 'struct S {
	value int
}

fn (s S) by_value() int {
	return s.value
}

fn (s &S) by_pointer() int {
	return s.value
}

fn main() {
	s := S{
		value: 7
	}
	p := &s
	println(int_str(p.by_value()))
	println(int_str(s.by_pointer()))
}
')
	assert out == '7\n7'
}

fn test_restrict_synthetic_hex_fallback_receivers() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_struct_hex_method', 'struct S {}\nfn main() {\n\t_ := S{}.hex()\n}\n',
		'unknown function')
	run_bad(v3_bin, 'bad_int_array_hex_method', 'fn main() {\n\t_ := [1, 2].hex()\n}\n',
		'unknown function')
	run_bad(v3_bin, 'bad_map_hex_method',
		'fn main() {\n\tm := map[string]int{}\n\t_ := m.hex()\n}\n', 'unknown function')
	run_bad(v3_bin, 'bad_float_hex_method', 'fn main() {\n\t_ := f32(1.5).hex()\n}\n',
		'unknown function')
	run_bad(v3_bin, 'bad_pointer_hex_method',
		'fn main() {\n\tx := 1\n\tp := &x\n\t_ := p.hex()\n}\n', 'unknown function')
	run_bad(v3_bin, 'bad_numeric_hex_arg',
		'fn side_effect() int {\n\treturn 1\n}\n\nfn main() {\n\t_ := u8(1).hex(side_effect())\n}\n',
		'expected 0 arguments, but got 1')
	out := run_good(v3_bin, 'supported_hex_methods',
		"fn main() {\n\tprintln(u8(15).hex())\n\tprintln(i64(255).hex())\n\tprintln([u8(1), 15, 255].hex())\n\tprintln(char(65).hex())\n\tprintln(`A`.hex())\n\tprintln('abc'.hex())\n}\n")
	assert out == '0f\nff\n010fff\n41\n41\n616263'
}

fn test_auto_str_rejects_arguments() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_auto_str_arg',
		'struct S {\n\tx int\n}\n\nfn side_effect() int {\n\treturn 1\n}\n\nfn main() {\n\t_ := S{\n\t\tx: 1\n\t}.str(side_effect())\n}\n',
		'expected 0 arguments, but got 1')
}

fn test_pointer_hex_receiver_methods_are_allowed() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_pointer_hex_receiver_method',
		"struct S {\n\tvalue int\n}\n\nfn (s &S) hex() string {\n\treturn 'ptr:' + int_str(s.value)\n}\n\nfn main() {\n\ts := S{\n\t\tvalue: 7\n\t}\n\tp := &s\n\tprintln(p.hex())\n}\n")
	assert out == 'ptr:7'
}

fn test_map_keys_and_values_reject_arguments() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_map_keys_arg',
		'fn main() {\n\tm := map[string]int{}\n\t_ := m.keys(123)\n}\n',
		'`.keys()` does not have any arguments')
	run_bad(v3_bin, 'bad_map_values_arg',
		"fn main() {\n\tm := map[string]int{}\n\t_ := m.values('x')\n}\n",
		'`.values()` does not have any arguments')
}

fn test_array_to_void_array_is_not_implicitly_compatible() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_array_to_void_array_param',
		'fn take(xs []void) {\n\t_ = xs\n}\n\nfn main() {\n\ttake([1, 2, 3])\n}\n',
		'cannot use `[]int` as argument 1 to `take`; expected `[]void`')
	run_bad(v3_bin, 'bad_array_to_void_array_user_receiver',
		'fn (xs []void) touch() int {\n\treturn xs.len\n}\n\nfn main() {\n\tnums := [1, 2, 3]\n\tprintln(nums.touch().str())\n}\n',
		'nums.touch')
	out := run_good(v3_bin, 'good_array_clone_ignores_void_array_receiver',
		'fn (xs []void) clone() int {\n\treturn 7\n}\n\nfn main() {\n\tnums := [1, 2, 3]\n\tcloned := nums.clone()\n\tprintln(int_str(cloned.len + cloned[2]))\n}\n')
	assert out == '6'
}

fn test_array_insert_and_prepend_reject_wrong_arity() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_array_prepend_missing_arg',
		'fn main() {\n\tmut a := [1, 2]\n\ta.prepend()\n}\n',
		'`array.prepend()` should have 1 argument')
	run_bad(v3_bin, 'bad_array_prepend_extra_arg',
		'fn side_effect() int {\n\treturn 3\n}\nfn main() {\n\tmut a := [1, 2]\n\ta.prepend(0, side_effect())\n}\n',
		'`array.prepend()` should have 1 argument')
	run_bad(v3_bin, 'bad_array_insert_missing_arg',
		'fn main() {\n\tmut a := [1, 2]\n\ta.insert(0)\n}\n',
		'`array.insert()` should have 2 arguments')
	run_bad(v3_bin, 'bad_array_insert_extra_arg',
		'fn side_effect() int {\n\treturn 3\n}\nfn main() {\n\tmut a := [1, 2]\n\ta.insert(0, 1, side_effect())\n}\n',
		'`array.insert()` should have 2 arguments')
	run_bad(v3_bin, 'bad_array_prepend_arg_type',
		"fn main() {\n\tmut a := [1, 2]\n\ta.prepend('x')\n}\n",
		'cannot prepend `string` to `[]int`')
	run_bad(v3_bin, 'bad_array_insert_index_type',
		"fn main() {\n\tmut a := [1, 2]\n\ta.insert('0', 3)\n}\n",
		'the first argument of `array.insert()` should be integer')
	run_bad(v3_bin, 'bad_array_insert_value_type',
		"fn main() {\n\tmut a := [1, 2]\n\ta.insert(0, 'x')\n}\n",
		'cannot use `string` as `int` in argument 2 to `array.insert()`')
	run_bad(v3_bin, 'bad_array_prepend_many_arg_type',
		"fn main() {\n\tmut a := [1, 2]\n\ta.prepend(['x'])\n}\n",
		'cannot prepend `[]string` to `[]int`')
	run_bad(v3_bin, 'bad_array_insert_many_arg_type',
		"fn main() {\n\tmut a := [1, 2]\n\ta.insert(0, ['x'])\n}\n",
		'cannot use `[]string` as `int` in argument 2 to `array.insert()`')
}

fn test_array_insert_and_prepend_accept_many_operands() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_array_insert_prepend_many_operands',
		"type Strings = []string\n\nfn main() {\n\tmut a := [3, 4]\n\ta.insert(0, [1, 2])\n\tb := [5, 6]\n\ta.insert(1, b)\n\ta.prepend([0])\n\tfixed := [7, 8]!\n\ta.insert(a.len, fixed)\n\tassert a == [0, 1, 5, 6, 2, 3, 4, 7, 8]\n\tmut strs := Strings(['hi'])\n\tstrs.insert(0, ['there'])\n\tstrs.prepend(['hello'])\n\tassert strs == ['hello', 'there', 'hi']\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_comptime_if_selected_bodies_are_checked() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_concrete_comptime_if_selected_call',
		'fn main() {\n\t$if int is int {\n\t\tmissing_selected_symbol()\n\t}\n}\n',
		'unknown function: missing_selected_symbol')
	out := run_good(v3_bin, 'good_generic_comptime_if_unselected_branch_is_not_checked',
		"fn ok() {}\n\nfn f[T]() {\n\t$if T is int {\n\t\tok()\n\t} $else {\n\t\tonly_for_other_t()\n\t}\n}\n\nfn main() {\n\tf[int]()\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_comptime_match_shadowed_const_subject_is_not_folded() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_comptime_match_parameter_shadows_const',
		"const mode = 'a'\n\nfn choose(mode string) {\n\t\$match mode {\n\t\t'a' {}\n\t\t\$else {}\n\t}\n}\n\nfn main() {}\n",
		'definition of `mode` is unknown at compile time')
	run_bad(v3_bin, 'bad_comptime_match_mutable_local_shadows_const',
		"const mode = 'a'\n\nfn main() {\n\tmut mode := 'b'\n\t\$match mode {\n\t\t'a' {}\n\t\t\$else {}\n\t}\n}\n",
		'`mode` is mut and may have changed since its definition')
	out := run_good(v3_bin, 'good_comptime_match_known_local',
		"fn main() {\n\tmode := 'b'\n\t\$match mode {\n\t\t'b' { println('ok') }\n\t\t\$else { println('bad') }\n\t}\n}\n")
	assert out == 'ok'
	type_out := run_good(v3_bin, 'good_comptime_match_generic_parameter_type',
		"fn classify[T](value T) string {\n\t\$match value {\n\t\tint { return 'int' }\n\t\t\$else { return 'other' }\n\t}\n}\n\nfn main() {\n\tprintln(classify(1))\n}\n")
	assert type_out == 'int'
}

fn test_explicit_generic_calls_use_all_type_arguments() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_multi_explicit_generic_call',
		"struct Pair[A, B] {\n\tleft  A\n\tright B\n}\n\nfn make_pair[A, B]() Pair[A, B] {\n\treturn Pair[A, B]{}\n}\n\nfn expect_pair(p Pair[int, string]) string {\n\treturn 'ok'\n}\n\nfn main() {\n\tp := make_pair[int, string]()\n\tprintln(expect_pair(p))\n}\n")
	assert out == 'ok'
	nested := run_good(v3_bin, 'good_nested_explicit_generic_call',
		"struct Pair[A, B] {\n\tleft  A\n\tright B\n}\n\nstruct Box[T] {\n\tvalue T\n}\n\nfn wrap[T]() Box[T] {\n\treturn Box[T]{}\n}\n\nfn expect_box(b Box[Pair[int, string]]) string {\n\treturn 'ok'\n}\n\nfn main() {\n\tb := wrap[Pair[int, string]]()\n\tprintln(expect_box(b))\n}\n")
	assert nested == 'ok'
	run_bad(v3_bin, 'bad_explicit_generic_too_many_type_args',
		'fn id[T](x T) T {\n\treturn x\n}\n\nfn main() {\n\t_ := id[int, string](1)\n}\n',
		'expected 1 generic parameter, got 2')
}

fn test_escaping_capturing_fn_literals_use_runtime_closures() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'return_capturing_fn_literal',
		'fn make(x int) fn () int {\n\treturn fn [x] () int {\n\t\treturn x\n\t}\n}\nfn main() {}\n')
	assert out == ''
	alias_out := run_good(v3_bin, 'return_capturing_fn_literal_alias',
		'fn make(x int) fn () int {\n\tf := fn [x] () int {\n\t\treturn x\n\t}\n\treturn f\n}\nfn main() {}\n')
	assert alias_out == ''
	field_out := run_good(v3_bin, 'struct_field_capturing_fn_literal',
		'struct Holder {\n\tcb fn () int\n}\nfn make(x int) Holder {\n\treturn Holder{\n\t\tcb: fn [x] () int {\n\t\t\treturn x\n\t\t}\n\t}\n}\nfn main() {}\n')
	assert field_out == ''
	field_alias_out := run_good(v3_bin, 'struct_field_capturing_fn_literal_alias',
		'struct Holder {\n\tcb fn () int\n}\nfn make(x int) Holder {\n\tf := fn [x] () int {\n\t\treturn x\n\t}\n\treturn Holder{\n\t\tcb: f\n\t}\n}\nfn main() {}\n')
	assert field_alias_out == ''
}

fn test_capturing_fn_literal_aliases_are_binding_scoped() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_capturing_fn_literal_inner_shadow',
		'fn plain() int {\n\treturn 3\n}\n\nfn make(x int) fn () int {\n\tcb := plain\n\tif x > 0 {\n\t\tcb := fn [x] () int {\n\t\t\treturn x\n\t\t}\n\t\t_ = cb\n\t}\n\treturn cb\n}\n\nfn main() {\n\tprintln(int_str(make(0)()))\n}\n')
	assert out == '3'
	lambda_out := run_good(v3_bin, 'good_lambda_capturing_fn_literal_shadow',
		'fn plain() int {\n\treturn 4\n}\n\nfn apply(cb fn (int) int) int {\n\treturn cb(1)\n}\n\nfn make() fn () int {\n\tcb := plain\n\t_ = apply(|n| if n > 0 {\n\t\tcb := fn [n] () int {\n\t\t\treturn n\n\t\t}\n\t\t_ = cb\n\t\tn\n\t} else {\n\t\tn\n\t})\n\treturn cb\n}\n\nfn main() {\n\tprintln(int_str(make()()))\n}\n')
	assert lambda_out == '4'
	outer_out := run_good(v3_bin, 'outer_capturing_alias_survives_inner_shadow',
		'fn make(x int) fn () int {\n\tcb := fn [x] () int {\n\t\treturn x\n\t}\n\tif x > 0 {\n\t\tcb := fn [x] () int {\n\t\t\treturn x + 1\n\t\t}\n\t\t_ = cb\n\t}\n\treturn cb\n}\nfn main() {}\n')
	assert outer_out == ''
}

fn test_reject_unsmartcasted_unique_sum_variant_field() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_unsmartcasted_unique_sum_field',
		'struct A {\n\tonly_on_a int\n}\nstruct B {}\ntype K = A | B\nfn main() {\n\tk := K(B{})\n\t_ := k.only_on_a\n}\n',
		'field `only_on_a` does not exist or have the same type in these sumtype `K` variants')
	out := run_good(v3_bin, 'good_smartcasted_unique_sum_field',
		'struct A {\n\tonly_on_a int\n}\nstruct B {}\ntype K = A | B\nfn main() {\n\tk := K(A{\n\t\tonly_on_a: 7\n\t})\n\tif k is A {\n\t\tprintln(int_str(k.only_on_a))\n\t}\n}\n')
	assert out == '7'
}

fn test_smartcasted_fn_sum_call_keeps_active_variant_arity() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_smartcasted_fn_sum_variant_arity', 'type Callback = fn () int | fn (int) int

fn no_args() int {
	return 7
}

fn invoke(cb Callback) {
	if cb is fn () int {
		_ := cb(1)
	}
}

fn main() {
	invoke(Callback(no_args))
}
',
		'expected 0 arguments, but got 1')
	out := run_good(v3_bin, 'good_smartcasted_fn_sum_variant_arity', 'type Callback = fn () int | fn (int) int

fn with_arg(value int) int {
	return value
}

fn invoke(cb Callback) int {
	if cb is fn (int) int {
		return cb(7)
	}
	return 0
}

fn main() {
	println(int_str(invoke(Callback(with_arg))))
}
')
	assert out == '7'
}

fn test_called_generic_functions_report_missing_return() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_called_generic_missing_return',
		'fn f[T]() int {\n}\nfn main() {\n\t_ := f[int]()\n}\n',
		'missing return at end of function `f`')
	run_bad(v3_bin, 'bad_generic_comptime_branch_missing_return',
		'fn f[T]() int {\n\t$if T is int {\n\t\treturn 1\n\t}\n}\nfn main() {\n\t_ := f[string]()\n}\n',
		'missing return at end of function `f`')
}

fn test_no_return_calls_satisfy_return_analysis() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_panic_satisfies_return_analysis',
		"fn choose(ok bool) string {\n\tif ok {\n\t\treturn 'ok'\n\t}\n\tpanic('unreachable')\n}\n\nfn pick(ok bool) int {\n\tif ok {\n\t\treturn 7\n\t}\n\treturn panic('unreachable')\n}\n\nfn main() {\n\tprintln(choose(true))\n\tprintln(int_str(pick(true)))\n}\n")
	assert out == 'ok\n7'
}

fn test_assignment_or_fallback_with_exhaustive_nested_match_returns_matches_v1() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'assignment_or_nested_match_returns', 'struct NestedFallbackAnswer {
	values []string
}

fn nested_fallback_source() !string {
	return error("dial failed")
}

fn nested_fallback_answer() NestedFallbackAnswer {
	response := nested_fallback_source() or {
		match err.msg() {
			"dial failed" {
				return NestedFallbackAnswer{
					values: ["handled"]
				}
			}
			else {
				return NestedFallbackAnswer{
					values: [err.msg()]
				}
			}
		}
	}
	println(response)
}

fn main() {
	answer := nested_fallback_answer()
	println(answer.values[0])
}
')
	assert out == 'handled'
}

fn test_builtin_panic_auto_stringifies_nested_generic_struct_during_static_checks() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'panic_nested_generic_auto_str', 'struct StaticPanicNode[T] {
	value T
}

struct StaticPanicContainer[T] {
	values []StaticPanicNode[T]
}

fn maybe_panic_nested_generic(should_panic bool) {
	if should_panic {
		panic(StaticPanicContainer[string]{})
	}
}

fn main() {
	maybe_panic_nested_generic(false)
	println("ok")
}
')
	assert out == 'ok'
}

fn test_parenthesized_no_return_return_uses_cgen_fallback() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_parenthesized_panic_satisfies_return_codegen',
		"fn f(ok bool) int {\n\tif ok {\n\t\treturn 9\n\t}\n\treturn (panic('x'))\n}\nfn main() {\n\tprintln(int_str(f(true)))\n}\n")
	assert out == '9'
}

fn test_return_panic_with_defer_evaluates_call_before_cleanup() {
	v3_bin := build_v3_review_checker()
	out := run_runtime_bad(v3_bin, 'bad_return_panic_defer_order',
		"fn f() int {\n\tdefer {\n\t\tprintln('cleanup-ran')\n\t}\n\treturn panic('boom')\n}\nfn main() {\n\t_ := f()\n}\n")
	assert out.contains('boom')
	assert !out.contains('cleanup-ran')
}

fn test_declared_c_exit_satisfies_return_analysis() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_declared_c_exit_satisfies_return_analysis',
		'fn C.exit(code int)\nfn f(ok bool) int {\n\tif ok {\n\t\treturn 7\n\t}\n\treturn C.exit(1)\n}\nfn main() {\n\tprintln(int_str(f(true)))\n}\n')
	assert out == '7'
}

fn test_no_return_analysis_requires_resolved_builtin_target() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_shadowed_os_exit_missing_return',
		'struct OsLike {}\nfn (x OsLike) exit() {}\nfn f(os OsLike) int {\n\tos.exit()\n}\nfn main() {}\n',
		'missing return at end of function `f`')
	run_bad(v3_bin, 'bad_local_os_exit_missing_return',
		'struct OsLike {}\nfn (x OsLike) exit() {}\nfn f() int {\n\tos := OsLike{}\n\tos.exit()\n}\nfn main() {}\n',
		'missing return at end of function `f`')
}

fn test_no_return_analysis_rejects_shadowed_builtin_fn_values() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_shadowed_exit_fn_value_missing_return',
		'fn f() int {\n\texit := fn () int { return 1 }\n\texit()\n}\nfn main() {}\n',
		'missing return at end of function `f`')
	run_bad(v3_bin, 'bad_shadowed_panic_fn_value_missing_return',
		'fn f() int {\n\tpanic := fn () int { return 1 }\n\tpanic()\n}\nfn main() {}\n',
		'missing return at end of function `f`')
	run_bad(v3_bin, 'bad_nested_shadowed_exit_fn_value_missing_return',
		'fn f() int {\n\t{\n\t\texit := fn () int { return 1 }\n\t\texit()\n\t}\n}\nfn main() {}\n',
		'missing return at end of function `f`')
	run_bad(v3_bin, 'bad_shadowed_exit_multi_return_branch',
		'fn main() {\n\texit := fn () int { return 1 }\n\tflag := true\n\ta, b := if flag {\n\t\t1\n\t\t2\n\t} else {\n\t\texit()\n\t}\n\tprintln(int_str(a + b))\n}\n',
		'multi-return assignment mismatch')
	run_bad(v3_bin, 'bad_nested_shadowed_exit_multi_return_branch',
		'fn main() {\n\tflag := true\n\ta, b := if flag {\n\t\t1\n\t\t2\n\t} else {\n\t\texit := fn () int { return 1 }\n\t\texit()\n\t}\n\tprintln(int_str(a + b))\n}\n',
		'multi-return assignment mismatch')
}

fn test_returning_receiver_method_named_exit_keeps_value() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_receiver_exit_return_value',
		'struct Plugin {}\nfn (p Plugin) exit() int {\n\treturn 9\n}\nfn f(plugin Plugin) int {\n\treturn plugin.exit()\n}\nfn main() {\n\tprintln(int_str(f(Plugin{})))\n}\n')
	assert out == '9'
}

fn test_imported_module_name_shadowed_by_receiver_for_no_return_analysis() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_shadowed_import_os_exit_missing_return',
		'import os\nstruct OsLike {}\nfn (x OsLike) exit(code int) {}\nfn f(os OsLike) int {\n\tos.exit(0)\n}\nfn main() {}\n',
		'missing return at end of function `f`')
}

fn test_import_symbol_parameter_conflict_is_rejected() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_shadowed_os_exit_return_value',
		'import os\nstruct OsLike {}\nfn (x OsLike) exit(code int) int {\n\treturn code + 1\n}\nfn f(os OsLike) int {\n\treturn os.exit(4)\n}\nfn main() {\n\tprintln(int_str(f(OsLike{})))\n}\n',
		'duplicate of an import symbol `os`')
}

fn test_no_return_fixed_array_return_uses_abi_wrapper() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_fixed_array_return_panic_abi_wrapper',
		"import os\nfn f() [3]int {\n\treturn panic('x')\n}\nfn main() {\n\tif os.args.len == -1 {\n\t\tarr := f()\n\t\tprintln(int_str(arr[0]))\n\t}\n}\n")
	assert out == ''
}

fn test_no_return_fn_return_uses_abi_typedef() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_fn_return_panic_abi_typedef',
		"import os\nfn f() fn () int {\n\treturn panic('x')\n}\nfn main() {\n\tif os.args.len == -1 {\n\t\tcb := f()\n\t\tprintln(int_str(cb()))\n\t}\n}\n")
	assert out == ''
}

fn test_local_identifiers_shadow_module_consts() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_const_shadowed_by_param_and_local',
		"const shadowed_value = 'const'\n\nfn param_shadow(shadowed_value int) int {\n\treturn shadowed_value + 1\n}\n\nfn local_shadow() int {\n\tshadowed_value := 2\n\treturn shadowed_value + 1\n}\n\nfn main() {\n\tprintln(int_str(param_shadow(1)))\n\tprintln(int_str(local_shadow()))\n\tprintln(shadowed_value)\n}\n")
	assert out == '2\n3\nconst'
}

fn test_match_const_int_does_not_narrow_subject_type() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'match_const_int_no_subject_narrow',
		'const size_224 = 28\n\nstruct E {\n\tsize int\n}\n\nfn check(hash_size int) !E {\n\tmatch hash_size {\n\t\tsize_224 {\n\t\t\treturn E{\n\t\t\t\tsize: hash_size\n\t\t\t}\n\t\t}\n\t\telse {}\n\t}\n\treturn E{\n\t\tsize: 0\n\t}\n}\n\nfn main() {\n\tprintln(int_str(check(28)!.size))\n}\n')
	assert out == '28'
}

fn test_builtin_function_callee_wins_over_unrelated_const_suffix() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'builtin_fn_unrelated_const_suffix',
		'import math\nfn main() {\n\t_ = math.pi\n\tprintln(f32(1).eq_epsilon(f32(1)))\n}\n')
	assert out == 'true'
}

fn test_fn_field_param_mutability_survives_type_identity_paths() {
	v3_bin := build_v3_review_checker()
	source_prefix := 'struct Counter {
mut:
	value int
}

struct Holder {
	callback fn (mut Counter)
}

fn increment(mut counter Counter) {
	counter.value++
}
'
	out := run_good(v3_bin, 'good_fn_field_mut_param', source_prefix +
		'
fn main() {
	holder := Holder{
		callback: increment
	}
	mut counter := Counter{
		value: 4
	}
	holder.callback(mut counter)
	println(int_str(counter.value))
}
')
	assert out == '5'
	run_bad(v3_bin, 'bad_fn_field_missing_mut_arg', source_prefix +
		'
fn main() {
	holder := Holder{
		callback: increment
	}
	mut counter := Counter{
		value: 4
	}
	holder.callback(counter)
}
',
		'is `mut`, so use `mut counter` instead')
}

fn test_mutable_array_field_copy_requires_clone() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_mutable_array_field_copy', 'struct Holder {
	items []int
}

fn main() {
	holder := Holder{
		items: [1]
	}
	mut copy := holder.items
	copy << 2
}
',
		'use `mut array2 := array1.clone()` instead of `mut array2 := array1` (or use `unsafe`)')
	out := run_good(v3_bin, 'good_mutable_array_field_clone', 'struct Holder {
	items []int
}

fn main() {
	holder := Holder{
		items: [1]
	}
	mut copy := holder.items.clone()
	copy << 2
	println(int_str(copy.len))
	println(int_str(holder.items.len))
}
')
	assert out == '2\n1'
}

fn test_mutable_map_clone_is_fresh() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_mutable_map_clone', "fn clone_and_extend(source map[string]bool) int {
	mut cloned := source.clone()
	cloned['new'] = true
	return cloned.len
}

fn main() {
	source := {
		'old': true
	}
	println(int_str(clone_and_extend(source)))
	println(int_str(source.len))
}
")
	assert out == '2\n1'
}

fn test_pr_review_parser_and_checker_safety_batch() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_shared_parameter_missing_marker', 'struct State {}

fn take(shared state State) {}

fn main() {
	shared state := State{}
	take(state)
}
',
		'function `take` parameter `state` is `shared`, so use `shared state` instead')
	run_bad(v3_bin, 'bad_shared_parameter_forwarded_unlocked', 'struct State {
	value int
}

fn read(state State) int {
	return state.value
}

fn forward(shared state State) int {
	return read(state)
}

fn main() {}
',
		'`state` is `shared` and must be `rlock`ed or `lock`ed to be passed as non-mut argument')
	run_bad(v3_bin, 'bad_interface_option_alias_return', 'type MyInt = int

interface Provider {
	value() ?MyInt
}

struct IntProvider {}

fn (_ IntProvider) value() ?int {
	return 1
}

fn main() {
	_ := Provider(IntProvider{})
}
',
		'expected return type `?MyInt`')
	run_bad(v3_bin, 'bad_interface_result_alias_return', 'type MyInt = int

interface Provider {
	value() !MyInt
}

struct IntProvider {}

fn (_ IntProvider) value() !int {
	return 1
}

fn main() {
	_ := Provider(IntProvider{})
}
',
		'expected return type `!MyInt`')
	run_bad(v3_bin, 'bad_operator_if_attribute_return', 'struct Value {
	n int
}

@[if debug]
fn (a Value) +(b Value) Value {
	return Value{
		n: a.n + b.n
	}
}

fn main() {}
',
		'only functions that do NOT return values can have `@[if debug]` tags')
	run_bad(v3_bin, 'bad_pointer_cast_to_map_alias', 'type FooMap = map[string]int

fn main() {
	_ := &FooMap(map[string]int{})
}
',
		'cannot cast to alias pointer `&FooMap` because `map[string]int` is a value')
}

fn test_vexeroot_insert_and_generated_function_name_exemptions() {
	v3_bin := build_v3_review_checker()
	insert_out := run_good(v3_bin, 'good_vexeroot_insert_outside_vlib', '#insert "@VEXEROOT/vlib/v3/tests/testdata/vexeroot_insert.h"

fn C.v3_vexeroot_insert_value() int

fn main() {
	println(int_str(C.v3_vexeroot_insert_value()))
}
')
	assert insert_out == '37'
	run_bad(v3_bin, 'bad_source_function_name_with_generic_marker', 'fn bad_T_Name() {}

fn main() {}
',
		'function name `bad_T_Name` cannot contain uppercase letters, use snake_case instead')
	generic_out := run_good(v3_bin, 'good_generated_generic_function_name', 'fn identity[T](value T) T {
	return value
}

fn main() {
	println(int_str(identity[int](7)))
}
')
	assert generic_out == '7'
}

fn test_forwarded_variadic_arrays_must_be_final() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_forwarded_variadic_function_trailing_arg', 'fn take(values ...int) {}

fn forward(values ...int) {
	take(...values, 2)
}

fn main() {}
',
		'when forwarding a variadic variable, it must be the final argument')
	run_bad(v3_bin, 'bad_forwarded_variadic_method_trailing_arg', 'struct Receiver {}

fn (receiver Receiver) take(values ...int) {}

fn (receiver Receiver) forward(values ...int) {
	receiver.take(...values, 2)
}

fn main() {}
',
		'when forwarding a variadic variable, it must be the final argument')
}

fn test_mut_pointer_local_after_mut_pointer_loop_keeps_pointer_type() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'mut_pointer_local_after_mut_pointer_loop', 'struct Item {
mut:
	value int
}

fn missing_item() &Item {
	return &Item(unsafe { nil })
}

fn main() {
	mut items := []&Item{}
	for mut item in items {
		item.value++
	}
	mut item := missing_item()
	if item == unsafe { nil } {
		println("nil")
	}
}
')
	assert out == 'nil'
}

fn test_pointer_to_sum_supports_type_pattern_membership() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'pointer_sum_type_pattern_membership', 'struct First {}

struct Second {}

type Node = First | Second

fn accepts(node &Node) bool {
	return node in [First, Second]
}

fn main() {
	mut node := Node(First{})
	println(accepts(&node))
}
')
	assert out == 'true'
}

fn test_explicit_mut_reference_parameter_uses_pointee_type_in_body() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'explicit_mut_reference_parameter', 'fn stop(mut running &bool) {
	running = false
}

fn set_second(mut values &f32) {
	values[1] = 2.5
}

fn main() {
	mut running := true
	stop(mut running)
	println(running)
	mut pointed := true
	mut pointer := &pointed
	stop(mut pointer)
	println(pointed)
	mut values := [f32(0), 0]
	mut values_pointer := unsafe { &values[0] }
	set_second(mut values_pointer)
	println(values[1])
}
')
	assert out == 'false
false
2.5'
}

fn test_smartcast_receiver_methods_keep_cast_paths_and_declared_sum_methods() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'smartcast_receiver_method_paths', 'struct Leaf {}

struct Container {
	left Expr
}

struct Assignment {
mut:
	left []Expr
}

struct EmptyStmt {}

type Expr = Container | Leaf
type Stmt = Assignment | EmptyStmt

struct Branch {
mut:
	stmt Stmt
}

fn (expr Expr) pos() int {
	return 7
}

fn (leaf &Leaf) is_mut() bool {
	return true
}

fn main() {
	container_expr := Expr(Container{
		left: Expr(Leaf{})
	})
	if container_expr is Container && (container_expr as Container).left is Leaf {
		println((container_expr as Container).left.is_mut())
	}
	mut branch := Branch{
		stmt: Stmt(Assignment{
			left: [Expr(Leaf{})]
		})
	}
	match mut branch.stmt {
		Assignment {
			if mut branch.stmt.left[0] is Leaf {
				println(branch.stmt.left[0].pos())
			}
		}
		else {}
	}
}
')
	assert out == 'true
7'
}

fn test_as_cast_value_can_bind_to_immutable_reference_parameter() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'as_cast_implicit_reference', 'struct First {
	value int
}

struct Second {}

type Value = First | Second

fn inspect(value &First) int {
	return value.value
}

fn main() {
	value := Value(First{
		value: 7
	})
	println(inspect(value as First))
}
')
	assert out == '7'
}

fn test_fresh_pointer_result_does_not_alias_immutable_pointer_argument() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'fresh_pointer_not_immutable_alias', 'struct State {
mut:
	values []int
}

fn fresh(seed &int) &State {
	return &State{
		values: [*seed]
	}
}

fn main() {
	seed := 1
	mut state := fresh(&seed)
	for i, value in state.values {
		state.values[i] = value + 1
	}
	println(state.values[0])
}
')
	assert out == '2'
}

fn test_forwarded_pointer_result_preserves_immutable_alias() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'forwarded_pointer_preserves_immutable_alias', 'struct State {
mut:
	values []int
}

fn identity(state &State) &State {
	return state
}

fn forward(state &State) &State {
	return identity(state)
}

fn main() {
	state := State{
		values: [1]
	}
	mut alias := forward(state)
	alias.values[0] = 2
}
',
		'aliases mutable data from an immutable value')
}
