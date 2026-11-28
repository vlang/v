import json2
import os
import v.cmdexec
import v.tempname

const v3_bin = @VEXE
const review_checker_fixture_dir = os.join_path(os.vtmp_dir(), 'v3_review_checker_${tempname.unique_token()}')

fn testsuite_begin() {
	os.mkdir(review_checker_fixture_dir) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(review_checker_fixture_dir) or {}
}

fn run_review_checker(compiler string, args []string) os.Result {
	return cmdexec.run_in_merged_with_env(compiler, args, '', {
		'VFLAGS': ''
	})
}

fn run_bad(compiler string, name string, src string, expected string) {
	bad_src := os.join_path(review_checker_fixture_dir, 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(review_checker_fixture_dir, 'v3_${name}')
	result := run_review_checker(compiler, ['${bad_src}', '-b', 'c', '-o', bad_bin])
	assert result.exit_code != 0, '${name}: expected failure, got success\n${result.output}'
	assert result.output.contains(expected), '${name}: expected `${expected}` in\n${result.output}'
	assert !result.output.contains('C compilation failed'), '${name}: reached C compilation\n${result.output}'
}

struct ReviewFixture {
	name     string
	source   string
	expected string
}

struct ReviewDiagnostic {
	file     string
	severity string
	message  string
}

fn write_review_batch(name string, cases []ReviewFixture, runnable bool) string {
	root := os.join_path(review_checker_fixture_dir, name)
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for case in cases {
		dir := os.join_path(root, case.name)
		os.mkdir_all(dir) or { panic(err) }
		mut source := 'module ${case.name}\n\n' + case.source
		if runnable {
			imports << 'import ${case.name}'
			calls << "\t\t'${case.name}' { ${case.name}.exercise() }"
			source = source.replace_once('\nfn main()', '\npub fn exercise()')
		} else {
			imports << 'import ${case.name} as _'
		}
		os.write_file(os.join_path(dir, '${case.name}.v'), source) or {
			panic(err)
		}
	}
	import_text := imports.join('\n')
	main := if runnable {
		"module main\n\nimport os\n${import_text}\n\nfn main() {\n\tmatch os.args[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n"
	} else {
		'module main\n\n${import_text}\n\nfn main() {}\n'
	}
	os.write_file(os.join_path(root, 'main.v'), main) or { panic(err) }
	return root
}

fn check_review_rejections(name string, cases []ReviewFixture) {
	root := write_review_batch(name, cases, false)
	result := run_review_checker(v3_bin, [root, '-json-errors', '-b', 'c', '-o',
		os.join_path(root, 'program')])
	assert result.exit_code != 0, result.output
	assert !result.output.contains('C compilation failed'), result.output
	project_prefix := os.file_name(root) + '.'
	mut errors := map[string][]string{}
	for line in result.output.split_into_lines() {
		if !line.starts_with('{') {
			continue
		}
		diagnostic := json2.decode[ReviewDiagnostic](line) or { panic(err) }
		if diagnostic.severity == 'error' {
			errors[os.file_name(diagnostic.file)] << diagnostic.message.replace(project_prefix, '')
		}
	}
	for case in cases {
		assert errors['${case.name}.v'].any(it.contains(case.expected)), '${case.name}: expected ${case.expected}\n${result.output}'
	}
}

fn compile_review_runtime(root string) string {
	bin_path := os.join_path(root, 'program')
	compile := run_review_checker(v3_bin, ['-enable-globals', root, '-b', 'c', '-o', bin_path])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	return bin_path
}

fn check_review_runtime(name string, cases []ReviewFixture) {
	root := write_review_batch(name, cases, true)
	bin_path := compile_review_runtime(root)
	for case in cases {
		result := os.exec([bin_path, case.name])
		assert result.exit_code == 0, '${case.name}: ${result.output}'
		assert result.output.trim_space() == case.expected, '${case.name}: expected ${case.expected}\n${result.output}'
	}
}

fn run_good_project(compiler string, name string, files map[string]string) string {
	root := os.join_path(review_checker_fixture_dir, 'v3_${name}_project')
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
	compile := run_review_checker(compiler, ['-nocache', '-b', 'c', '-o', bin_path,
		os.join_path(root, 'main.v')])
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	run := os.exec([bin_path])
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn run_runtime_bad(compiler string, name string, src string) string {
	bad_src := os.join_path(review_checker_fixture_dir, 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(review_checker_fixture_dir, 'v3_${name}')
	compile := run_review_checker(compiler, ['${bad_src}', '-b', 'c', '-o', bad_bin])
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.exec([bad_bin])
	assert run.exit_code != 0, '${name}: expected runtime failure, got success\n${run.output}'
	return run.output.trim_space()
}

fn test_sum_option_and_result_value_contexts() {
	check_review_runtime('sum_option_and_result_value_contexts', [
		ReviewFixture{'explicit_mut_sum_smartcasts', 'struct First {
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
', '8
12'},
		ReviewFixture{'as_cast_implicit_reference', 'struct First {
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
', '7'},
		ReviewFixture{'good_smartcasted_unique_sum_field', 'struct A {\n\tonly_on_a int\n}\nstruct B {}\ntype Kind = A | B\nfn main() {\n\tk := Kind(A{\n\t\tonly_on_a: 7\n\t})\n\tif k is A {\n\t\tprintln(int_str(k.only_on_a))\n\t}\n}\n', '7'},
		ReviewFixture{'good_smartcasted_fn_sum_variant_arity', 'type Callback = fn () int | fn (int) int

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
', '7'},
		ReviewFixture{'smartcast_receiver_method_paths', 'struct Leaf {}

struct Container {
	left &Expr
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
		left: &Expr(Leaf{})
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
', 'true
7'},
		ReviewFixture{'pointer_sum_type_pattern_membership', 'struct First {}

struct Second {}

type Node = First | Second

fn accepts(node &Node) bool {
	return node in [First, Second]
}

fn main() {
	mut node := Node(First{})
	println(accepts(&node))
}
', 'true'},
		ReviewFixture{'canonical_option_void_return', 'fn work() ? {
	return none
}

fn main() {
	work() or {
		println("none")
		return
	}
}
', 'none'},
		ReviewFixture{'good_unsafe_none_option_default', 'struct Holder {
	value ?int = unsafe { none }
}

fn main() {
	holder := Holder{}
	println((holder.value == none).str())
}
', 'true'},
		ReviewFixture{'good_none_ierror_contexts', 'struct Holder {\n\terr IError = none\n}\n\nfn take(e IError) int {\n\tif e is none {\n\t\treturn 1\n\t}\n\treturn 0\n}\n\nfn make() IError {\n\treturn none\n}\n\nfn main() {\n\tdefault := Holder{}\n\texplicit := Holder{\n\t\terr: none\n\t}\n\tprintln(int_str(take(none) + take(default.err) + take(explicit.err) + take(make())))\n}\n', '4'},
		ReviewFixture{'good_none_option_context', 'fn maybe() ?int {\n\treturn none\n}\n\nfn main() {\n\tif maybe() == none {\n\t\tprintln("option")\n\t}\n}\n', 'option'},
		ReviewFixture{'good_omitted_optional_parameter', 'fn consume(value ?int) int {\n\treturn value or { -1 }\n}\n\nfn main() {\n\tprintln(int_str(consume()))\n\tprintln(int_str(consume(7)))\n}\n', '-1\n7'},
		ReviewFixture{'good_constant_error_branch_in_result_return', "fn make_result() !int {\n\treturn if true { error('bad') } else { 1 }\n}\n\nfn main() {\n\t_ := make_result() or {\n\t\tprintln(err.msg())\n\t\treturn\n\t}\n}\n", 'bad'},
		ReviewFixture{'good_error_branch_in_result_return', "fn make_result(ok bool) !int {\n\treturn if ok { error('bad') } else { 1 }\n}\n\nfn main() {\n\tprintln(int_str(make_result(false) or { -1 }))\n\tprintln(int_str(make_result(true) or { -1 }))\n}\n", '1\n-1'},
		ReviewFixture{'good_map_or_and_unwrapped_string_concat', "fn maybe_name() ?string {\n\treturn 'Ada'\n}\n\nfn main() {\n\tnames := {\n\t\t'first': 'Grace'\n\t}\n\tprintln(names['first'] or { 'unknown' })\n\tprintln('hello ' + (maybe_name() or { 'unknown' }))\n}\n", 'Grace\nhello Ada'},
	])
}

fn test_sum_smartcast_and_interface_checker_rejections() {
	check_review_rejections('sum_smartcast_and_interface_checker_rejections', [
		ReviewFixture{'forwarded_mut_sum_argument_invalidates_smartcast', 'struct First {
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
', 'field `value` does not exist'},
		ReviewFixture{'forwarded_mut_sum_receiver_invalidates_smartcast', 'struct First {
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
', 'field `value` does not exist'},
		ReviewFixture{'bad_unsmartcasted_unique_sum_field', 'struct A {\n\tonly_on_a int\n}\nstruct B {}\ntype Kind = A | B\nfn main() {\n\tk := Kind(B{})\n\t_ := k.only_on_a\n}\n', 'field `only_on_a` does not exist or have the same type in these sumtype `bad_unsmartcasted_unique_sum_field.Kind` variants'},
		ReviewFixture{'bad_concrete_comptime_if_selected_call', 'fn main() {\n\t$if int is int {\n\t\tmissing_selected_symbol()\n\t}\n}\n', 'unknown function: missing_selected_symbol'},
		ReviewFixture{'bad_comptime_match_parameter_shadows_const', "const mode = 'a'\n\nfn choose(mode string) {\n\t\$match mode {\n\t\t'a' {}\n\t\t\$else {}\n\t}\n}\n\nfn main() {}\n", 'definition of `mode` is unknown at compile time'},
		ReviewFixture{'bad_comptime_match_mutable_local_shadows_const', "const mode = 'a'\n\nfn main() {\n\tmut mode := 'b'\n\t\$match mode {\n\t\t'a' {}\n\t\t\$else {}\n\t}\n}\n", '`mode` is mut and may have changed since its definition'},
		ReviewFixture{'bad_shared_parameter_missing_marker', 'struct State {}

fn take(shared state State) {}

fn main() {
	shared state := State{}
	take(state)
}
', 'function `take` parameter `state` is `shared`, so use `shared state` instead'},
		ReviewFixture{'bad_shared_parameter_forwarded_unlocked', 'struct State {
	value int
}

fn read(state State) int {
	return state.value
}

fn forward(shared state State) int {
	return read(state)
}

fn main() {}
', '`state` is `shared` and must be `rlock`ed or `lock`ed to be passed as non-mut argument'},
		ReviewFixture{'bad_interface_option_alias_return', 'type MyInt = int

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
', 'expected return type `?bad_interface_option_alias_return.MyInt`'},
		ReviewFixture{'bad_interface_result_alias_return', 'type MyInt = int

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
', 'expected return type `!bad_interface_result_alias_return.MyInt`'},
		ReviewFixture{'bad_operator_if_attribute_return', 'struct Value {
	n int
}

@[if debug]
fn (a Value) +(b Value) Value {
	return Value{
		n: a.n + b.n
	}
}

fn main() {}
', 'only functions that do NOT return values can have `@[if debug]` tags'},
		ReviewFixture{'bad_pointer_cast_to_map_alias', 'type FooMap = map[string]int

fn main() {
	_ := &FooMap(map[string]int{})
}
', 'cannot cast to alias pointer `&FooMap` because `map[string]int` is a value'},
		ReviewFixture{'bad_smartcasted_fn_sum_variant_arity', 'type Callback = fn () int | fn (int) int

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
', 'expected 0 arguments, but got 1'},
	])
}

fn test_reference_pointer_and_mutable_loop_bindings() {
	check_review_runtime('reference_pointer_and_mutable_loop_bindings', [
		ReviewFixture{'mut_index_alias', 'struct Item {
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
', '1'},
		ReviewFixture{'mut_pointer_local_after_mut_pointer_loop', 'struct Item {
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
', 'nil'},
		ReviewFixture{'unsafe_pointer_write', 'fn main() {
	mut value := 0
	p := &value
	unsafe {
		*p = 1
		(*p) = 2
	}
	println(value)
}
', '2'},
		ReviewFixture{'fresh_pointer_not_immutable_alias', 'struct State {
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
', '2'},
		ReviewFixture{'immutable_reference_and_previous_enum_value', '@[_allow_multiple_values]
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
', '3'},
		ReviewFixture{'parenthesized_reference_keeps_pointer_binding_mutable', 'fn main() {
	n := 1
	mut p := (&n)
	unsafe {
		*p = 10
	}
	println(*p)
}
', '10'},
		ReviewFixture{'good_alias_nested_type_separator', 'type Bits = [1 | 2]int\n\nfn values() Bits {\n\treturn [1, 2, 3]!\n}\n\nfn main() {\n\tbits := values()\n\tprintln(int_str(bits[0] + bits[1] + bits[2]))\n}\n', '6'},
		ReviewFixture{'generic_channel_alias_element', 'type Ch[T] = chan T

fn receive(ch Ch[int]) int {
	return <-ch
}

fn main() {
	ch := chan int{cap: 1}
	ch <- 42
	println(int_str(receive(ch)))
}
', '42'},
	])
}

fn test_generic_factory_can_initialize_its_own_noinit_struct_for_external_callers() {
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
	run_bad(v3_bin, 'result_channel_element', 'fn main() {\n\t_ := chan !int{}\n}\n',
		'cannot use chan with Result type')
}

fn test_method_value_option_alias_and_const_names_match_v1() {
	embedded_source := 'struct Base {
	id int
}

fn (b &Base) ref() int {
	return b.id
}

struct Plain {
	Base
}
'
	check_review_rejections('declaration_receiver_and_reference_rejections', [
		ReviewFixture{'immutable_reference_in_mut_field', 'struct Holder {
mut:
	ptr &int
}

fn main() {
	value := 1
	mut holder := Holder{
		ptr: &value
	}
	println(holder)
}
', '`value` is immutable, cannot have a mutable reference to an immutable object'},
		ReviewFixture{'result_struct_field', 'struct Holder {
	value !int
}

fn main() {}
', 'struct field does not support storing Result'},
		ReviewFixture{'mutable_primitive_parameter', 'fn update(mut value int) {
	value++
}

fn main() {}
', 'mutable arguments are only allowed for arrays, interfaces, maps, pointers, structs or their aliases'},
		ReviewFixture{'parameter_shadows_import', 'import arrays

fn update(arrays int) {
	_ = arrays
}

fn main() {}
', 'duplicate of an import symbol `arrays`'},
		ReviewFixture{'mutable_interface_smartcast', 'interface Value {}

struct Concrete {}

fn main() {
	mut value := Value(Concrete{})
	if value is Concrete {}
}
', 'smart casting a mutable interface value requires `if mut value is ...`'},
		ReviewFixture{'explicit_option_void_return', 'fn work() ?void {
	return none
}

fn main() {}
', 'use `?` instead of `?void`'},
		ReviewFixture{'mutable_reference_to_immutable_local_declaration', 'struct Foo {
mut:
	value int
}

fn main() {
	f := Foo{}
	mut pf := &f
	pf.value = 1
}
', '`f` is immutable, cannot have a mutable reference to it'},
		ReviewFixture{'mutable_reference_to_immutable_local_reassignment', 'struct Foo {
mut:
	value int
}

fn main() {
	f := Foo{}
	mut other := Foo{}
	mut pf := &other
	pf = &f
}
', '`f` is immutable, cannot have a mutable reference to it'},
		ReviewFixture{'duplicate_value_enum_forward_reference', '@[_allow_multiple_values]
enum ForwardValue {
	a = .c
	c = 2
}

fn main() {}
', '`ForwardValue.c` should be declared before using it'},
		ReviewFixture{'immutable_struct_field', 'struct S {\n\tread_only int\nmut:\n\twritable int\n}\n\nfn main() {\n\tmut s := S{}\n\ts.read_only = 1\n}\n', 'field `read_only` of struct `immutable_struct_field.S` is immutable'},
		ReviewFixture{'non_heap_pointer_receiver_method_value', 'struct Foo {}

fn (foo &Foo) ref() int {
	return 1
}

fn make() fn () int {
	foo := Foo{}
	return foo.ref
}

fn main() {}
', 'method `Foo.ref` cannot be used as a variable outside `unsafe` blocks'},
		ReviewFixture{'stack_pointer_alias_receiver_method_value', 'struct Foo {}

fn (foo &Foo) ref() int {
	return 1
}

fn make() fn () int {
	foo := Foo{}
	p := &foo
	return p.ref
}

fn main() {}
', 'method `Foo.ref` cannot be used as a variable outside `unsafe` blocks'},
		ReviewFixture{'embedded_non_heap_pointer_receiver_method_value', embedded_source + 'fn make() fn () int {
	p := Plain{}
	return p.ref
}

fn main() {}
', 'method `Base.ref` cannot be used as a variable outside `unsafe` blocks as its receiver might refer to an object stored on stack. Consider declaring `Plain` as `@[heap]`.'},
		ReviewFixture{'ambiguous_embedded_method_value', 'struct A {}

fn (a A) name() string {
	return "a"
}

struct B {}

fn (b B) name() string {
	return "b"
}

struct Both {
	A
	B
}

fn main() {
	x := Both{}
	f := x.name
	println(f())
}
', 'ambiguous method `name`'},
		ReviewFixture{'direct_option_alias_cast', 'type MaybeInt = ?int

fn main() {
	_ := MaybeInt(none)
}
', 'alias to Option type requires to be used as Option type (?MaybeInt(...))'},
		ReviewFixture{'uppercase_const_name', 'const Red = 1

fn main() {
	println(Red)
}
', 'const names cannot contain uppercase letters, use snake_case instead'},
		ReviewFixture{'unparenthesized_pointer_write', 'fn main() {
	value := 0
	p := &value
	*p = 1
}
', 'modifying variables via dereferencing can only be done in `unsafe` blocks'},
		ReviewFixture{'parenthesized_pointer_write', 'fn main() {
	value := 0
	p := &value
	(*p) = 1
}
', 'modifying variables via dereferencing can only be done in `unsafe` blocks'},
		ReviewFixture{'uninitialized_reference_field', 'struct Holder {
	value &int
}

fn main() {
	_ := Holder{}
}
', 'reference field `Holder.value` must be initialized'},
		ReviewFixture{'partial_positional_struct_literal', 'struct Pair {
	first int
	second int
}

fn main() {
	_ := Pair{1}
}
', 'too few fields in `Pair` literal (expecting 2, got 1)'},
		ReviewFixture{'local_shadows_import', 'import arrays

fn main() {
	arrays := 1
	_ = arrays
}
', 'duplicate of an import symbol `arrays`'},
		ReviewFixture{'spawn_mutable_value_argument', 'struct State {
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
', 'function in `spawn` statement cannot contain mutable non-reference arguments'},
		ReviewFixture{'spawn_mutable_value_receiver', 'struct State {
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
', 'method in `spawn` statement cannot have non-reference mutable receiver'},
		ReviewFixture{'bad_mut_receiver_immutable_value', 'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\ts := St{}\n\ts.bump()\n}\n', '`s` is immutable, declare it with `mut` to make it mutable'},
		ReviewFixture{'bad_generic_mut_receiver_immutable_value', 'struct Box[T] {\nmut:\n\tvalue T\n}\n\nfn (mut b Box[T]) set(value T) {\n\tb.value = value\n}\n\nfn main() {\n\tb := Box[int]{\n\t\tvalue: 1\n\t}\n\tb.set(2)\n}\n', '`b` is immutable, declare it with `mut` to make it mutable'},
		ReviewFixture{'bad_mut_receiver_address_of_immutable_value', 'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\ts := St{}\n\t(&s).bump()\n}\n', 'cannot pass expression as `mut`'},
		ReviewFixture{'bad_shared_receiver_plain_value', 'struct St {}\n\nfn (shared s St) f() {}\n\nfn main() {\n\ts := St{}\n\ts.f()\n}\n', 'cannot use shared method `f` as `s` is not a shared var'},
		ReviewFixture{'bad_shared_arg_shadowed_local', 'struct St {}\n\nfn take(shared s St) {}\n\nfn main() {\n\tshared s := St{}\n\tif true {\n\t\ts := St{}\n\t\ttake(s)\n\t}\n}\n', 'parameter `s` is `shared`, so use `shared s` instead'},
		ReviewFixture{'bad_explicit_shared_arg_plain_local', 'struct St {}\n\nfn take(shared s St) {}\n\nfn main() {\n\ts := St{}\n\ttake(shared s)\n}\n', 'cannot use non-shared `bad_explicit_shared_arg_plain_local.St` as argument 1'},
		ReviewFixture{'bad_value_receiver_extra_pointer_layers', 'struct S {}

fn (s S) value() int {
	return 1
}

fn main() {
	s := S{}
	p := &s
	pp := &p
	_ := pp.value()
}
', 'cannot use receiver `&&bad_value_receiver_extra_pointer_layers.S` as `bad_value_receiver_extra_pointer_layers.S`'},
		ReviewFixture{'bad_pointer_receiver_extra_pointer_layers', 'struct S {}

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
', 'cannot use receiver `&&&bad_pointer_receiver_extra_pointer_layers.S` as `&bad_pointer_receiver_extra_pointer_layers.S`'},
		ReviewFixture{'uppercase_struct_field', 'struct S {\n\tFoo int\n}\n\nfn main() {}\n', 'field name `Foo` cannot contain uppercase letters, use snake_case instead'},
		ReviewFixture{'forwarded_pointer_preserves_immutable_alias', 'struct State {
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
', 'aliases mutable data from an immutable value'},
	])
	check_review_runtime('receiver_calls_method_values_and_closure_captures', [
		ReviewFixture{'heap_pointer_receiver_method_value', 'struct Foo {}

fn (foo &Foo) ref() int {
	return 1
}

fn main() {
	foo := &Foo{}
	callback := foo.ref
	println(callback())
}
', '1'},
		ReviewFixture{'embedded_heap_pointer_receiver_method_value', embedded_source + 'fn main() {
	p := &Plain{
		Base: Base{3}
	}
	callback := p.ref
	println(callback())
}
', '3'},
		ReviewFixture{'return_capturing_fn_literal', 'fn make(x int) fn () int {
	return fn [x] () int {
		return x
	}
}
fn main() {
	assert make(7)() == 7
}
', ''},
		ReviewFixture{'return_capturing_fn_literal_alias', 'fn make(x int) fn () int {
	f := fn [x] () int {
		return x
	}
	return f
}
fn main() {
	assert make(7)() == 7
}
', ''},
		ReviewFixture{'struct_field_capturing_fn_literal', 'struct Holder {
	cb fn () int
}
fn make(x int) Holder {
	return Holder{
		cb: fn [x] () int {
			return x
		}
	}
}
fn main() {
	holder := make(7)
	assert holder.cb() == 7
}
', ''},
		ReviewFixture{'struct_field_capturing_fn_literal_alias', 'struct Holder {
	cb fn () int
}
fn make(x int) Holder {
	f := fn [x] () int {
		return x
	}
	return Holder{
		cb: f
	}
}
fn main() {
	holder := make(7)
	assert holder.cb() == 7
}
', ''},
		ReviewFixture{'good_capturing_fn_literal_inner_shadow', 'fn plain() int {\n\treturn 3\n}\n\nfn make(x int) fn () int {\n\tif x > 0 {\n\t\tcb := fn [x] () int {\n\t\t\treturn x\n\t\t}\n\t\t_ = cb\n\t}\n\tcb := plain\n\treturn cb\n}\n\nfn main() {\n\tprintln(int_str(make(0)()))\n\tprintln(int_str(make(5)()))\n}\n', '3\n3'},
		ReviewFixture{'good_lambda_capturing_fn_literal_shadow', 'fn plain() int {\n\treturn 4\n}\n\nfn apply(cb fn (int) int) int {\n\treturn cb(1)\n}\n\nfn make() fn () int {\n\tcb := plain\n\t_ = apply(|n| if n > 0 {\n\t\tcb := fn [n] () int {\n\t\t\treturn n\n\t\t}\n\t\t_ = cb\n\t\tn\n\t} else {\n\t\tn\n\t})\n\treturn cb\n}\n\nfn main() {\n\tprintln(int_str(make()()))\n}\n', '4'},
		ReviewFixture{'outer_capturing_alias_survives_inner_shadow', 'fn make(x int) fn () int {\n\tif x > 0 {\n\t\tcb := fn [x] () int {\n\t\t\treturn x + 1\n\t\t}\n\t\t_ = cb\n\t}\n\tcb := fn [x] () int {\n\t\treturn x\n\t}\n\treturn cb\n}\nfn main() {\n\tprintln(int_str(make(5)()))\n}\n', '5'},
		ReviewFixture{'good_exact_interface_method_param', 'interface Base {\n\tbase() int\n}\n\ninterface Handler {\n\thandle(value Base) int\n}\n\nstruct Value {}\n\nfn (v Value) base() int {\n\treturn 7\n}\n\nstruct Service {}\n\nfn (s Service) handle(value Base) int {\n\treturn value.base()\n}\n\nfn main() {\n\tprintln(Handler(Service{}).handle(Base(Value{})))\n}\n', '7'},
		ReviewFixture{'good_voidptr_pointer_arg', 'fn f(p voidptr) int {\n\t_ = p\n\treturn 7\n}\n\nfn main() {\n\tx := 1\n\tprintln(int_str(f(&x)))\n}\n', '7'},
		ReviewFixture{'good_mut_receiver_immutable_pointer_binding', 'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\tp := &s\n\tp.bump()\n\tprintln(int_str(s.value))\n}\n', '1'},
		ReviewFixture{'good_shared_arg_and_receiver', 'struct St {}\n\nfn take(shared s St) int {\n\treturn 1\n}\n\nfn (shared s St) f() int {\n\treturn 2\n}\n\nfn main() {\n\tshared s := St{}\n\tprintln(int_str(take(shared s) + s.f()))\n}\n', '3'},
		ReviewFixture{'good_mut_receiver_mutable_value', 'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\ts.bump()\n\tprintln(int_str(s.value))\n}\n', '1'},
		ReviewFixture{'good_mut_receiver_address_of_mutable_value', 'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\t(&s).bump()\n\tprintln(int_str(s.value))\n}\n', '1'},
		ReviewFixture{'good_mut_receiver_mutable_pointer_binding', 'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\tmut p := &s\n\tp.bump()\n\tprintln(int_str(s.value))\n}\n', '1'},
		ReviewFixture{'good_mut_receiver_global_pointer', 'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\n__global global_st = &St{}\n\nfn main() {\n\tglobal_st.bump()\n\tprintln(int_str(global_st.value))\n}\n', '1'},
		ReviewFixture{'good_mut_receiver_pointer_field', 'struct St {\nmut:\n\tvalue int\n}\n\nstruct Holder {\n\tst &St\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\tholder := Holder{\n\t\tst: &s\n\t}\n\tholder.st.bump()\n\tprintln(int_str(s.value))\n}\n', '1'},
		ReviewFixture{'good_mut_receiver_if_guard_binding', 'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn get() ?St {\n\treturn St{}\n}\n\nfn main() {\n\tif mut s := get() {\n\t\ts.bump()\n\t\tprintln(int_str(s.value))\n\t}\n}\n', '1'},
		ReviewFixture{'good_mut_receiver_explicit_mut_capture', 'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\tfn [mut s] () {\n\t\ts.bump()\n\t\tprintln(int_str(s.value))\n\t}()\n}\n', '1'},
		ReviewFixture{'good_pointer_receiver_immutable_value', 'struct St {\n\tvalue int\n}\n\nfn (s &St) get() int {\n\treturn s.value\n}\n\nfn main() {\n\ts := St{\n\t\tvalue: 2\n\t}\n\tprintln(int_str(s.get()))\n}\n', '2'},
		ReviewFixture{'good_receiver_single_pointer_adjustment', 'struct S {
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
', '7\n7'},
		ReviewFixture{'explicit_mut_reference_parameter', 'fn stop(mut running &bool) {
	unsafe {
		*running = false
	}
}

fn set_second(mut values &f32) {
	unsafe {
		values[1] = 2.5
	}
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
', 'false
false
2.5'},
	])
}

fn test_pointer_fixed_array_and_variadic_argument_rejections() {
	check_review_rejections('pointer_fixed_array_and_variadic_argument_rejections', [
		ReviewFixture{'bad_return_pointer_to_value', 'fn f() int {\n\tx := 1\n\treturn &x\n}\nfn main() {}\n', 'you are returning `&int` instead'},
		ReviewFixture{'bad_voidptr_scalar_arg', 'fn f(p voidptr) {}\n\nfn main() {\n\tf(1)\n}\n', 'expression cannot be passed as `voidptr`'},
		ReviewFixture{'bad_if_expr_pointer_value_branch', 'struct Foo {}\n\nfn main() {\n\t_ := if true {\n\t\tFoo{}\n\t} else {\n\t\t&Foo{}\n\t}\n}\n', 'mismatched types `bad_if_expr_pointer_value_branch.Foo` and `&bad_if_expr_pointer_value_branch.Foo`'},
		ReviewFixture{'bad_fixed_array_pointer_argument', 'fn consume(value &int) {}\n\nfn main() {\n\tconsume([1, 2]!)\n}\n', 'cannot use `[2]int` as `&int` in argument 1 to `bad_fixed_array_pointer_argument.consume`'},
		ReviewFixture{'bad_fixed_array_pointer_return', 'fn make_pointer() &int {\n\treturn [1, 2]!\n}\n\nfn main() {}\n', 'you are returning `[2]int` instead'},
		ReviewFixture{'bad_addressed_fixed_u8_array_pointer_argument', 'fn consume(value &u8) {}\n\nfn main() {\n\tbuf := [u8(1), 2]!\n\tconsume(&buf)\n}\n', 'cannot use `&[2]u8`'},
		ReviewFixture{'bad_fixed_i32_array_byte_pointer_assignment', 'fn main() {\n\tbuf := [i32(1), 2]!\n\tbyte := u8(0)\n\tmut ptr := &byte\n\tptr = &buf\n}\n', 'cannot reference fixed array `buf` outside `unsafe` blocks as it is supposed to be stored on stack'},
		ReviewFixture{'bad_address_mutable_array_element', 'fn main() {\n\tmut values := [1]\n\t_ := &values[0]\n}\n', 'cannot take the address of mutable array elements outside unsafe blocks'},
		ReviewFixture{'bad_address_nested_map_alias_value', "type Scores = map[string]int\ntype ScoresRef = &Scores\n\nfn main() {\n\tmut scores := Scores({'key': 1})\n\tscores_ref := ScoresRef(&scores)\n\t_ := &scores_ref['key']\n}\n", 'cannot take the address of map values outside `unsafe`'},
		ReviewFixture{'bad_variadic_multi_return_argument', 'fn pair() (int, []int) {\n\treturn 1, [2, 3]\n}\n\nfn consume(a int, rest ...int) {}\n\nfn main() {\n\tconsume(pair())\n}\n', 'cannot use `(int, []int)` as `int` in argument 1 to `bad_variadic_multi_return_argument.consume`'},
		ReviewFixture{'bad_forwarded_variadic_function_trailing_arg', 'fn take(values ...int) {}

fn forward(values ...int) {
	take(...values, 2)
}

fn main() {}
', 'when forwarding a variadic variable, it must be the final argument'},
		ReviewFixture{'bad_forwarded_variadic_method_trailing_arg', 'struct Receiver {}

fn (receiver Receiver) take(values ...int) {}

fn (receiver Receiver) forward(values ...int) {
	receiver.take(...values, 2)
}

fn main() {}
', 'when forwarding a variadic variable, it must be the final argument'},
		ReviewFixture{'bad_explicit_generic_too_many_type_args', 'fn id[T](x T) T {\n\treturn x\n}\n\nfn main() {\n\t_ := id[int, string](1)\n}\n', 'expected 1 generic parameter, got 2'},
		ReviewFixture{'bad_generic_channel_alias_element', 'type Ch[T] = chan T

fn receive(ch Ch[int]) int {
	return <-ch
}

fn main() {
	ch := chan string{cap: 1}
	_ := receive(ch)
}
', 'cannot use `chan string`'},
	])
}

fn test_function_returns_control_flow_and_option_result_rejections() {
	check_review_rejections('function_returns_control_flow_and_option_result_rejections', [
		ReviewFixture{'bad_result_value_in_option_return', 'fn make_result() !int {\n\treturn 7\n}\n\nfn make_option() ?int {\n\treturn make_result()\n}\n\nfn main() {}\n', 'cannot use `!int` as type `?int` in return argument'},
		ReviewFixture{'bad_result_value_in_optional_pointer_return', 'struct Item {}\n\nfn convert(res !Item) ?&Item {\n\treturn res\n}\n\nfn main() {}\n', 'cannot use `!bad_result_value_in_optional_pointer_return.Item` as type `?&bad_result_value_in_optional_pointer_return.Item` in return argument'},
		ReviewFixture{'bad_option_value_in_result_pointer_return', 'struct Item {}\n\nfn convert(opt ?Item) !&Item {\n\treturn opt\n}\n\nfn main() {}\n', 'cannot use `?bad_option_value_in_result_pointer_return.Item` as type `!&bad_option_value_in_result_pointer_return.Item` in return argument'},
		ReviewFixture{'bad_error_branch_in_option_return', "fn make_option(ok bool) ?int {\n\treturn if ok { error('bad') } else { 1 }\n}\n\nfn main() {}\n", 'cannot use `!int` as type `?int` in return argument'},
		ReviewFixture{'bad_constant_error_branch_in_option_return', "fn make_option() ?int {\n\treturn if true { error('bad') } else { 1 }\n}\n\nfn main() {}\n", 'cannot use `!int` as type `?int` in return argument'},
		ReviewFixture{'bad_option_void_returned_as_option_int', 'fn empty() ? {\n\treturn\n}\n\nfn value() ?int {\n\treturn empty()\n}\n\nfn main() {}\n', 'cannot use `?void` as type `?int` in return argument'},
		ReviewFixture{'bad_option_void_assigned_to_option_int', 'fn empty() ? {\n\treturn\n}\n\nfn main() {\n\tmut value := ?int(1)\n\tvalue = empty()\n}\n', 'cannot assign to `value`: expected `?int`, not `?void`'},
		ReviewFixture{'bad_non_optional_literal_or_block', 'fn main() {\n\t_ := 1 or { 2 }\n}\n', 'unexpected `or` block'},
		ReviewFixture{'bad_non_optional_call_or_block', 'fn value() int {\n\treturn 1\n}\n\nfn main() {\n\tvalue() or { panic(err) }\n}\n', 'unexpected `or` block'},
		ReviewFixture{'bad_non_optional_infix_or_block', 'fn maybe() ?int {\n\treturn 1\n}\n\nfn main() {\n\t_ := maybe() == none or { false }\n}\n', 'unexpected `or` block, expression of type `bool` is not an Option or a Result'},
		ReviewFixture{'bad_optional_string_concat', "fn maybe_name() ?string {\n\treturn 'Ada'\n}\n\nfn main() {\n\t_ := 'hello ' + maybe_name()\n}\n", '`?string` cannot be used as `string`, unwrap the option first'},
		ReviewFixture{'bad_result_string_concat', "fn result_name() !string {\n\treturn 'Ada'\n}\n\nfn main() {\n\t_ := result_name() + '!'\n}\n", 'unwrapped Result cannot be used in an infix expression'},
		ReviewFixture{'result_function_parameter', 'fn consume(value !int) {}\n\nfn main() {}\n', 'result type arguments are not supported'},
		ReviewFixture{'bad_int_alias_float_variable_return', 'type Id = int\n\nfn f(x f64) Id {\n\treturn x\n}\n\nfn main() {}\n', 'cannot use `f64` as type `bad_int_alias_float_variable_return.Id` in return argument'},
		ReviewFixture{'bad_int_alias_float_expression_return', 'type Id = int\n\nfn f(x f64) Id {\n\treturn x + 1.0\n}\n\nfn main() {}\n', 'cannot use `f64` as type `bad_int_alias_float_expression_return.Id` in return argument'},
		ReviewFixture{'bad_fn_value_integer_return_abi', 'fn wide() u64 {\n\treturn 257\n}\n\nfn invoke(callback fn () u8) u8 {\n\treturn callback()\n}\n\nfn main() {\n\t_ := invoke(wide)\n}\n', 'cannot use `fn () u64`'},
		ReviewFixture{'bad_fn_value_array_return_payload', 'fn numbers() []int {\n\treturn [1, 2]\n}\n\nfn invoke(callback fn () []string) []string {\n\treturn callback()\n}\n\nfn main() {\n\t_ := invoke(numbers)\n}\n', 'cannot use `fn () []int`'},
		ReviewFixture{'bad_shadowed_os_exit_missing_return', 'struct OsLike {}\nfn (x OsLike) exit() {}\nfn f(os OsLike) int {\n\tos.exit()\n}\nfn main() {}\n', 'missing return at end of function `f`'},
		ReviewFixture{'bad_local_os_exit_missing_return', 'struct OsLike {}\nfn (x OsLike) exit() {}\nfn f() int {\n\tos := OsLike{}\n\tos.exit()\n}\nfn main() {}\n', 'missing return at end of function `f`'},
		ReviewFixture{'bad_shadowed_exit_fn_value_missing_return', 'fn f() int {\n\texit := fn () int { return 1 }\n\texit()\n}\nfn main() {}\n', 'missing return at end of function `f`'},
		ReviewFixture{'bad_shadowed_panic_fn_value_missing_return', 'fn f() int {\n\tpanic := fn () int { return 1 }\n\tpanic()\n}\nfn main() {}\n', 'missing return at end of function `f`'},
		ReviewFixture{'bad_nested_shadowed_exit_fn_value_missing_return', 'fn f() int {\n\t{\n\t\texit := fn () int { return 1 }\n\t\texit()\n\t}\n}\nfn main() {}\n', 'missing return at end of function `f`'},
		ReviewFixture{'bad_shadowed_exit_multi_return_branch', 'fn main() {\n\texit := fn () int { return 1 }\n\tflag := true\n\ta, b := if flag {\n\t\t1, 2\n\t} else {\n\t\texit()\n\t}\n\tprintln(int_str(a + b))\n}\n', 'multi-return assignment mismatch'},
		ReviewFixture{'bad_nested_shadowed_exit_multi_return_branch', 'fn main() {\n\tflag := true\n\ta, b := if flag {\n\t\t1, 2\n\t} else {\n\t\texit := fn () int { return 1 }\n\t\texit()\n\t}\n\tprintln(int_str(a + b))\n}\n', 'multi-return assignment mismatch'},
		ReviewFixture{'bad_shadowed_import_os_exit_missing_return', 'import os\nstruct OsLike {}\nfn (x OsLike) exit(code int) {}\nfn f(os OsLike) int {\n\tos.exit(0)\n}\nfn main() {}\n', 'missing return at end of function `f`'},
		ReviewFixture{'bad_shadowed_os_exit_return_value', 'import os\nstruct OsLike {}\nfn (x OsLike) exit(code int) int {\n\treturn code + 1\n}\nfn f(os OsLike) int {\n\treturn os.exit(4)\n}\nfn main() {\n\tprintln(int_str(f(OsLike{})))\n}\n', 'duplicate of an import symbol `os`'},
		ReviewFixture{'bad_called_generic_missing_return', 'fn f[T]() int {\n}\nfn main() {\n\t_ := f[int]()\n}\n', 'missing return at end of function `f`'},
	])
}

fn test_return_types_abi_wrappers_and_numeric_receivers() {
	check_review_runtime('return_types_abi_wrappers_and_numeric_receivers', [
		ReviewFixture{'receiver_method_tail_multi_return', 'struct Receiver {}\n\nfn pair() (int, int) {\n\treturn 2, 3\n}\n\nfn (receiver Receiver) use(a int, b int) int {\n\treturn a * 10 + b\n}\n\nfn (receiver Receiver) use_with_prefix(prefix int, a int, b int) int {\n\treturn prefix * 100 + a * 10 + b\n}\n\nfn main() {\n\tprintln(int_str(Receiver{}.use(pair())))\n\tprintln(int_str(Receiver{}.use_with_prefix(1, pair())))\n}\n', '23\n123'},
		ReviewFixture{'good_multi_return_if_pointer_value_tail', 'struct S {\n\tn int\n}\nfn pick(ok bool) (S, int) {\n\ts := S{\n\t\tn: 5\n\t}\n\treturn if ok {\n\t\t&s\n\t\t1\n\t} else {\n\t\t&s\n\t\t2\n\t}\n}\nfn main() {\n\ta, b := pick(false)\n\tprintln(int_str(a.n) + "," + int_str(b))\n}\n', '5,2'},
		ReviewFixture{'assignment_or_nested_match_returns', 'struct NestedFallbackAnswer {
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
', 'handled'},
		ReviewFixture{'good_panic_satisfies_return_analysis', "fn choose(ok bool) string {\n\tif ok {\n\t\treturn 'ok'\n\t}\n\tpanic('unreachable')\n}\n\nfn pick(ok bool) int {\n\tif ok {\n\t\treturn 7\n\t}\n\treturn panic('unreachable')\n}\n\nfn main() {\n\tprintln(choose(true))\n\tprintln(int_str(pick(true)))\n}\n", 'ok\n7'},
		ReviewFixture{'good_parenthesized_panic_satisfies_return_codegen', "fn f(ok bool) int {\n\tif ok {\n\t\treturn 9\n\t}\n\treturn (panic('x'))\n}\nfn main() {\n\tprintln(int_str(f(true)))\n}\n", '9'},
		ReviewFixture{'good_declared_c_exit_satisfies_return_analysis', 'fn C.exit(code int)\nfn f(ok bool) int {\n\tif ok {\n\t\treturn 7\n\t}\n\treturn C.exit(1)\n}\nfn main() {\n\tprintln(int_str(f(true)))\n}\n', '7'},
		ReviewFixture{'good_receiver_exit_return_value', 'struct Plugin {}\nfn (p Plugin) exit() int {\n\treturn 9\n}\nfn f(plugin Plugin) int {\n\treturn plugin.exit()\n}\nfn main() {\n\tprintln(int_str(f(Plugin{})))\n}\n', '9'},
		ReviewFixture{'good_fixed_array_return_panic_abi_wrapper', "import os\nfn f() [3]int {\n\treturn panic('x')\n}\nfn main() {\n\tif os.args.len == -1 {\n\t\tarr := f()\n\t\tprintln(int_str(arr[0]))\n\t}\n}\n", ''},
		ReviewFixture{'good_fn_return_panic_abi_typedef', "import os\nfn f() fn () int {\n\treturn panic('x')\n}\nfn main() {\n\tif os.args.len == -1 {\n\t\tcb := f()\n\t\tprintln(int_str(cb()))\n\t}\n}\n", ''},
		ReviewFixture{'good_rune_receiver_methods', 'fn main() {\n\tr := `★`\n\tprintln(int_str(`A`.length_in_bytes()))\n\tprintln(int_str(r.bytes().len))\n\tprintln(`c`.to_upper().str())\n}\n', '1\n3\nC'},
		ReviewFixture{'supported_hex_methods', "fn main() {\n\tprintln(u8(15).hex())\n\tprintln(i64(255).hex())\n\tprintln([u8(1), 15, 255].hex())\n\tprintln(char(65).hex())\n\tprintln(`A`.hex())\n\tprintln('abc'.hex())\n}\n", '0f\nff\n010fff\n41\n41\n616263'},
		ReviewFixture{'good_pointer_hex_receiver_method', "struct S {\n\tvalue int\n}\n\nfn (s &S) hex() string {\n\treturn 'ptr:' + int_str(s.value)\n}\n\nfn main() {\n\ts := S{\n\t\tvalue: 7\n\t}\n\tp := &s\n\tprintln(p.hex())\n}\n", 'ptr:7'},
		ReviewFixture{'good_float_alias_int_return', 'type Amount = f64\n\nfn f() Amount {\n\treturn 1\n}\n\nfn main() {\n\tprintln(f().str())\n}\n', '1.0'},
		ReviewFixture{'good_explicit_float_to_int_alias_return', 'type Id = int\n\nfn f(x f64) Id {\n\treturn Id(x)\n}\n\nfn main() {\n\tprintln(int_str(f(1.5)))\n}\n', '1'},
		ReviewFixture{'good_fn_value_matching_integer_return_abi', 'fn letter() rune {\n\treturn `A`\n}\n\nfn invoke(callback fn () u32) u32 {\n\treturn callback()\n}\n\nfn main() {\n\tprintln(int_str(int(invoke(letter))))\n}\n', '65'},
		ReviewFixture{'good_overloaded_power', 'struct Exponent {
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
', '6'},
	])
}

fn test_synthetic_methods_and_collection_operations_rejections() {
	check_review_rejections('synthetic_methods_and_collection_operations_rejections', [
		ReviewFixture{'bad_bool_power', 'fn main() {\n\t_ := true ** 2\n}\n', 'operator `**` requires numeric operands; got `bool` and `int`'},
		ReviewFixture{'bad_array_power', 'fn main() {\n\t_ := [1] ** 2\n}\n', 'operator `**` requires numeric operands; got `[]int` and `int`'},
		ReviewFixture{'bad_string_power', "fn main() {\n\t_ := 'x' ** 2\n}\n", 'operator `**` requires numeric operands; got `string` and `int`'},
		ReviewFixture{'bad_bool_power_assign', 'fn main() {\n\tmut ok := true\n\tok **= false\n}\n', 'operator `**=` requires numeric operands; got `bool` and `bool`'},
		ReviewFixture{'bad_array_power_assign', 'fn main() {\n\tmut values := [1]\n\tvalues **= [2]\n}\n', 'operator `**=` requires numeric operands; got `[]int` and `[]int`'},
		ReviewFixture{'bad_string_power_assign', "fn main() {\n\tmut value := 'x'\n\tvalue **= 'y'\n}\n", 'operator `**=` requires numeric operands; got `string` and `string`'},
		ReviewFixture{'bad_implicit_str_sum_interface', 'interface Printable {\n\tstr() string\n}\ntype Value = int | string\nfn main() {\n\t_ := Printable(Value(1))\n}\n', 'does not implement interface'},
		ReviewFixture{'bad_implicit_str_fn_alias_interface', 'interface Printable {\n\tstr() string\n}\ntype Callback = fn ()\nfn noop() {}\nfn main() {\n\tcb := Callback(noop)\n\t_ := Printable(cb)\n}\n', 'cannot implement interface `bad_implicit_str_fn_alias_interface.Printable` using function'},
		ReviewFixture{'bad_struct_hex_method', 'struct S {}\nfn main() {\n\t_ := S{}.hex()\n}\n', 'unknown function'},
		ReviewFixture{'bad_int_array_hex_method', 'fn main() {\n\t_ := [1, 2].hex()\n}\n', 'unknown function'},
		ReviewFixture{'bad_map_hex_method', 'fn main() {\n\tm := map[string]int{}\n\t_ := m.hex()\n}\n', 'unknown function'},
		ReviewFixture{'bad_float_hex_method', 'fn main() {\n\t_ := f32(1.5).hex()\n}\n', 'unknown function'},
		ReviewFixture{'bad_pointer_hex_method', 'fn main() {\n\tx := 1\n\tp := &x\n\t_ := p.hex()\n}\n', 'unknown function'},
		ReviewFixture{'bad_numeric_hex_arg', 'fn side_effect() int {\n\treturn 1\n}\n\nfn main() {\n\t_ := u8(1).hex(side_effect())\n}\n', 'expected 0 arguments, but got 1'},
		ReviewFixture{'bad_auto_str_arg', 'struct S {\n\tx int\n}\n\nfn side_effect() int {\n\treturn 1\n}\n\nfn main() {\n\t_ := S{\n\t\tx: 1\n\t}.str(side_effect())\n}\n', 'expected 0 arguments, but got 1'},
		ReviewFixture{'bad_map_keys_arg', 'fn main() {\n\tm := map[string]int{}\n\t_ := m.keys(123)\n}\n', '`.keys()` does not have any arguments'},
		ReviewFixture{'bad_map_values_arg', "fn main() {\n\tm := map[string]int{}\n\t_ := m.values('x')\n}\n", '`.values()` does not have any arguments'},
		ReviewFixture{'bad_array_prepend_missing_arg', 'fn main() {\n\tmut a := [1, 2]\n\ta.prepend()\n}\n', '`array.prepend()` should have 1 argument'},
		ReviewFixture{'bad_array_prepend_extra_arg', 'fn side_effect() int {\n\treturn 3\n}\nfn main() {\n\tmut a := [1, 2]\n\ta.prepend(0, side_effect())\n}\n', '`array.prepend()` should have 1 argument'},
		ReviewFixture{'bad_array_insert_missing_arg', 'fn main() {\n\tmut a := [1, 2]\n\ta.insert(0)\n}\n', '`array.insert()` should have 2 arguments'},
		ReviewFixture{'bad_array_insert_extra_arg', 'fn side_effect() int {\n\treturn 3\n}\nfn main() {\n\tmut a := [1, 2]\n\ta.insert(0, 1, side_effect())\n}\n', '`array.insert()` should have 2 arguments'},
		ReviewFixture{'bad_array_prepend_arg_type', "fn main() {\n\tmut a := [1, 2]\n\ta.prepend('x')\n}\n", 'cannot prepend `string` to `[]int`'},
		ReviewFixture{'bad_array_insert_index_type', "fn main() {\n\tmut a := [1, 2]\n\ta.insert('0', 3)\n}\n", 'the first argument of `array.insert()` should be integer'},
		ReviewFixture{'bad_array_insert_value_type', "fn main() {\n\tmut a := [1, 2]\n\ta.insert(0, 'x')\n}\n", 'cannot insert `string` to `[]int`'},
		ReviewFixture{'bad_array_prepend_many_arg_type', "fn main() {\n\tmut a := [1, 2]\n\ta.prepend(['x'])\n}\n", 'cannot prepend `[]string` to `[]int`'},
		ReviewFixture{'bad_array_insert_many_arg_type', "fn main() {\n\tmut a := [1, 2]\n\ta.insert(0, ['x'])\n}\n", 'cannot insert `[]string` to `[]int`'},
		ReviewFixture{'bad_array_to_void_array_param', 'fn take(xs []void) {\n\t_ = xs\n}\n\nfn main() {\n\ttake([1, 2, 3])\n}\n', 'cannot use `[]int` as `[]void` in argument 1 to `bad_array_to_void_array_param.take`'},
		ReviewFixture{'bad_array_to_void_array_user_receiver', 'type Voids = []void\n\nfn (xs Voids) touch() int {\n\treturn xs.len\n}\n\nfn main() {\n\tnums := [1, 2, 3]\n\tprintln(nums.touch().str())\n}\n', 'unknown function: nums.touch'},
		ReviewFixture{'bad_mutable_array_field_copy', 'struct Holder {
	items []int
}

fn main() {
	holder := Holder{
		items: [1]
	}
	mut copy := holder.items
	copy << 2
}
', 'use `mut array2 := array1.clone()` instead of `mut array2 := array1` (or use `unsafe`)'},
	])
}

fn test_called_generic_functions_report_missing_return() {
	run_bad(v3_bin, 'bad_generic_comptime_branch_missing_return',
		'fn f[T]() int {\n\t$if T is int {\n\t\treturn 1\n\t}\n}\nfn main() {\n\t_ := f[string]()\n}\n',
		'missing return at end of function `f`')
}

fn test_return_panic_with_defer_evaluates_call_before_cleanup() {
	out := run_runtime_bad(v3_bin, 'bad_return_panic_defer_order',
		"fn f() int {\n\tdefer {\n\t\tprintln('cleanup-ran')\n\t}\n\treturn panic('boom')\n}\nfn main() {\n\t_ := f()\n}\n")
	assert out.contains('boom')
	assert !out.contains('cleanup-ran')
}

fn test_vexeroot_insert_and_generated_function_name_exemptions() {
	check_review_runtime('comptime_name_resolution_and_collection_lowering', [
		ReviewFixture{'good_generic_comptime_if_unselected_branch_is_not_checked', "fn ok() {}\n\nfn f[T]() {\n\t$if T is int {\n\t\tok()\n\t} $else {\n\t\tonly_for_other_t()\n\t}\n}\n\nfn main() {\n\tf[int]()\n\tprintln('ok')\n}\n", 'ok'},
		ReviewFixture{'good_comptime_match_known_local', "fn main() {\n\tmode := 'b'\n\t\$match mode {\n\t\t'b' { println('ok') }\n\t\t\$else { println('bad') }\n\t}\n}\n", 'ok'},
		ReviewFixture{'good_comptime_match_generic_parameter_type', "fn classify[T](value T) string {\n\t\$match value {\n\t\tint { return 'int' }\n\t\t\$else { return 'other' }\n\t}\n}\n\nfn main() {\n\tprintln(classify(1))\n}\n", 'int'},
		ReviewFixture{'good_multi_explicit_generic_call', "struct Pair[A, B] {\n\tleft  A\n\tright B\n}\n\nfn make_pair[A, B]() Pair[A, B] {\n\treturn Pair[A, B]{}\n}\n\nfn expect_pair(p Pair[int, string]) string {\n\treturn 'ok'\n}\n\nfn main() {\n\tp := make_pair[int, string]()\n\tprintln(expect_pair(p))\n}\n", 'ok'},
		ReviewFixture{'good_nested_explicit_generic_call', "struct Pair[A, B] {\n\tleft  A\n\tright B\n}\n\nstruct Box[T] {\n\tvalue T\n}\n\nfn wrap[T]() Box[T] {\n\treturn Box[T]{}\n}\n\nfn expect_box(b Box[Pair[int, string]]) string {\n\treturn 'ok'\n}\n\nfn main() {\n\tb := wrap[Pair[int, string]]()\n\tprintln(expect_box(b))\n}\n", 'ok'},
		ReviewFixture{'good_const_shadowed_by_param_and_local', "const shadowed_value = 'const'\n\nfn param_shadow(shadowed_value int) int {\n\treturn shadowed_value + 1\n}\n\nfn local_shadow() int {\n\tshadowed_value := 2\n\treturn shadowed_value + 1\n}\n\nfn main() {\n\tprintln(int_str(param_shadow(1)))\n\tprintln(int_str(local_shadow()))\n\tprintln(shadowed_value)\n}\n", '2\n3\nconst'},
		ReviewFixture{'builtin_fn_unrelated_const_suffix', 'import math\nfn main() {\n\t_ = math.pi\n\tprintln(f32(1).eq_epsilon(f32(1)))\n}\n', 'true'},
		ReviewFixture{'good_vexeroot_insert_outside_vlib', '#insert "@VEXEROOT/vlib/v/compiler_tests/testdata/vexeroot_insert.h"

fn C.v3_vexeroot_insert_value() int

fn main() {
	println(int_str(C.v3_vexeroot_insert_value()))
}
', '37'},
		ReviewFixture{'good_generated_generic_function_name', 'fn identity[T](value T) T {
	return value
}

fn main() {
	println(int_str(identity[int](7)))
}
', '7'},
		ReviewFixture{'good_array_clone_ignores_void_array_receiver', 'type Voids = []void\n\nfn (xs Voids) clone() int {\n\treturn 7\n}\n\nfn main() {\n\tnums := [1, 2, 3]\n\tcloned := nums.clone()\n\tprintln(int_str(cloned.len + cloned[2]))\n}\n', '6'},
		ReviewFixture{'good_mutable_array_field_clone', 'struct Holder {
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
', '2\n1'},
		ReviewFixture{'good_mutable_map_clone', "fn clone_and_extend(source map[string]bool) int {
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
", '2\n1'},
		ReviewFixture{'good_array_insert_prepend_many_operands', "type Strings = []string\n\nfn main() {\n\tmut a := [3, 4]\n\ta.insert(0, [1, 2])\n\tb := [5, 6]\n\ta.insert(1, b)\n\ta.prepend([0])\n\tfixed := [7, 8]!\n\ta.insert(a.len, fixed)\n\tassert a == [0, 1, 5, 6, 2, 3, 4, 7, 8]\n\tmut strs := Strings(['hi'])\n\tstrs.insert(0, ['there'])\n\tstrs.prepend(['hello'])\n\tassert strs == ['hello', 'there', 'hi']\n\tprintln('ok')\n}\n", 'ok'},
	])
}

fn write_review_sources(name string, files map[string]string) string {
	root := os.join_path(review_checker_fixture_dir, name)
	os.mkdir(root) or { panic(err) }
	for file, contents in files {
		os.write_file(os.join_path(root, file), contents) or { panic(err) }
	}
	return root
}

fn check_review_main_runtime(name string, files map[string]string, expected map[string]string) {
	root := write_review_sources(name, files)
	bin_path := compile_review_runtime(root)
	for case, output in expected {
		result := os.exec([bin_path, case])
		assert result.exit_code == 0, '${case}: ${result.output}'
		assert result.output.trim_space() == output, '${case}: expected ${output}\n${result.output}'
	}
}

fn check_review_main_rejections(name string, files map[string]string, expected []string) {
	root := write_review_sources(name, files)
	result := run_review_checker(v3_bin, [root, '-b', 'c', '-o', os.join_path(root, 'program')])
	assert result.exit_code != 0, result.output
	assert !result.output.contains('C compilation failed'), result.output
	for diagnostic in expected {
		assert result.output.contains(diagnostic), 'expected ${diagnostic}\n${result.output}'
	}
}

const review_callback_fixture = 'struct Counter {
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

fn test_review_shared_runtime_fixtures() {
	check_review_main_runtime('runtime_batch', {
		'recursive_str.v':            'type Scalar = bool | int | string

fn (value Scalar) str() string {
	return value.display()
}

fn (value Scalar) display() string {
	return match value {
		bool, int { value.str() }
		string { value }
	}
}

fn case_recursive_str() {
	println(Scalar(7).str())
}
'
		'shared_global.v':            '@[has_globals]
module main

struct SharedCounter {
mut:
	value int
}

__global shared_counter shared SharedCounter

fn case_shared_global() {
	lock shared_counter {
		shared_counter.value = 42
	}
	value := rlock shared_counter {
		shared_counter.value
	}
	println(value)
}
'
		'unsafe_fixed.v':             'type Fixed = [2]u8

fn case_unsafe_fixed() {
	buf := Fixed([u8(65), 66]!)
	ptr := unsafe { &buf }
	println(int_str(int((*ptr)[0])))
}
'
		'enum_and_array.v':           '
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

fn case_enum_and_array() {
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
'
		'nested_panic.v':             'struct StaticPanicNode[T] {
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

fn case_nested_panic() {
	maybe_panic_nested_generic(false)
	println("ok")
}
'
		'mutable_callback.v':         review_callback_fixture + 'fn case_mutable_callback() {
	holder := Holder{
		callback: increment
	}
	mut counter := Counter{
		value: 4
	}
	holder.callback(mut counter)
	println(int_str(counter.value))
}
'
		'v3_review_checker_insert.h': 'static inline int v3_review_checker_inserted(void) { return 6; }
'
		// Keep the constant-match case in main so its Result unwrap uses main propagation.
		'main.v':                     "const size_224 = 28

struct E {
	size int
}

fn check(hash_size int) !E {
	match hash_size {
		size_224 {
			return E{
				size: hash_size
			}
		}
		else {}
	}
	return E{
		size: 0
	}
}

fn main() {
	match arguments()[1] {
		'recursive_str' { case_recursive_str() }
		'shared_global' { case_shared_global() }
		'unsafe_fixed' { case_unsafe_fixed() }
		'enum_and_array' { case_enum_and_array() }
		'nested_panic' { case_nested_panic() }
		'match_const' {
			println(int_str(check(28)!.size))
		}
		'mutable_callback' { case_mutable_callback() }
		else { panic('unknown fixture') }
	}
}
"
	}, {
		'recursive_str':    '7'
		'shared_global':    '42'
		'unsafe_fixed':     '65'
		'enum_and_array':   'enum\n42\n9\n42\n1.0\n42.0\n8\ntrue\n0\nA\nnone\n6'
		'nested_panic':     'ok'
		'match_const':      '28'
		'mutable_callback': '5'
	})
}

fn test_review_shared_rejection_fixtures() {
	check_review_main_rejections('rejections_batch', {
		'wide_assignment.v': 'struct WideAssignmentHolder {
	item ?string
}

fn case_wide_assignment() {
	holder := WideAssignmentHolder{}
	mut value := 0
' + ' '.repeat(32768) + 'value = holder.item or { "" }
}
'
		'missing_mut.v':     review_callback_fixture + 'fn case_missing_mut() {
	holder := Holder{
		callback: increment
	}
	mut counter := Counter{
		value: 4
	}
	holder.callback(counter)
}
'
		'function_name.v':   'fn bad_T_Name() {}

fn case_function_name() {}
'
		'main.v':            'module main

fn main() {
	case_wide_assignment()
	case_missing_mut()
	case_function_name()
}
'
	}, [
		'wide_assignment.v:8:32784: error: cannot assign to `value`: expected `int`, not `string`',
		'missing_mut.v:21:18: error: method `callback` parameter 1 is `mut`, so use `mut counter` instead',
		'function_name.v:1:1: error: function name `bad_T_Name` cannot contain uppercase letters, use snake_case instead',
	])
}

fn test_review_translated_runtime_fixtures() {
	check_review_main_runtime('translated_runtime_batch', {
		'heap_method.v':        '@[translated]
module main

const Red = 2

@[heap]
struct Foo {}

fn (foo &Foo) ref() int {
	return 1
}

fn case_heap_method() {
	foo := Foo{}
	callback := foo.ref
	println(callback() + Red)
}
'
		'pointer_assignment.v': '@[translated]
module main

fn case_pointer_assignment() {
	mut values := [1, 2]!
	mut ptr := unsafe { &values[0] }
	println(int_str(*ptr))
}
'
		'main.v':               "module main

fn main() {
	match arguments()[1] {
		'heap_method' { case_heap_method() }
		'pointer_assignment' { case_pointer_assignment() }
		else { panic('unknown fixture') }
	}
}
"
	}, {
		'heap_method':        '3'
		'pointer_assignment': '1'
	})
}

fn test_review_translated_rejection_fixtures() {
	check_review_main_rejections('translated_rejections_batch', {
		'pointer_return.v':     '@[translated]
module main

fn make_pointer() &int {
	return [1, 2]!
}

fn case_pointer_return() {}
'
		'pointer_assignment.v': '@[translated]
module main

fn case_pointer_assignment() {
	mut ptr := &int(0)
	ptr = [u16(1), 2]!
}
'
		'main.v':               'module main

fn main() {
	case_pointer_return()
	case_pointer_assignment()
}
'
	}, [
		'pointer_return.v:5:9: error: fn `make_pointer` expects you to return a reference type `&int`, but you are returning `[2]int` instead',
		'pointer_assignment.v:6:6: error: mismatched types `&int` and `[2]u16`',
	])
}
