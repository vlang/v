import os
import v.tempname

fn generic_sum_type_fixture_dir(name string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_generic_sum_type_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

fn generic_sum_type_compile_run_source(name string, source string) string {
	root := generic_sum_type_fixture_dir(name)
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'app')
	src := os.join_path(root, 'main.v')
	os.write_file(src, source) or { panic(err) }

	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	return os.read_file(bin + '.c') or { panic(err) }
}

// Generic struct instances declared in main use the `main__` C namespace, while
// the sum type instances keep their bare names.
fn generic_sum_type_assert_no_bst_branch_wrap_leaks(c_code string) {
	assert c_code.contains('struct main__Node_int {'), c_code
	assert c_code.contains('struct main__Node_f64 {'), c_code
	assert c_code.contains('struct Tree_int {'), c_code
	assert c_code.contains('struct Tree_f64 {'), c_code
	assert c_code.contains('main__Node_int* _ptr_Node_int;'), c_code
	assert c_code.contains('main__Node_f64* _ptr_Node_f64;'), c_code
	assert !c_code.contains('Tree_unknown'), c_code
	assert !c_code.contains('Node_unknown'), c_code
	assert !c_code.contains('\nNode '), c_code
	assert !c_code.contains(' Node '), c_code
	assert !c_code.contains('Node[T]'), c_code
	assert !c_code.contains('[T]'), c_code
	assert !c_code.contains('return (main__Node_f64)'), c_code
	assert !c_code.contains('return ((main__Node_f64)'), c_code
	assert !c_code.contains('return main__Node_f64'), c_code
	assert !c_code.contains('Node_int main__insert_'), c_code
	assert !c_code.contains('Node_f64 main__insert_'), c_code
}

fn test_generic_sum_recursive_pointer_variants_emit_concrete_c_types() {
	c_code := generic_sum_type_compile_run_source('recursive_pointer_variants', '
struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | &Node[T]

enum Direction {
	left
	right
	same
}

fn direction[T](tree &Node[T], value T) Direction {
	if value < tree.value {
		return .left
	}
	if tree.value < value {
		return .right
	}
	return .same
}

fn (tree Tree[T]) size[T]() int {
	return match tree {
		Empty { 0 }
		&Node[T] { 1 + tree.left.size() + tree.right.size() }
	}
}

fn (tree Tree[T]) insert[T](value T) Tree[T] {
	return match tree {
		Empty {
			&Node[T]{value, tree, tree}
		}
		&Node[T] {
			if value < tree.value {
				&Node[T]{
					...tree
					left: tree.left.insert(value)
				}
			} else if tree.value < value {
				&Node[T]{
					...tree
					right: tree.right.insert(value)
				}
			} else {
				tree
			}
		}
	}
}

fn insert_match[T](tree Tree[T], value T) Tree[T] {
	return match tree {
		Empty {
			&Node[T]{value, tree, tree}
		}
		&Node[T] {
			match direction(tree, value) {
				.left {
					&Node[T]{
						...tree
						left: insert_match(tree.left, value)
					}
				}
				.right {
					&Node[T]{
						...tree
						right: insert_match(tree.right, value)
					}
				}
				.same {
					tree
				}
			}
		}
	}
}

fn leaf[T](value T, empty Tree[T]) Tree[T] {
	return &Node[T]{value, empty, empty}
}

fn (tree Tree[T]) add_left[T](value T) Tree[T] {
	return match tree {
		Empty {
			&Node[T]{value, tree, tree}
		}
		&Node[T] {
			&Node[T]{
				...tree
				left: &Node[T]{value, Empty{}, Empty{}}
			}
		}
	}
}

fn check_tree_construction() {
	empty_int := Tree[int](Empty{})
	mut ints := leaf(10, empty_int)
	ints = ints.add_left(5)
	assert ints.size() == 2

	empty_f64 := Tree[f64](Empty{})
	mut floats := leaf(1.25, empty_f64)
	floats = floats.add_left(0.5)
	assert floats.size() == 2
}

fn check_bst_insertion() {
	mut ints := Tree[int](Empty{})
	ints = insert_match(ints, 4)
	ints = insert_match(ints, 2)
	ints = insert_match(ints, 6)
	ints = insert_match(ints, 2)
	assert ints.size() == 3

	mut floats := Tree[f64](Empty{})
	floats = insert_match(floats, 4.0)
	floats = insert_match(floats, 2.0)
	floats = insert_match(floats, 6.0)
	floats = insert_match(floats, 6.0)
	assert floats.size() == 3

	mut ints_method := Tree[int](Empty{})
	ints_method = ints_method.insert(4)
	ints_method = ints_method.insert(2)
	ints_method = ints_method.insert(6)
	ints_method = ints_method.insert(2)
	assert ints_method.size() == 3

	mut floats_method := Tree[f64](Empty{})
	floats_method = floats_method.insert(4.0)
	floats_method = floats_method.insert(2.0)
	floats_method = floats_method.insert(6.0)
	floats_method = floats_method.insert(6.0)
	assert floats_method.size() == 3
}

fn main() {
	check_tree_construction()
	check_bst_insertion()
	println("ok")
}
')
	generic_sum_type_assert_no_bst_branch_wrap_leaks(c_code)
	assert c_code.contains('typedef struct Tree_int Tree_int;'), c_code
	assert c_code.contains('typedef struct Tree_f64 Tree_f64;'), c_code
	assert !c_code.contains('Node_T'), c_code
	assert !c_code.contains('Tree_int ints = (Tree){'), c_code
	assert !c_code.contains('Tree_f64 floats = (Tree){'), c_code
}

fn test_generic_sum_concrete_value_variants() {
	c_code := generic_sum_type_compile_run_source('concrete_value_variants', '
struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn value(tree Tree[int]) int {
	return match tree {
		Node {
			tree.value
		}
		else {
			0
		}
	}
}

fn empty_tree() Tree[int] {
	return Tree[int](Empty{})
}

fn empty_tree_defer() Tree[int] {
	defer {
		_ := 1
	}
	return Tree[int](Empty{})
}

fn check_bare_variant() {
	tree := Tree[int](Node[int]{
		value: 42
	})
	assert value(tree) == 42
	assert value(Tree[int](Empty{})) == 0
}

fn check_constructor_returns() {
	tree := empty_tree()
	assert tree is Empty
	tree_defer := empty_tree_defer()
	assert tree_defer is Empty
}

struct Box[T] {
	value T
}

type BoxSum = Box[int] | Box[string]

fn score(s BoxSum) int {
	return match s {
		Box[int] { 10 + s.value }
		Box[string] { 100 + s.value.len }
	}
}

fn check_distinct_tags() {
	s := BoxSum(Box[string]{
		value: "ok"
	})
	assert s is Box[string]
	assert !(s is Box[int])
	b := s as Box[string]
	assert b.value == "ok"
	assert score(s) == 102

	i := BoxSum(Box[int]{
		value: 7
	})
	assert i is Box[int]
	assert !(i is Box[string])
	assert score(i) == 17
}

struct A {}
struct B {}

type Value = A | B

fn match_count[T](value T) int {
	mut count := 0
	$for variant in T.variants {
		if value is variant {
			count++
		}
	}
	return count
}

fn check_comptime_variant_pattern() {
	value := Value(A{})
	assert match_count(value) == 1
}

fn main() {
	check_bare_variant()
	check_constructor_returns()
	check_distinct_tags()
	check_comptime_variant_pattern()
	println("ok")
}
')
	assert !c_code.contains('if (true)'), c_code
	assert !c_code.contains('tree == Node'), c_code
	assert !c_code.contains('Tree((Empty){})'), c_code
	for name, expected in {
		'empty_tree':       'return (Tree_int){'
		'empty_tree_defer': '(Tree_int){'
	} {
		signature := 'Tree_int ${name}(void) {'
		assert c_code.contains(signature), c_code
		body := c_code.all_after(signature).all_before('\n}')
		assert body.contains(expected), body
	}
}

fn test_generic_sum_rejects_unknown_is_pattern_after_specialization() {
	root := generic_sum_type_fixture_dir('unknown_is')
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'app')
	src := os.join_path(root, 'main.v')
	os.write_file(src, '
struct A {}
struct B {}

type Value = A | B

fn has_missing[T](value T) bool {
	return value is Missing
}

fn main() {
	value := Value(A{})
	println(has_missing(value))
}
') or {
		panic(err)
	}

	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('`Missing` is not a variant of sum type `Value`'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_generic_sum_rejects_mismatched_and_qualified_variants() {
	root := generic_sum_type_fixture_dir('variant_errors')
	defer { os.rmdir_all(root) or {} }
	left_dir := os.join_path(root, 'left')
	right_dir := os.join_path(root, 'right')
	os.mkdir_all(left_dir) or { panic(err) }
	os.mkdir_all(right_dir) or { panic(err) }
	os.write_file(os.join_path(left_dir, 'left.v'), 'module left

pub struct Foo {}
') or {
		panic(err)
	}
	os.write_file(os.join_path(right_dir, 'right.v'), 'module right

pub struct Foo {}

pub struct Empty {}

pub struct Node[T] {
	pub:
	value T
}
') or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	source := 'module main

import left
import right

struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn bad_type_arg(tree Tree[left.Foo]) int {
	return match tree {
		Node[right.Foo] { 1 }
		else { 0 }
	}
}

fn bad_module(tree Tree[left.Foo]) int {
	return match tree {
		right.Node[left.Foo] { 1 }
		else { 0 }
	}
}

fn bad_node(tree Tree[left.Foo]) int {
	return match tree {
		right.Node { 1 }
		else { 0 }
	}
}

fn bad_empty(tree Tree[left.Foo]) int {
	return match tree {
		right.Empty { 1 }
		else { 0 }
	}
}

fn bad() Tree[int] {
	return Node[string]{"nope"}
}

fn main() {}
'
	os.write_file(src, source) or {
		panic(err)
	}

	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, src, '-b', 'c', '-nocolor', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	lines := source.split_into_lines()
	errors := compile.output.split_into_lines().filter(it.contains(': error: '))
	// Match diagnostics to each pattern's source line. A generic variant's display name
	// can omit its module qualifier.
	for pattern in ['Node[right.Foo]', 'right.Node[left.Foo]', 'right.Node', 'right.Empty'] {
		line := lines.index('\t\t${pattern} { 1 }') + 1
		assert line > 0, pattern
		location := 'main.v:${line}:'
		assert errors.any(it.contains(location) && it.contains('`Tree[left.Foo]` has no variant')), 'expected rejection of ${pattern} at ${location}\n${compile.output}'
	}
	mismatch_line := lines.index('\treturn Node[string]{"nope"}') + 1
	assert mismatch_line > 0
	location := 'main.v:${mismatch_line}:'
	assert errors.any(it.contains(location) && (it.contains('cannot use `Node[string]` as type `Tree[int]`')
		|| it.contains('cannot return') || it.contains('incompatible'))), compile.output
}

fn test_generic_sum_accepts_declared_qualified_bare_variant_pattern() {
	root := generic_sum_type_fixture_dir('declared_qualified_bare')
	defer { os.rmdir_all(root) or {} }
	foo_dir := os.join_path(root, 'foo')
	os.mkdir_all(foo_dir) or { panic(err) }
	os.write_file(os.join_path(foo_dir, 'foo.v'), 'module foo

pub struct Node[T] {
	pub:
	value T
}
') or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, 'module main

import foo

struct Empty {}

type Tree[T] = Empty | foo.Node[T]

fn value(tree Tree[int]) int {
	return match tree {
		foo.Node[int] { tree.value }
		Empty { 0 }
	}
}

fn main() {}
') or {
		panic(err)
	}

	c_out := os.join_path(root, 'out.c')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', c_out])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('tcc.exe'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	assert os.exists(c_out)
}

fn test_generic_sum_type_param_variants_resolve_across_modules() {
	root := generic_sum_type_fixture_dir('param_variant_scopes')
	defer { os.rmdir_all(root) or {} }
	maybe_dir := os.join_path(root, 'maybe')
	payload_dir := os.join_path(root, 'payload')
	client_dir := os.join_path(root, 'client')
	os.mkdir_all(maybe_dir) or { panic(err) }
	os.mkdir_all(payload_dir) or { panic(err) }
	os.mkdir_all(client_dir) or { panic(err) }
	os.write_file(os.join_path(maybe_dir, 'maybe.v'), 'module maybe

pub struct Err {}

pub type Maybe[T] = T | Err

pub fn wrap_int(v int) Maybe[int] {
	return Maybe[int](v)
}

pub fn wrap_err() Maybe[int] {
	return Maybe[int](Err{})
}

pub fn describe(x Maybe[int]) string {
	return match x {
		int { "int" }
		Err { "err" }
	}
}

pub fn unwrap_int(x Maybe[int]) int {
	assert x is int
	y := x as int
	assert y == 7
	return y
}

pub fn wrap[T](v T) Maybe[T] {
	return Maybe[T](v)
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(payload_dir, 'payload.v'), 'module payload

pub struct Payload {
pub:
	name string
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(client_dir, 'client.v'), 'module client

import maybe
import payload { Payload }

pub struct Local {
pub:
	value int
}

pub fn make() maybe.Maybe[Local] {
	return maybe.wrap(Local{
		value: 7
	})
}

pub fn make_payload() maybe.Maybe[Payload] {
	return maybe.wrap(Payload{
		name: "payload"
	})
}

pub fn describe_local(x maybe.Maybe[Local]) int {
	return match x {
		Local { x.value }
		maybe.Err { -1 }
	}
}

pub fn describe_payload(x maybe.Maybe[Payload]) string {
	return match x {
		Payload { x.name }
		maybe.Err { "err" }
	}
}

pub fn unwrap_local(x maybe.Maybe[Local]) Local {
	assert x is Local
	y := x as Local
	assert y.value == 7
	return y
}

pub fn unwrap_payload(x maybe.Maybe[Payload]) Payload {
	assert x is Payload
	y := x as Payload
	assert y.name == "payload"
	return y
}
') or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, 'module main

import client
// Keep this import spelling distinct from the client module.
import maybe as int_maybe

fn main() {
	assert int_maybe.describe(int_maybe.wrap_int(7)) == "int"
	assert int_maybe.describe(int_maybe.wrap_err()) == "err"
	assert int_maybe.unwrap_int(int_maybe.wrap_int(7)) == 7

	local_sum := client.make()
	assert client.describe_local(local_sum) == 7
	local := client.unwrap_local(local_sum)
	assert local.value == 7
	payload_sum := client.make_payload()
	assert client.describe_payload(payload_sum) == "payload"
	payload := client.unwrap_payload(payload_sum)
	assert payload.name == "payload"
	println("ok")
}
') or {
		panic(err)
	}

	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output

	c_code := os.read_file(bin + '.c') or { panic(err) }
	wrap_err_signature := 'maybe__Maybe_int maybe__wrap_err(void) {'
	assert c_code.contains(wrap_err_signature), c_code
	wrap_err_body := c_code.all_after(wrap_err_signature).all_before('\n}')
	assert wrap_err_body.contains('.maybe__Err = (maybe__Err)'), wrap_err_body
	assert !c_code.contains('maybe__Maybe_int y = x'), c_code
	assert !c_code.contains('maybe__T'), c_code
	assert !c_code.contains('x == v_int'), c_code
	assert c_code.contains('maybe__Maybe_client__Local'), c_code
	assert c_code.contains('maybe__Maybe_payload__Payload'), c_code
	assert c_code.contains('payload__Payload'), c_code
	assert c_code.contains('_payload__Payload'), c_code
	assert !c_code.contains('maybe__Maybe_Local'), c_code
	assert !c_code.contains('maybe__Maybe_Payload'), c_code
	assert !c_code.contains('maybe__Payload'), c_code
	assert !c_code.contains('x.Local'), c_code
	assert !c_code.contains('x.Payload'), c_code
	assert !c_code.contains('x.maybe__Local'), c_code
	assert !c_code.contains('x.maybe__Payload'), c_code
	assert !c_code.contains('x == Local'), c_code
	assert !c_code.contains('x == Payload'), c_code
}
