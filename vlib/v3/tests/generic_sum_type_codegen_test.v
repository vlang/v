import os

const generic_sum_type_vexe = @VEXE
const generic_sum_type_tests_dir = os.dir(@FILE)
const generic_sum_type_v3_dir = os.dir(generic_sum_type_tests_dir)
const generic_sum_type_vlib_dir = os.dir(generic_sum_type_v3_dir)
const generic_sum_type_v3_src = os.join_path(generic_sum_type_v3_dir, 'v3.v')

fn generic_sum_type_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_sum_type_test_${os.getpid()}')
	if os.exists(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${generic_sum_type_vexe} -gc none -no-parallel -path "${generic_sum_type_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${generic_sum_type_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn generic_sum_type_compile_run_source(name string, source string) string {
	v3_bin := generic_sum_type_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_generic_sum_type_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }

	bin := os.join_path(os.temp_dir(), 'v3_generic_sum_type_${name}_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	return os.read_file(bin + '.c') or { panic(err) }
}

fn generic_sum_type_assert_no_bst_branch_wrap_leaks(c_code string) {
	assert c_code.contains('struct Node_int {'), c_code
	assert c_code.contains('struct Node_f64 {'), c_code
	assert c_code.contains('struct Tree_int {'), c_code
	assert c_code.contains('struct Tree_f64 {'), c_code
	assert !c_code.contains('Tree_unknown'), c_code
	assert !c_code.contains('Node_unknown'), c_code
	assert !c_code.contains('\nNode '), c_code
	assert !c_code.contains(' Node '), c_code
	assert !c_code.contains('Node[T]'), c_code
	assert !c_code.contains('[T]'), c_code
	assert !c_code.contains('return (Node_f64)'), c_code
	assert !c_code.contains('return ((Node_f64)'), c_code
	assert !c_code.contains('return Node_f64'), c_code
	assert !c_code.contains('Node_int main__insert_'), c_code
	assert !c_code.contains('Node_f64 main__insert_'), c_code
}

fn test_generic_sum_instances_emit_concrete_c_types() {
	v3_bin := generic_sum_type_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_generic_sum_type_input_${os.getpid()}.v')
	os.write_file(src, '
struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

fn leaf[T](value T, empty Tree[T]) Tree[T] {
	return Node[T]{value, empty, empty}
}

fn (tree Tree[T]) size[T]() int {
	return match tree {
		Empty { 0 }
		Node[T] { 1 + tree.left.size() + tree.right.size() }
	}
}

fn (tree Tree[T]) add_left[T](value T) Tree[T] {
	return match tree {
		Empty {
			Node[T]{value, tree, tree}
		}
		Node[T] {
			Node[T]{
				...tree
				left: Node[T]{value, Empty{}, Empty{}}
			}
		}
	}
}

fn main() {
	empty_int := Tree[int](Empty{})
	mut ints := leaf(10, empty_int)
	ints = ints.add_left(5)
	assert ints.size() == 2

	empty_f64 := Tree[f64](Empty{})
	mut floats := leaf(1.25, empty_f64)
	floats = floats.add_left(0.5)
	assert floats.size() == 2
	println("ok")
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_generic_sum_type_input_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('typedef struct Tree_int Tree_int;'), c_code
	assert c_code.contains('typedef struct Tree_f64 Tree_f64;'), c_code
	assert c_code.contains('struct Node_int {'), c_code
	assert c_code.contains('struct Node_f64 {'), c_code
	assert c_code.contains('struct Tree_int {'), c_code
	assert c_code.contains('struct Tree_f64 {'), c_code
	assert !c_code.contains('Array_fixed_Node_T'), c_code
	assert !c_code.contains('Node_T'), c_code
	assert !c_code.contains('tree.Node_T'), c_code
	assert !c_code.contains('Tree_int ints = (Tree){'), c_code
	assert !c_code.contains('Tree_f64 floats = (Tree){'), c_code
}

fn test_generic_sum_match_bst_branches_wrap_node_after_monomorphization() {
	c_code := generic_sum_type_compile_run_source('bst_match_branch_wrap', '
struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

enum Direction {
	left
	right
	same
}

fn direction[T](tree Node[T], value T) Direction {
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
		Node[T] { 1 + tree.left.size() + tree.right.size() }
	}
}

fn (tree Tree[T]) insert[T](value T) Tree[T] {
	return match tree {
		Empty {
			Node[T]{value, tree, tree}
		}
		Node[T] {
			if value < tree.value {
				Node[T]{
					...tree
					left: tree.left.insert(value)
				}
			} else if tree.value < value {
				Node[T]{
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
			Node[T]{value, tree, tree}
		}
		Node[T] {
			match direction(tree, value) {
				.left {
					Node[T]{
						...tree
						left: insert_match(tree.left, value)
					}
				}
				.right {
					Node[T]{
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

fn main() {
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

	println("ok")
}
')
	generic_sum_type_assert_no_bst_branch_wrap_leaks(c_code)
}

fn test_generic_sum_bare_variant_match_lowers_to_tag_check() {
	v3_bin := generic_sum_type_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_generic_sum_type_bare_match_${os.getpid()}.v')
	os.write_file(src, '
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

fn main() {
	tree := Tree[int](Node[int]{
		value: 42
	})
	println(value(tree).str())
	println(value(Tree[int](Empty{})).str())
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_generic_sum_type_bare_match_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42\n0'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert !c_code.contains('if (true)'), c_code
	assert !c_code.contains('tree == Node'), c_code
}

fn test_generic_sum_constructor_return_uses_concrete_c_type() {
	v3_bin := generic_sum_type_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_generic_sum_type_return_ctor_${os.getpid()}.v')
	os.write_file(src, '
struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn empty_tree() Tree[int] {
	return Tree[int](Empty{})
}

fn empty_tree_defer() Tree[int] {
	defer {
		_ := 1
	}
	return Tree[int](Empty{})
}

fn main() {
	tree := empty_tree()
	assert tree is Empty
	tree_defer := empty_tree_defer()
	assert tree_defer is Empty
	println("ok")
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_generic_sum_type_return_ctor_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert !c_code.contains('Tree((Empty){})'), c_code
	assert c_code.contains('return (Tree_int){'), c_code
}

fn test_generic_sum_rejects_mismatched_concrete_variant() {
	v3_bin := generic_sum_type_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_generic_sum_type_negative_${os.getpid()}.v')
	os.write_file(src, '
struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn bad() Tree[int] {
	return Node[string]{"nope"}
}

fn main() {}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_generic_sum_type_negative_${os.getpid()}')
	os.rm(bin) or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return') || compile.output.contains('incompatible'), compile.output

	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_generic_sum_rejects_unknown_is_pattern_after_specialization() {
	v3_bin := generic_sum_type_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_generic_sum_type_unknown_is_${os.getpid()}.v')
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

	bin := os.join_path(os.temp_dir(), 'v3_generic_sum_type_unknown_is_${os.getpid()}')
	os.rm(bin) or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('`Missing` is not a variant of sum type `Value`'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_generic_sum_comptime_variant_pattern_still_specializes() {
	generic_sum_type_compile_run_source('comptime_variant_pattern', '
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

fn main() {
	value := Value(A{})
	assert match_count(value) == 1
	println("ok")
}
')
}

fn test_generic_sum_rejects_mismatched_qualified_generic_variant_pattern() {
	v3_bin := generic_sum_type_build_v3()
	root := os.join_path(os.temp_dir(), 'v3_generic_sum_type_qualified_negative_${os.getpid()}')
	os.rmdir_all(root) or {}
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
') or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, 'module main

import left
import right

struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn bad(tree Tree[left.Foo]) int {
	return match tree {
		Node[right.Foo] { 1 }
		else { 0 }
	}
}

fn main() {}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_generic_sum_type_qualified_negative_${os.getpid()}')
	os.rm(bin) or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('Node[right.Foo]'), compile.output
	assert compile.output.contains('Tree[left.Foo]'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_generic_sum_rejects_qualified_generic_variant_from_other_module() {
	v3_bin := generic_sum_type_build_v3()
	root := os.join_path(os.temp_dir(),
		'v3_generic_sum_type_qualified_variant_negative_${os.getpid()}')
	os.rmdir_all(root) or {}
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

pub struct Node[T] {
	pub:
	value T
}
') or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, 'module main

import left
import right

struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn bad(tree Tree[left.Foo]) int {
	return match tree {
		right.Node[left.Foo] { 1 }
		else { 0 }
	}
}

fn main() {}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(),
		'v3_generic_sum_type_qualified_variant_negative_${os.getpid()}')
	os.rm(bin) or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('right.Node[left.Foo]'), compile.output
	assert compile.output.contains('Tree[left.Foo]'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_generic_sum_rejects_qualified_bare_variants_from_other_module() {
	v3_bin := generic_sum_type_build_v3()
	root := os.join_path(os.temp_dir(),
		'v3_generic_sum_type_qualified_bare_variant_negative_${os.getpid()}')
	os.rmdir_all(root) or {}
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

pub struct Empty {}

pub struct Node[T] {
	pub:
	value T
}
') or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, 'module main

import left
import right

struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

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

fn main() {}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(),
		'v3_generic_sum_type_qualified_bare_variant_negative_${os.getpid()}')
	os.rm(bin) or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('right.Node'), compile.output
	assert compile.output.contains('right.Empty'), compile.output
	assert compile.output.contains('Tree[left.Foo]'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_generic_sum_accepts_declared_qualified_bare_variant_pattern() {
	v3_bin := generic_sum_type_build_v3()
	root := os.join_path(os.temp_dir(),
		'v3_generic_sum_type_declared_qualified_bare_${os.getpid()}')
	os.rmdir_all(root) or {}
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
		foo.Node { tree.value }
		Empty { 0 }
	}
}

fn main() {}
') or {
		panic(err)
	}

	c_out := os.join_path(os.temp_dir(),
		'v3_generic_sum_type_declared_qualified_bare_${os.getpid()}.c')
	os.rm(c_out) or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${c_out}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('tcc.exe'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	assert os.exists(c_out)
}

fn test_generic_sum_concrete_generic_variants_keep_distinct_tags() {
	v3_bin := generic_sum_type_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_generic_sum_type_distinct_tags_${os.getpid()}.v')
	os.write_file(src, '
struct Box[T] {
	value T
}

type S = Box[int] | Box[string]

fn score(s S) int {
	return match s {
		Box[int] { 10 + s.value }
		Box[string] { 100 + s.value.len }
	}
}

fn main() {
	s := S(Box[string]{
		value: "ok"
	})
	assert s is Box[string]
	assert !(s is Box[int])
	b := s as Box[string]
	assert b.value == "ok"
	assert score(s) == 102

	i := S(Box[int]{
		value: 7
	})
	assert i is Box[int]
	assert !(i is Box[string])
	assert score(i) == 17
	println("ok")
}
	') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_generic_sum_type_distinct_tags_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_generic_sum_type_param_variant_in_imported_module() {
	v3_bin := generic_sum_type_build_v3()
	root := os.join_path(os.temp_dir(), 'v3_generic_sum_type_param_variant_${os.getpid()}')
	os.rmdir_all(root) or {}
	maybe_dir := os.join_path(root, 'maybe')
	os.mkdir_all(maybe_dir) or { panic(err) }
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
') or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, 'module main

import maybe

fn main() {
	println(maybe.describe(maybe.wrap_int(7)))
	println(maybe.describe(maybe.wrap_err()))
	println(maybe.unwrap_int(maybe.wrap_int(7)))
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_generic_sum_type_param_variant_bin_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'int\nerr\n7'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('maybe__Err'), c_code
	assert !c_code.contains('maybe__Maybe_int y = x'), c_code
	assert !c_code.contains('maybe__T'), c_code
	assert !c_code.contains('x == v_int'), c_code
}

fn test_generic_sum_type_args_keep_non_main_caller_scope_and_selective_imports() {
	v3_bin := generic_sum_type_build_v3()
	root := os.join_path(os.temp_dir(), 'v3_generic_sum_type_caller_scope_${os.getpid()}')
	os.rmdir_all(root) or {}
	maybe_dir := os.join_path(root, 'maybe')
	payload_dir := os.join_path(root, 'payload')
	client_dir := os.join_path(root, 'client')
	os.mkdir_all(maybe_dir) or { panic(err) }
	os.mkdir_all(payload_dir) or { panic(err) }
	os.mkdir_all(client_dir) or { panic(err) }
	os.write_file(os.join_path(maybe_dir, 'maybe.v'), 'module maybe

pub struct Err {}

pub type Maybe[T] = T | Err

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

fn main() {
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

	bin := os.join_path(os.temp_dir(), 'v3_generic_sum_type_caller_scope_bin_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	c_code := os.read_file(bin + '.c') or { panic(err) }
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
