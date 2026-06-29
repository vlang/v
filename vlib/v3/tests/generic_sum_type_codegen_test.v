import os

const generic_sum_type_vexe = @VEXE
const generic_sum_type_tests_dir = os.dir(@FILE)
const generic_sum_type_v3_dir = os.dir(generic_sum_type_tests_dir)
const generic_sum_type_vlib_dir = os.dir(generic_sum_type_v3_dir)
const generic_sum_type_v3_src = os.join_path(generic_sum_type_v3_dir, 'v3.v')

fn generic_sum_type_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_sum_type_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${generic_sum_type_vexe} -gc none -path "${generic_sum_type_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${generic_sum_type_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
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
	assert !c_code.contains('Tree_int ints = (Tree){'), c_code
	assert !c_code.contains('Tree_f64 floats = (Tree){'), c_code
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
