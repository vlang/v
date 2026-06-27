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
