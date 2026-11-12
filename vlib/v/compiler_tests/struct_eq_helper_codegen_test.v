import os

const struct_eq_vexe = @VEXE

const struct_eq_source = "module main

struct Leaf {
	name   string
	values []int
}

struct Branch {
	left  Leaf
	right ?&Leaf
	tags  map[string]int
}

struct Tree {
mut:
	root     Branch
	branches []Branch
	id       int
}

struct Point {
	x int
	y int
}

struct NeverCompared {
	inner Branch
}

// Big is larger than tcc's arm64 backend can pass by value.
struct Big {
mut:
	bytes [5000]u8
	leaf  Leaf
}

fn unused_compare(a NeverCompared, b NeverCompared) bool {
	return a == b
}

fn main() {
	leaf := Leaf{
		name:   'a'
		values: [1, 2]
	}
	other := Leaf{
		name:   'a'
		values: [1, 2]
	}
	t1 := Tree{
		root:     Branch{
			left:  leaf
			right: &other
			tags:  {
				'k': 1
			}
		}
		branches: [Branch{
			left: leaf
		}]
		id:       7
	}
	t2 := Tree{
		root:     Branch{
			left:  other
			right: &leaf
			tags:  {
				'k': 1
			}
		}
		branches: [Branch{
			left: other
		}]
		id:       7
	}
	mut t3 := t2
	t3.branches = [Branch{
		left: Leaf{
			name: 'b'
		}
	}]
	p1 := Point{1, 2}
	p2 := Point{1, 2}
	never := NeverCompared{}
	mut big1 := &Big{
		leaf: leaf
	}
	mut big2 := &Big{
		leaf: other
	}
	println(t1 == t2)
	println(t1 != t3)
	println(t1.root == t2.root)
	println(p1 == p2)
	println(never.inner.left.name.len)
	println(big1 == big2)
	big2.bytes[4999] = 1
	println(big1 == big2)
	big1.bytes[4999] = 1
	big1.leaf = Leaf{
		values: [3]
	}
	println(big1 == big2)
}
"

fn struct_eq_paths() (string, string, string) {
	dir := os.join_path(os.vtmp_dir(), 'struct_eq_helper_codegen_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	return os.join_path(dir, 'main.v'), os.join_path(dir, 'main.c'), os.join_path(dir, 'main')
}

// struct_eq_c_function returns the body of a generated helper, skipping its prototype.
fn struct_eq_c_function(source string, name string) string {
	mut pos := 0
	for {
		start := source.index_after('bool ${name}(', pos) or { return '' }
		line_end := source.index_after('\n', start) or { return '' }
		if source[start..line_end].trim_space().ends_with('{') {
			end := source.index_after('\n}\n', line_end) or { return '' }
			return source[line_end..end]
		}
		pos = line_end
	}
	return ''
}

fn test_struct_equality_compares_nested_aggregates_through_helpers() {
	src, c_file, _ := struct_eq_paths()
	os.write_file(src, struct_eq_source) or { panic(err) }
	compile := os.exec([struct_eq_vexe, '-nocache', '-o', c_file, src])
	assert compile.exit_code == 0, compile.output
	c := os.read_file(c_file) or { panic(err) }
	tree := struct_eq_c_function(c, '__v3_struct_eq_Tree__v3_program')
	branch := struct_eq_c_function(c, '__v3_struct_eq_Branch__v3_program')
	leaf := struct_eq_c_function(c, '__v3_struct_eq_Leaf__v3_program')
	assert tree.len > 0, 'missing Tree helper'
	assert branch.len > 0, 'missing Branch helper'
	assert leaf.len > 0, 'missing Leaf helper'
	// Helpers take both structs by reference instead of copying them.
	assert c.contains('bool __v3_struct_eq_Tree__v3_program(main__Tree* __struct_eq_a, main__Tree* __struct_eq_b)')
	// Each helper compares its own fields and calls the helper of a nested struct
	// instead of expanding the nested fields again.
	assert tree.contains('__v3_struct_eq_Branch__v3_program(')
	assert !tree.contains('values') && !tree.contains('tags')
	assert branch.contains('__v3_struct_eq_Leaf__v3_program(')
	assert !branch.contains('values')
	assert leaf.contains('values')
	// A flat struct stays inline, and a struct compared only in an unused
	// function gets no helper.
	assert !c.contains('__v3_struct_eq_Point')
	assert !c.contains('__v3_struct_eq_NeverCompared')
}

fn test_struct_equality_through_helpers_keeps_deep_semantics() {
	src, _, bin := struct_eq_paths()
	os.write_file(src, struct_eq_source) or { panic(err) }
	compile := os.exec([struct_eq_vexe, '-nocache', '-o', bin, src])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	// Different `right` pointers to equal leaves compare equal by value.
	assert run.output.trim_space().split_into_lines() == ['true', 'true', 'true', 'true', '0',
		'true', 'false', 'false']
}
