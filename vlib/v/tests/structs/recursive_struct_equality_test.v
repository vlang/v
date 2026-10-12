import os

struct RecursiveEqualityNode {
	name     string
	children []RecursiveEqualityNode
}

fn recursive_equality_tree(depth int, leaf string) RecursiveEqualityNode {
	if depth == 0 {
		return RecursiveEqualityNode{ name: [leaf, ''].join('') }
	}
	return RecursiveEqualityNode{
		name:     ['branch', depth.str()].join('')
		children: [recursive_equality_tree(depth - 1, leaf)]
	}
}

fn test_recursive_struct_equality_compares_content_at_every_depth() {
	for depth in [1, 2, 5] {
		a := recursive_equality_tree(depth, ['le', 'af'].join(''))
		b := recursive_equality_tree(depth, ['l', 'eaf'].join(''))
		different := recursive_equality_tree(depth, 'other')
		assert a == b
		assert !(a != b)
		assert a != different
		assert !(a == different)
		assert [a] == [b]
		assert a in [b]
		left_map := {
			'node': a
		}
		right_map := {
			'node': b
		}
		different_map := {
			'node': different
		}
		assert left_map == right_map
		assert left_map != different_map
	}
}

struct RecursiveEqualityParent {
	name     string
	branches []RecursiveEqualityBranch
}

struct RecursiveEqualityBranch {
	value   string
	parents []RecursiveEqualityParent
}

fn mutually_recursive_equality_tree(value string) RecursiveEqualityParent {
	return RecursiveEqualityParent{
		name:     'root'
		branches: [RecursiveEqualityBranch{
			value:   'branch'
			parents: [RecursiveEqualityParent{
				name: [value, ''].join('')
			}]
		}]
	}
}

fn test_mutually_recursive_struct_equality_compares_allocated_strings() {
	a := mutually_recursive_equality_tree(['le', 'af'].join(''))
	b := mutually_recursive_equality_tree(['l', 'eaf'].join(''))
	assert a == b
	assert a != mutually_recursive_equality_tree('other')
}

fn test_recursive_struct_equality_keeps_imported_generic_type_context() {
	root := os.join_path(os.vtmp_dir(), 'recursive_struct_equality_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'tree'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'tree', 'tree.v'), 'module tree
pub struct Node[T] {
pub:
 name string
 value T
 children []Node[T]
}
pub fn make[T](value T, name string) Node[T] {
 return Node[T]{name: "root", children: [Node[T]{name: name, value: value}]}
}
pub fn equal[T](a Node[T], b Node[T]) bool { return a == b }
')!
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'module main
import tree
fn main() {
 a := tree.make(7, ["le", "af"].join(""))
 b := tree.make(7, ["l", "eaf"].join(""))
 assert a == b
 assert tree.equal(a, b)
 assert a != tree.make(8, "leaf")
 assert a != tree.make(7, "other")
}
')!
	result := os.exec([@VEXE, '-b', 'c', 'run', path])
	assert result.exit_code == 0, result.output
}
