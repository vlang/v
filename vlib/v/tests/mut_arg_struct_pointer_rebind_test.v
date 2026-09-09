module main

@[heap]
struct Node {
mut:
	children []&Node
}

@[heap]
struct Tree {
mut:
	root &Node
}

fn find_leaf(mut node Node) &Node {
	for {
		if node.children.len == 0 {
			return node
		}
		node = node.children[0]
	}
	return node
}

fn test_mut_struct_arg_passed_as_pointer_keeps_original_tree_root() {
	mut n1 := &Node{}
	mut t := &Tree{
		root: n1
	}

	mut n2 := &Node{}
	t.root.children << n2
	mut n3 := &Node{}
	t.root.children[0].children << n3

	found := find_leaf(mut t.root)
	assert found == n3
	assert t.root.children.len == 1
	assert t.root.children[0] == n2
	assert t.root.children[0].children.len == 1
	assert t.root.children[0].children[0] == n3
}
