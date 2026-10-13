@[has_globals]
module main

// A `.c.v` file is C interop code: like the null cast `&Node(0)`, the null constants
// `0` and `voidptr(0)` stay accepted in it where a reference is expected.
struct Node {
	x int
}

__global c_global_null &Node = 0

struct Holder {
mut:
	node &Node = voidptr(0)
}

fn null_node() &Node {
	return voidptr(0)
}

fn zero_node() &Node {
	return 0
}

fn node_is_null(node &Node) bool {
	return node == voidptr(0)
}

fn test_null_constants_as_references_in_a_c_file() {
	assert node_is_null(c_global_null)
	assert node_is_null(null_node())
	assert node_is_null(zero_node())
	assert node_is_null(voidptr(0))
	assert node_is_null(&Node(0))
	mut h := Holder{}
	assert node_is_null(h.node)
	h.node = &Node{
		x: 1
	}
	assert !node_is_null(h.node)
	h.node = voidptr(0)
	assert node_is_null(h.node)
}
