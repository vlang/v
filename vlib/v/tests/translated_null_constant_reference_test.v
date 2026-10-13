@[has_globals; translated]
module main

// C code translated by c2v returns and stores `0` for a null pointer. Like the null
// cast `&Node(0)`, the null constants stay accepted in a translated file.
struct Node {
	x int
}

__global translated_global_null &Node = 0

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

fn test_null_constants_as_references_in_a_translated_file() {
	assert node_is_null(translated_global_null)
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
