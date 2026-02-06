struct Node {
pub mut:
	parent ?&Node
	id     int
}

fn set_trace(n &Node) int {
	if n.parent != none {
		set_trace(n.parent)
		assert n.id != 0
	} else {
		assert n.id == 1
	}
	return n.id
}

fn test_main() {
	mut initial_node := &Node{
		parent: none
		id:     1
	}
	mut child_node := &Node{
		parent: initial_node
		id:     2
	}
	assert set_trace(child_node) == 2
}
