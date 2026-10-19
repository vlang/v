// An `or {}` block on an option that a none check already proved present must
// use the payload, even when the payload struct has its own field named `value`.
@[heap]
struct ValueNode {
	value  int
	parent ?&ValueNode
}

fn parent_value_in_branch(n &ValueNode) int {
	if n.parent != none {
		p := n.parent or { return -1 }
		return p.value
	}
	return 0
}

fn parent_value_after_guard(n &ValueNode) int {
	if n.parent == none {
		return 0
	}
	p := n.parent or { return -1 }
	return p.value
}

fn test_or_block_on_narrowed_selector_with_value_field() {
	leaf := &ValueNode{
		value:  1
		parent: &ValueNode{
			value: 2
		}
	}
	root := &ValueNode{
		value: 5
	}
	assert parent_value_in_branch(leaf) == 2
	assert parent_value_in_branch(root) == 0
	assert parent_value_after_guard(leaf) == 2
	assert parent_value_after_guard(root) == 0
}
