// Unwrapping an option-of-pointer struct field (`?&T`) after a `== nil` /
// `== none` check and accessing a member must emit a single dereference, the
// same as the smartcast (`if p := ... {}`) form. The option's `.data` buffer
// holds a single `&T`, so `*(T**)(data)` yields the pointer. See issue #27549.
struct SessionNode {
	id     string
	parent ?&SessionNode
}

fn parent_id_nil_check(node &SessionNode) string {
	return unsafe {
		if node.parent == nil {
			'root'
		} else {
			node.parent.id
		}
	}
}

fn parent_id_smartcast(node &SessionNode) string {
	if p := node.parent {
		return p.id
	}
	return 'root'
}

fn test_option_ptr_field_nil_check_unwrap() {
	root := &SessionNode{
		id: 'root-node'
	}
	child := &SessionNode{
		id:     'child'
		parent: root
	}

	// the `== nil` ternary form must agree with the smartcast form
	assert parent_id_nil_check(child) == 'root-node'
	assert parent_id_nil_check(root) == 'root'

	assert parent_id_smartcast(child) == 'root-node'
	assert parent_id_smartcast(root) == 'root'
}
