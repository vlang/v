struct LastAppendNode {
mut:
	children []LastAppendNode
	n        int
}

fn last_append_tree(level int) LastAppendNode {
	if level == 0 {
		return LastAppendNode{
			n: 1
		}
	}
	left := last_append_tree(level - 1)
	right := last_append_tree(level - 1)
	mut children := []LastAppendNode{}
	children << left
	children << right
	return LastAppendNode{
		children: children
	}
}

fn last_append_leaves(node LastAppendNode) int {
	if node.children.len == 0 {
		return node.n
	}
	mut total := 0
	for child in node.children {
		total += last_append_leaves(child)
	}
	return total
}

fn test_last_use_nested_append_preserves_tree_values() {
	root := last_append_tree(8)
	assert last_append_leaves(root) == 256
	assert root.children.len == 2
}

fn test_live_mutable_nested_append_keeps_independent_storage() {
	mut value := LastAppendNode{
		children: [LastAppendNode{ n: 1 }]
	}
	mut out := []LastAppendNode{}
	out << value
	value.children[0].n = 2
	assert out[0].children[0].n == 1
}

fn last_append_identity(value LastAppendNode) LastAppendNode {
	return value
}

struct LastAppendHolder {
	value LastAppendNode
}

fn test_last_use_call_and_literal_keep_external_aliases_detached() {
	mut original := LastAppendNode{ children: [LastAppendNode{ n: 1 }] }
	value := last_append_identity(original)
	mut called := []LastAppendNode{}
	called << value
	holder := LastAppendHolder{ value: original }
	mut literal := []LastAppendNode{}
	literal << holder.value
	original.children[0].n = 2
	assert called[0].children[0].n == 1
	assert literal[0].children[0].n == 1
}

struct LastAppendShared {
mut:
	left  []int
	right []int
}

fn last_append_shared() LastAppendShared {
	values := [1]
	return LastAppendShared{ left: values, right: values }
}

fn test_last_use_factory_keeps_shared_fields_detached() {
	value := last_append_shared()
	mut out := []LastAppendShared{}
	out << value
	out[0].left[0] = 2
	assert out[0].right[0] == 1
}

struct LastAppendCustom {
	n int
}

fn (value LastAppendCustom) clone() LastAppendCustom {
	return LastAppendCustom{ n: value.n + 10 }
}

fn last_append_custom() LastAppendCustom {
	return LastAppendCustom{ n: 1 }
}

fn test_last_use_append_runs_explicit_clone_methods() {
	value := last_append_custom()
	mut out := []LastAppendCustom{}
	out << value
	assert out[0].n == 11
}
