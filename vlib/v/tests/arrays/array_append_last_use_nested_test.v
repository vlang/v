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

fn test_live_mutable_nested_append_shares_storage() {
	mut value := LastAppendNode{
		children: [LastAppendNode{ n: 1 }]
	}
	mut out := []LastAppendNode{}
	out << value
	value.children[0].n = 2
	assert out[0].children[0].n == 2
}

fn last_append_identity(value LastAppendNode) LastAppendNode {
	return value
}

struct LastAppendHolder {
	value LastAppendNode
}

fn test_append_call_and_field_share_external_storage() {
	mut original := LastAppendNode{ children: [LastAppendNode{ n: 1 }] }
	value := last_append_identity(original)
	mut called := []LastAppendNode{}
	called << value
	holder := LastAppendHolder{ value: original }
	mut literal := []LastAppendNode{}
	literal << holder.value
	original.children[0].n = 2
	assert called[0].children[0].n == 2
	assert literal[0].children[0].n == 2
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

fn test_append_preserves_shared_fields() {
	value := last_append_shared()
	mut out := []LastAppendShared{}
	out << value
	out[0].left[0] = 2
	assert out[0].right[0] == 2
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

fn test_append_only_runs_explicit_clone_calls() {
	value := last_append_custom()
	mut out := []LastAppendCustom{}
	out << value
	assert out[0].n == 1
	out << value.clone()
	assert out[1].n == 11
}

fn test_append_matches_assignment_literal_and_bulk_append() {
	mut value := LastAppendNode{ children: [LastAppendNode{ n: 1 }] }
	copied := value
	literal := [value]
	mut single := []LastAppendNode{}
	single << value
	mut bulk := []LastAppendNode{}
	bulk << [value]
	value.children[0].n = 2
	assert copied.children[0].n == 2
	assert literal[0].children[0].n == 2
	assert single[0].children[0].n == 2
	assert bulk[0].children[0].n == 2
	value.n = 10
	assert single[0].n == 0
}

fn test_append_copies_array_headers() {
	mut value := LastAppendNode{ children: []LastAppendNode{cap: 4} }
	value.children << LastAppendNode{ n: 1 }
	mut out := []LastAppendNode{}
	out << value
	value.children << LastAppendNode{ n: 2 }
	assert value.children.len == 2
	assert out[0].children.len == 1
	value.children[0].n = 3
	assert out[0].children[0].n == 3
	value.children = [LastAppendNode{ n: 4 }]
	assert out[0].children[0].n == 3
}

fn test_repeated_append_shares_nested_storage() {
	value := LastAppendNode{ children: [LastAppendNode{ n: 1 }] }
	mut out := []LastAppendNode{}
	for _ in 0 .. 2 {
		out << value
	}
	out[0].children[0].n = 2
	assert out[1].children[0].n == 2
	assert value.children[0].n == 2
}

fn test_append_indexed_struct_shares_nested_storage() {
	mut values := [LastAppendNode{ children: [LastAppendNode{ n: 1 }] }]
	mut out := []LastAppendNode{}
	out << values[0]
	values[0].children[0].n = 2
	assert out[0].children[0].n == 2
}

fn test_append_to_map_array_shares_nested_storage() {
	mut value := LastAppendNode{ children: [LastAppendNode{ n: 1 }] }
	mut out := map[string][]LastAppendNode{}
	out['nodes'] << value
	value.children[0].n = 2
	assert out['nodes'][0].children[0].n == 2
}

fn test_append_array_element_shares_storage() {
	mut row := [1]
	mut out := [][]int{}
	out << row
	row[0] = 2
	assert out[0][0] == 2
}

fn test_append_explicit_array_clone_is_independent() {
	mut row := [1]
	mut out := [][]int{}
	out << row.clone()
	row[0] = 2
	assert out[0][0] == 1
}

fn test_append_fixed_array_element_copies_inline_storage() {
	mut row := [1, 2]!
	mut out := [][2]int{}
	out << row
	row[0] = 3
	assert out[0] == [1, 2]!
}

fn test_append_to_optional_array_shares_nested_storage() {
	mut value := LastAppendNode{ children: [LastAppendNode{ n: 1 }] }
	mut out := ?[]LastAppendNode([]LastAppendNode{})
	out? << value
	value.children[0].n = 2
	assert out?[0].children[0].n == 2
}

fn test_append_pointer_source_shares_nested_storage() {
	mut holder := &LastAppendNode{ children: [LastAppendNode{ n: 1 }] }
	mut out := []LastAppendNode{}
	out << *holder
	holder.children[0].n = 2
	assert out[0].children[0].n == 2
}
