// Entries of a map clone may be replaced after the clone is passed as `mut` to a
// function that only reads or replaces its entries (here recursively).
struct CloneElem {
	id int
}

fn clone_elem_created(depth int, mut by_key map[string]&CloneElem) ?&CloneElem {
	key := 'k${depth}'
	e := by_key[key] or { &CloneElem{depth} }
	if depth > 0 {
		clone_elem_created(depth - 1, mut by_key) or { return none }
	}
	by_key[key] = e
	return e
}

fn clone_elem_apply(elements map[string]&CloneElem) int {
	mut by_key := elements.clone()
	by_key['x'] = &CloneElem{9}
	clone_elem_created(2, mut by_key) or { return 0 }
	by_key['y'] = &CloneElem{10}
	return by_key.len
}

fn test_map_clone_passed_as_mut_to_entry_writer() {
	elements := {
		'a': &CloneElem{1}
	}
	assert clone_elem_apply(elements) == 6
	assert elements.len == 1
}
