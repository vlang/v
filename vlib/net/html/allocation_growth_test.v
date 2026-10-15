module html

fn test_tree_growth_keeps_parent_and_child_indices() {
	mut tree := BTree{}
	assert tree.add_children(Tag{ name: 'root' }) == 0
	assert tree.add_children(Tag{ name: 'first' }) == 1
	tree.move_pointer(1)
	assert tree.add_children(Tag{ name: 'leaf' }) == 2
	tree.move_pointer(0)
	assert tree.add_children(Tag{ name: 'second' }) == 3
	assert tree.get_children() == [1, 3]
	tree.move_pointer(1)
	assert tree.get_children() == [2]
	assert tree.get_parent() == 0
	tree.move_pointer(2)
	assert tree.get_parent() == 1
	assert tree.get_stored().name == 'leaf'
	tree.move_pointer(3)
	assert tree.get_parent() == 0
}

fn allocation_growth_bytes(fragment string, count int) usize {
	page := fragment.repeat(count)
	before := gc_heap_usage().total_bytes
	doc := parse(page)
	allocated := gc_heap_usage().total_bytes - before
	assert doc.get_tags(name: 'div').len == count
	return allocated
}

fn test_parse_allocations_grow_with_input_size() {
	$if gcboehm ? {
		_ = parse('<div>text</div>')
		for fragment in ['<div>text</div>', '<div><span>t</span></div>'] {
			small := allocation_growth_bytes(fragment, 256)
			large := allocation_growth_bytes(fragment, 1024)
			assert small > 0
			// Four times the input may grow backing capacities, but must not copy
			// the complete parent/child tables on every inserted node.
			assert large < small * 6, '${fragment}: ${small} -> ${large} allocated bytes'
		}
	}
}
