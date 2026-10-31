struct TreeNode {
mut:
	children &map[string]int = unsafe { nil }
}

fn test_in_works_on_a_map_reference() {
	mut node := &TreeNode{
		children: &map[string]int{}
	}
	unsafe {
		(*node.children)['a'] = 1
	}
	assert 'a' in node.children
	assert 'b' !in node.children
	assert !('b' in node.children)
	assert !('a' !in node.children)
}

fn count_present(m &map[string]int, keys []string) int {
	mut n := 0
	for key in keys {
		if key in m {
			n++
		}
	}
	return n
}

fn test_in_works_on_a_map_reference_parameter() {
	m := {
		'x': 1
		'y': 2
	}
	assert count_present(&m, ['x', 'z', 'y']) == 2
}
