struct TreeFile {
	name string
	size int
}

struct TreeDirectory {
	name string
mut:
	left  PrintTree
	right PrintTree
}

type PrintTree = TreeDirectory | TreeFile

fn test_recursive_sumtype_stringifies_distinct_payloads() {
	mut tree := PrintTree(TreeFile{'leaf.txt', 220})
	for i in 0 .. 8 {
		tree = TreeDirectory{'level${i}', TreeFile{'file${i}.txt', i}, tree}
	}
	text := tree.str()
	assert !text.contains('<circular>'), text
	assert !text.contains('unknown sum type value'), text
	assert text.contains("name: 'leaf.txt'"), text
	assert text.contains('size: 220'), text
	for i in 0 .. 8 {
		assert text.contains("name: 'level${i}'"), text
		assert text.contains("name: 'file${i}.txt'"), text
	}
	assert '${tree}' == text
	directory := TreeDirectory{'root', tree, tree}
	directory_text := directory.str()
	assert !directory_text.contains('<circular>'), directory_text
	assert directory_text.count("name: 'leaf.txt'") == 2, directory_text
}

fn test_recursive_sumtype_shared_payload_is_not_circular() {
	leaf := PrintTree(TreeDirectory{'shared', TreeFile{'left', 1}, TreeFile{'right', 2}})
	tree := PrintTree(TreeDirectory{'root', leaf, leaf})
	text := tree.str()
	assert !text.contains('<circular>'), text
	assert text.count("name: 'shared'") == 2, text
}

fn test_recursive_sumtype_actual_cycle_stops() {
	mut tree := PrintTree(TreeDirectory{'cycle', TreeFile{'left', 1}, TreeFile{'right', 2}})
	back_reference := tree
	match mut tree {
		TreeDirectory { tree.right = back_reference }
		TreeFile {}
	}
	text := tree.str()
	assert text.contains('<circular>'), text
	assert text.len < 1000, text
	assert text.contains("name: 'left'"), text
}
