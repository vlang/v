struct TreeFile {
	name string
	size int
}

struct TreeDirectory {
	name string
mut:
	left  FileTree
	right FileTree
}

type FileTree = TreeDirectory | TreeFile

fn tree_size(tree FileTree) int {
	return match tree {
		TreeFile { tree.size }
		TreeDirectory { tree_size(tree.left) + tree_size(tree.right) }
	}
}

fn test_recursive_sum_tree_str_keeps_distinct_nested_objects() {
	left := TreeDirectory{'documents', TreeFile{'syntax.txt', 250}, TreeDirectory{'docs', TreeFile{'picture.jpg', 137}, TreeDirectory{'docs', TreeFile{'comments.md', 350}, TreeDirectory{'docs', TreeFile{'end.md', 220}, TreeFile{'background.jpg', 220}}}}}
	right := TreeDirectory{'music', TreeFile{'melody.mp3', 250}, TreeFile{'classic.mp3', 900}}
	tree := TreeDirectory{'home', left, right}
	text := tree.str()
	assert !text.contains('<circular>'), text
	for name in ['syntax.txt', 'picture.jpg', 'comments.md', 'end.md', 'background.jpg', 'melody.mp3',
		'classic.mp3'] {
		assert text.contains(name), text
	}
	assert tree_size(tree) == 2327
	assert tree.str() == text
}

fn test_recursive_sum_tree_str_stops_actual_object_cycles() {
	mut tree := FileTree(TreeDirectory{'cycle', TreeFile{'leaf', 1}, TreeFile{'other', 2}})
	cycle := tree
	if mut tree is TreeDirectory { tree.left = cycle }
	text := tree.str()
	assert text.contains('<circular>'), text
	assert text.contains('other'), text
	assert text.len < 4000
	assert tree.str() == text
}

fn test_recursive_sum_tree_str_prints_shared_siblings() {
	shared := FileTree(TreeDirectory{'shared', TreeFile{'leaf', 1}, TreeFile{'other', 2}})
	root := TreeDirectory{'root', shared, shared}
	text := root.str()
	assert !text.contains('<circular>'), text
	assert text.count("name: 'shared'") == 2, text
}
