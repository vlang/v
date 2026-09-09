// vtest vflags: -autofree
// vtest build: !sanitize-address-gcc && !sanitize-address-clang

type Tree = Empty | Node

struct Empty {}

struct Node {
	value int
	left  Tree
	right Tree
}

fn (tree Tree) delete(x int) Tree {
	return match tree {
		Empty {
			tree
		}
		Node {
			if tree.left !is Empty && tree.right !is Empty {
				if x < tree.value {
					Node{
						...tree
						left: tree.left.delete(x)
					}
				} else {
					tree
				}
			} else {
				tree
			}
		}
	}
}

fn test_autofree_match() {
	mut tree := Tree(Empty{})
	tree = tree.delete(5)
	assert tree is Empty
}
