// vtest vflags: -autofree

type Tree[T] = Empty | Node[T]

struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

fn (tree Tree[T]) delete[T](x T) Tree[T] {
	return match tree {
		Empty {
			tree
		}
		Node[T] {
			if tree.left !is Empty && tree.right !is Empty {
				if x < tree.value {
					Node[T]{
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
	mut tree := Tree[int](Empty{})
	tree = tree.delete(5)
	assert tree is Empty
}
