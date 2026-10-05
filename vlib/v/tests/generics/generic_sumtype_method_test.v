struct Empty {}

struct Node[T] {
	value T
	left  &Leaf[T]
	right &Leaf[T]
}

type Leaf[T] = Empty | Node[T]

// return size(number of nodes) of BST
fn (leaf Leaf[T]) size[T]() int {
	return match leaf {
		Empty { 0 }
		Node[T] { 1 + leaf.left.size() + leaf.right.size() }
	}
}

fn test_generic_sumtype_method() {
	empty := Leaf[int](Empty{})
	r := Leaf[int](Node[int]{
		value: 20
		left:  &empty
		right: &empty
	})
	tree := Leaf[int](Node[int]{
		value: 10
		left:  &empty
		right: &r
	})
	println(tree.size())
	assert tree.size() == 2
}
