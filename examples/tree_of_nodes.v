type Tree = Leaf | Node

struct Leaf {}

struct Node {
	value int
	left  Tree
	right Tree
}

// NB: a match expression, infers the type of its result
// from the type of the return value in the first branch,
// => it needs an explicit int(0) cast here:
fn size(tree Tree) int {
	return match tree {
		Leaf { int(0) }
		Node { 1 + size(tree.left) + size(tree.right) }
	}
}

fn main() {
	node1 := Node{30, Leaf{}, Leaf{}}
	node2 := Node{20, Leaf{}, Leaf{}}
	tree := Node{10, node1, node2}
	println('tree structure:\n $tree')
	println('tree size: ${size(tree)}')
}
