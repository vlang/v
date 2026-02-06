type Tree = Empty | Node

struct Empty {}

struct Node {
	value int
	left  Tree
	right Tree
}

// Note: a match expression, infers the type of its result
// from the type of the return value in the first branch,
// => it needs an explicit int(0) cast here:
fn size(tree Tree) int {
	return match tree {
		Empty { int(0) }
		Node { 1 + size(tree.left) + size(tree.right) }
	}
}

fn main() {
	node1 := Node{30, Empty{}, Empty{}}
	node2 := Node{20, Empty{}, Empty{}}
	tree := Node{10, node1, node2}
	println('tree structure:\n ${tree}')
	println('tree size: ${size(tree)}')
}
