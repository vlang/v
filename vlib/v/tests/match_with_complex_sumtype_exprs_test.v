struct Empty {}

struct Node {
	value f64
	left  Tree
	right Tree
}

type Tree = Empty | Node

// return size(number of nodes) of BST
fn size(tree Tree) int {
	return match tree {
		// TODO: remove int() once match gets smarter
		Empty { int(0) }
		Node { 1 + size(tree.left) + size(tree.right) }
	}
}

// insert a value to BST
fn insert(tree Tree, x f64) Tree {
	return match tree {
		Empty {
			Node{x, tree, tree}
		}
		Node {
			if x == tree.value {
				tree
			} else if x < tree.value {
				Node{
					...tree
					left: insert(tree.left, x)
				}
			} else {
				Node{
					...tree
					right: insert(tree.right, x)
				}
			}
		}
	}
}

// whether able to find a value in BST
fn search(tree Tree, x f64) bool {
	return match tree {
		Empty {
			false
		}
		Node {
			if x == tree.value {
				true
			} else if x < tree.value {
				search(tree.left, x)
			} else {
				search(tree.right, x)
			}
		}
	}
}

// find the minimal value of a BST
fn min(tree Tree) f64 {
	return match tree {
		Empty {
			1e100
		}
		Node {
			if tree.value < min(tree.left) {
				tree.value
			} else {
				min(tree.left)
			}
		}
	}
}

// delete a value in BST (if nonexistant do nothing)
fn delete(tree Tree, x f64) Tree {
	return match tree {
		Empty {
			tree
		}
		Node {
			if tree.left is Node && tree.right is Node {
				if x < tree.value {
					Node{
						...tree
						left: delete(tree.left, x)
					}
				} else if x > tree.value {
					Node{
						...tree
						right: delete(tree.right, x)
					}
				} else {
					Node{
						...tree
						value: min(tree.right)
						right: delete(tree.right, min(tree.right))
					}
				}
			} else if tree.left is Node {
				if x == tree.value {
					tree.left
				} else {
					Node{
						...tree
						left: delete(tree.left, x)
					}
				}
			} else {
				if x == tree.value {
					tree.right
				} else {
					Node{
						...tree
						right: delete(tree.right, x)
					}
				}
			}
		}
	}
}

fn test_match_with_complex_sumtype_exprs() {
	mut tree := Tree(Empty{})
	input := [0.3, 0.2, 0.5, 0.0, 0.6, 0.8, 0.9, 1.0, 0.1, 0.4, 0.7]
	for i in input {
		tree = insert(tree, i)
	}
	print('[1] after insertion tree size is ') // 11
	println(size(tree))
	del := [-0.3, 0.0, 0.3, 0.6, 1.0, 1.5]
	for i in del {
		tree = delete(tree, i)
	}
	print('[2] after deletion tree size is ') // 7
	print(size(tree))
	print(', and these elements were deleted: ') // 0.0 0.3 0.6 1.0
	assert size(tree) == 7
	for i in input {
		if !search(tree, i) {
			print(i)
			print(' ')
		}
	}
	println('')
	assert true
}
