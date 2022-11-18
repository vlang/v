struct Empty {}

struct Node<T> {
	value T
	left  Tree<T>
	right Tree<T>
}

type Tree<T> = Empty | Node<T>

// return size(number of nodes) of BST
fn size<T>(tree Tree<T>) int {
	return match tree {
		Empty { 0 }
		Node<T> { 1 + size<T>(tree.left) + size<T>(tree.right) }
	}
}

// insert a value to BST
fn insert<T>(tree Tree<T>, x T) Tree<T> {
	return match tree {
		Empty {
			Node<T>{x, tree, tree}
		}
		Node<T> {
			if x == tree.value {
				tree
			} else if x < tree.value {
				Node<T>{
					...tree
					left: insert<T>(tree.left, x)
				}
			} else {
				Node<T>{
					...tree
					right: insert<T>(tree.right, x)
				}
			}
		}
	}
}

// whether able to find a value in BST
fn search<T>(tree Tree<T>, x T) bool {
	return match tree {
		Empty {
			false
		}
		Node<T> {
			if x == tree.value {
				true
			} else if x < tree.value {
				search<T>(tree.left, x)
			} else {
				search<T>(tree.right, x)
			}
		}
	}
}

// find the minimal value of a BST
fn min<T>(tree Tree<T>) T {
	return match tree {
		Empty {
			1e100
		}
		Node<T> {
			if tree.value < min<T>(tree.left) {
				tree.value
			} else {
				min<T>(tree.left)
			}
		}
	}
}

// delete a value in BST (if nonexistant do nothing)
fn delete<T>(tree Tree<T>, x T) Tree<T> {
	return match tree {
		Empty {
			tree
		}
		Node<T> {
			if tree.left is Node<T> && tree.right is Node<T> {
				if x < tree.value {
					Node<T>{
						...tree
						left: delete<T>(tree.left, x)
					}
				} else if x > tree.value {
					Node<T>{
						...tree
						right: delete<T>(tree.right, x)
					}
				} else {
					Node<T>{
						...tree
						value: min<T>(tree.right)
						right: delete<T>(tree.right, min<T>(tree.right))
					}
				}
			} else if tree.left is Node<T> {
				if x == tree.value {
					tree.left
				} else {
					Node<T>{
						...tree
						left: delete<T>(tree.left, x)
					}
				}
			} else {
				if x == tree.value {
					tree.right
				} else {
					Node<T>{
						...tree
						right: delete<T>(tree.right, x)
					}
				}
			}
		}
	}
}

fn test_generics_complex_sumtype() {
	mut tree := Tree<f64>(Empty{})
	input := [0.3, 0.2, 0.5, 0.0, 0.6, 0.8, 0.9, 1.0, 0.1, 0.4, 0.7]
	for i in input {
		tree = insert(tree, i)
	}
	println('[1] after insertion tree size is ${size(tree)}') // 11
	del := [-0.3, 0.0, 0.3, 0.6, 1.0, 1.5]
	for i in del {
		tree = delete(tree, i)
	}
	print('[2] after deletion tree size is ${size(tree)}, ') // 7
	print('and these elements were deleted: ') // 0.0 0.3 0.6 1.0
	for i in input {
		if !search(tree, i) {
			print('${i} ')
		}
	}
	println('')
	assert size(tree) == 7
}
