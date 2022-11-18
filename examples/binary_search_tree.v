// Binary Search Tree example by @SleepyRoy

struct Empty {}

struct Node<T> {
	value T
	left  Tree<T>
	right Tree<T>
}

type Tree<T> = Empty | Node<T>

// return size(number of nodes) of BST
fn (tree Tree<T>) size<T>() int {
	return match tree {
		Empty { 0 }
		Node<T> { 1 + tree.left.size() + tree.right.size() }
	}
}

// insert a value to BST
fn (tree Tree<T>) insert<T>(x T) Tree<T> {
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
					left: tree.left.insert(x)
				}
			} else {
				Node<T>{
					...tree
					right: tree.right.insert(x)
				}
			}
		}
	}
}

// whether able to find a value in BST
fn (tree Tree<T>) search<T>(x T) bool {
	return match tree {
		Empty {
			false
		}
		Node<T> {
			if x == tree.value {
				true
			} else if x < tree.value {
				tree.left.search(x)
			} else {
				tree.right.search(x)
			}
		}
	}
}

// find the minimal value of a BST
fn (tree Tree<T>) min<T>() T {
	return match tree {
		Empty {
			T(1e9)
		}
		Node<T> {
			if tree.value < tree.left.min() {
				tree.value
			} else {
				tree.left.min()
			}
		}
	}
}

// delete a value in BST (if nonexistant do nothing)
fn (tree Tree<T>) delete<T>(x T) Tree<T> {
	return match tree {
		Empty {
			tree
		}
		Node<T> {
			if tree.left !is Empty && tree.right !is Empty {
				if x < tree.value {
					Node<T>{
						...tree
						left: tree.left.delete(x)
					}
				} else if x > tree.value {
					Node<T>{
						...tree
						right: tree.right.delete(x)
					}
				} else {
					Node<T>{
						...tree
						value: tree.right.min()
						right: tree.right.delete(tree.right.min())
					}
				}
			} else if tree.left !is Empty {
				if x == tree.value {
					tree.left
				} else {
					Node<T>{
						...tree
						left: tree.left.delete(x)
					}
				}
			} else {
				if x == tree.value {
					tree.right
				} else {
					Node<T>{
						...tree
						right: tree.right.delete(x)
					}
				}
			}
		}
	}
}

fn main() {
	mut tree := Tree<f64>(Empty{})
	vals := [0.2, 0.0, 0.5, 0.3, 0.6, 0.8, 0.9, 1.0, 0.1, 0.4, 0.7]
	for i in vals {
		tree = tree.insert(i)
	}
	println('[1] after insertion tree size is ${tree.size()}') // 11
	del_vals := [-0.3, 0.0, 0.3, 0.6, 1.0, 1.5]
	for i in del_vals {
		tree = tree.delete(i)
	}
	print('[2] after deletion tree size is ${tree.size()}, ') // 7
	print('and these elements were deleted: ') // 0.0 0.3 0.6 1.0
	for i in vals {
		if !tree.search(i) {
			print('${i} ')
		}
	}
	println('')
}
