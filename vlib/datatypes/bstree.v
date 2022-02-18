module datatypes

[heap]
struct BSTreeNode<T> {
mut:
	// Mark a node as ready to be walked
	is_init bool
	// Value of the node
	value T
	// The parent of the node
	parent &BSTreeNode<T> = 0
	// The left side with value less than the
	// value of this node
	left &BSTreeNode<T> = 0
	// The right side with value grater than the
	// value of thiss node
	right &BSTreeNode<T> = 0
}

// Create new root bst node
fn new_root_node<T>(value T) &BSTreeNode<T> {
	return &BSTreeNode<T>{
		is_init: true
		value: value
		parent: new_none_node<T>(true)
		left: new_none_node<T>(false)
		right: new_none_node<T>(false)
	}
}

// Create a new generic bst node, this help to create
// node during the walking tree
fn new_node<T>(parent &BSTreeNode<T>, value T) &BSTreeNode<T> {
	return &BSTreeNode<T>{
		is_init: true
		value: value
		parent: parent
	}
}

// Create a dummy node
// FIXME: adding the init parameter as optional
fn new_none_node<T>(init bool) &BSTreeNode<T> {
	return &BSTreeNode<T>{
		is_init: false
	}
}

// Binary Seach Tree implementation
//
// Simple implementation of the Binary Search Tree
// Time complexity of all the operation O(log N)
// Space complexity O(N)
pub struct BSTree<T> {
mut:
	root &BSTreeNode<T> = 0
}

// Insert an element in order inside the data structure
pub fn (mut bst BSTree<T>) insert(value T) bool {
	if bst.is_empty() {
		bst.root = new_root_node(value)
		return true
	}
	return bst.insert_helper(mut bst.root, value)
}

// Helper function that give the possibility to walk the tree and make
// the insert operation in the correct position.
fn (mut bst BSTree<T>) insert_helper(mut node BSTreeNode<T>, value T) bool {
	if node.value < value {
		if node.right != 0 && node.right.is_init {
			return bst.insert_helper(mut node.right, value)
		}
		node.right = new_node(node, value)
		return true
	} else if node.value > value {
		if node.left != 0 && node.left.is_init {
			return bst.insert_helper(mut node.left, value)
		}
		node.left = new_node(node, value)
		return true
	}
	return false
}

// Check if an element with a given `value` is inside the data structure
pub fn (bst &BSTree<T>) contains(value T) bool {
	return bst.contains_helper(bst.root, value)
}

// Helper function to walk the tree, and check the result
fn (bst &BSTree<T>) contains_helper(node &BSTreeNode<T>, value T) bool {
	if node == 0 || !node.is_init {
		return false
	}
	if node.value < value {
		return bst.contains_helper(node.right, value)
	} else if node.value > value {
		return bst.contains_helper(node.left, value)
	}
	assert node.value == value
	return true
}

// Remove the element with the value from the data structure
pub fn (mut bst BSTree<T>) remove(value T) bool {
	return false
}

// Check if the tree is empty
pub fn (bst &BSTree<T>) is_empty() bool {
	return bst.root == 0
}

// In order Traverse on the BST, and return the result as an array
pub fn (bst &BSTree<T>) in_order_traversals() []T {
	mut result := []T{}
	bst.in_order_traversals_helper(bst.root, mut result)
	return result
}

// In order Traversals helper logic to implement the startegy to walk the BST.
fn (bst &BSTree<T>) in_order_traversals_helper(node &BSTreeNode<T>, mut result []T) {
	if node == 0 || !node.is_init {
		return
	}

	bst.in_order_traversals_helper(node.left, mut result)
	result << node.value
	bst.in_order_traversals_helper(node.right, mut result)
}

// Post order traversal on the BST, and return the result as an array
pub fn (bst &BSTree<T>) post_order_traversal() []T {
	mut result := []T{}
	bst.post_order_traversal_helper(bst.root, mut result)
	return result
}

// Post order traversal helper in other to implement the walk on the BST.
fn (bst &BSTree<T>) post_order_traversal_helper(node &BSTreeNode<T>, mut result []T) {
	if node == 0 || !node.is_init {
		return
	}

	bst.post_order_traversal_helper(node.left, mut result)
	bst.post_order_traversal_helper(node.right, mut result)
	result << node.value
}

pub fn (bst &BSTree<T>) pre_order_traversal() []T {
	mut result := []T{}
	bst.pre_order_traversal_helper(bst.root, mut result)
	return result
}

fn (bst &BSTree<T>) pre_order_traversal_helper(node &BSTreeNode<T>, mut result []T) {
	if node == 0 || !node.is_init {
		return
	}
	result << node.value
	bst.pre_order_traversal_helper(node.left, mut result)
	bst.pre_order_traversal_helper(node.right, mut result)
}

fn (bst &BSTree<T>) get_node(node &BSTreeNode<T>, value T) &BSTreeNode<T> {
	if node == 0 || !node.is_init {
		return new_none_node<T>(false)
	}
	if node.value == value {
		return node
	}

	if node.value < value {
		return bst.get_node(node.right, value)
	}
	return bst.get_node(node.left, value)
}

// Return the element to the left of a value
pub fn (bst &BSTree<T>) to_left(value T) (T, bool) {
	node := bst.get_node(bst.root, value)
	if !node.is_init {
		return node.value, false
	}
	left_node := node.left
	return left_node.value, left_node.is_init
}

// Return the element to the right of the value
pub fn (bst &BSTree<T>) to_right(value T) (T, bool) {
	node := bst.get_node(bst.root, value)
	if !node.is_init {
		return node.value, false
	}
	right_node := node.right
	return right_node.value, right_node.is_init
}
