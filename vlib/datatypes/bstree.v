module datatypes

[heap]
struct BTreeNode<T> {
mut:
	// Mark a node as ready to be walked
	is_init bool
	// Value of the node
	value T
	// The parent of the node
	parent &BTreeNode<T> = 0
	// The left side with value less than the
	// value of this node
	left &BTreeNode<T> = 0
	// The right side with value grater than the
	// value of thiss node
	right &BTreeNode<T> = 0
}

// Create new root bst node
fn new_root_node<T>(value T) &BTreeNode<T> {
	return &BTreeNode<T>{
		is_init: true
		value: value
		parent: new_none_node<T>(true)
		left: new_none_node<T>(false)
		right: new_none_node<T>(false)
	}
}

// Create a new generic bst node, this help to create
// node during the walking tree
fn new_node<T>(parent &BTreeNode<T>, value T) &BTreeNode<T> {
	return &BTreeNode<T>{
		is_init: true
		value: value
		parent: parent
	}
}

// Create a dummy node
// FIXME: adding the init parameter as optional
fn new_none_node<T>(init bool) &BTreeNode<T> {
	return &BTreeNode<T>{
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
	root &BTreeNode<T> = 0
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
fn (mut bst BSTree<T>) insert_helper(mut node BTreeNode<T>, value T) bool {
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
// FIXME: the walk function can be generilized with a general function as params?
fn (bst &BSTree<T>) contains_helper(node &BTreeNode<T>, value T) bool {
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

/*
// Return the element to the left of a value
pub fn (bst &BSTree<T>) to_left(value T) T {
	panic('TODO')
	return 0;
}

// Return the element to the right of the value
pub fn (bst &BSTree<T>) to_right(value T) T {
	panic('TODO')
}
*/
