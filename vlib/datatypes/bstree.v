module datatypes

struct BTreeNode<T> {
mut:
	is_init bool
	value   T
	parent  &BTreeNode<T> = 0
	left    &BTreeNode<T> = 0
	right   &BTreeNode<T> = 0
}

fn new_root_node<T>(value T) &BTreeNode<T> {
	return &BTreeNode<T>{
		is_init: true
		value: value
	}
}

fn new_node<T>(parent BTreeNode<T>, value T) &BTreeNode<T> {
	return &BTreeNode<T>{
		is_init: true
		value: value
		parent: &parent
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
	if !bst.root.is_init {
		bst.root = new_root_node(value)
		return true
	}

	if bst.root.value < value {
		return bst.insert_helper(mut bst.root.right, value)
	} else if bst.root.value > value {
		return bst.insert_helper(mut bst.root.left, value)
	}
	// we don't accept duplicate for the moment
	return false
}

fn (mut bst BSTree<T>) insert_helper(mut node BTreeNode<T>, value T) bool {
	if !node.is_init {
		return false
	}

	if node.value < value {
		if node.right.is_init {
			return bst.insert_helper(mut node.right, value)
		}
		node.right = new_node(node, value)
		return true
	} else if node.value > value {
		if node.left.is_init {
			return bst.insert_helper(mut node.left, value)
		}
		node.left = new_node(node, value)
		return true
	}
	return false
}

// Check if an element with a given `value` is inside the data structure
pub fn (bst &BSTree<T>) contains(value T) bool {
	return false
}

// Remove the element with the value from the data structure
pub fn (mut bst BSTree<T>) remove(value T) bool {
	return false
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
