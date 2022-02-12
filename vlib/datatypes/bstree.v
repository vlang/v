module datatypes

struct Node<T> {
mut:
	value  &T       = 0
	parent &Node<T> = 0
	left   &Node<T> = 0
	right  &Node<T> = 0
}

// Binary Seach Tree implementation
//
// Simple implementation of the Binary Search Tree
// Time complexity of all the operation O(log N)
// Space complexity O(N)
pub struct BSTree<T> {
mut:
	root &Node<T> = 0
}

// Insert an element in order inside the data structure
pub fn (mut bst BSTree<T>) insert(T value) bool {
	return true
}

// Check if an element wiht a value is inside the data structure
pub fn (bst &BSTree<T>) contains(T value) bool {
	return false
}

// Remove the element with the value from the data structure
pub fn (mut bst BSTree<T>) remove(T value) bool {
	return false
}

// Return the element to le left of a value
pub fn (bst &BSTree<T>) to_left(T value) T {
	panic('TODO')
}

// Return the element to the right of the value
pub fn (bst &BSTree<T>) to_right(T value) T {
	panic('TODO')
}
