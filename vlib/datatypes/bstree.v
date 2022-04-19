module datatypes

/// Internal rapresentation of the tree node
[heap]
struct BSTreeNode<T> {
mut:
	// Mark a node as initialized
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

// new_node creates a new bst node with a parent reference.
fn new_node<T>(parent &BSTreeNode<T>, value T) &BSTreeNode<T> {
	return &BSTreeNode<T>{
		is_init: true
		value: value
		parent: parent
	}
}

// new_none_node creates a dummy node.
fn new_none_node<T>(init bool) &BSTreeNode<T> {
	return &BSTreeNode<T>{
		is_init: init
	}
}

// bind to an actual instance of a node.
fn (mut node BSTreeNode<T>) bind(mut to_bind BSTreeNode<T>, left bool) {
	node.left = to_bind.left
	node.right = to_bind.right
	node.value = to_bind.value
	node.is_init = to_bind.is_init
	to_bind = new_none_node<T>(false)
}

// Pure Binary Seach Tree implementation
//
// Pure V implementation of the Binary Search Tree
// Time complexity of main operation O(log N)
// Space complexity O(N)
pub struct BSTree<T> {
mut:
	root &BSTreeNode<T> = 0
}

// insert give the possibility to insert an element in the BST.
pub fn (mut bst BSTree<T>) insert(value T) bool {
	if bst.is_empty() {
		bst.root = new_root_node(value)
		return true
	}
	return bst.insert_helper(mut bst.root, value)
}

// insert_helper walks the tree and inserts the given node.
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

// contains checks if an element with a given `value` is inside the BST.
pub fn (bst &BSTree<T>) contains(value T) bool {
	return bst.contains_helper(bst.root, value)
}

// contains_helper is a helper function to walk the tree, and return
// the absence or presence of the `value`.
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

// remove removes an element with `value` from the BST.
pub fn (mut bst BSTree<T>) remove(value T) bool {
	if bst.is_empty() {
		return false
	}
	return bst.remove_helper(mut bst.root, value, false)
}

fn (mut bst BSTree<T>) remove_helper(mut node BSTreeNode<T>, value T, left bool) bool {
	if !node.is_init {
		return false
	}
	if node.value == value {
		if node.left != 0 && node.left.is_init {
			// In order to remove the element we need to bring up as parent the max of the
			// left sub-tree.
			mut max_node := bst.get_max_from_right(node.left)
			node.bind(mut max_node, true)
		} else if node.right != 0 && node.right.is_init {
			// Bring up the element with the minimum value in the right sub-tree.
			mut min_node := bst.get_min_from_left(node.right)
			node.bind(mut min_node, false)
		} else {
			mut parent := node.parent
			if left {
				parent.left = new_none_node<T>(false)
			} else {
				parent.right = new_none_node<T>(false)
			}
			node = new_none_node<T>(false)
		}
		return true
	}

	if node.value < value {
		return bst.remove_helper(mut node.right, value, false)
	}
	return bst.remove_helper(mut node.left, value, true)
}

// get_max_from_right returns the max element of the BST following the right branch.
fn (bst &BSTree<T>) get_max_from_right(node &BSTreeNode<T>) &BSTreeNode<T> {
	if node == 0 {
		return new_none_node<T>(false)
	}
	right_node := node.right
	if right_node == 0 || !right_node.is_init {
		return node
	}
	return bst.get_max_from_right(right_node)
}

// get_min_from_left returns the min element of the BST by following the left branch.
fn (bst &BSTree<T>) get_min_from_left(node &BSTreeNode<T>) &BSTreeNode<T> {
	if node == 0 {
		return new_none_node<T>(false)
	}
	left_node := node.left
	if left_node == 0 || !left_node.is_init {
		return node
	}
	return bst.get_min_from_left(left_node)
}

// is_empty checks if the BST is empty
pub fn (bst &BSTree<T>) is_empty() bool {
	return bst.root == 0
}

// in_order_traversal traverses the BST in order, and returns the result as an array.
pub fn (bst &BSTree<T>) in_order_traversal() []T {
	mut result := []T{}
	bst.in_order_traversal_helper(bst.root, mut result)
	return result
}

// in_order_traversal_helper helps traverse the BST, and accumulates the result in the `result` array.
fn (bst &BSTree<T>) in_order_traversal_helper(node &BSTreeNode<T>, mut result []T) {
	if node == 0 || !node.is_init {
		return
	}
	bst.in_order_traversal_helper(node.left, mut result)
	result << node.value
	bst.in_order_traversal_helper(node.right, mut result)
}

// post_order_traversal traverses the BST in post order, and returns the result in an array.
pub fn (bst &BSTree<T>) post_order_traversal() []T {
	mut result := []T{}
	bst.post_order_traversal_helper(bst.root, mut result)
	return result
}

// post_order_traversal_helper is a helper function that traverses the BST in post order,
// accumulating the result in an array.
fn (bst &BSTree<T>) post_order_traversal_helper(node &BSTreeNode<T>, mut result []T) {
	if node == 0 || !node.is_init {
		return
	}

	bst.post_order_traversal_helper(node.left, mut result)
	bst.post_order_traversal_helper(node.right, mut result)
	result << node.value
}

// pre_order_traversal traverses the BST in pre order, and returns the result as an array.
pub fn (bst &BSTree<T>) pre_order_traversal() []T {
	mut result := []T{}
	bst.pre_order_traversal_helper(bst.root, mut result)
	return result
}

// pre_order_traversal_helper is a helper function to traverse the BST
// in pre order and accumulates the results in an array.
fn (bst &BSTree<T>) pre_order_traversal_helper(node &BSTreeNode<T>, mut result []T) {
	if node == 0 || !node.is_init {
		return
	}
	result << node.value
	bst.pre_order_traversal_helper(node.left, mut result)
	bst.pre_order_traversal_helper(node.right, mut result)
}

// get_node is a helper method to ge the internal rapresentation of the node with the `value`.
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

// to_left returns the value of the node to the left of the node with `value` specified if it exists,
// otherwise the a false value is returned.
//
// An example of usage can be the following one
//```v
// left_value, exist := bst.to_left(10)
//```
pub fn (bst &BSTree<T>) to_left(value T) ?T {
	if bst.is_empty() {
		return none
	}
	node := bst.get_node(bst.root, value)
	if !node.is_init {
		return none
	}
	left_node := node.left
	return left_node.value
}

// to_right return the value of the element to the right of the node with `value` specified, if exist
// otherwise, the boolean value is false
// An example of usage can be the following one
//
//```v
// left_value, exist := bst.to_right(10)
//```
pub fn (bst &BSTree<T>) to_right(value T) ?T {
	if bst.is_empty() {
		return none
	}
	node := bst.get_node(bst.root, value)
	if !node.is_init {
		return none
	}
	right_node := node.right
	return right_node.value
}

// max return the max element inside the BST.
// Time complexity O(N) if the BST is not balanced
pub fn (bst &BSTree<T>) max() ?T {
	if bst.is_empty() {
		return none
	}
	max := bst.get_max_from_right(bst.root)
	if !max.is_init {
		return none
	}
	return max.value
}

// min return the minimum element in the BST.
// Time complexity O(N) if the BST is not balanced.
pub fn (bst &BSTree<T>) min() ?T {
	if bst.is_empty() {
		return none
	}
	min := bst.get_min_from_left(bst.root)
	if !min.is_init {
		return none
	}
	return min.value
}
