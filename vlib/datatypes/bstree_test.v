module datatypes

// Make an insert of one element and check if
// the bst is able to fin it.
fn test_insert_into_bst_one() {
	mut bst := BSTree<int>{}
	assert bst.insert(10) == true
	assert bst.contains(10) == true
	assert bst.contains(20) == false
}

// Make the insert of more element inside the BST
// and check if the BST is able to find all the values
fn test_insert_into_bst_two() {
	mut bst := BSTree<int>{}
	assert bst.insert(10)
	assert bst.insert(20)
	assert bst.insert(9)

	assert bst.contains(9)
	assert bst.contains(10)
	assert bst.contains(20)
	assert bst.contains(11) == false
}

// Test if the in_order_traversals list return the correct
// result array
fn test_in_order_bst_visit_one() {
	mut bst := BSTree<int>{}
	assert bst.insert(10)
	assert bst.insert(20)
	assert bst.insert(21)
	assert bst.insert(1)

	assert bst.in_order_traversal() == [1, 10, 20, 21]
}

// Test if the post_order_bst_visit return the correct
// result array
fn test_post_order_bst_visit_one() {
	mut bst := BSTree<int>{}
	assert bst.insert(10)
	assert bst.insert(20)
	assert bst.insert(21)
	assert bst.insert(1)

	assert bst.post_order_traversal() == [1, 21, 20, 10]
}

// Test if the pre_order_traversal return the correct result array
fn test_pre_order_bst_visit_one() {
	mut bst := BSTree<int>{}
	assert bst.insert(10)
	assert bst.insert(20)
	assert bst.insert(21)
	assert bst.insert(1)

	assert bst.pre_order_traversal() == [10, 1, 20, 21]
}

// After many insert check if we are abe to get the correct
// right and left value of the root.
fn test_get_left_root() {
	mut bst := BSTree<int>{}
	assert bst.insert(10)
	assert bst.insert(20)
	assert bst.insert(21)
	assert bst.insert(1)

	left_val := bst.to_left(10) or { -1 }
	assert left_val == 1

	right_val := bst.to_right(10) or { -1 }
	assert right_val == 20
}

// Check if BST panic if we call some operation on an empty BST.
fn test_get_left_on_empty_bst() {
	mut bst := BSTree<int>{}

	left_val := bst.to_left(10) or { -1 }
	assert left_val == -1

	right_val := bst.to_right(10) or { -1 }
	assert right_val == -1
}

// Check the remove operation if it is able to remove
// all elements required, and mantains the BST propriety.
fn test_remove_from_bst_one() {
	mut bst := BSTree<int>{}
	assert bst.insert(10)
	assert bst.insert(20)
	assert bst.insert(21)
	assert bst.insert(1)
	assert bst.in_order_traversal() == [1, 10, 20, 21]
	assert bst.remove(21)

	assert bst.in_order_traversal() == [1, 10, 20]
}

// Another test n the remove BST, this remove an intermidia node
// that it is a triky operation.
fn test_remove_from_bst_two() {
	mut bst := BSTree<int>{}
	assert bst.insert(10)
	assert bst.insert(20)
	assert bst.insert(21)
	assert bst.insert(1)
	assert bst.in_order_traversal() == [1, 10, 20, 21]
	assert bst.remove(20)

	assert bst.in_order_traversal() == [1, 10, 21]
}

// check if we are able to get the max from the BST.
fn test_get_max_in_bst() {
	mut bst := BSTree<int>{}
	assert (bst.max() or { -1 }) == -1
	assert bst.insert(10)
	assert bst.insert(20)
	assert bst.insert(21)
	assert bst.insert(1)
	max := bst.max() or { -1 }
	assert max == 21
}

// check if we are able to get the min from the BST.
fn test_get_min_in_bst() {
	mut bst := BSTree<int>{}
	assert (bst.min() or { -1 }) == -1
	assert bst.insert(10)
	assert bst.insert(20)
	assert bst.insert(21)
	assert bst.insert(1)
	min := bst.min() or { -1 }
	assert min == 1
}
