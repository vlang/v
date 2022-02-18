module datatypes

fn test_insert_into_bst_one() {
	mut bst := BSTree<int>{}
	assert bst.insert(10) == true
	assert bst.contains(10) == true
}

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

fn test_in_order_bst_visit_one() {
	mut bst := BSTree<int>{}
	assert bst.insert(10)
	assert bst.insert(20)
	assert bst.insert(21)
	assert bst.insert(1)

	assert bst.in_order_traversals() == [1, 10, 20, 21]
}

fn test_post_order_bst_visit_one() {
	mut bst := BSTree<int>{}
	assert bst.insert(10)
	assert bst.insert(20)
	assert bst.insert(21)
	assert bst.insert(1)

	assert bst.post_order_traversal() == [1, 21, 20, 10]
}

fn test_prep_order_bst_visit_one() {
	mut bst := BSTree<int>{}
	assert bst.insert(10)
	assert bst.insert(20)
	assert bst.insert(21)
	assert bst.insert(1)

	assert bst.pre_order_traversal() == [10, 1, 20, 21]
}

fn test_get_left_root() {
	mut bst := BSTree<int>{}
	assert bst.insert(10)
	assert bst.insert(20)
	assert bst.insert(21)
	assert bst.insert(1)

	left_val, found_left := bst.to_left(10)
	assert found_left
	assert left_val == 1

	right_val, found_right := bst.to_right(10)
	assert found_right
	assert right_val == 20
}

fn test_get_left_on_empty_bst() {
	mut bst := BSTree<int>{}

	left_val, found_left := bst.to_left(10)
	assert found_left == false

	right_val, found_right := bst.to_right(10)
	assert found_right == false
}
