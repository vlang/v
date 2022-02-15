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
