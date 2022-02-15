module datatypes

fn test_insert_into_bst_one() {
	mut bst := BSTree<int>{}
	assert bst.insert(10) == true
}
