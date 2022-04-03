module datatypes

fn test_init_rbtree() {
	rbtree := RBTree<int>{}
	assert rbtree.is_empty()
}
