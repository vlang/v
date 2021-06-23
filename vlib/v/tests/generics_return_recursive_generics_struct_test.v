struct Node<T> {
mut:
	val  T
	next &Node<T>
}

fn make_node<T>(val []T) Node<T> {
	return Node{
		val: val[0]
		next: 0
	}
}

fn test_generics_return_recursive_generics_struct() {
	n := make_node([1, 2, 3])
	println(n.val)
	assert n.val == 1
}
