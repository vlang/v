struct Node<T> {
mut:
	val  T
	next &Node<T>
}

fn make_node<T>(val []T) Node<T> {
	return Node<T>{
		val: val[0]
		next: 0
	}
}

fn test_generics_return_recursive_generics_struct() {
	n1 := make_node([1, 2, 3])
	println(n1.val)
	assert n1.val == 1

	n2 := make_node([1.1, 2.2, 3.3])
	println(n2.val)
	assert n2.val == 1.1

	n3 := make_node([true, false, true])
	println(n3.val)
	assert n3.val == true

	n4 := make_node(['aa', 'bb', 'cc'])
	println(n4.val)
	assert n4.val == 'aa'
}
