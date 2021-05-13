struct Node<T> {
	data T
}

fn foo<T>(n Node<T>) string {
	return '$n'
}

fn test_generics_fn_infer_struct() {
	ret1 := foo(Node<int>{})
	println(ret1)
	assert ret1.contains('Node<int>{')
	assert ret1.contains('data: 0')

	ret2 := foo(Node<f64>{})
	println(ret2)
	assert ret2.contains('Node<f64>{')
	assert ret2.contains('data: 0')

	ret3 := foo(Node<byte>{})
	println(ret3)
	assert ret3.contains('Node<byte>{')
	assert ret3.contains('data: 0')
}
