fn foo_24678[A, B](name string, data A) Response24678[B] {
	return Response24678[B]{
		response: B{}
		err:      'none'
	}
}

struct Response24678[T] {
	response T
	err      string
}

fn test_generic_fn_return_can_reference_later_declared_generic_struct() {
	x := foo_24678[string, int]('foo', '')
	assert x.response == 0
	assert x.err == 'none'
}
