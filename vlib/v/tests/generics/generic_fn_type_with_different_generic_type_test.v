type Fn[T] = fn () T

fn problem[X](f Fn[X]) X {
	return f[X]()
}

fn foo() int {
	return 1
}

fn test_generic_fn_type_with_different_generic_type() {
	t := problem[int](foo)
	println(t)
	assert t == 1
}
