fn foo[T]() string {
	return typeof[T]().name
}

fn bar[T]() string {
	return typeof[T]()
}

fn test_main() {
	assert foo[int]() == 'int'
	assert bar[int]() == 'int'
}
