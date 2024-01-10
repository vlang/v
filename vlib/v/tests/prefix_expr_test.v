fn value(n int) int {
	return n
}

struct Foo {
	n int
}

fn (foo Foo) value() int {
	return foo.n
}

fn test_negative() {
	one := 1
	negative_one := -1
	assert -one == negative_one
	assert one == -negative_one

	assert -value(1) == -1

	// issue #9643
	foo := Foo{1}
	assert -foo.value() == -1
	assert -(foo.value()) == -1

	arr := [1, 2, 3]
	assert -arr[0] == -1
	assert -arr[1] == -2
}
