import strconv

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

fn test_positive() {
	two := 2
	assert +two == 2
	assert +(two - 1) == 1
}

fn test_prefix_expr_in_assert_comparison() ! {
	i := 2
	assert strconv.atoi('1')! == 1
	assert strconv.atoi('-2')! == -2
	assert strconv.atoi('+2')! == i - 1 + 1
	assert strconv.atoi('+2')! == -(-2)
	assert strconv.atoi('+2')! == +2
}
