struct Foo {
	i int
}

fn (a Foo) < (b Foo) bool {
	return a.i < b.i
}

fn (a Foo) == (b Foo) bool {
	return a.i == b.i
}

fn test_operator_overloading_cmp() {
	a := Foo{
		i: 38
	}
	b := Foo{
		i: 38
	}
	mut arr := [a, b]

	assert (a > b) == false
	assert (a < b) == false
	//// /// //
	assert a >= b
	assert a <= b
	//// /// //
	assert b >= a
	assert b <= a
	//// /// //
	arr.sort(a > b)
	assert arr[0].i == 38
	arr.sort(a < b)
	assert arr[0].i == 38
}
