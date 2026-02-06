type Array1 = []u8

type Array2 = []u8

fn (a Array2) == (other Array2) bool {
	return a[0] == other[0]
}

fn (a Array2) < (other Array2) bool {
	return a[0] < other[0]
}

fn test_array_of_alias_op_overload() {
	a := Array1([u8(127), 0, 0, 1])
	b := Array1([u8(127), 0, 0, 1])

	c := Array2([u8(127), 0, 0, 1])
	d := Array2([u8(127), 0, 0, 1])

	ret0 := a == b
	println(ret0)
	assert ret0

	ret1 := c == d
	println(ret1)
	assert ret1

	ret2 := c < d
	println(ret2)
	assert !ret2
}
