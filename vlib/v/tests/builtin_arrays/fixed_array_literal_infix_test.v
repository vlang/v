fn test_array_of_fixed_array_in_op() {
	mut a := [][2]int{}
	a << [1, 2]!
	println([1, 2]! in a)
	ret := [1, 2]! in a
	println(ret)
	assert ret
}

fn test_fixed_array_of_fixed_array_in_op() {
	mut a := [2][2]int{}
	a[0] = [1, 2]!
	println([1, 2]! in a)
	ret := [1, 2]! in a
	println(ret)
	assert ret
}

fn test_array_of_fixed_array_index() {
	mut a := [][2]int{}
	a << [1, 2]!
	println(a.index([1, 2]!))
	ret := a.index([1, 2]!)
	println(ret)
	assert ret == 0
}

fn test_fixed_array_of_fixed_array_index() {
	mut a := [2][2]int{}
	a[0] = [1, 2]!
	println(a.index([1, 2]!))
	ret := a.index([1, 2]!)
	println(ret)
	assert ret == 0
}
