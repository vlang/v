fn abc[T]() []int {
	a := []?int{len: 2}
	mut s := []int{}
	for v in a {
		s << dump(v)
	}
	return s
}

fn test_main() {
	arr := abc[int]()
	assert arr.len == 2
	assert arr[0] == 0
	assert arr[1] == 0
}
