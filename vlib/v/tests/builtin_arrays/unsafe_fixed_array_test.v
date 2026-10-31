fn test_fixed_array_on_unsafe_int() {
	x := [3]int{}
	y := unsafe { x }
	assert x == [0, 0, 0]!
	assert y == x
}

fn test_fixed_array_on_unsafe_string() {
	x := [3]string{init: ''}
	y := unsafe { x }
	assert x == ['', '', '']!
	assert y == x
}
