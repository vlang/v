fn func(mut a []int) {
	a = [1, 2, 3, 4]
	println('inside fn: $a')
	assert '$a' == '[1, 2, 3, 4]'
}

fn test_fn_mut_args() {
	mut a := [1, 2, 3]
	func(mut a)
	println('inside main: $a')
	assert '$a' == '[1, 2, 3, 4]'
}
