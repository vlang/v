fn func(mut a []int) {
	a = [1, 2, 3, 4]
	println('inside fn: $a')
	assert '$a' == '[1, 2, 3, 4]'
}

fn test_fn_mut_args_of_array() {
	mut a := [1, 2, 3]
	func(mut a)
	println('inside main: $a')
	assert '$a' == '[1, 2, 3, 4]'
}

fn init_map(mut n map[string]int) {
	n = map{
		'one': 1
	}
}

fn test_fn_mut_args_of_map() {
	mut m := map[string]int{}
	init_map(mut m)
	println(m)
	assert m == map{
		'one': 1
	}
}
