struct Test {
mut:
	value [4]int
}

fn (mut t Test) set(new_value [4]int) {
	t.value = new_value
}

fn test_fn_with_fixed_array_argument() {
	mut t := Test{}

	println(t)
	assert '$t.value' == '[0, 0, 0, 0]'

	t.set([1, 2, 3, 4]!)

	println(t)
	assert '$t.value' == '[1, 2, 3, 4]'
}
