struct Test1 {
mut:
	value [4]int
}

fn (mut t Test1) set(new_value [4]int) {
	t.value = new_value
}

fn test_fn_with_fixed_array_argument_1() {
	mut t := Test1{}

	println(t)
	assert '${t.value}' == '[0, 0, 0, 0]'

	t.set([1, 2, 3, 4]!)

	println(t)
	assert '${t.value}' == '[1, 2, 3, 4]'
}

struct Test2 {
mut:
	fixed_value   [2][4]int
	dynamic_value [][4]int
}

fn (mut t Test2) set(index int, new_value [4]int) {
	t.fixed_value[index] = new_value
	t.dynamic_value << new_value
}

fn test_fn_with_fixed_array_argument_2() {
	mut t := Test2{}

	println(t)
	assert '${t.fixed_value}' == '[[0, 0, 0, 0], [0, 0, 0, 0]]'
	assert '${t.dynamic_value}' == '[]'

	t.set(0, [1, 2, 3, 4]!)
	println(t)
	assert '${t.fixed_value}' == '[[1, 2, 3, 4], [0, 0, 0, 0]]'
	assert '${t.dynamic_value}' == '[[1, 2, 3, 4]]'
}
