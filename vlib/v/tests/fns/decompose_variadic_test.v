import arrays

type ConcatValue = []u8 | string

fn get() []int {
	return [1, 2, 3, 4, 5]
}

fn get2() [][]int {
	return [[0], [1, 2, 3, 4, 5]]
}

fn receive(a ...int) {
}

fn test_main() {
	assert arrays.concat([0], ...get().map(it)) == [0, 1, 2, 3, 4, 5]
	assert arrays.concat[int]([], ...get().map(it)) == [1, 2, 3, 4, 5]
	assert arrays.concat[[]int]([[0]], ...[get().map(it)]) == [
		[0],
		[1, 2, 3, 4, 5],
	]
	assert arrays.concat[int](...get2()) == [0, 1, 2, 3, 4, 5]
}

fn test_decompose_variadic_generic_sumtype_array_arg() {
	mut params := []ConcatValue{}
	ids_bin := [
		[u8(`a`), `b`],
		[u8(`c`), `d`],
	]
	params = arrays.concat(params, ...ids_bin)
	assert params.len == 2
	assert params[0] is []u8
	assert params[1] is []u8
	assert (params[0] as []u8).bytestr() == 'ab'
	assert (params[1] as []u8).bytestr() == 'cd'
}

fn test_decompose_variadic_nested_string_array_arg() {
	a := [['0']]
	b := [['aa', 'bb']]
	c := arrays.concat(a, ...b)
	assert c == [['0'], ['aa', 'bb']]
	assert c.str() == "[['0'], ['aa', 'bb']]"
}
