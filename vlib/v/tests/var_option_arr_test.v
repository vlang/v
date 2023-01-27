fn abc() ?[]int {
	return [1, 2, 3]
}

fn arr_opt(arr ?[]string) {
	assert arr != none
	assert arr?.len == 4

	for k, v in arr {
		assert arr?[k] == 'foo'
		assert v == 'foo'
	}
}

fn test_main() {
	mut var2 := abc()?
	assert var2.len == 3

	var := ?[]int{len: 10, init: 2}
	assert var?.len == 10
	assert var != none

	var3 := ?[]string{}
	assert var3 == none

	arr_opt(?[]string{len: 4, init: 'foo'})
	arr_opt([]string{len: 4, init: 'foo'})
	arr_opt(['foo', 'foo', 'foo', 'foo'])
}
