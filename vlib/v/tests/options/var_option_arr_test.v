fn abc() ?[]int {
	return [1, 2, 3]
}

fn arr_opt(arr ?[]string) ? {
	assert arr != none
	assert arr?.len != 0

	for k, v in arr {
		assert arr?[k] == 'foo'
		assert v == 'foo'
	}
}

fn test_main() {
	mut var2 := abc()?
	assert var2.len == 3

	mut var := ?[]int{}
	assert var == none
	if var == none {
		var = [1]
	}
	assert var != none
	assert var?[0] == 1
	assert var?.len == 1

	mut var3 := ?[]string{}
	assert var3 == none
	var3 = ['foo']
	assert var3 != none

	arr_opt(var3)
	arr_opt([]string{len: 4, init: 'foo'})
	arr_opt(['foo', 'foo', 'foo', 'foo'])
}
