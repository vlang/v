type Test = int

fn test_array_len_int_alias() {
	x := Test(12)
	arr := []int{len: x}
	assert arr.len == 12
}
