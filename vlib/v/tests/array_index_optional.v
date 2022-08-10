struct Arrs {
mut:
	x [][]int
}

fn test_mut_array_index_optional() {
	mut arr := Arrs{}
	for mut sub_arr in arr.x {
		x := sub_arr[0] or { 666 }
		assert x == 666
	}
}

fn test_array_index_optional_with_if_expr() {
	ret := []string{}[0] or {
		if true { 'a' } else { 'b' }
	}
	println(ret)
	assert ret == 'a'
}
