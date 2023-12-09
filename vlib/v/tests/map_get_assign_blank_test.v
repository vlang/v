type Abc = int | string

fn test_map_get_decl_assign_blank() {
	x := map[string]Abc{}
	_ := unsafe { x['nonexisting'] }
	if y := x['nonexisting'] {
		println(y)
	}
	assert true
}

fn test_map_get_assign_blank() {
	x := map[string]Abc{}
	_ = unsafe { x['nonexisting'] }
	if y := x['nonexisting'] {
		println(y)
	}
	assert true
}

fn get_value() int {
	mut m := map[string]int{}
	_ := m['a'] or { return 1 }
	println('a')
	return 0
}

fn test_map_get_assign_blank_with_or_expr() {
	ret := get_value()
	println(ret)
	assert ret == 1
}
