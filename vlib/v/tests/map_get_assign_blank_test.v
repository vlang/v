type Abc = int | string

fn test_map_get_decl_assign_blank() {
	x := map[string]Abc{}
	_ := x['nonexisting']
	if y := x['nonexisting'] {
		println(y)
	}
	assert true
}

fn test_map_get_assign_blank() {
	x := map[string]Abc{}
	_ = x['nonexisting']
	if y := x['nonexisting'] {
		println(y)
	}
	assert true
}
