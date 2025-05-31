fn info(type int) int {
	return type
}

fn print_info() {
	for type in [1, 2, 3] {
		println(type)
	}
}

fn test_var_name_using_type() {
	ret := info(22)
	println(ret)
	assert ret == 22

	type := 33
	println(type)
	assert type == 33

	print_info()
	assert true
}
