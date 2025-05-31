fn test_var_name_using_reserved() {
	error := {
		0: 1
		1: 2
		2: 3
	}
	ret := error[4] or { 42 }
	println(ret)
	assert ret == 42
}
