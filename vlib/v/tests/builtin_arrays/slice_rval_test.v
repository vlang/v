fn test_arr_rval() {
	a := [1, 2]
	s := (*&a)[..]
	assert s == a

	b := unsafe { *&[]int(&a) }[..]
	assert b == a
}
