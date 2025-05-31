fn test_cast_bool_to_int() {
	i := true
	x := int(i)
	nx := int(!i)
	dump(x)
	dump(nx)
	a := [1, 2, 3]

	println(a[int(!i)])
	assert a[int(!i)] == 1

	println(a[int(i)])
	assert a[int(i)] == 2
}
