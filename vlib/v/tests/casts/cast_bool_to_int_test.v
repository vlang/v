fn test_cast_bool_to_int() {
	i := true
	x := int(i)
	nx := int(!i)
	dump(x)
	dump(nx)
	a := [1, 2, 3]

	$if windows && tinyc {
		// workaround for tcc/windows bug
		println(a[nx])
		assert a[nx] == 1

		println(a[x])
		assert a[x] == 2
	} $else {
		println(a[int(!i)])
		assert a[int(!i)] == 1

		println(a[int(i)])
		assert a[int(i)] == 2
	}
}
