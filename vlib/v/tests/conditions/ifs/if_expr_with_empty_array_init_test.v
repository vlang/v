fn test_if_expr_with_empty_array_init() {
	arr1 := ['Peter', 'Bob']
	arr2 := ['Sam', 'Mike']
	typ := 1
	names := if typ == 1 {
		arr1
	} else if typ == 2 {
		arr2
	} else {
		[]
	}
	println(names)
	assert names == ['Peter', 'Bob']
}
