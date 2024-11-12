fn test_match_expr_with_empty_array_init() {
	arr1 := ['Peter', 'Bob']
	arr2 := ['Sam', 'Mike']
	typ := 1
	names := match typ {
		1 {
			arr1
		}
		2 {
			arr2
		}
		else {
			[]
		}
	}
	println(names)
	assert names == ['Peter', 'Bob']
}
