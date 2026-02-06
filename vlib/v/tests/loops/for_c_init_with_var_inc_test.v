fn test_for_c_init_with_var_inc() {
	mut results := []int{}
	mut i := 0
	for i++; i < 10; i++ {
		println(i)
		results << i
	}
	assert results == [1, 2, 3, 4, 5, 6, 7, 8, 9]
}
