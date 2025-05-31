fn test_for_c_init_with_var_assign() {
	mut v := 4
	mut r := 0
	for v >>= 1; v != 0; v >>= 1 {
		r++
	}
	println(r)
	assert r == 2
}
