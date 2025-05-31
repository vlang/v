fn test_array_of_anon_fn_call() {
	xs := [fn (s string) (string, u32) {
		return s, 0
	}]
	r, n := xs[0]('e')
	assert n == 0
	assert r == 'e'
}
