fn test_nested_map_of_fn_call() {
	mut a := map[string]map[string]fn () int{}
	a['a']['a'] = fn () int {
		return 0
	}
	b := a['a']['a']()
	println(b)
	assert b == 0
}
