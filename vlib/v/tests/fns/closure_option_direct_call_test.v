fn test_closure_option_direct_call() {
	tmp_wd := 'aaa'
	b := fn [tmp_wd] () !string {
		return tmp_wd
	}() or { panic(err) }
	println(b)
	assert b == 'aaa'
}
