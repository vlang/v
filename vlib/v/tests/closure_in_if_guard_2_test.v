fn get_info() ?(int, bool) {
	return 45, true
}

fn test_closure_in_if_guard() {
	mut ret := ''
	if v1, v2 := get_info() {
		func := fn [v1, v2] () string {
			println(v1)
			println(v2)
			return '${v1}, ${v2}'
		}
		ret = func()
	}
	assert ret == '45, true'
}
