struct Crasher {
	value int
}

fn crash(c [1]Crasher) fn () int {
	return fn [c] () int {
		println(c[0].value)
		return c[0].value
	}
}

fn test_closure_with_fixed_array_var() {
	crash_fn := crash([Crasher{1}]!)
	ret := crash_fn()
	assert ret == 1
}
