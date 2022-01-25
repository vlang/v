fn create() ?(int, string, bool) {
	return 5, 'aa', true
}

fn test_if_guard_with_multi_return() {
	if r1, r2, r3 := create() {
		println(r1)
		assert r1 == 5

		println(r2)
		assert r2 == 'aa'

		println(r3)
		assert r3 == true
	} else {
		assert false
	}
}
