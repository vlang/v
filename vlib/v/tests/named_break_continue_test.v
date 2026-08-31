fn test_labelled_for() {
	mut i := 4
	unsafe {
		goto L1
	}
	L1: for {
		i++
		for {
			if i < 7 {
				continue L1
			} else {
				break L1
			}
		}
	}
	assert i == 7

	unsafe {
		goto L2
	}
	L2: for ; true; i++ {
		for {
			if i < 17 {
				continue L2
			} else {
				break L2
			}
		}
	}
	assert i == 17

	unsafe {
		goto L3
	}
	L3: for e in [1, 2, 3, 4] {
		i = e
		for {
			if i < 3 {
				continue L3
			} else {
				break L3
			}
		}
	}
	assert i == 3

	mut seen := []int{}
	unsafe {
		goto L4
	}
	L4: for a, b := 0, 10; a < 4; a++, b-- {
		seen << a + b
		for {
			if a < 2 {
				continue L4
			} else {
				break L4
			}
		}
	}
	assert seen == [10, 10, 10]
}
