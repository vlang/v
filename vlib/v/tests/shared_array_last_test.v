module main

fn test_shared_array_last() {
	shared a := []int{}
	lock {
		a << 1
		a << 2
	}
	rlock a {
		println(a.last())
		assert a.last() == 2
	}
}
