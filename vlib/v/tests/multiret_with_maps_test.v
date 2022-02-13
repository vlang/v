fn many_maps() (map[int]int, map[int]int, map[int]int) {
	return {
		1: 2
	}, {
		3: 4
	}, {
		5: 1000
	}
}

fn test_fn_returning_many_maps_at_the_same_time() {
	mut a, mut b, mut c := {
		0: 0
	}, {
		0: 0
	}, {
		0: 0
	}
	a, b, c = many_maps()
	dump(a)
	dump(b)
	dump(c)
	assert a == {
		1: 2
	}
	assert b == {
		3: 4
	}
	assert c == {
		5: 1000
	}
}
