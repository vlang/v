struct Abc {
	x [20]int
	y [30]int
}

struct Xyz {
	x int = 5
}

fn test_assert_offsetof() {
	assert __offsetof(Abc, y) == 1
}

fn test_assert_sizeof() {
	assert sizeof(Abc) == sizeof(Xyz)
}
