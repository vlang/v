struct Name<T> {
	v T
}

struct Point {
}

fn test_generic_ptr_cast() {
	p := &Point(0)
	assert p == 0
	pn := &Name<int>(0)
	assert pn == 0
}
