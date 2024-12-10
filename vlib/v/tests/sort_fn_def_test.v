import arrays

struct Abc {
	x int
}

fn (s1 Abc) < (s2 Abc) bool {
	return s1.x < s2.x
}

fn test_main() {
	a := [Abc{123}, Abc{42}, Abc{999}, Abc{42}, Abc{123}]
	dump(arrays.distinct(a))
	assert true
}
