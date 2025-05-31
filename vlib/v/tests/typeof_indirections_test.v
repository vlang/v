fn indirections[T](val T) int {
	return T.indirections
}

fn test_main() {
	a := 0
	assert typeof(a).indirections == 0
	assert indirections(a) == 0
	b := &a
	assert typeof(b).indirections == 1
	assert indirections(b) == 1
	c := [1]
	assert typeof(c).indirections == 0
	assert indirections(c) == 0
	d := &c
	assert typeof(d).indirections == 1
	assert indirections(d) == 1
}
