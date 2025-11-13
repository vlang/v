struct MyInt {
	x int
}

fn (i1 MyInt) == (i2 MyInt) bool {
	return i1.x == i2.x
}

fn test_main() {
	assert (MyInt{30} == MyInt{2}) == false
}
