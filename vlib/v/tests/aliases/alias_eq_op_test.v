type MyInt = int

fn (i1 MyInt) == (i2 MyInt) bool {
	return int(i1) == int(i2)
}

fn test_main() {
	c := MyInt(3) == MyInt(1)
	assert c == false
}
