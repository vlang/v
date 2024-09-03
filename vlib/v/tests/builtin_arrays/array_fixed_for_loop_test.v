fn double() (bool, [4]i16) {
	return true, [4]i16{}
}

fn foo() [4]i16 {
	_, b := double()
	for i in b {
		println(i)
	}
	return b
}

fn test_main() {
	arr := foo()
	assert arr == [4]i16{}
}
