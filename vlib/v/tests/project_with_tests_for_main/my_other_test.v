module main

fn test_iadd_3_4() {
	a := iadd(3, 4)
	assert a == 7
	assert iadd(10, 20) == 30
}
