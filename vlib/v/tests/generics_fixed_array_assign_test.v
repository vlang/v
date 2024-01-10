fn init_type[T]() {
	_ := T{}
}

fn test_main() {
	init_type[[3]int]()
	assert true
}
