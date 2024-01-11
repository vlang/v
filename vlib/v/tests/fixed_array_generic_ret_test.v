fn example[T]() ?T {
	return T{}
}

fn test_main() {
	dump(example[[1]int]())

	a := example[[1]int]()
	assert a? == [0]!
}
