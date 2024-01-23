fn example[T]() ?T {
	return T{}
}

fn test_option() {
	dump(example[[1]int]())

	a := example[[1]int]()
	assert a? == [0]!
}

fn example2[T]() !T {
	return T{}
}

fn test_result() {
	dump(example2[[1]int]()!)

	a := example2[[1]int]()!
	assert a == [0]!
}
