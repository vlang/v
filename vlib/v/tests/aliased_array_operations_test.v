type Ints = []int

fn test_first() {
	ints := Ints([5, 10])
	assert ints.first() == 5
}

fn test_last() {
	ints := Ints([7, 4])
	assert ints.last() == 4
}

fn test_index() {
	ints := Ints([1, 5, 2, 3])
	assert ints[2] == 2
}
