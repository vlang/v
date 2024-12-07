fn test_3dims() {
	mut array := [][][]int{}
	array << [[1]]
	dump(array)
	array << [[[1]]]
	dump(array)
	array << [[[]]]
	println(array)
	assert array == [[[int(1)]], [[1]], [[]int{}]]
}

fn test_2dims() {
	mut array := [][]int{}
	array << [1]
	dump(array)
	array << [[1]]
	dump(array)
	array << [[]]
	println(array)
	assert array == [[int(1)], [1], []int{}]
}
