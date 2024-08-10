type Ints = []int
type Ints2 = [][]int

fn test_level_1() {
	v1 := Ints([5, 7, 9])
	v2 := []int(v1)

	assert v2 == [5, 7, 9]
}

fn test_level_2() {
	v3 := Ints2([[5, 7, 9]])
	v4 := [][]int(v3)

	assert v4 == [[5, 7, 9]]
}
