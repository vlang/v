type Test = []int

fn test_index() {
	mut t := Test([2, 4])
	assert t[1] == 4
	assert t == Test([2, 4])
	t << 6
	assert t == Test([2, 4, 6])
	t << Test([8, 10])
	assert t == Test([2, 4, 6, 8, 10])
}
