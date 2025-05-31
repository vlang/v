import arrays

fn get() []int {
	return [1, 2, 3, 4, 5]
}

fn get2() [][]int {
	return [[0], [1, 2, 3, 4, 5]]
}

fn receive(a ...int) {
}

fn test_main() {
	assert arrays.concat([0], ...get().map(it)) == [0, 1, 2, 3, 4, 5]
	assert arrays.concat[int]([], ...get().map(it)) == [1, 2, 3, 4, 5]
	assert arrays.concat[[]int]([[0]], ...[get().map(it)]) == [
		[0],
		[1, 2, 3, 4, 5],
	]
	assert arrays.concat[int](...get2()) == [0, 1, 2, 3, 4, 5]
}
