import x.json2.decoder2 as json

fn test_array_of_strings() {
	assert json.decode[[]int]('[1, 2, 3]')! == [1, 2, 3]

	assert json.decode[[]string]('["a", "b", "c"]')! == ['a', 'b', 'c']

	assert json.decode[[]bool]('[true, false, true]')! == [true, false, true]

	assert json.decode[[]f64]('[1.1, 2.2, 3.3]')! == [1.1, 2.2, 3.3]

	// nested arrays
	assert json.decode[[][]int]('[[1, 22], [333, 4444]]')! == [
		[1, 22],
		[333, 4444],
	]

	assert json.decode[[][]string]('[["a", "b"], ["c", "d"]]')! == [
		['a', 'b'],
		['c', 'd'],
	]

	assert json.decode[[][]bool]('[[true, false], [false, true]]')! == [
		[true, false],
		[false, true],
	]
}
