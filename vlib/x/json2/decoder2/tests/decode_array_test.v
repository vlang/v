import x.json2.decoder2 as json

struct StructType[T] {
mut:
	val T
}

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

fn test_array_of_struct() {
	assert json.decode[[]StructType[int]]('[{"val": 1}, {"val": 2}, {"val": 3}, {"val": 4}]')! == [
		StructType{
			val: 1
		},
		StructType{
			val: 2
		},
		StructType{
			val: 3
		},
		StructType{
			val: 4
		},
	]
}
