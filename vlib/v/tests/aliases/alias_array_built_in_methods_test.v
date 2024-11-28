type IntArray = []int

fn test_alias_array_method() {
	mut arr := IntArray([0, 1, 2, 3])
	res := arr.filter(it > 0)
	assert res == IntArray([1, 2, 3])

	assert arr.first() == 0
	assert arr.last() == 3
	arr.pop()
	assert arr == IntArray([0, 1, 2])

	arr2 := arr.map(it)
	assert arr2 == [0, 1, 2]

	assert arr.any(it > 0)
	assert !arr.all(it > 0)

	arr.sort(a > b)
	assert arr == IntArray([2, 1, 0])
	arr.sort()
	assert arr == IntArray([0, 1, 2])

	assert arr.contains(1)
}
