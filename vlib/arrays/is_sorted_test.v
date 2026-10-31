module arrays

fn test_is_sorted() {
	assert is_sorted([1, 2, 2, 3])
	assert !is_sorted([3, 1])
	assert is_sorted([]int{})
	assert is_sorted([42])
	assert is_sorted(['a', 'b', 'c'])
	assert !is_sorted(['b', 'a'])
	assert is_sorted([1.5, 2.5, 2.5])
	assert !is_sorted([2.5, 1.5])
	mut v := [5, 3, 4]
	v.sort()
	assert is_sorted(v)
}
