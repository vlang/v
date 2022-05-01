fn test_array_with_it() {
	assert [0, 1, 2, 3, 4, 5]! == [6]int{init: it}
	a1 := [6]int{init: it}
	assert a1 == [0, 1, 2, 3, 4, 5]!

	assert [0, 1, 4, 9, 16, 25] == []int{len: 6, init: it * it}
	a2 := []int{len: 6, init: it * it}
	assert a2 == [0, 1, 4, 9, 16, 25]

	assert [1, 2, 3, 4, 5] == []int{len: 5, init: it + 1}
	a3 := []int{len: 5, init: it + 1}
	assert a3 == [1, 2, 3, 4, 5]

	assert [5, 4, 3, 2, 1] == []int{len: 5, init: 5 - it}
	a4 := []int{len: 5, init: 5 - it}
	assert a4 == [5, 4, 3, 2, 1]
}
