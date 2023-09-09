fn test_sorted_immutable_original_should_not_change() {
	a := ['hi', '1', '5', '3']
	b := a.sorted()
	assert a == ['hi', '1', '5', '3']
	assert b == ['1', '3', '5', 'hi']
}

fn test_sorted_mutable_original_should_not_change() {
	mut a := ['hi', '1', '5', '3']
	b := a.sorted()
	assert a == ['hi', '1', '5', '3']
	assert b == ['1', '3', '5', 'hi']
}

fn test_sorted_reversed() {
	aa := ['hi', '1', '5', '3']
	bb := aa.sorted(a > b)
	assert aa == ['hi', '1', '5', '3']
	assert bb == ['hi', '5', '3', '1']
}

fn test_sorted_by_len() {
	a := ['hi', 'abc', 'a', 'zzzzz']
	c := a.sorted(a.len < b.len)
	assert c == ['a', 'hi', 'abc', 'zzzzz']
}

fn test_sorted_can_be_called_on_an_array_literals() {
	b := [5, 1, 9].sorted()
	assert b == [1, 5, 9]
	assert [5.0, 1.2, 9.4].sorted() == [1.2, 5.0, 9.4]
	assert ['a', '00', 'z', 'dd', 'xyz'].sorted(a > b) == ['z', 'xyz', 'dd', 'a', '00']
	assert ['a', '00', 'zzzzz', 'dddd', 'xyz'].sorted(a.len > b.len) == ['zzzzz', 'dddd', 'xyz',
		'00', 'a']
	assert ['a', '00', 'zzzzz', 'dddd', 'xyz'].sorted(a.len < b.len) == ['a', '00', 'xyz', 'dddd',
		'zzzzz']
}

fn iarr() []int {
	return [5, 1, 9, 1, 2]
}

fn test_sorted_can_be_called_on_the_result_of_a_fn() {
	assert iarr().sorted() == [1, 1, 2, 5, 9]
	assert iarr().sorted(a > b) == [9, 5, 2, 1, 1]
}

fn sum(prev int, curr int) int {
	return prev + curr
}

fn test_sorting_2d_arrays() {
	assert [[1, 2], [3, 4, 5], [2]].sorted(a.len > b.len) == [
		[3, 4, 5],
		[1, 2],
		[2],
	]
	assert [[1, 2], [3, 4, 5], [2]].sorted(a.len < b.len) == [
		[2],
		[1, 2],
		[3, 4, 5],
	]
	assert unsafe { [[1, 2], [3, 4, 5], [2]].sorted(a[0] > b[0]) } == [
		[3, 4, 5],
		[2],
		[1, 2],
	]
	// assert [[1, 2], [3, 4, 5], [2]].sorted( a.reduce(sum) > b.reduce(sum) ) == ... // TODO
}

fn test_sorting_3d_arrays() {
	assert [[][]int{}, [[2, 22], [2]], [[3, 33], [3333], [33, 34, 35]],
		[[444]]].sorted(a.len > b.len) == [[[3, 33], [3333], [33, 34, 35]],
		[[2, 22], [2]], [[444]], [][]int{}]
}

fn test_sorted_with_compare() {
	aa := ['hi', '1', '5', '3']
	bb := aa.sorted_with_compare(fn (a &string, b &string) int {
		if a < b {
			return -1
		}
		if a > b {
			return 1
		}
		return 0
	})
	assert aa == ['hi', '1', '5', '3'], 'aa should stay unmodified'
	assert bb == ['1', '3', '5', 'hi'], 'bb should be sorted, according to the custom comparison callback fn'
}
