struct Split<T> {
	arr  []T
	pred fn (T) bool
mut:
	idx int
}

fn (mut iter Split<T>) next() ?[]T {
	start := iter.idx
	for iter.idx < iter.arr.len {
		if iter.pred(iter.arr[iter.idx]) {
			iter.idx++
			return iter.arr[start..iter.idx]
		}
		iter.idx++
	}
	return none
}

fn split<T>(arr []T, pred fn (T) bool) Split<T> {
	return Split<T>{arr, pred, 0}
}

fn test_for_in_iterator_of_generic_struct() {
	items := [0, 1, 2, 3, 4, 5, 6, 7, 8]
	mut iter := split<int>(items, fn (item int) bool {
		return item % 3 == 0
	})
	iter.next() or {}
	mut ret_list := [][]int{}
	for item in iter {
		dump(item)
		ret_list << item
	}
	println(ret_list)
	assert ret_list.len == 2
	assert ret_list[0] == [1, 2, 3]
	assert ret_list[1] == [4, 5, 6]
}
