module arrays

struct Pair {
	key int
	val int
}

fn test_stable_sort_basic() {
	mut a := [3, 1, 2]
	stable_sort(mut a, fn (x int, y int) bool { return x < y })
	assert a == [1, 2, 3]
}

fn test_stable_sort_empty() {
	mut a := []int{}
	stable_sort(mut a, fn (x int, y int) bool { return x < y })
	assert a == []
}

fn test_stable_sort_stability() {
	mut a := [Pair{2, 1}, Pair{1, 2}, Pair{2, 3}, Pair{1, 4}]
	stable_sort(mut a, fn (x Pair, y Pair) bool { return x.key < y.key })
	assert a[0].val == 2
	assert a[1].val == 4
	assert a[2].val == 1
	assert a[3].val == 3
}

fn test_stable_sort_descending() {
	mut a := [1, 2, 3, 4]
	stable_sort(mut a, fn (x int, y int) bool { return x > y })
	assert a == [4, 3, 2, 1]
}
