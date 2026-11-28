module arrays

struct TimPair {
	key int
	val int
}

fn test_timsort_basic() {
	mut a := [5, 2, 1, 10, 3]
	timsort(mut a, fn (x int, y int) bool { return x < y })
	assert a == [1, 2, 3, 5, 10]
}

fn test_timsort_empty() {
	mut a := []int{}
	timsort(mut a, fn (x int, y int) bool { return x < y })
	assert a == []
}

fn test_timsort_singleton() {
	mut a := [42]
	timsort(mut a, fn (x int, y int) bool { return x < y })
	assert a == [42]
}

fn test_timsort_stability() {
	mut a := [TimPair{2, 1}, TimPair{1, 2}, TimPair{2, 3}, TimPair{1, 4}]
	timsort(mut a, fn (x TimPair, y TimPair) bool { return x.key < y.key })
	assert a[0].val == 2
	assert a[1].val == 4
	assert a[2].val == 1
	assert a[3].val == 3
}

fn test_timsort_matches_merge_sort() {
	for seed in 0 .. 300 {
		mut n := 1 + (seed * 37) % 300
		mut a := []int{len: n}
		mut b := []int{len: n}
		for i in 0 .. n {
			v := (i * 97 + seed * 13) % 50
			a[i] = v
			b[i] = v
		}
		timsort(mut a, fn (x int, y int) bool { return x < y })
		// use stable_sort (now timsort-backed) reference via merge_sort? merge_sort still available
		mut aux := []int{len: n}
		merge_sort(mut b, mut aux, 0, n, fn (x int, y int) bool { return x < y })
		assert a == b
	}
}
