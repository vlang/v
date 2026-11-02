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

fn test_stable_sort_small_arrays_do_not_call_comparator() {
	mut empty := []int{}
	mut singleton := [42]
	never := fn (x int, y int) bool {
		panic('no comparison is needed for fewer than two elements')
	}
	stable_sort(mut empty, never)
	stable_sort(mut singleton, never)
	assert empty == []
	assert singleton == [42]
}

fn test_stable_sort_equal_elements_keep_input_order() {
	mut a := [Pair{1, 10}, Pair{1, 20}, Pair{1, 30}, Pair{1, 40}, Pair{1, 50}]
	expected := a.clone()
	stable_sort(mut a, fn (x Pair, y Pair) bool { return x.key < y.key })
	assert a == expected
}

fn test_stable_sort_duplicate_keys_across_merge_boundaries() {
	mut a := []Pair{}
	for i in 0 .. 257 {
		a << Pair{(i * 17) % 13, i}
	}
	mut expected := []Pair{}
	for key in 0 .. 13 {
		for value in a {
			if value.key == key {
				expected << value
			}
		}
	}
	stable_sort(mut a, fn (x Pair, y Pair) bool { return x.key < y.key })
	assert a == expected
}

fn test_stable_sort_descending_stability() {
	mut a := [Pair{2, 0}, Pair{1, 1}, Pair{3, 2}, Pair{2, 3}, Pair{3, 4}]
	stable_sort(mut a, fn (x Pair, y Pair) bool { return x.key > y.key })
	assert a == [Pair{3, 2}, Pair{3, 4}, Pair{2, 0}, Pair{2, 3}, Pair{1, 1}]
}

fn test_stable_sort_slice_changes_only_its_range() {
	mut a := [99, 4, 1, 3, 2, 88]
	// Keep the slice aliased so mutations must remain within its range in the parent.
	mut part := unsafe { a[1..5] }
	stable_sort(mut part, fn (x int, y int) bool { return x < y })
	assert part == [1, 2, 3, 4]
	assert a == [99, 1, 2, 3, 4, 88]
}

fn test_stable_sort_nested_arrays() {
	mut a := [[3, 4, 5], [1, 2], [4], [7, 8]]
	stable_sort(mut a, fn (x []int, y []int) bool { return x.len < y.len })
	assert a == [[4], [1, 2], [7, 8], [3, 4, 5]]
}

fn test_stable_sort_references() {
	first := &Pair{2, 10}
	second := &Pair{1, 20}
	third := &Pair{2, 30}
	mut a := [first, second, third]
	stable_sort(mut a, fn (x &Pair, y &Pair) bool { return x.key < y.key })
	assert a == [second, first, third]
}

fn test_stable_sort_strings_by_length() {
	mut a := ['apple', 'cat', 'a', 'pear', 'dog', 'b']
	stable_sort(mut a, fn (x string, y string) bool { return x.len < y.len })
	assert a == ['a', 'b', 'cat', 'dog', 'pear', 'apple']
}
