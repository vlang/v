// A fixed array used where a dynamic array is expected is copied into a new array, or
// viewed in place behind a reference or a `mut` parameter. Only the fixed array itself
// is converted: an array or a map of fixed arrays is not one of dynamic arrays (see
// vlib/v/checker/tests/container_fixed_array_elem_mismatch_err.vv).
interface Named {
	name() string
}

struct Item {
	n int
}

fn (item Item) name() string {
	return 'item${item.n}'
}

fn sum(values &[]int) int {
	mut total := 0
	for i in 0 .. values.len {
		total += values[i]
	}
	return total
}

fn set_first(mut values []int) {
	values[0] = 9
}

fn count_rows(rows [][]int) int {
	return rows.len
}

fn names(values []Named) []string {
	return values.map(it.name())
}

fn test_fixed_array_value_is_copied() {
	fixed := [1, 2]!
	mut values := []int{}
	values = fixed
	values << 3
	assert values == [1, 2, 3]
	assert fixed[0] == 1
	mut rows := [][]int{}
	rows << fixed
	rows[0][0] = 9
	assert rows == [[9, 2]]
	assert fixed[0] == 1
	mut by_key := map[string][]int{}
	by_key['a'] = fixed
	assert by_key['a'] == [1, 2]
}

fn test_fixed_array_of_dynamic_arrays_is_copied() {
	fixed := [[1, 2], [3]]!
	assert count_rows(fixed) == 2
	mut rows := [][]int{}
	rows = fixed
	assert rows == [[1, 2], [3]]
}

fn test_fixed_array_elements_are_boxed() {
	fixed := [Item{1}, Item{2}]!
	assert names(fixed) == ['item1', 'item2']
}

fn test_fixed_array_is_viewed_in_place() {
	mut fixed := [1, 2]!
	assert sum(&fixed) == 3
	set_first(mut fixed)
	assert fixed[0] == 9
	assert fixed[1] == 2
}
