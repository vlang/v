struct Entry {
	name string
}

fn compare_entries(a &Entry, b &Entry) int {
	return compare_strings(a.name, b.name)
}

fn test_array_append_sorted_with_compare() {
	groups := {
		'x': [Entry{'b'}, Entry{'a'}]
	}
	mut sorted := []Entry{}
	sorted << groups['x'].sorted_with_compare(compare_entries)
	assert sorted.map(it.name) == ['a', 'b']
	mut nums := []int{}
	nums << [3, 1, 2].sorted_with_compare(fn (a &int, b &int) int {
		return *a - *b
	})
	assert nums == [1, 2, 3]
}
