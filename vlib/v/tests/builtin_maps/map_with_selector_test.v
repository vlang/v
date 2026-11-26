fn test_main() {
	k := [10, 20, 30]
	println(k.map(it.hex()))
	dump(k.map(it.hex))
	a := k.map(it.hex)
	assert dump(a[0]()) == 'a'
	assert dump(a[1]()) == '14'
	assert dump(a[2]()) == '1e'
}

struct MapSelectorItem {
	n int
}

fn (i MapSelectorItem) label() string {
	return 'item${i.n}'
}

// A bound method value mapped from `it` is an element of the result, so the
// result prints as an array of functions instead of being read as `[]string`.
fn test_map_with_method_value_selector_keeps_fn_elements() {
	k := [10, 20, 30]
	assert k.map(it.hex).str() == '[fn () string, fn () string, fn () string]'
	items := [MapSelectorItem{1}, MapSelectorItem{2}]
	assert items.map(it.label).str() == '[fn () string, fn () string]'
	labels := items.map(it.label)
	assert labels[1]() == 'item2'
}
