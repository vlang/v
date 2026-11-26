// https://github.com/vlang/v/issues/32946: `a << f(b)`, where `f` is a generic
// function returning `[]T`, appends the elements of the result.

struct Item {
	name string
}

fn copy_of[T](a []T) []T {
	return a.clone()
}

struct Bag {
mut:
	items []Item
}

fn (mut b Bag) add(more []Item) {
	b.items << copy_of(more)
}

fn test_append_generic_fn_array_result() {
	mut items := [Item{'a'}]
	items << copy_of([Item{'b'}, Item{'c'}])
	assert items == [Item{'a'}, Item{'b'}, Item{'c'}]

	mut ints := [1]
	ints << copy_of([2, 3])
	assert ints == [1, 2, 3]

	mut bag := Bag{}
	bag.add([Item{'x'}, Item{'y'}])
	assert bag.items.len == 2
	assert bag.items[1].name == 'y'
}

fn test_append_generic_fn_array_result_as_one_element() {
	mut nested := [][]int{}
	nested << copy_of([1, 2])
	assert nested == [[1, 2]]
}
