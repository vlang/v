// Generic calls used directly as struct-init field values must keep the
// type inferred from their arguments, for every shape of the return type.
fn element_counts[T](array []T) map[T]int {
	mut counts := map[T]int{}
	for element in array {
		counts[element]++
	}
	return counts
}

fn first[T](array []T) T {
	return array[0]
}

fn same[T](array []T) []T {
	return array
}

struct Box[T] {
	items []T
}

fn boxed[T](array []T) Box[T] {
	return Box[T]{
		items: array
	}
}

struct Bag[T] {
	items []T
}

fn (b Bag[T]) counts() map[T]int {
	return element_counts(b.items)
}

struct Attribute {
mut:
	counts  map[string]int
	initial string
	all     []string
}

struct Holder[T] {
	counts map[T]int
	box    Box[T]
}

fn make_holder[T](array []T) Holder[T] {
	return Holder[T]{
		counts: element_counts(array)
		box:    boxed(array)
	}
}

fn test_generic_call_map_return_in_struct_init() {
	words := ['apple', 'banana', 'apple', 'pear', 'banana', 'apple']
	att := Attribute{
		counts:  element_counts(words)
		initial: first(words)
		all:     same(words)
	}
	assert att.counts == {
		'apple':  3
		'banana': 2
		'pear':   1
	}
	assert att.initial == 'apple'
	assert att.all == words
}

fn test_explicit_generic_call_in_struct_init_still_works() {
	words := ['x', 'y', 'x']
	att := Attribute{
		counts: element_counts[string](words)
	}
	assert att.counts == {
		'x': 2
		'y': 1
	}
}

fn test_generic_method_call_in_struct_init() {
	bag := Bag[string]{
		items: ['a', 'b', 'a']
	}
	att := Attribute{
		counts: bag.counts()
	}
	assert att.counts == {
		'a': 2
		'b': 1
	}
}

fn test_generic_struct_return_in_struct_init() {
	numbers := [3, 1, 2]
	holder := Holder[int]{
		counts: element_counts(numbers)
		box:    boxed(numbers)
	}
	assert holder.counts == {
		3: 1
		1: 1
		2: 1
	}
	assert holder.box.items == numbers
}

fn test_struct_init_inside_generic_fn() {
	holder := make_holder(['p', 'q', 'p'])
	assert holder.counts == {
		'p': 2
		'q': 1
	}
	assert holder.box.items == ['p', 'q', 'p']
}
