module main

import datatypes

struct Item {
	priority int
}

fn (a Item) < (b Item) bool {
	return a.priority < b.priority
}

fn (a Item) == (b Item) bool {
	return a.priority == b.priority
}

// Issue https://github.com/vlang/v/issues/13318
fn test_comparison_operator_override_works_with_generic_datatypes() {
	mut heap := datatypes.MinHeap<Item>{}
	heap.insert(Item{10})
	assert heap.peek()?.priority == 10
	heap.insert(Item{100})
	assert heap.peek()?.priority == 10
	heap.insert(Item{2})
	assert heap.peek()?.priority == 2
	heap.insert(Item{5})
	assert heap.peek()?.priority == 2
	heap.insert(Item{1})
	assert heap.peek()?.priority == 1
	println(heap)
}
