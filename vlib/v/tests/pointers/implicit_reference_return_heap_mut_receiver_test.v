@[heap]
struct HeapItem {
mut:
	value int
}

fn (mut item HeapItem) reference() &HeapItem {
	return item
}

fn test_implicit_reference_return_from_heap_mut_receiver() {
	mut item := HeapItem{
		value: 1
	}
	mut reference := item.reference()
	reference.value = 2
	assert item.value == 2
}
