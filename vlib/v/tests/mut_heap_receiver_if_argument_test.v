@[heap]
struct HeapCounter {
mut:
	value int
}

fn (mut counter HeapCounter) add(value int) int {
	counter.value += value
	return counter.value
}

fn test_mut_heap_receiver_with_if_argument_updates_original() {
	mut counter := HeapCounter{}
	result := counter.add(if counter.value == 0 { 1 } else { 2 })
	assert result == 1
	assert counter.value == 1
}
