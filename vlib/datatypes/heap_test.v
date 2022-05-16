module datatypes

fn test_min_heap() ? {
	mut heap := MinHeap<int>{}
	heap.insert(2)
	heap.insert(0)
	heap.insert(8)
	heap.insert(4)
	heap.insert(1)

	assert heap.pop()? == 0
	assert heap.pop()? == 1
	assert heap.pop()? == 2
	assert heap.pop()? == 4
	assert heap.pop()? == 8
	if _ := heap.pop() {
		panic('expected none')
	}
}

struct Item {
	data     string
	priority int
}

fn (lhs Item) < (rhs Item) bool {
	return rhs.priority < lhs.priority
}

fn test_min_heap_custom() ? {
	mut heap := MinHeap<Item>{}
	heap.insert(Item{'buz', 10})
	heap.insert(Item{'qux', 0})
	heap.insert(Item{'baz', 50})
	heap.insert(Item{'foo', 100})
	heap.insert(Item{'bar', 80})

	assert heap.pop()?.data == 'foo'
	assert heap.pop()?.data == 'bar'
	assert heap.pop()?.data == 'baz'
	assert heap.pop()?.data == 'buz'
	assert heap.pop()?.data == 'qux'
	if _ := heap.pop() {
		panic('expected none')
	}
}

fn test_heap_len() ? {
	mut heap := MinHeap<int>{}
	heap.insert(2)
	assert heap.len() == 1
	heap.insert(0)
	heap.insert(8)
	heap.insert(4)
	assert heap.len() == 4
	heap.insert(1)

	assert heap.len() == 5
	heap.pop()?
	heap.pop()?
	heap.pop()?
	assert heap.len() == 2
	heap.pop()?
	heap.pop()?
	assert heap.len() == 0
	heap.pop() or {}
	assert heap.len() == 0
}
