module datatypes

// MinHeap is a binary minimum heap data structure.
pub struct MinHeap[T] {
mut:
	data []T
}

// insert adds an element to the heap.
pub fn (mut heap MinHeap[T]) insert(item T) {
	// push item to the end of the array
	heap.data << item
	// swap the new node with its parent until the heap is in order
	mut child := heap.data.len - 1
	mut parent := heap.parent(child)
	for heap.data[parent] > heap.data[child] {
		heap.data[parent], heap.data[child] = heap.data[child], heap.data[parent]
		child = parent
		parent = heap.parent(child)
	}
}

// insert array of elements to the heap.
pub fn (mut heap MinHeap[T]) insert_many(elements []T) {
	for v in elements {
		heap.insert(v)
	}
}

// pop removes the top-most element from the heap.
pub fn (mut heap MinHeap[T]) pop() !T {
	if heap.data.len == 0 {
		return error('Heap is empty')
	} else if heap.data.len == 1 {
		return heap.data.pop()
	}
	item := heap.data[0]
	// move last element to root
	heap.data[0] = heap.data.pop()
	// swap the new root with its minimum child until the heap is in order
	mut parent := 0
	mut left := heap.left_child(parent) or { return item }
	mut right := heap.right_child(parent) or { left }
	for heap.data[parent] > heap.data[left] || heap.data[parent] > heap.data[right] {
		// choose min for min heap
		swap := if heap.data[left] <= heap.data[right] { left } else { right }
		heap.data[parent], heap.data[swap] = heap.data[swap], heap.data[parent]
		parent = swap
		left = heap.left_child(parent) or { break }
		right = heap.right_child(parent) or { left }
	}
	return item
}

// peek gets the top-most element from the heap without removing it.
pub fn (heap MinHeap[T]) peek() !T {
	if heap.data.len == 0 {
		return error('Heap is empty')
	}
	return heap.data[0]
}

// len returns the number of elements in the heap.
pub fn (heap MinHeap[T]) len() int {
	return heap.data.len
}

// left_child is a helper function that returns the index of the left
// child given a parent idx, or none if there is no left child.
fn (heap MinHeap[T]) left_child(idx int) !int {
	child := 2 * idx + 1
	if child >= heap.data.len {
		return error('none')
	}
	return child
}

// right_child is a helper function that returns the index of the right
// child given a parent idx, or none if there is no right child.
fn (heap MinHeap[T]) right_child(idx int) !int {
	child := 2 * idx + 2
	if child >= heap.data.len {
		return error('none')
	}
	return child
}

// parent is a helper function that returns the parent index of the child.
fn (heap MinHeap[T]) parent(idx int) int {
	return (idx - 1) / 2
}
