module datatypes

fn test_heapify_matches_sorted() {
	mut h := heapify([5, 3, 8, 1, 9, 2, 7])
	assert h.len() == 7
	mut drained := []int{}
	for {
		drained << h.pop() or { break }
	}
	assert drained == [1, 2, 3, 5, 7, 8, 9]
	assert h.len() == 0
}

fn test_heapify_empty_and_single() {
	mut h := heapify([]int{})
	assert h.len() == 0
	if _ := h.pop() {
		assert false, 'empty heap pop must error'
	} else {
		assert err.msg() == 'Heap is empty'
	}
	single := heapify([42])
	assert single.peek()! == 42
}

fn test_pushpop_and_replace() {
	mut h := heapify([1, 3, 5])
	assert h.pushpop(0)! == 0, 'incoming smaller than root: returns it'
	assert h.pop()! == 1
	mut h2 := heapify([1, 3, 5])
	assert h2.replace(4)! == 1, 'replace pops first, then pushes'
	assert h2.pop()! == 3
	assert h2.pop()! == 4
	assert h2.pop()! == 5
}

fn test_nlargest() {
	assert nlargest(3, [5, 1, 9, 3, 7]) == [9, 7, 5]
	assert nlargest(1, [5, 1, 9, 3]) == [9]
	assert nlargest(0, [1, 2, 3]).len == 0
	assert nlargest(10, [4, 2, 8]) == [8, 4, 2], 'k > len returns all, sorted'
	assert nlargest(2, []int{}).len == 0
}

fn test_nsmallest() {
	assert nsmallest(3, [5, 1, 9, 3, 7]) == [1, 3, 5]
	assert nsmallest(2, [5, 1, 9, 3]) == [1, 3]
	assert nsmallest(10, [4, 2, 8]) == [2, 4, 8]
	assert nsmallest(0, [1, 2]).len == 0
	assert nsmallest(2, [1, 1, 1]) == [1, 1], 'duplicates are kept'
}

fn test_heapq_edges_and_input_ownership() {
	mut empty := heapify([]int{})
	assert empty.pushpop(7)! == 7
	assert empty.len() == 0
	if _ := empty.replace(4) {
		assert false, 'replace needs an existing element'
	} else {
		assert err.msg() == 'Heap is empty'
	}
	input := [3, 1, 2]
	mut heap := heapify(input)
	assert input == [3, 1, 2]
	assert heap.pushpop(1)! == 1
	assert heap.len() == 3
	assert heap.pop()! == 1
	assert heap.pushpop(4)! == 2
	assert heap.pop()! == 3
	assert heap.pop()! == 4
	assert nlargest(2, ['b', 'a', 'c']) == ['c', 'b']
	assert nsmallest(2, ['b', 'a', 'c']) == ['a', 'b']
	assert nlargest(-1, input) == []int{}
	assert nsmallest(-1, input) == []int{}
	assert input == [3, 1, 2]
}
