module datatypes

// heapify builds a MinHeap from elements in O(n) using Floyd's
// bottom-up heap construction (repeated insert would be O(n log n)).
// It is the V-idiomatic equivalent of Python's heapq.heapify.
pub fn heapify[T](elements []T) MinHeap[T] {
	mut h := MinHeap[T]{
		data: elements.clone()
	}
	// start from the last parent and sift down to the root
	for i := h.data.len / 2 - 1; i >= 0; i-- {
		h.sift_down(i)
	}
	return h
}

// pushpop pushes item and pops the smallest element (Python's
// heapq.heappushpop). When item is the smallest, the heap is left untouched
// and item is returned, so no sift work is needed; otherwise the old root is
// returned and item takes its place.
pub fn (mut h MinHeap[T]) pushpop(item T) !T {
	if h.data.len == 0 || item <= h.data[0] {
		return item
	}
	root := h.pop()!
	h.insert(item)
	return root
}

// replace pops the smallest element and pushes item afterwards (the reverse
// order of pushpop, matching Python's heapq.heapreplace).
pub fn (mut h MinHeap[T]) replace(item T) !T {
	old := h.pop()!
	h.insert(item)
	return old
}

// sift_down restores the heap property below idx (Floyd's sift-down).
fn (mut h MinHeap[T]) sift_down(idx int) {
	mut parent := idx
	for {
		mut left := h.left_child(parent) or { return }
		mut right := h.right_child(parent) or { left }
		mut smallest := parent
		if h.data[left] < h.data[smallest] {
			smallest = left
		}
		if h.data[right] < h.data[smallest] {
			smallest = right
		}
		if smallest == parent {
			return
		}
		h.data[parent], h.data[smallest] = h.data[smallest], h.data[parent]
		parent = smallest
	}
}

// nlargest returns the n largest elements of items in descending order
// (Python's heapq.nlargest). k <= 0 yields an empty array; the input is
// not modified. A min-heap of size k is maintained, so the cost is
// O(len * log k).
pub fn nlargest[T](n int, items []T) []T {
	if n <= 0 || items.len == 0 {
		return []T{}
	}
	if n >= items.len {
		mut all := items.clone()
		sort_desc(mut all)
		return all
	}
	mut heap := heapify(items[..n].clone()) // min-heap holding the n largest
	for v in items[n..] {
		if v > heap.data[0] {
			heap.data[0] = v
			heap.sift_down(0)
		}
	}
	mut out := heap.data.clone()
	sort_desc(mut out)
	return out
}

// nsmallest returns the n smallest elements of items in ascending order
// (Python's heapq.nsmallest).
pub fn nsmallest[T](n int, items []T) []T {
	if n <= 0 {
		return []T{}
	}
	mut h := heapify(items)
	mut out := []T{cap: if n > items.len { items.len } else { n }}
	for out.len < n {
		out << h.pop() or { break }
	}
	return out
}

// sort_desc sorts a in descending order, in place.
fn sort_desc[T](mut a []T) {
	a.sort_with_compare(fn (x &T, y &T) int {
		if *x < *y {
			return 1
		}
		if *x > *y {
			return -1
		}
		return 0
	})
}
