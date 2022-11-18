module datatypes

struct DoublyListNode<T> {
mut:
	data T
	next &DoublyListNode<T> = unsafe { 0 }
	prev &DoublyListNode<T> = unsafe { 0 }
}

// DoublyLinkedList<T> represents a generic doubly linked list of elements, each of type T.
pub struct DoublyLinkedList<T> {
mut:
	head &DoublyListNode<T> = unsafe { 0 }
	tail &DoublyListNode<T> = unsafe { 0 }
	// Internal iter pointer for allowing safe modification
	// of the list while iterating. TODO: use an option
	// instead of a pointer to determine it is initialized.
	iter &DoublyListIter<T> = unsafe { 0 }
	len  int
}

// is_empty checks if the linked list is empty
pub fn (list DoublyLinkedList<T>) is_empty() bool {
	return list.len == 0
}

// len returns the length of the linked list
pub fn (list DoublyLinkedList<T>) len() int {
	return list.len
}

// first returns the first element of the linked list
pub fn (list DoublyLinkedList<T>) first() ?T {
	if list.is_empty() {
		return error('Linked list is empty')
	}
	return list.head.data
}

// last returns the last element of the linked list
pub fn (list DoublyLinkedList<T>) last() ?T {
	if list.is_empty() {
		return error('Linked list is empty')
	}
	return list.tail.data
}

// push_back adds an element to the end of the linked list
pub fn (mut list DoublyLinkedList<T>) push_back(item T) {
	mut new_node := &DoublyListNode<T>{
		data: item
	}
	if list.is_empty() {
		// first node case
		list.head = new_node
		list.tail = new_node
	} else {
		list.tail.next = new_node
		new_node.prev = list.tail
		list.tail = new_node
	}
	list.len += 1
}

// push_front adds an element to the beginning of the linked list
pub fn (mut list DoublyLinkedList<T>) push_front(item T) {
	mut new_node := &DoublyListNode<T>{
		data: item
	}
	if list.is_empty() {
		// first node case
		list.head = new_node
		list.tail = new_node
	} else {
		list.head.prev = new_node
		new_node.next = list.head
		list.head = new_node
	}
	list.len += 1
}

// pop_back removes the last element of the linked list
pub fn (mut list DoublyLinkedList<T>) pop_back() ?T {
	if list.is_empty() {
		return error('Linked list is empty')
	}
	defer {
		list.len -= 1
	}
	if list.len == 1 {
		// head == tail
		value := list.tail.data
		list.head = unsafe { nil }
		list.tail = unsafe { nil }
		return value
	}
	value := list.tail.data
	list.tail.prev.next = unsafe { nil } // unlink tail
	list.tail = list.tail.prev
	return value
}

// pop_front removes the last element of the linked list
pub fn (mut list DoublyLinkedList<T>) pop_front() ?T {
	if list.is_empty() {
		return error('Linked list is empty')
	}
	defer {
		list.len -= 1
	}
	if list.len == 1 {
		// head == tail
		value := list.head.data
		list.head = unsafe { nil }
		list.tail = unsafe { nil }
		return value
	}
	value := list.head.data
	list.head.next.prev = unsafe { nil } // unlink head
	list.head = list.head.next
	return value
}

// insert adds an element to the linked list at the given index
pub fn (mut list DoublyLinkedList<T>) insert(idx int, item T) ? {
	if idx < 0 || idx > list.len {
		return error('Index out of bounds')
	} else if idx == 0 {
		// new head
		list.push_front(item)
	} else if idx == list.len {
		// new tail
		list.push_back(item)
	} else if idx <= list.len / 2 {
		list.insert_front(idx, item)
	} else {
		list.insert_back(idx, item)
	}
}

// insert_back walks from the tail and inserts a new item at index idx
// (determined from the forward index). This function should be called
// when idx > list.len/2. This helper function assumes idx bounds have
// already been checked and idx is not at the edges.
fn (mut list DoublyLinkedList<T>) insert_back(idx int, item T) {
	mut node := list.node(idx + 1)
	mut prev := node.prev
	//   prev       node
	//  ------     ------
	//  |next|---->|next|
	//  |prev|<----|prev|
	//  ------     ------
	new := &DoublyListNode<T>{
		data: item
		next: node
		prev: prev
	}
	//   prev       new        node
	//  ------     ------     ------
	//  |next|---->|next|---->|next|
	//  |prev|<----|prev|<----|prev|
	//  ------     ------     ------
	node.prev = new
	prev.next = new
	list.len += 1
}

// insert_front walks from the head and inserts a new item at index idx
// (determined from the forward index). This function should be called
// when idx <= list.len/2. This helper function assumes idx bounds have
// already been checked and idx is not at the edges.
fn (mut list DoublyLinkedList<T>) insert_front(idx int, item T) {
	mut node := list.node(idx - 1)
	mut next := node.next
	//   node       next
	//  ------     ------
	//  |next|---->|next|
	//  |prev|<----|prev|
	//  ------     ------
	new := &DoublyListNode<T>{
		data: item
		next: next
		prev: node
	}
	//   node       new        next
	//  ------     ------     ------
	//  |next|---->|next|---->|next|
	//  |prev|<----|prev|<----|prev|
	//  ------     ------     ------
	node.next = new
	next.prev = new
	list.len += 1
}

// node walks from the head or tail and finds the node at index idx.
// This helper function assumes the list is not empty and idx is in
// bounds.
fn (list &DoublyLinkedList<T>) node(idx int) &DoublyListNode<T> {
	if idx <= list.len / 2 {
		mut node := list.head
		for h := 0; h < idx; h += 1 {
			node = node.next
		}
		return node
	}
	mut node := list.tail
	for t := list.len - 1; t >= idx; t -= 1 {
		node = node.prev
	}
	return node
}

// index searches the linked list for item and returns the forward index
// or none if not found.
pub fn (list &DoublyLinkedList<T>) index(item T) ?int {
	mut hn := list.head
	mut tn := list.tail
	for h, t := 0, list.len - 1; h <= t; {
		if hn.data == item {
			return h
		} else if tn.data == item {
			return t
		}
		h += 1
		hn = hn.next
		t -= 1
		tn = tn.prev
	}
	return none
}

// delete removes index idx from the linked list and is safe to call
// for any idx.
pub fn (mut list DoublyLinkedList<T>) delete(idx int) {
	if idx < 0 || idx >= list.len {
		return
	} else if idx == 0 {
		list.pop_front() or {}
		return
	} else if idx == list.len - 1 {
		list.pop_back() or {}
		return
	}
	// node should be somewhere in the middle
	mut node := list.node(idx)
	node.prev.next = node.next
	node.next.prev = node.prev
	list.len -= 1
}

// str returns a string representation of the linked list
pub fn (list DoublyLinkedList<T>) str() string {
	return list.array().str()
}

// array returns a array representation of the linked list
pub fn (list DoublyLinkedList<T>) array() []T {
	mut result_array := []T{cap: list.len}
	mut node := list.head
	for unsafe { node != 0 } {
		result_array << node.data
		node = node.next
	}
	return result_array
}

// next implements the iter interface to use DoublyLinkedList with
// V's `for x in list {` loop syntax.
pub fn (mut list DoublyLinkedList<T>) next() ?T {
	if list.iter == unsafe { nil } {
		// initialize new iter object
		list.iter = &DoublyListIter<T>{
			node: list.head
		}
		return list.next()
	}
	if list.iter.node == unsafe { nil } {
		list.iter = unsafe { nil }
		return none
	}
	defer {
		list.iter.node = list.iter.node.next
	}
	return list.iter.node.data
}

// iterator returns a new iterator instance for the `list`.
pub fn (mut list DoublyLinkedList<T>) iterator() DoublyListIter<T> {
	return DoublyListIter<T>{
		node: list.head
	}
}

// back_iterator returns a new backwards iterator instance for the `list`.
pub fn (mut list DoublyLinkedList<T>) back_iterator() DoublyListIterBack<T> {
	return DoublyListIterBack<T>{
		node: list.tail
	}
}

// DoublyListIter<T> is an iterator for DoublyLinkedList.
// It starts from *the start* and moves forwards to *the end* of the list.
// It can be used with V's `for x in iter {` construct.
// One list can have multiple independent iterators, pointing to different positions/places in the list.
// A DoublyListIter iterator instance always traverses the list from *start to finish*.
pub struct DoublyListIter<T> {
mut:
	node &DoublyListNode<T> = unsafe { 0 }
}

// next returns *the next* element of the list, or `none` when the end of the list is reached.
// It is called by V's `for x in iter{` on each iteration.
pub fn (mut iter DoublyListIter<T>) next() ?T {
	if iter.node == unsafe { nil } {
		return none
	}
	res := iter.node.data
	iter.node = iter.node.next
	return res
}

// DoublyListIterBack<T> is an iterator for DoublyLinkedList.
// It starts from *the end* and moves backwards to *the start* of the list.
// It can be used with V's `for x in iter {` construct.
// One list can have multiple independent iterators, pointing to different positions/places in the list.
// A DoublyListIterBack iterator instance always traverses the list from *finish to start*.
pub struct DoublyListIterBack<T> {
mut:
	node &DoublyListNode<T> = unsafe { 0 }
}

// next returns *the previous* element of the list, or `none` when the start of the list is reached.
// It is called by V's `for x in iter{` on each iteration.
pub fn (mut iter DoublyListIterBack<T>) next() ?T {
	if iter.node == unsafe { nil } {
		return none
	}
	res := iter.node.data
	iter.node = iter.node.prev
	return res
}
