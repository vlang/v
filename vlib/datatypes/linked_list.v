module datatypes

@[heap]
pub struct ListNode[T] {
mut:
	data T
	next &ListNode[T] = unsafe { nil }
}

@[heap]
pub struct LinkedList[T] {
mut:
	head &ListNode[T] = unsafe { nil }
	tail &ListNode[T] = unsafe { nil }
	len  int
	// Internal iter pointer for allowing safe modification
	// of the list while iterating. TODO: use an option
	// instead of a pointer to determine if it is initialized.
	iter &ListIter[T] = unsafe { nil }
}

// is_empty checks if the linked list is empty
@[inline]
pub fn (list LinkedList[T]) is_empty() bool {
	return list.len == 0
}

// len returns the length of the linked list
@[inline]
pub fn (list LinkedList[T]) len() int {
	return list.len
}

// first returns the first element of the linked list
@[inline]
pub fn (list LinkedList[T]) first() !T {
	return if !list.is_empty() { list.head.data } else { error('Linked list is empty') }
}

// last returns the last element of the linked list
@[inline]
pub fn (list LinkedList[T]) last() !T {
	return if !list.is_empty() { list.tail.data } else { error('Linked list is empty') }
}

// index returns the element at the given index of the linked list
pub fn (list LinkedList[T]) index(idx int) !T {
	if list.is_empty() {
		return error('Linked list is empty')
	}
	if idx < 0 || idx >= list.len {
		return error('Index ${idx} out of bounds')
	}
	mut node := list.head
	for _ in 0 .. idx {
		node = node.next
	}
	return node.data
}

// push adds an element to the end of the linked list
pub fn (mut list LinkedList[T]) push(item T) {
	new_node := &ListNode[T]{
		data: item
	}
	if list.is_empty() {
		// first node case
		list.head = new_node
	} else {
		list.tail.next = new_node
	}
	list.tail = new_node
	list.len += 1
}

// push adds an array of elements to the end of the linked list
pub fn (mut list LinkedList[T]) push_many(elements []T) {
	for v in elements {
		list.push(v)
	}
}

// pop removes the last element of the linked list
pub fn (mut list LinkedList[T]) pop() !T {
	if list.is_empty() {
		return error('Linked list is empty')
	}
	mut node := list.head
	mut to_return := unsafe { node.data }
	if isnil(node.next) {
		// first node case
		// set to null
		list.head = unsafe { nil }
		list.tail = unsafe { nil }
	} else {
		for !isnil(node.next.next) {
			node = node.next
		}
		to_return = unsafe { node.next.data }
		// set to null
		node.next = unsafe { nil }
		list.tail = node
	}
	list.len -= 1
	return to_return
}

// shift removes the first element of the linked list
pub fn (mut list LinkedList[T]) shift() !T {
	if list.is_empty() {
		return error('Linked list is empty')
	} else {
		list.len -= 1
		node := list.head
		list.head = node.next
		if list.is_empty() {
			list.tail = unsafe { nil }
		}
		return node.data
	}
}

// insert adds an element to the linked list at the given index
pub fn (mut list LinkedList[T]) insert(idx int, item T) ! {
	if idx < 0 || idx > list.len {
		return error('Index ${idx} out of bounds [0..${list.len}]')
	}
	if idx == list.len {
		list.push(item)
		return
	}
	mut new_node := &ListNode[T]{
		data: item
	}
	if list.is_empty() {
		list.head = new_node
		list.tail = new_node
	} else {
		mut node := list.head

		if idx == 0 {
			// first node case
			new_node.next = node
			list.head = new_node
		} else {
			for i := 0; i < idx - 1; i++ {
				node = node.next
			}
			new_node.next = node.next
			node.next = new_node
			if isnil(new_node.next) {
				list.tail = new_node
			}
		}
	}
	list.len += 1
}

// prepend adds an element to the beginning of the linked list (equivalent to insert(0, item))
pub fn (mut list LinkedList[T]) prepend(item T) {
	list.insert(0, item) or {}
}

// str returns a string representation of the linked list
pub fn (list LinkedList[T]) str() string {
	return list.array().str()
}

// array returns a array representation of the linked list
pub fn (list LinkedList[T]) array() []T {
	mut result_array := []T{cap: list.len}
	mut node := list.head
	for !isnil(node) {
		result_array << node.data
		node = node.next
	}
	return result_array
}

// next implements the iteration interface to use LinkedList
// with V's `for` loop syntax.
pub fn (mut list LinkedList[T]) next() ?T {
	if isnil(list.iter) {
		// initialize new iter object
		list.iter = &ListIter[T]{
			node: list.head
		}
		return list.next()
	}
	if isnil(list.iter.node) {
		list.iter = unsafe { nil }
		return none
	}
	defer {
		list.iter.node = list.iter.node.next
	}
	return list.iter.node.data
}

// iterator returns a new iterator instance for the `list`.
pub fn (mut list LinkedList[T]) iterator() ListIter[T] {
	return ListIter[T]{
		node: list.head
	}
}

// ListIter[T] is an iterator for LinkedList.
// It can be used with V's `for x in iter {` construct.
// One list can have multiple independent iterators, pointing to different positions/places in the list.
// An iterator instance always traverses the list from start to finish.
pub struct ListIter[T] {
mut:
	node &ListNode[T] = unsafe { nil }
}

// next returns the next element of the list, or `none` when the end of the list is reached.
// It is called by V's `for x in iter{` on each iteration.
pub fn (mut iter ListIter[T]) next() ?T {
	if isnil(iter.node) {
		return none
	}
	res := unsafe { iter.node.data }
	iter.node = iter.node.next
	return res
}
