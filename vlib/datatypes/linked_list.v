module datatypes

pub struct ListNode<T> {
mut:
	data T
	next &ListNode<T> = unsafe { 0 }
}

pub struct LinkedList<T> {
mut:
	head &ListNode<T> = unsafe { 0 }
	len  int
	// Internal iter pointer for allowing safe modification
	// of the list while iterating. TODO: use an option
	// instead of a pointer to determine if it is initialized.
	iter &ListIter<T> = unsafe { 0 }
}

// is_empty checks if the linked list is empty
pub fn (list LinkedList<T>) is_empty() bool {
	return list.len == 0
}

// len returns the length of the linked list
pub fn (list LinkedList<T>) len() int {
	return list.len
}

// first returns the first element of the linked list
pub fn (list LinkedList<T>) first() ?T {
	return if !list.is_empty() { list.head.data } else { error('Linked list is empty') }
}

// last returns the last element of the linked list
pub fn (list LinkedList<T>) last() ?T {
	if unsafe { list.head == 0 } {
		return error('Linked list is empty')
	} else {
		mut node := list.head
		for unsafe { node.next != 0 } {
			node = node.next
		}
		return node.data
	}
}

// index returns the element at the given index of the linked list
pub fn (list LinkedList<T>) index(idx int) ?T {
	if unsafe { list.head == 0 } {
		return error('Linked list is empty')
	} else {
		mut node := list.head
		mut iterations := 0
		for unsafe { node.next != 0 } && iterations < idx {
			node = node.next
			iterations++
		}
		if iterations == idx {
			return node.data
		} else {
			return error('Index out of bounds')
		}
	}
}

// push adds an element to the end of the linked list
pub fn (mut list LinkedList<T>) push(item T) {
	new_node := &ListNode<T>{
		data: item
	}
	if unsafe { list.head == 0 } {
		// first node case
		list.head = new_node
	} else {
		mut node := list.head
		for unsafe { node.next != 0 } {
			node = node.next
		}
		node.next = new_node
	}
	list.len += 1
}

// pop removes the last element of the linked list
pub fn (mut list LinkedList<T>) pop() ?T {
	if unsafe { list.head == 0 } {
		return error('Linked list is empty')
	}
	mut node := list.head
	mut to_return := unsafe { node.data }
	if unsafe { node.next == 0 } {
		// first node case
		// set to null
		list.head = unsafe { nil }
	} else {
		for unsafe { node.next.next != 0 } {
			node = node.next
		}
		to_return = unsafe { node.next.data }
		// set to null
		node.next = unsafe { nil }
	}
	list.len -= 1
	return to_return
}

// shift removes the first element of the linked list
pub fn (mut list LinkedList<T>) shift() ?T {
	if unsafe { list.head == 0 } {
		return error('Linked list is empty')
	} else {
		list.len -= 1
		node := list.head
		list.head = node.next
		return node.data
	}
}

// insert adds an element to the linked list at the given index
pub fn (mut list LinkedList<T>) insert(idx int, item T) ? {
	if idx < 0 || idx > list.len {
		return error('Index out of bounds')
	} else if list.len == 0 {
		list.push(item)
	} else {
		list.len += 1
		mut node := list.head

		if idx == 0 {
			// first node case
			list.head = &ListNode<T>{
				data: item
				next: node
			}
		} else {
			for i := 0; i < idx - 1; i++ {
				node = node.next
			}
			node.next = &ListNode<T>{
				data: item
				next: node.next
			}
		}
	}
}

// prepend adds an element to the beginning of the linked list (equivalent to insert(0, item))
pub fn (mut list LinkedList<T>) prepend(item T) {
	list.insert(0, item) or {}
}

// str returns a string representation of the linked list
pub fn (list LinkedList<T>) str() string {
	return list.array().str()
}

// array returns a array representation of the linked list
pub fn (list LinkedList<T>) array() []T {
	mut result_array := []T{cap: list.len}
	mut node := list.head
	for unsafe { node != 0 } {
		result_array << node.data
		node = node.next
	}
	return result_array
}

// next implements the iteration interface to use LinkedList
// with V's `for` loop syntax.
pub fn (mut list LinkedList<T>) next() ?T {
	if list.iter == unsafe { nil } {
		// initialize new iter object
		list.iter = &ListIter<T>{
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
pub fn (mut list LinkedList<T>) iterator() ListIter<T> {
	return ListIter<T>{
		node: list.head
	}
}

// ListIter<T> is an iterator for LinkedList.
// It can be used with V's `for x in iter {` construct.
// One list can have multiple independent iterators, pointing to different positions/places in the list.
// An iterator instance always traverses the list from start to finish.
pub struct ListIter<T> {
mut:
	node &ListNode<T> = unsafe { 0 }
}

// next returns the next element of the list, or `none` when the end of the list is reached.
// It is called by V's `for x in iter{` on each iteration.
pub fn (mut iter ListIter<T>) next() ?T {
	if iter.node == unsafe { nil } {
		return none
	}
	res := iter.node.data
	iter.node = iter.node.next
	return res
}
