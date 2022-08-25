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
	mut result_array := []T{}
	mut node := list.head
	for unsafe { node != 0 } {
		result_array << node.data
		node = node.next
	}
	return result_array
}
