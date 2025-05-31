struct Node {
	value int
mut:
	prev ?&Node
	next ?&Node
}

struct LinkedList {
mut:
	head ?&Node
	tail ?&Node
}

pub fn (mut l LinkedList) push(value int) {
	node := &Node{
		value: value
		prev:  l.tail
	}
	if l.head == none {
		l.head = node
	} else {
		mut tail := l.tail or { panic('head is not none but tail is') }
		tail.next = node
	}
	l.tail = node
}

pub fn (mut l LinkedList) pop() int {
	tail := l.tail or { panic('Empty list not supported') }
	if tail.prev == none { // single element
		l.head = none
		l.tail = none
	} else { // multiple elements
		mut prev := tail.prev
		prev.next = none
		l.tail = prev
	}
	return tail.value
}

fn test_option_struct_init_with_ref_opt() {
	mut ll := LinkedList{}
	ll.push(11)
	ll.push(22)
	ret1 := ll.pop()
	println(ret1)
	assert ret1 == 22
	ret2 := ll.pop()
	println(ret2)
	assert ret2 == 11
}
