struct Node {
mut:
	next ?Node
}

fn test_struct_nested_option() {
	mut a := Node{}
	a.next = Node{}
	dump(a)
	dump(a.next?)
	dump(a.next?.next)
	assert a.next != none
	assert a.next?.next == none
}
