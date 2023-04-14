struct Node {
mut:
	next ?&Node
}

fn test_struct_nested_option() {
	mut b := Node{}
	mut a := Node{
		next: &b
	}
	dump(a)
	assert a.next != none
	dump(a.next?)
	assert a.next?.next == none
}
