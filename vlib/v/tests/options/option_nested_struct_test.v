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
	dump(a.next)
	dump(a.next?.next)
	assert a.next?.next == none
}
