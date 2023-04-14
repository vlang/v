struct Node {
mut:
	next ?&Node = unsafe { nil }
}

fn test_struct_nested_option() {
	mut b := Node{
		next: unsafe { nil }
	}
	mut a := Node{
		next: &b
	}
	dump(a)
	assert a.next != none
	dump(a.next?)
	assert a.next?.next == none
}
