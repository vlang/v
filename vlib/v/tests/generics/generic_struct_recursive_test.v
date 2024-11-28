struct Node[T] {
mut:
	value T
	next  ?&Node[T]
}

fn test_main() {
	mut n := Node[int]{
		value: 1
	}
	mut m := Node[int]{
		value: 2
	}
	n.next = &m
	a := n.next or { return }
	dump(a)
	dump(m)
	dump(n)
	assert a.value == 2
}
