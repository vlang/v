@[heap]
struct Node[T] {
mut:
	value T
	next  ?&Node[T]
}

fn print_t(node ?&Node[int]) ?&Node[int] {
	println(node)
	assert node == none
	return node
}

fn test_main() {
	n := Node[int]{
		value: 5
	}
	t := print_t(n.next)
	assert t == none
}
