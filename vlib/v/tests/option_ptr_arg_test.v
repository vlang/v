module main

struct Node[T] {
mut:
	value T
	next  ?&Node[T]
}

fn print_t1(node ?&Node[int]) {
	println(node)
}

fn print_t2(mut node ?&Node[int]) {
	n := node or { return }
	println(n)
}

fn test_main() {
	mut n := Node[int]{
		value: 5
	}
	print_t1(n)
	print_t2(mut n.next)
}
