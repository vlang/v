module main

struct Empty {}

struct Node<T> {
	value T
mut:
	next Tree<T>
}

type Tree<T> = Empty | Node<T>

fn create<T>() Tree<T> {
	empty := Empty{}
	mut curr := Node<T>{10, empty}
	for _ in 0 .. 10 {
		curr.next = Node<T>{20, empty}
	}

	return curr
}

fn create_node<T>(args []T) Tree<T> {
	empty := Empty{}
	if args.len == 0 {
		return empty
	}

	mut curr := Node<T>{args[0], empty}

	for i := 1; i < args.len; i += 1 {
		curr.next = Node<T>{args[i], empty}
		curr = curr.next as Node<T>
	}

	return curr
}

fn merge_nodes<T>(head Tree<T>) Tree<T> {
	println('$head')

	return Empty{}
}

fn test_generic_sumtype_cast() {
	node := create_node<int>([0, 3, 1, 0, 4, 5, 2, 0])
	merge_nodes(node)
	create<int>()
	assert true
}
