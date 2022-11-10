struct Node<T> {
mut:
	data T
	next &Node<T> = unsafe { nil }
}

struct SinglyLinkedList<T> {
mut:
	first_node &Node<T> = unsafe { nil }
}

fn init_singlylinkedlist<T>(nodes []Node<T>) SinglyLinkedList<T> {
	mut current_node := &nodes[0]

	for i in 0 .. nodes.len - 1 {
		current_node = &nodes[i]
		current_node.next = &nodes[i + 1]
	}

	return SinglyLinkedList<T>{&nodes[0]}
}

fn test_generic_array_typedef() {
	sll := init_singlylinkedlist<int>([Node<int>{ data: 1 }, Node<int>{
		data: 2
	}, Node<int>{
		data: 798
	}])
	println(sll.first_node.next)
	assert sll.first_node.next.data == 2
}
