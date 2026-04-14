module main

struct Node[T] {
mut:
	data T
	next &Node[T] = unsafe { nil }
}

fn (mut node Node[T]) tail_data() T {
	mut current_node := node
	for current_node.next != unsafe { nil } {
		current_node = current_node.next
	}
	return current_node.data
}

fn test_generic_mut_receiver_local_copy_can_follow_recursive_next() {
	mut node2 := Node[int]{
		data: 2
	}
	mut node1 := Node[int]{
		data: 1
		next: &node2
	}
	assert node1.tail_data() == 2
}
