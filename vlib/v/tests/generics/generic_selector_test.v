pub struct List[T] {
mut:
	head ?&Node[T]
	size int
}

pub fn (mut l List[T]) prepend(mut node Node[T]) {
	if head := l.head {
		node.next = head
		l.head = &node
		l.size = l.size + 1
	} else {
		l.size = 1
		l.head = &node
	}
}

pub fn (mut l List[T]) append(mut node Node[T]) ?int {
	if h := l.head {
		_ := h
	} else {
		l.head = &node
		l.size = 0
		return l.size
	}

	mut curr_node := l.head
	for {
		if mut curr_node != none {
			if next_node := curr_node.next {
				curr_node = next_node
			} else {
				curr_node.next = &node
				l.size = l.size + 1
				break
			}
		}
	}
	return l.size
}

pub fn (mut l List[T]) find_last(node ?&Node[T]) ?&Node[T] {
	if next := node?.next {
		return l.find_last(next)
	} else {
		return node
	}
}

@[heap]
pub struct Node[T] {
mut:
	data T
	next ?&Node[T]
}

fn test_main() {
	mut list := List[string]{}
	list.prepend(mut Node{ data: 'zero' })
	list.prepend(mut Node{ data: 'first' })
	list.append(mut Node{ data: 'last' }) or { panic('unable to append linked list') }
	list.append(mut Node{ data: 'very last' }) or { panic('unable to append linked list') }

	assert list.find_last(list.head)? == Node{
		data: 'very last'
	}
	assert list.head?.next?.next?.next? == Node{
		data: 'very last'
	}
}
