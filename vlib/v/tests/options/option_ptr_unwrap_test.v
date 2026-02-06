import time
// 41s.

struct Node[T] {
mut:
	data T
	prev ?&Node[T]
	next ?&Node[T]
}

struct LinkedList[T] {
mut:
	size usize
	head ?&Node[T]
}

fn new_linked_list[T]() &LinkedList[T] {
	return &LinkedList[T]{}
}

fn (mut li LinkedList[T]) add[T](data T) ? {
	mut node := &Node[T]{data, none, none}

	if li.head == none {
		li.head = node
		node.next = node
		node.prev = node
	} else {
		node.next = li.head
		node.prev = li.head.prev
		node.prev?.next = node
		li.head.prev = node
	}

	li.size += 1
}

fn (mut li LinkedList[T]) pop[T]() ?T {
	if li.head == none {
		return none
	}
	if li.size == 1 {
		data := li.head?.data
		li.head?.next = none
		li.head?.prev = none
		li.head = none
		li.size -= 1
		return data
	}

	mut tail := li.head?.prev?
	mut curr := tail.prev?
	curr.next = li.head
	li.head?.prev = curr

	tail.next = none
	tail.prev = none
	li.size -= 1

	return tail.data
}

@[heap]
struct Integer {
	value int
}

fn test_main() {
	max_itr := 2
	t := time.now()
	for itr in 0 .. max_itr {
		mut list := new_linked_list[&Integer]()
		println('Itr#${itr} list size: ${list.size}')

		list.add(&Integer{10})
		println('Itr#${itr} list size: ${list.size}')
		list.add(&Integer{20})
		println('Itr#${itr} list size: ${list.size}')

		mut n := list.pop()
		println('Itr#${itr} list size: ${list.size}, data: ${n?}')
		n = list.pop()
		println('Itr#${itr} list size: ${list.size}, data: ${n?}')
		n = list.pop()
		println('Itr#${itr} list size: ${list.size}, data: ${n}')
	}
	d := time.since(t)
	println('Bye(time ${d})!')
}
