struct Foo[T] {}

fn (self Foo[T]) bar() ?&Foo[T] {
	return none
}

fn test_generic_method_returning_option_pointer_compares_to_none() {
	assert Foo[int]{}.bar() == none
}

@[heap]
struct Integer21559 {
mut:
	value int
}

fn (mut i Integer21559) inc() {
	i.value++
}

struct Node21559[T] {
mut:
	data T
	prev ?&Node21559[T]
	next ?&Node21559[T]
}

struct LinkedList21559[T] {
mut:
	size usize
	head ?&Node21559[T]
}

fn (mut li LinkedList21559[T]) push(data T) {
	mut node := &Node21559[T]{data, none, none}
	if li.head == none {
		li.head = node
		node.next = node
		node.prev = node
	} else {
		mut head := li.head or { panic('head missing') }
		mut head_prev := head.prev or { panic('head.prev missing') }
		node.next = head
		node.prev = head_prev
		head_prev.next = node
		head.prev = node
	}
	li.size++
}

fn (mut li LinkedList21559[T]) pop() ?T {
	if li.head == none {
		return none
	}
	if li.size == 1 {
		mut head := li.head?
		data := head.data
		head.next = none
		head.prev = none
		li.head = none
		li.size--
		return data
	}
	mut tail := li.head?.prev?
	mut curr := tail.prev?
	curr.next = li.head
	li.head?.prev = curr
	tail.next = none
	tail.prev = none
	li.size--
	return tail.data
}

fn test_generic_method_returning_option_reference_preserves_payload() {
	mut list := LinkedList21559[&Integer21559]{}
	first := &Integer21559{
		value: 10
	}
	second := &Integer21559{
		value: 20
	}
	list.push(first)
	list.push(second)

	mut popped := list.pop() or { panic(err) }
	assert popped.value == 20
	popped.inc()
	assert second.value == 21
	assert first.value == 10

	popped = list.pop() or { panic(err) }
	assert popped.value == 10
	popped.inc()
	assert first.value == 11
	assert list.pop() == none
}

fn test_generic_method_returning_option_reference_preserves_many_values() {
	mut list := LinkedList21559[&Integer21559]{}
	count := 1024
	for i in 0 .. count {
		mut item := &Integer21559{
			value: i
		}
		list.push(item)
		item.inc()
		assert item.value == i + 1
	}
	for i in 0 .. count {
		item := list.pop() or { panic(err) }
		assert item.value == count - i
	}
	assert list.size == 0
}
