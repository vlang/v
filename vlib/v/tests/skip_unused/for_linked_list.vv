struct Node {
mut:
	value int
	prev  ?&Node
	next  ?&Node
}

struct LinkedList {
mut:
	length int
	head   ?&Node
}

pub fn (mut l LinkedList) push(value int) {
	mut new_node := &Node{
		value: value
	}

	if mut head := l.head {
		for head.next != none {
			if mut head_next := head.next {
				head = head_next
			}
		}
		head.next = new_node
		new_node.prev = head
	} else {
		l.head = new_node
	}
}

fn test_main() {
	mut list := LinkedList{
		length: 0
		head:   none
	}
	list.push(1)
	list.push(2)
	list.push(3)
}
