module datatypes

fn test_is_empty() {
	mut list := DoublyLinkedList[int]{}
	assert list.is_empty() == true
	list.push_back(1)
	assert list.is_empty() == false
}

fn test_len() {
	mut list := DoublyLinkedList[int]{}
	assert list.len() == 0
	list.push_back(1)
	assert list.len() == 1
	list.pop_back()!
	assert list.len() == 0
}

fn test_first() {
	mut list := DoublyLinkedList[int]{}
	list.push_back(1)
	assert list.first()! == 1
	list.push_back(2)
	assert list.first()! == 1
	list = DoublyLinkedList[int]{}
	list.first() or { return }
	assert false
}

fn test_last() {
	mut list := DoublyLinkedList[int]{}
	list.push_back(1)
	assert list.last()! == 1
	list.push_back(2)
	assert list.last()! == 2
	list = DoublyLinkedList[int]{}
	list.last() or { return }
	assert false
}

fn test_push() {
	mut list := DoublyLinkedList[int]{}
	list.push_back(1)
	assert list.last()! == 1
	list.push_back(2)
	assert list.last()! == 2
	list.push_back(3)
	assert list.last()! == 3
}

fn test_push_many_back() {
	mut list := DoublyLinkedList[int]{}
	elem := [1, 2, 3]
	list.push_many(elem, Direction.back)
	assert list.last()! != 1
	assert list.last()! != 2
	assert list.last()! == 3
}

fn test_push_many_front() {
	mut list := DoublyLinkedList[int]{}
	mut elem := [1, 2, 3]
	list.push_many(elem, Direction.back)
	elem = [111]
	list.push_many(elem, Direction.front)
	assert list.first()! == 111
}

fn test_pop() {
	mut list := DoublyLinkedList[int]{}
	list.push_back(1)
	list.push_back(2)
	list.push_back(3)
	assert list.pop_back()! == 3
	list.push_back(4)
	assert list.pop_back()! == 4
	assert list.pop_back()! == 2
	list = DoublyLinkedList[int]{}
	list.pop_back() or { return }
	assert false
}

fn test_pop_front() {
	mut list := DoublyLinkedList[int]{}
	list.push_back(1)
	list.push_back(2)
	list.push_back(3)
	assert list.pop_front()! == 1
	list.push_back(4)
	assert list.pop_front()! == 2
	assert list.pop_front()! == 3
	list = DoublyLinkedList[int]{}
	list.pop_front() or { return }
	assert false
}

fn test_insert() {
	mut list := DoublyLinkedList[int]{}
	list.push_back(1)
	list.push_back(2)
	list.push_back(3)
	// [1, 2, 3]
	list.insert(1, 111)!
	// [1, 111, 2, 3]
	list.insert(3, 222)!
	// [1, 111, 2, 222, 3]
	assert list.pop_back()! == 3
	assert list.pop_back()! == 222
	assert list.pop_front()! == 1
	assert list.pop_front()! == 111
}

fn test_push_front() {
	mut list := DoublyLinkedList[int]{}
	list.push_back(1)
	list.push_back(2)
	list.push_back(3)
	list.push_front(111)
	assert list.first()! == 111
}

fn test_delete() {
	mut list := DoublyLinkedList[int]{}
	list.push_back(0)
	list.push_back(1)
	list.push_back(2)
	list.delete(1)
	assert list.first()! == 0
	assert list.last()! == 2
	assert list.len() == 2
	list.delete(1)
	assert list.first()! == 0
	assert list.last()! == 0
	assert list.len() == 1
	list.delete(0)
	assert list.len() == 0
}

fn test_iter() {
	mut list := DoublyLinkedList[int]{}
	for i := 0; i < 10; i++ {
		list.push_back(i * 10)
	}

	mut count := 0
	for i, v in list {
		count += 1
		assert int(i * 10) == v
	}
	assert count == 10

	// test it gets reset
	count = 0
	for i, v in list {
		count += 1
		assert int(i * 10) == v
	}
	assert count == 10
}

fn test_index() {
	mut list := DoublyLinkedList[int]{}
	for i := 0; i < 10; i++ {
		list.push_back(i * 10)
	}

	for i := 0; i < 10; i++ {
		assert list.index(i * 10)! == i
	}
}

fn test_str() {
	mut list := DoublyLinkedList[int]{}
	list.push_back(1)
	list.push_back(2)
	list.push_back(3)
	assert list.str() == '[1, 2, 3]'
}

fn test_array() {
	mut list := DoublyLinkedList[int]{}
	list.push_back(1)
	list.push_back(2)
	list.push_back(3)
	assert list.array() == [1, 2, 3]
}

fn test_string_array() {
	mut list := DoublyLinkedList[[]string]{}
	list.push_back(['a'])
	list.push_back(['b'])
	list.push_back(['c'])
	assert list.array() == [['a'], ['b'], ['c']]
}

fn test_iteration_with_for() {
	mut list := DoublyLinkedList[int]{}
	list.push_back(1)
	list.push_back(2)
	list.push_back(3)
	mut res := []int{}
	for x in list {
		res << x
	}
	assert res == [1, 2, 3]
}

fn test_iterator() {
	mut list := DoublyLinkedList[int]{}
	list.push_back(1)
	list.push_back(2)
	list.push_back(3)
	mut iter := list.iterator()
	mut res := []int{}
	for x in iter {
		res << x
	}
	assert res == [1, 2, 3]
}

fn test_back_iterator() {
	mut list := DoublyLinkedList[int]{}
	list.push_back(1)
	list.push_back(2)
	list.push_back(3)
	mut iter := list.back_iterator()
	mut res := []int{}
	for x in iter {
		res << x
	}
	assert res == [3, 2, 1]
}

fn test_push_many() {
	mut list := DoublyLinkedList[int]{}
	list.push_back(1)
	list.push_back(2)
	list.push_back(3)
	list.push_many([4, 5, 6], .front)
	list.push_many([7, 8, 9], .back)

	mut res := []int{}
	for x in list {
		res << x
	}
	assert res == [4, 5, 6, 1, 2, 3, 7, 8, 9]
}
