module datatypes

fn test_is_empty() {
	mut list := LinkedList<int>{}
	assert list.is_empty() == true
	list.push(1)
	assert list.is_empty() == false
}

fn test_len() ? {
	mut list := LinkedList<int>{}
	assert list.len() == 0
	list.push(1)
	assert list.len() == 1
	list.pop()?
	assert list.len() == 0
}

fn test_first() ? {
	mut list := LinkedList<int>{}
	list.push(1)
	assert list.first()? == 1
	list.push(2)
	assert list.first()? == 1
	list = LinkedList<int>{}
	list.first() or { return }
	assert false
}

fn test_last() ? {
	mut list := LinkedList<int>{}
	list.push(1)
	assert list.last()? == 1
	list.push(2)
	assert list.last()? == 2
	list = LinkedList<int>{}
	list.last() or { return }
	assert false
}

fn test_index() ? {
	mut list := LinkedList<int>{}
	list.push(1)
	assert list.index(0)? == 1
	list.push(2)
	assert list.index(1)? == 2
	list.pop()?
	list.index(1) or { return }
	assert false
}

fn test_push() ? {
	mut list := LinkedList<int>{}
	list.push(1)
	assert list.last()? == 1
	list.push(2)
	assert list.last()? == 2
	list.push(3)
	assert list.last()? == 3
}

fn test_pop() ? {
	mut list := LinkedList<int>{}
	list.push(1)
	list.push(2)
	list.push(3)
	assert list.pop()? == 3
	list.push(4)
	assert list.pop()? == 4
	assert list.pop()? == 2
	list = LinkedList<int>{}
	list.pop() or { return }
	assert false
}

fn test_shift() ? {
	mut list := LinkedList<int>{}
	list.push(1)
	list.push(2)
	list.push(3)
	assert list.shift()? == 1
	list.push(4)
	assert list.shift()? == 2
	assert list.shift()? == 3
	list = LinkedList<int>{}
	list.shift() or { return }
	assert false
}

fn test_insert() ? {
	mut list := LinkedList<int>{}
	list.push(1)
	list.push(2)
	list.push(3)
	list.insert(1, 111) or { return }
	assert true
}

fn test_prepend() ? {
	mut list := LinkedList<int>{}
	list.push(1)
	list.push(2)
	list.push(3)
	list.prepend(111)
	assert list.first()? == 111
}

fn test_str() ? {
	mut list := LinkedList<int>{}
	list.push(1)
	list.push(2)
	list.push(3)
	assert list.str() == '[1, 2, 3]'
}

fn test_array() ? {
	mut list := LinkedList<int>{}
	list.push(1)
	list.push(2)
	list.push(3)
	assert list.array() == [1, 2, 3]
}

fn test_linked_list_iterating_with_for() ? {
	mut list := LinkedList<int>{}
	list.push(1)
	list.push(2)
	list.push(3)
	mut res := []int{}
	for x in list {
		res << x
	}
	assert res == [1, 2, 3]
}

fn test_linked_list_separate_iterators() ? {
	mut list := LinkedList<int>{}
	list.push(1)
	list.push(2)
	list.push(3)
	mut it1 := list.iterator()
	mut it2 := list.iterator()
	mut it3 := list.iterator()
	assert it1.next()? == 1
	assert it1.next()? == 2
	assert it1.next()? == 3
	assert it2.next()? == 1
	if _ := it1.next() {
		assert false
	} else {
		assert true
	}
	if _ := it1.next() {
		assert false
	} else {
		assert true
	}
	assert it2.next()? == 2
	assert it2.next()? == 3
	if _ := it2.next() {
		assert false
	} else {
		assert true
	}
	mut res := []int{}
	for x in it3 {
		res << x
	}
	assert res == [1, 2, 3]
}
