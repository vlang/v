module datatypes

fn test_is_empty() {
	mut queue := Queue<int>{}
	assert queue.is_empty() == true
	queue.push(1)
	assert queue.is_empty() == false
}

fn test_len() ? {
	mut queue := Queue<int>{}
	assert queue.len() == 0
	queue.push(1)
	assert queue.len() == 1
	queue.pop()?
	assert queue.len() == 0
}

fn test_peek() ? {
	mut queue := Queue<int>{}
	queue.push(1)
	assert queue.peek()? == 1
	queue.push(2)
	assert queue.peek()? == 1
	queue = Queue<int>{}
	queue.peek() or { return }
	assert false
}

fn test_last() ? {
	mut queue := Queue<int>{}
	queue.push(1)
	assert queue.last()? == 1
	queue.push(2)
	assert queue.last()? == 2
	queue = Queue<int>{}
	queue.last() or { return }
	assert false
}

fn test_index() ? {
	mut queue := Queue<int>{}
	queue.push(1)
	assert queue.index(0)? == 1
	queue.push(2)
	assert queue.index(1)? == 2
	queue.pop()?
	queue.index(1) or { return }
	assert false
}

fn test_push() ? {
	mut queue := Queue<int>{}
	queue.push(1)
	queue.push(2)
	assert queue.peek()? == 1
}

fn test_pop() ? {
	mut queue := Queue<int>{}
	queue.push(1)
	queue.push(2)
	queue.push(3)
	assert queue.pop()? == 1
	queue.push(4)
	assert queue.pop()? == 2
	assert queue.pop()? == 3
	queue = Queue<int>{}
	queue.pop() or { return }
	assert false
}

fn test_array() ? {
	mut queue := Queue<int>{}
	queue.push(1)
	queue.push(2)
	assert queue.array() == [1, 2]
}
