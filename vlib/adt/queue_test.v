module adt

fn test_is_empty() {
	mut queue := Queue<int>{}
	assert queue.is_empty() == true
	queue.push(1)
	assert queue.is_empty() == false
}

fn test_peek() ? {
	mut queue := Queue<int>{}
	queue.push(1)
	assert queue.peek() ? == 1
	queue.push(2)
	assert queue.peek() ? == 1
	queue = Queue<int>{}
	queue.peek() or { return }
	assert false
}

fn test_push() ? {
	mut queue := Queue<int>{}
	queue.push(1)
	queue.push(2)
	assert queue.peek() ? == 1
}

fn test_pop() ? {
	mut queue := Queue<int>{}
	queue.push(1)
	queue.push(2)
	queue.push(3)
	assert queue.pop() ? == 1
	queue.push(4)
	assert queue.pop() ? == 2
	assert queue.pop() ? == 3
	queue = Queue<int>{}
	queue.pop() or { return }
	assert false
}
