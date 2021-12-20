module adt

fn test_is_empty() {
	mut stack := Stack<int>{}
	assert stack.is_empty() == true
	stack.push(1)
	assert stack.is_empty() == false
}

fn test_peek() ? {
	mut stack := Stack<int>{}
	stack.push(1)
	assert stack.peek() ? == 1
	stack.push(2)
	assert stack.peek() ? == 2
	stack = Stack<int>{}
	stack.peek() or { return }
	assert false
}

fn test_push() ? {
	mut stack := Stack<int>{}
	stack.push(1)
	assert stack.peek() ? == 1
	stack.push(2)
	assert stack.peek() ? == 2
	stack.push(3)
	assert stack.peek() ? == 3
}

fn test_pop() ? {
	mut stack := Stack<int>{}
	stack.push(1)
	stack.push(2)
	stack.push(3)
	assert stack.pop() ? == 3
	stack.push(4)
	assert stack.pop() ? == 4
	assert stack.pop() ? == 2
	stack = Stack<int>{}
	stack.pop() or { return }
	assert false
}
