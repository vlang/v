import datatypes as dt

fn test_is_empty() {
	mut stack := dt.Stack<int>{}
	assert stack.is_empty() == true
	stack.push(1)
	assert stack.is_empty() == false
}

fn test_len() ? {
	mut stack := dt.Stack<int>{}
	assert stack.len() == 0
	stack.push(1)
	assert stack.len() == 1
	stack.pop()?
	assert stack.len() == 0
}

fn test_peek() ? {
	mut stack := dt.Stack<int>{}
	stack.push(1)
	assert stack.peek()? == 1
	stack.push(2)
	assert stack.peek()? == 2
	stack = dt.Stack<int>{}
	stack.peek() or { return }
	assert false
}

fn test_push() ? {
	mut stack := dt.Stack<int>{}
	stack.push(1)
	assert stack.peek()? == 1
	stack.push(2)
	assert stack.peek()? == 2
	stack.push(3)
	assert stack.peek()? == 3
}

fn test_pop() ? {
	mut stack := dt.Stack<int>{}
	stack.push(1)
	stack.push(2)
	stack.push(3)
	assert stack.pop()? == 3
	stack.push(4)
	assert stack.pop()? == 4
	assert stack.pop()? == 2
	stack = dt.Stack<int>{}
	stack.pop() or { return }
	assert false
}

fn test_array() ? {
	mut stack := dt.Stack<int>{}
	stack.push(1)
	stack.push(2)
	assert stack.array() == [1, 2]
}
