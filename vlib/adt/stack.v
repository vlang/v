module adt

struct Stack<T> {
mut:
	elements []T
pub mut:
	len int
}

fn (stack Stack<T>) is_empty() bool {
	return stack.len <= 0
}

fn (stack Stack<T>) peek() ?T {
	return if !stack.is_empty() { stack.elements.last() } else { error('Stack is empty') }
}

fn (mut stack Stack<T>) push(item T) {
	if stack.elements.len > stack.len {
		stack.elements[stack.len] = item
	} else {
		stack.elements << item
	}
	stack.len++
}

fn (mut stack Stack<T>) pop() ?T {
	if !stack.is_empty() {
		stack.len--
		return stack.elements[stack.len]
	}
	return error('Stack is empty')
}

fn (mut stack Stack<T>) str() string {
	return stack.elements.str()
}
