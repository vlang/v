module adt

pub struct Stack<T> {
mut:
	elements []T
pub mut:
	len int
}

// is_empty checks if the stack is empty
pub fn (stack Stack<T>) is_empty() bool {
	return stack.len <= 0
}

// peek returns the top of the stack
pub fn (stack Stack<T>) peek() ?T {
	return if !stack.is_empty() { stack.elements.last() } else { error('Stack is empty') }
}

// push adds an element to the top of the stack
pub fn (mut stack Stack<T>) push(item T) {
	if stack.elements.len > stack.len {
		stack.elements[stack.len] = item
	} else {
		stack.elements << item
	}
	stack.len++
}

// pop removes the element at the top of the stack and returns it
pub fn (mut stack Stack<T>) pop() ?T {
	if !stack.is_empty() {
		stack.len--
		return stack.elements[stack.len]
	}
	return error('Stack is empty')
}

// str returns a string representation of the stack
pub fn (mut stack Stack<T>) str() string {
	return stack.elements.str()
}
