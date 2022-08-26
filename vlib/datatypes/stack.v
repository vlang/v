module datatypes

pub struct Stack<T> {
mut:
	elements []T
}

// is_empty checks if the stack is empty
pub fn (stack Stack<T>) is_empty() bool {
	return stack.elements.len == 0
}

// len returns the length of the stack
pub fn (stack Stack<T>) len() int {
	return stack.elements.len
}

// peek returns the top of the stack
pub fn (stack Stack<T>) peek() ?T {
	return if !stack.is_empty() { stack.elements.last() } else { error('Stack is empty') }
}

// push adds an element to the top of the stack
pub fn (mut stack Stack<T>) push(item T) {
	stack.elements << item
}

// pop removes the element at the top of the stack and returns it
pub fn (mut stack Stack<T>) pop() ?T {
	return if !stack.is_empty() { stack.elements.pop() } else { error('Stack is empty') }
}

// str returns a string representation of the stack
pub fn (stack Stack<T>) str() string {
	return stack.elements.str()
}

// array returns a array representation of the stack
pub fn (stack Stack<T>) array() []T {
	return stack.elements
}
