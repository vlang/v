struct Stack[T] {
mut:
	elements []T
}

pub fn (stack Stack[T]) is_empty() bool {
	return stack.elements.len == 0
}

pub fn (stack Stack[T]) peek() ?T {
	return if !stack.is_empty() { stack.elements.last() } else { none }
}

pub fn (stack Stack[T]) peek2() !T {
	return if !stack.is_empty() { stack.elements.last() } else { error('Stack is empty') }
}

@[heap]
struct Element {
mut:
	name  string
	value string
}

fn test_main() {
	mut parent := &Element{
		name: 'parent element'
	}
	mut stack := Stack[&Element]{}
}
