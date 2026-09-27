module defaults

pub struct Stack[T] {
pub:
	elements []T
	max_size int = 50
}

pub struct State {
pub:
	undo Stack[string]
	redo Stack[string]
}

pub struct Number[T] {
pub:
	value T = T(42)
}
