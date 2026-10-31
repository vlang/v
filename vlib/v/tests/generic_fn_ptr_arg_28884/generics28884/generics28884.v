module generics28884

pub struct Foo[T] {
pub mut:
	value T
}

// set stores `value` in the container.
pub fn (mut self Foo[T]) set(value T) {
	self.value = value
}

pub struct Box[T] {
pub:
	v T
}
