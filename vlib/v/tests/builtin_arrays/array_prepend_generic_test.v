struct Buffer[T] {
	size int
mut:
	content []T
}

pub fn create_buffer[T](size int) Buffer[T] {
	return Buffer[T]{
		size:    size
		content: []T{}
	}
}

pub fn (mut b Buffer[T]) write(value T) {
	b.content.prepend(value)
}

fn test_main() {
	mut buffer := create_buffer[int](3)
	buffer.write(1)
	assert buffer.content[0] == 1
}
