module genstream

pub interface Reader[T] {
mut:
	next() ?T
}
