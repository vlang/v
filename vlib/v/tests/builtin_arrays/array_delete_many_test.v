struct Test[T] {
mut:
	values []T
}

fn (mut t Test[T]) delete_many(start int, end int) {
	t.values.delete_many(start, end)
}

fn test_main() {
	mut x := Test[int]{}
	x.values << 1
	x.delete_many(0, 1)
	assert x.values.len == 0
}
