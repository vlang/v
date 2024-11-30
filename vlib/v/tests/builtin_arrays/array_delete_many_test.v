struct Test[T] {
mut:
	values []T
}

fn (mut t Test[T]) delete_many(start int, end int) {
	t.values.delete_many(start, end)
}

fn test_main() {
	mut x := Test[int]{}
	x.values = []int{len: 5, init: index}
	x.delete_many(1, 3)
	assert x.values.len == 2
	assert x.values[0] == 0
	assert x.values[1] == 4
}
