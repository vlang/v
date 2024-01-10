interface Iter[T] {
mut:
	next() ?T
}

fn (mut it Iter[T]) collect[T]() []T {
	mut data := []T{}
	for {
		val := it.next() or { break }
		data << val
	}
	return data
}

struct ArrayIter[T] {
	data []T
mut:
	index int
}

fn (mut it ArrayIter[T]) next[T]() ?T {
	if it.index >= it.data.len {
		return none
	}
	defer {
		it.index++
	}
	return it.data[it.index]
}

fn test_generics_interface_method() {
	mut iter := Iter[int](ArrayIter[int]{
		data: [1, 2, 3]
	})
	assert iter.collect() == [1, 2, 3]
}
