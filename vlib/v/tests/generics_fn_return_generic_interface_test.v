interface Iter[T] {
mut:
	next() ?T
}

struct ArrayIter[T] {
	data []T
mut:
	index int
}

fn (mut i ArrayIter[T]) next[T]() ?T {
	if i.data.len == 0 {
		return none
	}
	i.index += 1
	return i.data[i.index]
}

fn iter[T](arr []T) Iter[T] {
	return ArrayIter[T]{
		data:  arr
		index: 0
	}
}

fn test_generics_fn_return_generic_interface() {
	mut x := iter([1, 2, 3])
	println(x)
	y := x.next() or { 0 }
	println(y)
	assert y == 2
}
