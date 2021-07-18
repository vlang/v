interface Iter<T> {
	next() ?T
}

struct ArrayIter<T> {
	data []T
}

fn (mut i ArrayIter<T>) next<T>() ?T {
	if i.data.len == 0 {
		return none
	}
	return i.data[0]
}

fn iter<T>(arr []T) Iter<T> {
	return ArrayIter<T>{
		data: arr
	}
}

fn test_generics_fn_return_generic_interface() {
	x := iter([1, 2, 3])
	println(x)
	y := x.next() or { 0 }
	println(y)
	assert y == 1
}
