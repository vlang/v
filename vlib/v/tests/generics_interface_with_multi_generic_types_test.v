interface Iter<T, U> {
mut:
	next() ?(T, U)
}

struct ArrIter<T, U> {
	t []T
	u []U
mut:
	index int
}

fn (mut it ArrIter<T, U>) next<T, U>() ?(T, U) {
	if it.index >= it.t.len || it.index >= it.u.len {
		return none
	}
	defer {
		it.index++
	}
	return it.t[it.index], it.u[it.index]
}

fn iter<T, U>(t []T, u []U) Iter<T, U> {
	return ArrIter<T, U>{
		t: t
		u: u
	}
}

fn test_generics_interface_with_multi_generic_types() {
	mut x := iter<int, string>([1, 2, 3], ['foo', 'bar', 'baz'])
	ret := 0
	a, b := x.next() or { ret, '' }
	println(a)
	println(b)
	assert a == 1
	assert b == 'foo'
}
