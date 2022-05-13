interface Iter<T> {
mut:
	next() ?T
}

fn (it Iter<T>) skip<T>(n int) Iter<T> {
	return SkipIter<T>{
		n: n
	}
}

struct ArrIter<T> {
	data []T
mut:
	index int
}

fn (mut it ArrIter<T>) next<T>() ?T {
	if it.index >= it.data.len {
		return none
	}
	defer {
		it.index++
	}
	return it.data[it.index]
}

struct SkipIter<T> {
mut:
	n    int
	iter Iter<T>
}

fn (mut it SkipIter<T>) next<T>() ?T {
	for it.n > 0 {
		it.iter.next()?
		it.n--
	}
	return it.iter.next()
}

fn test_generics_interface_with_multi_generic_structs() {
	mut x := Iter<int>(ArrIter<int>{
		data: [1, 2, 3]
	})
	println(x)

	mut ret := x.next() or { 0 }
	println(ret)
	assert ret == 1

	ret = x.next() or { 0 }
	println(ret)
	assert ret == 2

	ret = x.next() or { 0 }
	println(ret)
	assert ret == 3
}
