pub struct Vec<T> {
mut:
	data &T    [required]
	cap  usize [required]
	len  usize [required]
}

pub fn new<T>() Vec<T> {
	return Vec<T>{
		data: unsafe { nil }
		cap: 0
		len: 0
	}
}

pub fn (ar &Vec<T>) iter() Iter<T> {
	return Iter<T>{
		v: unsafe { ar }
	}
}

pub struct Iter<T> {
mut:
	v   &Vec<T> [required]
	pos usize
}

pub fn (mut iter Iter<T>) next() ?&T {
	if iter.pos >= iter.v.len {
		return none
	}
	defer {
		iter.pos++
	}
	return unsafe { &iter.v.data[iter.pos] }
}

fn test_generics_for_in_iterate() {
	mut goods := new<int>()
	goods.call_generic_fn(fn (a &int) bool {
		return *a > 1
	})
	assert true
}

fn (arr Vec<T>) call_generic_fn(cb fn (&T) bool) {
	for val in arr.iter() {
		println(cb(val))
	}
}
