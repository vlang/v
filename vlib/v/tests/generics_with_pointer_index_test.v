module main

pub struct Vec<T> {
mut:
	data &T
	cap  int
	len  int
}

pub struct Iter<T> {
mut:
	v   &Vec<T>
	pos int
}

pub fn with_cap<T>(cap int) Vec<T> {
	new_data := unsafe { C.malloc(cap * int(sizeof(T))) }
	unsafe { C.memset(new_data, 0, cap * int(sizeof(T))) }

	return Vec<T>{
		data: new_data
		cap: cap
		len: 0
	}
}

pub fn (ar &Vec<T>) iter() Iter<T> {
	return Iter<T>{
		v: unsafe { ar }
	}
}

pub fn (mut iter Iter<T>) next() ?&T {
	if iter.pos >= iter.v.len {
		return none
	}
	res := unsafe { &iter.v.data[iter.pos] }
	iter.pos++
	return res
}

pub fn (mut ar Vec<T>) push(elm T) {
	unsafe {
		ar.data[ar.len - 1] = elm
	}
}

struct Product {
	price f64
}

fn test_generic_with_pointer_index() {
	vec1 := with_cap<Product>(5)
	println(vec1)
	assert vec1.len == 0
	assert vec1.cap == 5

	vec2 := with_cap<int>(5)
	println(vec2)
	assert vec2.len == 0
	assert vec2.cap == 5
}
