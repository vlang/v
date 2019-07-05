// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

struct array {
	// Using a void pointer allows to implement arrays without generics and without generating
	// extra code for every type.
pub:
	data         voidptr
	len          int
	cap          int
	element_size int
}

// Private function, used by V (`nums := []int`)
fn new_array(mylen, cap, elm_size int) array {
	arr := array {
		len: mylen
		cap: cap
		element_size: elm_size
		data: malloc(cap * elm_size)
	}
	return arr
}


// TODO 
pub fn _make(len, cap, elm_size int) array {
	return new_array(len, cap, elm_size) 
} 

// Private function, used by V (`nums := [1, 2, 3]`)
fn new_array_from_c_array(len, cap, elm_size int, c_array voidptr) array {
	arr := array {
		len: len
		cap: cap
		element_size: elm_size
		data: malloc(cap * elm_size)
	}
	// TODO Write all memory functions (like memcpy) in V
	C.memcpy(arr.data, c_array, len * elm_size)
	return arr
}

// Private function, used by V (`nums := [1, 2, 3] !`)
fn new_array_from_c_array_no_alloc(len, cap, elm_size int, c_array voidptr) array {
	arr := array {
		len: len
		cap: cap
		element_size: elm_size
		data: c_array
	}
	return arr
}

// Private function, used by V  (`[0; 100]`)
fn array_repeat(val voidptr, nr_repeats, elm_size int) array {
	arr := array {
		len: nr_repeats
		cap: nr_repeats
		element_size: elm_size
		data: malloc(nr_repeats * elm_size)
	}
	for i := 0; i < nr_repeats; i++ {
		C.memcpy(arr.data + i * elm_size, val, elm_size)
	}
	return arr
}

pub fn (a mut array) sort_with_compare(compare voidptr) {
	C.qsort(a.data, a.len, a.element_size, compare)
}

pub fn (a mut array) insert(i int, val voidptr) {
	if i >= a.len {
		panic('array.insert: index larger than length')
		return
	}
	a._push(val)
	size := a.element_size
	C.memmove(a.data + (i + 1) * size, a.data + i * size, (a.len - i) * size)
	a.set(i, val)
}

pub fn (a mut array) prepend(val voidptr) {
	a.insert(0, val)
}

pub fn (a mut array) delete(idx int) {
	size := a.element_size
	C.memmove(a.data + idx * size, a.data + (idx + 1) * size, (a.len - idx) * size)
	a.len--
	a.cap--
}

fn (a array) _get(i int) voidptr {
	if i < 0 || i >= a.len {
		panic('array index out of range: $i/$a.len')
	}
	return a.data + i * a.element_size
}

pub fn (a array) first() voidptr {
	if a.len == 0 {
		panic('array.first: empty array')
	}
	return a.data + 0
}

pub fn (a array) last() voidptr {
	if a.len == 0 {
		panic('array.last: empty array')
	}
	return a.data + (a.len - 1) * a.element_size
}

pub fn (s array) left(n int) array {
	if n >= s.len {
		return s
	}
	return s.slice(0, n)
}

pub fn (s array) right(n int) array {
	if n >= s.len {
		return s
	}
	return s.slice(n, s.len)
}

pub fn (s array) slice(start, _end int) array {
	mut end := _end
	if start > end {
		panic('invalid slice index: $start > $end')
	}
	if end >= s.len {
		end = s.len
	}
	l := end - start
	res := array {
		element_size: s.element_size
		data: s.data + start * s.element_size
		len: l
		cap: l
	}
	return res
}

pub fn (a mut array) set(idx int, val voidptr) {
	if idx < 0 || idx >= a.len {
		panic('array index out of range: $idx / $a.len')
	}
	C.memcpy(a.data + a.element_size * idx, val, a.element_size)
}

fn (arr mut array) _push(val voidptr) {
	if arr.len >= arr.cap - 1 {
		cap := (arr.len + 1) * 2
		// println('_push: realloc, new cap=$cap')
		if arr.cap == 0 {
			arr.data = malloc(cap * arr.element_size)
		}
		else {
			arr.data = C.realloc(arr.data, cap * arr.element_size)
		}
		arr.cap = cap
	}
	C.memcpy(arr.data + arr.element_size * arr.len, val, arr.element_size)
	arr.len++
}

// `val` is array.data 
// TODO make private, right now it's used by strings.Builder 
pub fn (arr mut array) _push_many(val voidptr, size int) {
	if arr.len >= arr.cap - size {
		cap := (arr.len + size) * 2
		// println('_push: realloc, new cap=$cap')
		if arr.cap == 0 {
			arr.data = malloc(cap * arr.element_size)
		}
		else {
			arr.data = C.realloc(arr.data, cap * arr.element_size)
		}
		arr.cap = cap
	}
	C.memcpy(arr.data + arr.element_size * arr.len, val, arr.element_size * size)
	arr.len += size
}

pub fn (a []int) str() string {
	mut res := '['
	for i := 0; i < a.len; i++ {
		val := a[i]
		res += '$val'
		if i < a.len - 1 {
			res += ', '
		}
	}
	res += ']'
	return res
}

pub fn (a []int) free() {
	C.free(a.data)
}

// TODO generic
// "[ 'a', 'b', 'c' ]"
pub fn (a []string) str() string {
	mut res := '['
	for i := 0; i < a.len; i++ {
		val := a[i]
		res += '"$val"'
		if i < a.len - 1 {
			res += ', '
		}
	}
	res += ']'
	return res
}

fn free(a voidptr) {
	C.free(a)
}

