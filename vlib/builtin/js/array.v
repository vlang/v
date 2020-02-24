// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

import strings

pub struct array {
pub:
	data         voidptr
	len          int
	cap          int
	element_size int
}

/*
// Private function, used by V (`nums := []int`)
fn new_array(mylen, cap, elm_size int) array {
	arr := array {
		len: mylen
		cap: cap
		element_size: elm_size
	}
	return arr
}


// TODO
pub fn _make(len, cap, elm_size int) array {
	return new_array(len, cap, elm_size)
}


*/

pub fn make(len, cap, elm_size int) array {
	return array{}
}

fn array_repeat(val voidptr, nr_repeats, elm_size int) array {
	return val
}

pub fn (a array) repeat(nr_repeats int) array {
	#return Array(a[0]).fill(nr_repeats)
	return a
}

pub fn (a mut array) sort_with_compare(compare voidptr) {
}

pub fn (a mut array) insert(i int, val voidptr) {
}

pub fn (a mut array) prepend(val voidptr) {
	a.insert(0, val)
}

pub fn (a mut array) delete_elm(idx int) {
}

/*
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
*/

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
	return s
}

pub fn (a array) reverse() array {
	return a
}

pub fn (a array) clone() array {
	return a
}

pub fn (a array) free() {
}

// "[ 'a', 'b', 'c' ]"
pub fn (a []string) str() string {
	mut sb := strings.new_builder(a.len * 3)
	sb.write('[')
	for i in 0..a.len {
		val := a[i]
		sb.write('"')
		sb.write(val)
		sb.write('"')
		if i < a.len - 1 {
			sb.write(', ')
		}
	}
	sb.write(']')
	return sb.str()
}

pub fn (b []byte) hex() string {
	return 'sdf'
}

pub fn (arr mut array) push_many(val voidptr, size int) {
}

pub fn free(voidptr) {
	
}

