// This file contains non-pub versions of array functions
// that allocale new memory using `GC_MALLOC_ATOMIC()`
// when `-gc boehm_*_opt` is used. These memory areas are not
// scanned for pointers.

module builtin

// non-pub "noscan" versions of some above functions
fn __new_array_noscan(mylen int, cap int, elm_size int) array {
	cap_ := if cap < mylen { mylen } else { cap }
	arr := array{
		element_size: elm_size
		data: vcalloc_noscan(cap_ * elm_size)
		len: mylen
		cap: cap_
	}
	return arr
}

fn __new_array_with_default_noscan(mylen int, cap int, elm_size int, val voidptr) array {
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		data: vcalloc_noscan(cap_ * elm_size)
		len: mylen
		cap: cap_
	}
	if val != 0 {
		for i in 0 .. arr.len {
			unsafe { arr.set_unsafe(i, val) }
		}
	}
	return arr
}

fn __new_array_with_array_default_noscan(mylen int, cap int, elm_size int, val array) array {
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		data: vcalloc_noscan(cap_ * elm_size)
		len: mylen
		cap: cap_
	}
	for i in 0 .. arr.len {
		val_clone := val.clone()
		unsafe { arr.set_unsafe(i, &val_clone) }
	}
	return arr
}

// Private function, used by V (`nums := [1, 2, 3]`)
fn new_array_from_c_array_noscan(len int, cap int, elm_size int, c_array voidptr) array {
	cap_ := if cap < len { len } else { cap }
	arr := array{
		element_size: elm_size
		data: vcalloc_noscan(cap_ * elm_size)
		len: len
		cap: cap_
	}
	// TODO Write all memory functions (like memcpy) in V
	unsafe { C.memcpy(arr.data, c_array, len * elm_size) }
	return arr
}

// Private function. Doubles array capacity if needed.
fn (mut a array) ensure_cap_noscan(required int) {
	if required <= a.cap {
		return
	}
	mut cap := if a.cap > 0 { a.cap } else { 2 }
	for required > cap {
		cap *= 2
	}
	new_size := cap * a.element_size
	new_data := vcalloc_noscan(new_size)
	if a.data != voidptr(0) {
		unsafe { C.memcpy(new_data, a.data, a.len * a.element_size) }
		// TODO: the old data may be leaked when no GC is used (ref-counting?)
	}
	a.data = new_data
	a.offset = 0
	a.cap = cap
}

// repeat returns a new array with the given array elements repeated given times.
fn (a array) repeat_noscan(count int) array {
	if count < 0 {
		panic('array.repeat: count is negative: $count')
	}
	mut size := count * a.len * a.element_size
	if size == 0 {
		size = a.element_size
	}
	arr := array{
		element_size: a.element_size
		data: vcalloc_noscan(size)
		len: count * a.len
		cap: count * a.len
	}
	size_of_array := int(sizeof(array))
	for i in 0 .. count {
		if a.len > 0 && a.element_size == size_of_array {
			ary := array{}
			unsafe { C.memcpy(&ary, a.data, size_of_array) }
			ary_clone := ary.clone()
			unsafe { C.memcpy(arr.get_unsafe(i * a.len), &ary_clone, a.len * a.element_size) }
		} else {
			unsafe { C.memcpy(arr.get_unsafe(i * a.len), &byte(a.data), a.len * a.element_size) }
		}
	}
	return arr
}

// insert inserts a value in the array at index `i`
pub fn (mut a array) insert_noscan(i int, val voidptr) {
	$if !no_bounds_checking ? {
		if i < 0 || i > a.len {
			panic('array.insert: index out of range (i == $i, a.len == $a.len)')
		}
	}
	a.ensure_cap_noscan(a.len + 1)
	unsafe {
		C.memmove(a.get_unsafe(i + 1), a.get_unsafe(i), (a.len - i) * a.element_size)
		a.set_unsafe(i, val)
	}
	a.len++
}

// insert_many inserts many values into the array from index `i`.
[unsafe]
pub fn (mut a array) insert_many_noscan(i int, val voidptr, size int) {
	$if !no_bounds_checking ? {
		if i < 0 || i > a.len {
			panic('array.insert_many: index out of range (i == $i, a.len == $a.len)')
		}
	}
	a.ensure_cap_noscan(a.len + size)
	elem_size := a.element_size
	unsafe {
		iptr := a.get_unsafe(i)
		C.memmove(a.get_unsafe(i + size), iptr, (a.len - i) * elem_size)
		C.memcpy(iptr, val, size * elem_size)
	}
	a.len += size
}

// prepend prepends one value to the array.
pub fn (mut a array) prepend_noscan(val voidptr) {
	a.insert_noscan(0, val)
}

// prepend_many prepends another array to this array.
[unsafe]
pub fn (mut a array) prepend_many_noscan(val voidptr, size int) {
	unsafe { a.insert_many_noscan(0, val, size) }
}

pub fn (a &array) clone_noscan() array {
	mut size := a.cap * a.element_size
	if size == 0 {
		size++
	}
	mut arr := array{
		element_size: a.element_size
		data: vcalloc_noscan(size)
		len: a.len
		cap: a.cap
	}
	// Recursively clone-generated elements if array element is array type
	size_of_array := int(sizeof(array))
	if a.element_size == size_of_array {
		mut is_elem_array := true
		for i in 0 .. a.len {
			ar := array{}
			unsafe { C.memcpy(&ar, a.get_unsafe(i), size_of_array) }
			if ar.len > ar.cap || ar.cap <= 0 || ar.element_size <= 0 {
				is_elem_array = false
				break
			}
			ar_clone := ar.clone()
			unsafe { arr.set_unsafe(i, &ar_clone) }
		}
		if is_elem_array {
			return arr
		}
	}

	if !isnil(a.data) {
		unsafe { C.memcpy(&byte(arr.data), a.data, a.cap * a.element_size) }
	}
	return arr
}

fn (a &array) slice_clone_noscan(start int, _end int) array {
	mut end := _end
	$if !no_bounds_checking ? {
		if start > end {
			panic('array.slice: invalid slice index ($start > $end)')
		}
		if end > a.len {
			panic('array.slice: slice bounds out of range ($end >= $a.len)')
		}
		if start < 0 {
			panic('array.slice: slice bounds out of range ($start < 0)')
		}
	}
	mut data := &byte(0)
	offset := start * a.element_size
	unsafe {
		data = &byte(a.data) + offset
	}
	l := end - start
	res := array{
		element_size: a.element_size
		data: data
		offset: offset
		len: l
		cap: l
	}
	return res.clone_noscan()
}

fn (a array) reverse_noscan() array {
	if a.len < 2 {
		return a
	}
	mut arr := array{
		element_size: a.element_size
		data: vcalloc_noscan(a.cap * a.element_size)
		len: a.len
		cap: a.cap
	}
	for i in 0 .. a.len {
		unsafe { arr.set_unsafe(i, a.get_unsafe(a.len - 1 - i)) }
	}
	return arr
}
