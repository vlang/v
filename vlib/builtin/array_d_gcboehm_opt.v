// non-pub versions of array functions
// that allocale new memory using `GC_MALLOC_ATOMIC()`
// when `-gc boehm_*_opt` is used. These memory areas are not
// scanned for pointers.

module builtin

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
	unsafe { vmemcpy(arr.data, c_array, len * elm_size) }
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
		unsafe { vmemcpy(new_data, a.data, a.len * a.element_size) }
		// TODO: the old data may be leaked when no GC is used (ref-counting?)
	}
	a.data = new_data
	a.offset = 0
	a.cap = cap
}

// repeat returns a new array with the given array elements repeated given times.
// `cgen` will replace this with an apropriate call to `repeat_to_depth()`

// version of `repeat()` that handles multi dimensional arrays
// `unsafe` to call directly because `depth` is not checked
[unsafe]
fn (a array) repeat_to_depth_noscan(count int, depth int) array {
	if count < 0 {
		panic('array.repeat: count is negative: $count')
	}
	mut size := count * a.len * a.element_size
	if size == 0 {
		size = a.element_size
	}
	arr := array{
		element_size: a.element_size
		data: if depth > 0 { vcalloc(size) } else { vcalloc_noscan(size) }
		len: count * a.len
		cap: count * a.len
	}
	if a.len > 0 {
		for i in 0 .. count {
			if depth > 0 {
				ary_clone := unsafe { a.clone_to_depth_noscan(depth) }
				unsafe { vmemcpy(arr.get_unsafe(i * a.len), &u8(ary_clone.data), a.len * a.element_size) }
			} else {
				unsafe { vmemcpy(arr.get_unsafe(i * a.len), &u8(a.data), a.len * a.element_size) }
			}
		}
	}
	return arr
}

// insert inserts a value in the array at index `i`
fn (mut a array) insert_noscan(i int, val voidptr) {
	$if !no_bounds_checking ? {
		if i < 0 || i > a.len {
			panic('array.insert: index out of range (i == $i, a.len == $a.len)')
		}
	}
	a.ensure_cap_noscan(a.len + 1)
	unsafe {
		vmemmove(a.get_unsafe(i + 1), a.get_unsafe(i), (a.len - i) * a.element_size)
		a.set_unsafe(i, val)
	}
	a.len++
}

// insert_many inserts many values into the array from index `i`.
[unsafe]
fn (mut a array) insert_many_noscan(i int, val voidptr, size int) {
	$if !no_bounds_checking ? {
		if i < 0 || i > a.len {
			panic('array.insert_many: index out of range (i == $i, a.len == $a.len)')
		}
	}
	a.ensure_cap_noscan(a.len + size)
	elem_size := a.element_size
	unsafe {
		iptr := a.get_unsafe(i)
		vmemmove(a.get_unsafe(i + size), iptr, (a.len - i) * elem_size)
		vmemcpy(iptr, val, size * elem_size)
	}
	a.len += size
}

// prepend prepends one value to the array.
fn (mut a array) prepend_noscan(val voidptr) {
	a.insert_noscan(0, val)
}

// prepend_many prepends another array to this array.
[unsafe]
fn (mut a array) prepend_many_noscan(val voidptr, size int) {
	unsafe { a.insert_many_noscan(0, val, size) }
}

// pop returns the last element of the array, and removes it.
fn (mut a array) pop_noscan() voidptr {
	// in a sense, this is the opposite of `a << x`
	$if !no_bounds_checking ? {
		if a.len == 0 {
			panic('array.pop: array is empty')
		}
	}
	new_len := a.len - 1
	last_elem := unsafe { &u8(a.data) + new_len * a.element_size }
	a.len = new_len
	// Note: a.cap is not changed here *on purpose*, so that
	// further << ops on that array will be more efficient.
	return unsafe { memdup_noscan(last_elem, a.element_size) }
}

// `clone_static_to_depth_noscan()` returns an independent copy of a given array.
// Unlike `clone_to_depth_noscan()` it has a value receiver and is used internally
// for slice-clone expressions like `a[2..4].clone()` and in -autofree generated code.
fn (a array) clone_static_to_depth_noscan(depth int) array {
	return unsafe { a.clone_to_depth_noscan(depth) }
}

// recursively clone given array - `unsafe` when called directly because depth is not checked
[unsafe]
fn (a &array) clone_to_depth_noscan(depth int) array {
	mut size := a.cap * a.element_size
	if size == 0 {
		size++
	}
	mut arr := array{
		element_size: a.element_size
		data: if depth == 0 { vcalloc_noscan(size) } else { vcalloc(size) }
		len: a.len
		cap: a.cap
	}
	// Recursively clone-generated elements if array element is array type
	if depth > 0 {
		for i in 0 .. a.len {
			ar := array{}
			unsafe { vmemcpy(&ar, a.get_unsafe(i), int(sizeof(array))) }
			ar_clone := unsafe { ar.clone_to_depth_noscan(depth - 1) }
			unsafe { arr.set_unsafe(i, &ar_clone) }
		}
		return arr
	} else {
		if !isnil(a.data) {
			unsafe { vmemcpy(&u8(arr.data), a.data, a.cap * a.element_size) }
		}
		return arr
	}
}

fn (mut a array) push_noscan(val voidptr) {
	a.ensure_cap_noscan(a.len + 1)
	unsafe { vmemcpy(&u8(a.data) + a.element_size * a.len, val, a.element_size) }
	a.len++
}

// push_many implements the functionality for pushing another array.
// `val` is array.data and user facing usage is `a << [1,2,3]`
[unsafe]
fn (mut a3 array) push_many_noscan(val voidptr, size int) {
	if a3.data == val && !isnil(a3.data) {
		// handle `arr << arr`
		copy := a3.clone()
		a3.ensure_cap_noscan(a3.len + size)
		unsafe {
			vmemcpy(a3.get_unsafe(a3.len), copy.data, a3.element_size * size)
		}
	} else {
		a3.ensure_cap_noscan(a3.len + size)
		if !isnil(a3.data) && !isnil(val) {
			unsafe { vmemcpy(a3.get_unsafe(a3.len), val, a3.element_size * size) }
		}
	}
	a3.len += size
}

// reverse returns a new array with the elements of the original array in reverse order.
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

// grow_cap grows the array's capacity by `amount` elements.
fn (mut a array) grow_cap_noscan(amount int) {
	a.ensure_cap_noscan(a.cap + amount)
}

// grow_len ensures that an array has a.len + amount of length
[unsafe]
fn (mut a array) grow_len_noscan(amount int) {
	a.ensure_cap_noscan(a.len + amount)
	a.len += amount
}
