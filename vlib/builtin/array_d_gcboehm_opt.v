// non-pub versions of array functions
// that allocale new memory using `GC_MALLOC_ATOMIC()`
// when `-gc boehm_*_opt` is used. These memory areas are not
// scanned for pointers.

module builtin

fn __new_array_noscan(mylen int, cap int, elm_size int) array {
	panic_on_negative_len(mylen)
	panic_on_negative_cap(cap)
	cap_ := if cap < mylen { mylen } else { cap }
	arr := array{
		element_size: elm_size
		data:         vcalloc_noscan(u64(cap_) * u64(elm_size))
		len:          mylen
		cap:          cap_
	}
	return arr
}

fn __new_array_with_default_noscan(mylen int, cap int, elm_size int, val voidptr) array {
	panic_on_negative_len(mylen)
	panic_on_negative_cap(cap)
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		data:         vcalloc_noscan(u64(cap_) * u64(elm_size))
		len:          mylen
		cap:          cap_
	}
	if val != 0 && arr.data != unsafe { nil } {
		if elm_size == 1 {
			byte_value := *(&u8(val))
			dptr := &u8(arr.data)
			for i in 0 .. arr.len {
				unsafe {
					dptr[i] = byte_value
				}
			}
		} else {
			for i in 0 .. arr.len {
				unsafe { arr.set_unsafe(i, val) }
			}
		}
	}
	return arr
}

fn __new_array_with_multi_default_noscan(mylen int, cap int, elm_size int, val voidptr) array {
	panic_on_negative_len(mylen)
	panic_on_negative_cap(cap)
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		data:         vcalloc_noscan(u64(cap_) * u64(elm_size))
		len:          mylen
		cap:          cap_
	}
	if val != 0 && arr.data != unsafe { nil } {
		for i in 0 .. arr.len {
			unsafe { arr.set_unsafe(i, charptr(val) + i * elm_size) }
		}
	}
	return arr
}

fn __new_array_with_array_default_noscan(mylen int, cap int, elm_size int, val array) array {
	panic_on_negative_len(mylen)
	panic_on_negative_cap(cap)
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		data:         vcalloc_noscan(u64(cap_) * u64(elm_size))
		len:          mylen
		cap:          cap_
	}
	for i in 0 .. arr.len {
		val_clone := val.clone()
		unsafe { arr.set_unsafe(i, &val_clone) }
	}
	return arr
}

// Private function, used by V (`nums := [1, 2, 3]`)
fn new_array_from_c_array_noscan(len int, cap int, elm_size int, c_array voidptr) array {
	panic_on_negative_len(len)
	panic_on_negative_cap(cap)
	cap_ := if cap < len { len } else { cap }
	arr := array{
		element_size: elm_size
		data:         vcalloc_noscan(u64(cap_) * u64(elm_size))
		len:          len
		cap:          cap_
	}
	// TODO: Write all memory functions (like memcpy) in V
	unsafe { vmemcpy(arr.data, c_array, u64(len) * u64(elm_size)) }
	return arr
}

// Private function. Doubles array capacity if needed.
fn (mut a array) ensure_cap_noscan(required int) {
	if required <= a.cap {
		return
	}
	if a.flags.has(.nogrow) {
		panic_n('array.ensure_cap_noscan: array with the flag `.nogrow` cannot grow in size, array required new size:',
			required)
	}
	mut cap := if a.cap > 0 { i64(a.cap) } else { i64(2) }
	for required > cap {
		cap *= 2
	}
	if cap > max_int {
		if a.cap < max_int {
			// limit the capacity, since bigger values, will overflow the 32bit integer used to store it
			cap = max_int
		} else {
			panic_n('array.ensure_cap_noscan: array needs to grow to cap (which is > 2^31):',
				cap)
		}
	}
	new_size := u64(cap) * u64(a.element_size)
	new_data := vcalloc_noscan(new_size)
	if a.data != unsafe { nil } {
		unsafe { vmemcpy(new_data, a.data, u64(a.len) * u64(a.element_size)) }
		// TODO: the old data may be leaked when no GC is used (ref-counting?)
	}
	a.data = new_data
	a.offset = 0
	a.cap = int(cap)
}

// repeat returns a new array with the given array elements repeated given times.
// `cgen` will replace this with an appropriate call to `repeat_to_depth()`

// version of `repeat()` that handles multi dimensional arrays
// `unsafe` to call directly because `depth` is not checked
@[unsafe]
fn (a array) repeat_to_depth_noscan(count int, depth int) array {
	if count < 0 {
		panic_n('array.repeat: count is negative:', count)
	}
	mut size := u64(count) * u64(a.len) * u64(a.element_size)
	if size == 0 {
		size = u64(a.element_size)
	}
	arr := array{
		element_size: a.element_size
		data:         if depth > 0 { vcalloc(size) } else { vcalloc_noscan(size) }
		len:          count * a.len
		cap:          count * a.len
	}
	if a.len > 0 {
		a_total_size := u64(a.len) * u64(a.element_size)
		arr_step_size := u64(a.len) * u64(arr.element_size)
		mut eptr := &u8(arr.data)
		unsafe {
			for _ in 0 .. count {
				if depth > 0 {
					ary_clone := a.clone_to_depth_noscan(depth)
					vmemcpy(eptr, &u8(ary_clone.data), a_total_size)
				} else {
					vmemcpy(eptr, &u8(a.data), a_total_size)
				}
				eptr += arr_step_size
			}
		}
	}
	return arr
}

// insert inserts a value in the array at index `i`
fn (mut a array) insert_noscan(i int, val voidptr) {
	if i < 0 || i > a.len {
		panic_n2('array.insert_noscan: index out of range (i,a.len):', i, a.len)
	}
	if a.len == max_int {
		panic('array.insert_noscan: a.len reached max_int')
	}
	a.ensure_cap_noscan(a.len + 1)
	unsafe {
		vmemmove(a.get_unsafe(i + 1), a.get_unsafe(i), u64(a.len - i) * u64(a.element_size))
		a.set_unsafe(i, val)
	}
	a.len++
}

// insert_many inserts many values into the array from index `i`.
@[unsafe]
fn (mut a array) insert_many_noscan(i int, val voidptr, size int) {
	if i < 0 || i > a.len {
		panic_n2('array.insert_many: index out of range (i, a.len):', i, a.len)
	}
	new_len := i64(a.len) + i64(size)
	if new_len > max_int {
		panic_n('array.insert_many_noscan: max_int will be exceeded by a.len:', new_len)
	}
	a.ensure_cap_noscan(a.len + size)
	elem_size := a.element_size
	unsafe {
		iptr := a.get_unsafe(i)
		vmemmove(a.get_unsafe(i + size), iptr, u64(a.len - i) * u64(elem_size))
		vmemcpy(iptr, val, u64(size) * u64(elem_size))
	}
	a.len += size
}

// prepend prepends one value to the array.
fn (mut a array) prepend_noscan(val voidptr) {
	a.insert_noscan(0, val)
}

// prepend_many prepends another array to this array.
@[unsafe]
fn (mut a array) prepend_many_noscan(val voidptr, size int) {
	unsafe { a.insert_many_noscan(0, val, size) }
}

// pop returns the last element of the array, and removes it.
fn (mut a array) pop_noscan() voidptr {
	// in a sense, this is the opposite of `a << x`
	if a.len == 0 {
		panic('array.pop: array is empty')
	}
	new_len := a.len - 1
	last_elem := unsafe { &u8(a.data) + u64(new_len) * u64(a.element_size) }
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
@[unsafe]
fn (a &array) clone_to_depth_noscan(depth int) array {
	mut size := u64(a.cap) * u64(a.element_size)
	if size == 0 {
		size++
	}
	mut arr := array{
		element_size: a.element_size
		data:         if depth == 0 { vcalloc_noscan(size) } else { vcalloc(size) }
		len:          a.len
		cap:          a.cap
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
		if a.data != 0 {
			unsafe { vmemcpy(&u8(arr.data), a.data, u64(a.cap) * u64(a.element_size)) }
		}
		return arr
	}
}

fn (mut a array) push_noscan(val voidptr) {
	if a.len < 0 {
		panic('array.push_noscan: negative len')
	}
	if a.len >= max_int {
		panic('array.push_noscan: len bigger than max_int')
	}
	if a.len >= a.cap {
		a.ensure_cap_noscan(a.len + 1)
	}
	unsafe { vmemcpy(&u8(a.data) + u64(a.element_size) * u64(a.len), val, a.element_size) }
	a.len++
}

// push_many implements the functionality for pushing another array.
// `val` is array.data and user facing usage is `a << [1,2,3]`
@[unsafe]
fn (mut a array) push_many_noscan(val voidptr, size int) {
	if size == 0 || val == unsafe { nil } {
		return
	}
	new_len := i64(a.len) + i64(size)
	if new_len > max_int {
		// string interpolation also uses <<; avoid it, use a fixed string for the panic
		panic('array.push_many_noscan: new len exceeds max_int')
	}
	if a.data == val && a.data != 0 {
		// handle `arr << arr`
		copy := a.clone()
		a.ensure_cap_noscan(a.len + size)
		unsafe {
			vmemcpy(a.get_unsafe(a.len), copy.data, u64(a.element_size) * u64(size))
		}
	} else {
		a.ensure_cap_noscan(a.len + size)
		if a.data != 0 && val != 0 {
			unsafe { vmemcpy(a.get_unsafe(a.len), val, u64(a.element_size) * u64(size)) }
		}
	}
	a.len = int(new_len)
}

// reverse returns a new array with the elements of the original array in reverse order.
fn (a array) reverse_noscan() array {
	if a.len < 2 {
		return a
	}
	mut arr := array{
		element_size: a.element_size
		data:         vcalloc_noscan(u64(a.cap) * u64(a.element_size))
		len:          a.len
		cap:          a.cap
	}
	for i in 0 .. a.len {
		unsafe { arr.set_unsafe(i, a.get_unsafe(a.len - 1 - i)) }
	}
	return arr
}

// grow_cap grows the array's capacity by `amount` elements.
fn (mut a array) grow_cap_noscan(amount int) {
	new_cap := i64(amount) + i64(a.cap)
	if new_cap > max_int {
		panic_n('array.grow_cap: max_int will be exceeded by new cap:', new_cap)
	}
	a.ensure_cap_noscan(int(new_cap))
}

// grow_len ensures that an array has a.len + amount of length
@[unsafe]
fn (mut a array) grow_len_noscan(amount int) {
	new_len := i64(amount) + i64(a.len)
	if new_len > max_int {
		panic_n('array.grow_len: max_int will be exceeded by new len:', new_len)
	}
	a.ensure_cap_noscan(int(new_len))
	a.len = int(new_len)
}
