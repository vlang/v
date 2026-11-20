// "noscan" versions of array initialization routines.
//
// They are used when the compiler can prove that the elements do not contain
// any pointers. Such objects can be placed in a memory area that is not
// scanned by the garbage collector.
//
// These mirror `array_d_gcboehm_opt.v`, but route through the backend-agnostic
// `vcalloc_noscan` / `malloc_noscan_uninit` entry points instead of calling the
// atomic allocator directly, so every GC mode (including `-gc vgc`, where they
// land on noscan spans and set the `noscan_data` flag) benefits. Under
// collectors without a noscan path they behave exactly like the scanning
// constructors.

module builtin

// these are needed for calls inside `$if gcboehm_opt ? { ... }` in array.v
// and for the `$if vgc` noscan branches there. They use the backend-agnostic
// `vcalloc_noscan` / `malloc_noscan_uninit` entry points, so every GC mode
// (including `-gc vgc`, where they land on noscan spans) benefits.
@[inline]
fn alloc_array_data_noscan(total_size u64) voidptr {
	raw := vcalloc_noscan(array_data_allocation_size(total_size))
	return init_array_data(raw)
}

@[inline]
fn alloc_array_data_noscan_uninit(total_size u64) voidptr {
	raw := unsafe { malloc_noscan_uninit(array_data_allocation_size(total_size)) }
	return init_array_data(raw)
}

// this is needed in `string.v`
@[markused]
fn __new_array_noscan(mylen int, cap int, elm_size int) array {
	panic_on_negative_len(mylen)
	panic_on_negative_cap(cap)
	cap_ := if cap < mylen { mylen } else { cap }
	total_size := u64(cap_) * u64(elm_size)
	mut data := unsafe { nil }
	if cap_ > 0 && mylen == 0 {
		data = alloc_array_data_noscan_uninit(total_size)
	} else if cap_ > 0 {
		data = alloc_array_data_noscan(total_size)
	}
	arr := array{
		element_size: elm_size
		data:         data
		len:          mylen
		cap:          cap_
		flags:        .managed | .noscan_data
	}
	return arr
}

@[markused]
fn __new_array_with_default_noscan(mylen int, cap int, elm_size int, val voidptr) array {
	panic_on_negative_len(mylen)
	panic_on_negative_cap(cap)
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		len:          mylen
		cap:          cap_
		flags:        .managed | .noscan_data
	}
	if cap_ > 0 {
		arr.data = alloc_array_data_noscan(u64(cap_) * u64(elm_size))
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

@[markused]
fn __new_array_with_multi_default_noscan(mylen int, cap int, elm_size int, val voidptr) array {
	panic_on_negative_len(mylen)
	panic_on_negative_cap(cap)
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		len:          mylen
		cap:          cap_
		flags:        .managed | .noscan_data
	}
	if cap_ > 0 {
		arr.data = alloc_array_data_noscan(u64(cap_) * u64(elm_size))
	}
	if val != 0 && arr.data != unsafe { nil } {
		for i in 0 .. arr.len {
			unsafe { arr.set_unsafe(i, charptr(val) + i * elm_size) }
		}
	}
	return arr
}

@[markused]
fn __new_array_with_array_default_noscan(mylen int, cap int, elm_size int, val array, depth int) array {
	panic_on_negative_len(mylen)
	panic_on_negative_cap(cap)
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		len:          mylen
		cap:          cap_
		flags:        .managed | .noscan_data
	}
	if cap_ > 0 {
		arr.data = alloc_array_data_noscan(u64(cap_) * u64(elm_size))
	}
	for i in 0 .. arr.len {
		val_clone := val.clone()
		unsafe { arr.set_unsafe(i, &val_clone) }
	}
	return arr
}

// Private function, used by V (`nums := [1, 2, 3]`)
@[markused]
fn new_array_from_c_array_noscan(len int, cap int, elm_size int, c_array voidptr) array {
	panic_on_negative_len(len)
	panic_on_negative_cap(cap)
	cap_ := if cap < len { len } else { cap }
	total_size := u64(cap_) * u64(elm_size)
	copy_size := u64(len) * u64(elm_size)
	// The first `len` elements are copied over below; only the spare capacity is zeroed.
	arr := array{
		element_size: elm_size
		data:         if total_size > 0 {
			alloc_array_data_noscan_uninit(total_size)
		} else {
			alloc_array_data_noscan(total_size)
		}
		len:          len
		cap:          cap_
		flags:        .managed | .noscan_data
	}
	// TODO: Write all memory functions (like memcpy) in V
	unsafe {
		vmemcpy(arr.data, c_array, copy_size)
		if total_size > copy_size {
			vmemset(&u8(arr.data) + copy_size, 0, total_size - copy_size)
		}
	}
	return arr
}

fn (mut a array) ensure_cap_noscan(required int) {
	a.ensure_cap(required)
}

fn (a array) repeat_to_depth_noscan(count int, depth int) array {
	return unsafe { a.repeat_to_depth(count, depth) }
}

fn (mut a array) insert_noscan(i int, val voidptr) {
	a.insert(i, val)
}

fn (mut a array) insert_many_noscan(i int, val voidptr, size int) {
	unsafe { a.insert_many(i, val, size) }
}

fn (mut a array) prepend_noscan(val voidptr) {
	a.prepend(val)
}

fn (mut a array) prepend_many_noscan(val voidptr, size int) {
	unsafe { a.prepend_many(val, size) }
}

fn (mut a array) pop_left_noscan() voidptr {
	return a.pop_left()
}

fn (mut a array) pop_noscan() voidptr {
	return a.pop()
}

fn (a array) clone_static_to_depth_noscan(depth int) array {
	return a.clone_static_to_depth(depth)
}

fn (a &array) clone_to_depth_noscan(depth int) array {
	return unsafe { a.clone_to_depth(depth) }
}

fn (mut a array) push_noscan(val voidptr) {
	a.push(val)
}

fn (mut a array) push_many_noscan(val voidptr, size int) {
	unsafe { a.push_many(val, size) }
}

fn (a array) reverse_noscan() array {
	return a.reverse()
}

fn (mut a array) grow_cap_noscan(amount int) {
	a.grow_cap(amount)
}

fn (mut a array) grow_len_noscan(amount int) {
	unsafe { a.grow_len(amount) }
}
