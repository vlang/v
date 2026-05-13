// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

import strings

// array is a struct, used for denoting all array types in V.
// `.data` is a void pointer to the backing heap memory block,
// which avoids using generics and thus without generating extra
// code for every type.
pub struct array {
pub mut:
	data   voidptr
	offset int // in bytes (should be `usize`), to avoid copying data while making slices, unless it starts changing
	len    int // length of the array in elements.
	cap    int // capacity of the array in elements.
	flags  ArrayFlags
pub:
	element_size int // size in bytes of one element in the array.
}

@[flag]
pub enum ArrayFlags {
	noslices    // when <<, `.noslices` will free the old data block immediately (you have to be sure, that there are *no slices* to that specific array). TODO: integrate with reference counting/compiler support for the static cases.
	noshrink    // when `.noslices` and `.noshrink` are *both set*, .delete(x) will NOT allocate new memory and free the old. It will just move the elements in place, and adjust .len.
	nogrow      // the array will never be allowed to grow past `.cap`. set `.nogrow` and `.noshrink` for a truly fixed heap array
	nofree      // `.data` will never be freed
	managed     // `.data` uses the builtin managed array allocation with a metadata header
	noscan_data // `.data` was allocated in a no-scan heap block and can stay atomic when cloned or resized
	is_slice    // this array is a slice view into another array's managed buffer
}

@[_packed]
struct ArrayDataHeader {
mut:
	has_slices bool
}

// Must be aligned to at least the maximum fundamental type alignment (pointer size)
// so that the array data following the header is properly aligned.
//
// Keep this as a function, not a const. When V bootstraps from generated C, a const
// would bake in the snapshot generator's pointer size instead of the target C ABI.
@[inline]
fn array_data_header_size() int {
	return int(sizeof(voidptr))
}

@[inline]
fn array_data_allocation_size(total_size u64) u64 {
	return u64(array_data_header_size()) + __at_least_one(total_size)
}

@[inline]
fn alloc_array_data(total_size u64) voidptr {
	raw := vcalloc(array_data_allocation_size(total_size))
	return unsafe { &u8(raw) + array_data_header_size() }
}

@[inline]
fn alloc_array_data_uninit(total_size u64) voidptr {
	raw := unsafe { malloc_uninit(array_data_allocation_size(total_size)) }
	unsafe {
		(&ArrayDataHeader(raw)).has_slices = false
		return &u8(raw) + array_data_header_size()
	}
}

@[inline]
fn (a array) uses_noscan_data() bool {
	return a.flags.has(.noscan_data)
}

@[inline]
fn (a array) alloc_array_data_like(total_size u64) voidptr {
	$if gcboehm_opt ? {
		if a.uses_noscan_data() {
			return alloc_array_data_noscan(total_size)
		}
	}
	return alloc_array_data(total_size)
}

@[inline]
fn (a array) alloc_array_data_like_uninit(total_size u64) voidptr {
	$if gcboehm_opt ? {
		if a.uses_noscan_data() {
			return alloc_array_data_noscan_uninit(total_size)
		}
	}
	return alloc_array_data_uninit(total_size)
}

@[inline; unsafe]
fn (a array) data_header() &ArrayDataHeader {
	if !a.flags.has(.managed) || a.data == unsafe { nil } {
		return unsafe { nil }
	}
	base_data := unsafe { &u8(a.data) - u64(a.offset) }
	return unsafe { &ArrayDataHeader(base_data - array_data_header_size()) }
}

@[inline]
fn (a array) buffer_has_slices() bool {
	if !a.flags.has(.managed) || a.data == unsafe { nil } {
		return false
	}
	header := unsafe { a.data_header() }
	if header == unsafe { nil } {
		return false
	}
	return unsafe { header.has_slices }
}

@[inline]
fn (mut a array) mark_buffer_has_slices() {
	if !a.flags.has(.managed) || a.data == unsafe { nil } {
		return
	}
	unsafe {
		header := a.data_header()
		if header != nil {
			header.has_slices = true
		}
	}
}

@[inline]
fn (mut a array) set_managed_flags(is_slice bool) {
	unsafe {
		a.flags.set(.managed)
		if is_slice {
			a.flags.set(.is_slice)
		} else {
			a.flags.clear(.is_slice)
		}
	}
}

@[inline]
fn (mut a array) clone_shallow_to_cap(new_cap int) {
	if new_cap <= 0 {
		unsafe { a.flags.clear(.managed | .noscan_data | .is_slice) }
		a.data = unsafe { nil }
		a.offset = 0
		a.cap = 0
		return
	}
	use_noscan_data := a.uses_noscan_data()
	total_size := u64(new_cap) * u64(a.element_size)
	new_data := a.alloc_array_data_like_uninit(total_size)
	copy_size := u64(a.len) * u64(a.element_size)
	if a.data != unsafe { nil } && copy_size > 0 {
		unsafe { vmemcpy(new_data, a.data, copy_size) }
	}
	a.data = new_data
	a.offset = 0
	a.cap = new_cap
	unsafe {
		if use_noscan_data {
			a.flags.set(.noscan_data)
		} else {
			a.flags.clear(.noscan_data)
		}
	}
	a.set_managed_flags(false)
}

@[inline]
fn v_ni_index(i int, len int) int {
	return if i < 0 { len + i } else { i }
}

// Internal function, used by V (`nums := []int`)
fn __new_array(mylen int, cap int, elm_size int) array {
	panic_on_negative_len(mylen)
	panic_on_negative_cap(cap)
	cap_ := if cap < mylen { mylen } else { cap }
	total_size := u64(cap_) * u64(elm_size)
	mut data := unsafe { nil }
	if cap_ > 0 && mylen == 0 {
		data = alloc_array_data_uninit(total_size)
	} else {
		data = alloc_array_data(total_size)
	}
	arr := array{
		element_size: elm_size
		data:         data
		len:          mylen
		cap:          cap_
		flags:        .managed
	}
	return arr
}

fn __new_array_with_default(mylen int, cap int, elm_size int, val voidptr) array {
	panic_on_negative_len(mylen)
	panic_on_negative_cap(cap)
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		len:          mylen
		cap:          cap_
		flags:        .managed
	}
	// x := []EmptyStruct{cap:5} ; for clang/gcc with -gc none,
	//    -> sizeof(EmptyStruct) == 0 -> elm_size == 0
	//    -> total_size == 0 -> malloc(0) -> panic;
	//    to avoid it, just allocate a single byte
	total_size := u64(cap_) * u64(elm_size)
	if cap_ > 0 && mylen == 0 {
		arr.data = alloc_array_data_uninit(total_size)
	} else {
		arr.data = alloc_array_data(total_size)
	}
	if val != 0 {
		mut eptr := &u8(arr.data)
		unsafe {
			if eptr != nil {
				if arr.element_size == 1 {
					byte_value := *(&u8(val))
					for i in 0 .. arr.len {
						eptr[i] = byte_value
					}
				} else {
					for _ in 0 .. arr.len {
						vmemcpy(eptr, val, arr.element_size)
						eptr += arr.element_size
					}
				}
			}
		}
	}
	return arr
}

fn __new_array_with_multi_default(mylen int, cap int, elm_size int, val voidptr) array {
	panic_on_negative_len(mylen)
	panic_on_negative_cap(cap)
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		len:          mylen
		cap:          cap_
		flags:        .managed
	}
	// x := []EmptyStruct{cap:5} ; for clang/gcc with -gc none,
	//    -> sizeof(EmptyStruct) == 0 -> elm_size == 0
	//    -> total_size == 0 -> malloc(0) -> panic;
	//    to avoid it, just allocate a single byte
	total_size := u64(cap_) * u64(elm_size)
	arr.data = alloc_array_data(total_size)
	if val != 0 {
		mut eptr := &u8(arr.data)
		unsafe {
			if eptr != nil {
				for i in 0 .. arr.len {
					vmemcpy(eptr, charptr(val) + i * arr.element_size, arr.element_size)
					eptr += arr.element_size
				}
			}
		}
	}
	return arr
}

fn __new_array_with_array_default(mylen int, cap int, elm_size int, val array, depth int) array {
	panic_on_negative_len(mylen)
	panic_on_negative_cap(cap)
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		data:         alloc_array_data(u64(cap_) * u64(elm_size))
		len:          mylen
		cap:          cap_
		flags:        .managed
	}
	mut eptr := &u8(arr.data)
	unsafe {
		if eptr != nil {
			for _ in 0 .. arr.len {
				val_clone := val.clone_to_depth(depth)
				vmemcpy(eptr, &val_clone, arr.element_size)
				eptr += arr.element_size
			}
		}
	}
	return arr
}

fn __new_array_with_map_default(mylen int, cap int, elm_size int, val map) array {
	panic_on_negative_len(mylen)
	panic_on_negative_cap(cap)
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		data:         alloc_array_data(u64(cap_) * u64(elm_size))
		len:          mylen
		cap:          cap_
		flags:        .managed
	}
	mut eptr := &u8(arr.data)
	unsafe {
		if eptr != nil {
			for _ in 0 .. arr.len {
				val_clone := val.clone()
				vmemcpy(eptr, &val_clone, arr.element_size)
				eptr += arr.element_size
			}
		}
	}
	return arr
}

// Private function, used by V (`nums := [1, 2, 3]`)
fn new_array_from_c_array(len int, cap int, elm_size int, c_array voidptr) array {
	panic_on_negative_len(len)
	panic_on_negative_cap(cap)
	mut cap_ := cap
	if cap < len {
		cap_ = len
	}
	arr := array{
		element_size: elm_size
		data:         alloc_array_data(u64(cap_) * u64(elm_size))
		len:          len
		cap:          cap_
		flags:        .managed
	}
	// TODO: Write all memory functions (like memcpy) in V
	unsafe { vmemcpy(arr.data, c_array, u64(len) * u64(elm_size)) }
	return arr
}

// Private function, used by V (`nums := [1, 2, 3] !`)
fn new_array_from_c_array_no_alloc(len int, cap int, elm_size int, c_array voidptr) array {
	panic_on_negative_len(len)
	panic_on_negative_cap(cap)
	arr := array{
		element_size: elm_size
		data:         c_array
		len:          len
		cap:          cap
	}
	return arr
}

// ensure_cap increases the `cap` of an array to the required value, if needed.
// It does so by copying the data to a new memory location (creating a clone),
// unless `a.cap` is already large enough.
pub fn (mut a array) ensure_cap(required int) {
	if required <= a.cap {
		return
	}
	if a.flags.has(.nogrow) {
		panic_n('array.ensure_cap: array with the flag `.nogrow` cannot grow in size, array required new size:',
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
			panic_n('array.ensure_cap: array needs to grow to cap (which is > 2^31):', cap)
		}
	}
	new_size := u64(cap) * u64(a.element_size)
	use_noscan_data := a.uses_noscan_data()
	new_data := a.alloc_array_data_like_uninit(new_size)
	if a.data != unsafe { nil } {
		unsafe { vmemcpy(new_data, a.data, u64(a.len) * u64(a.element_size)) }
		// TODO: the old data may be leaked when no GC is used (ref-counting?)
		if a.flags.has(.noslices) && !a.flags.has(.is_slice) && !a.buffer_has_slices() {
			unsafe {
				if a.flags.has(.managed) {
					free(&u8(a.data) - u64(array_data_header_size()))
				} else {
					free(a.data)
				}
			}
		}
	}
	a.data = new_data
	a.offset = 0
	a.cap = int(cap)
	unsafe {
		if use_noscan_data {
			a.flags.set(.noscan_data)
		} else {
			a.flags.clear(.noscan_data)
		}
	}
	a.set_managed_flags(false)
}

// repeat returns a new array with the given array elements repeated given times.
// `cgen` will replace this with an appropriate call to `repeat_to_depth()`
//
// This is a dummy placeholder that will be overridden by `cgen` with an appropriate
// call to `repeat_to_depth()`. However the `checker` needs it here.
pub fn (a array) repeat(count int) array {
	return unsafe { a.repeat_to_depth(count, 0) }
}

// repeat_to_depth is an unsafe version of `repeat()` that handles
// multi-dimensional arrays.
//
// It is `unsafe` to call directly because `depth` is not checked
@[direct_array_access; unsafe]
pub fn (a array) repeat_to_depth(count int, depth int) array {
	if count < 0 {
		panic_n('array.repeat: count is negative:', count)
	}
	mut size := u64(count) * u64(a.len) * u64(a.element_size)
	if size == 0 {
		size = u64(a.element_size)
	}
	use_noscan_data := depth == 0 && a.uses_noscan_data()
	mut data := unsafe { nil }
	if use_noscan_data {
		data = a.alloc_array_data_like(size)
	} else {
		data = alloc_array_data(size)
	}
	arr := array{
		element_size: a.element_size
		data:         data
		len:          count * a.len
		cap:          count * a.len
		flags:        if use_noscan_data { .managed | .noscan_data } else { .managed }
	}
	if a.len > 0 {
		a_total_size := u64(a.len) * u64(a.element_size)
		arr_step_size := u64(a.len) * u64(arr.element_size)
		mut eptr := &u8(arr.data)
		unsafe {
			if eptr != nil {
				for _ in 0 .. count {
					if depth > 0 {
						ary_clone := a.clone_to_depth(depth)
						vmemcpy(eptr, ary_clone.data, a_total_size)
					} else {
						vmemcpy(eptr, a.data, a_total_size)
					}
					eptr += arr_step_size
				}
			}
		}
	}
	return arr
}

@[inline]
fn (a array) needs_unique_shift(required int) bool {
	return required <= a.cap && (a.flags.has(.is_slice) || a.buffer_has_slices())
}

@[inline]
fn (a array) needs_unique_append(required int) bool {
	return required <= a.cap && a.flags.has(.is_slice)
}

@[inline]
fn (a array) needs_unique_shrink() bool {
	return a.flags.has(.is_slice) || a.buffer_has_slices()
}

// insert inserts a value in the array at index `i` and increases
// the index of subsequent elements by 1.
//
// This function is type-aware and can insert items of the same
// or lower dimensionality as the original array. That is, if
// the original array is `[]int`, then the insert `val` may be
// `int` or `[]int`. If the original array is `[][]int`, then `val`
// may be `[]int` or `[][]int`. Consider the examples.
//
// Example:
// ```v
// mut a := [1, 2, 4]
// a.insert(2, 3)          // a now is [1, 2, 3, 4]
// mut b := [3, 4]
// b.insert(0, [1, 2])     // b now is [1, 2, 3, 4]
// mut c := [[3, 4]]
// c.insert(0, [1, 2])     // c now is [[1, 2], [3, 4]]
// ```
pub fn (mut a array) insert(i int, val voidptr) {
	if i < 0 || i > a.len {
		panic_n2('array.insert: index out of range (i,a.len):', i, a.len)
	}
	if a.len == max_int {
		panic('array.insert: a.len reached max_int')
	}
	required := a.len + 1
	if a.needs_unique_shift(required) {
		a.clone_shallow_to_cap(a.cap)
	} else if required > a.cap {
		a.ensure_cap(required)
	}
	unsafe {
		vmemmove(a.get_unsafe(i + 1), a.get_unsafe(i), u64((a.len - i)) * u64(a.element_size))
		a.set_unsafe(i, val)
	}
	a.len++
}

// insert_many is used internally to implement inserting many values
// into an the array beginning at `i`.
@[unsafe]
fn (mut a array) insert_many(i int, val voidptr, size int) {
	if i < 0 || i > a.len {
		panic_n2('array.insert_many: index out of range (i,a.len):', i, a.len)
	}
	new_len := i64(a.len) + i64(size)
	if new_len > max_int {
		panic_n('array.insert_many: max_int will be exceeded by a.len:', new_len)
	}
	if a.needs_unique_shift(int(new_len)) {
		a.clone_shallow_to_cap(a.cap)
	} else if int(new_len) > a.cap {
		a.ensure_cap(int(new_len))
	}
	elem_size := a.element_size
	unsafe {
		iptr := a.get_unsafe(i)
		vmemmove(a.get_unsafe(i + size), iptr, u64(a.len - i) * u64(elem_size))
		vmemcpy(iptr, val, u64(size) * u64(elem_size))
	}
	a.len = int(new_len)
}

// prepend prepends one or more elements to an array.
// It is shorthand for `.insert(0, val)`
pub fn (mut a array) prepend(val voidptr) {
	a.insert(0, val)
}

// prepend_many prepends another array to this array.
// NOTE: `.prepend` is probably all you need.
// NOTE: This code is never called in all of vlib
@[unsafe]
fn (mut a array) prepend_many(val voidptr, size int) {
	unsafe { a.insert_many(0, val, size) }
}

// delete deletes array element at index `i`.
// Deleting the last element uses the same in-place fast path as `.delete_last()`.
// NOTE: When deleting the last element, this operates in-place.
// Other positions create a copy of the array, skipping over the
// element at `i`, and then point the original variable to the new
// memory location.
//
// Example:
// ```v
// mut a := ['0', '1', '2', '3', '4', '5']
// a.delete(1) // a is now ['0', '2', '3', '4', '5']
// ```
pub fn (mut a array) delete(i int) {
	if i < 0 || i >= a.len {
		panic_n2('array.delete: index out of range (i,a.len):', i, a.len)
	}
	if i == a.len - 1 && !a.needs_unique_shrink() {
		a.len--
		return
	}
	a.delete_many(i, 1)
}

// delete_many deletes `size` elements beginning with index `i`
// NOTE: This function does NOT operate in-place. Internally, it
// creates a copy of the array, skipping over `size` elements
// starting at `i`, and then points the original variable
// to the new memory location.
//
// Example:
// ```v
// mut a := [1, 2, 3, 4, 5, 6, 7, 8, 9]
// b := unsafe { a[..9] } // creates a `slice` of `a`, not a clone
// a.delete_many(4, 3) // replaces `a` with a modified clone
// dump(a) // a: [1, 2, 3, 4, 8, 9] // `a` is now different
// dump(b) // b: [1, 2, 3, 4, 5, 6, 7, 8, 9] // `b` is still the same
// ```
pub fn (mut a array) delete_many(i int, size int) {
	if i < 0 || i64(i) + i64(size) > i64(a.len) {
		if size > 1 {
			panic_n3('array.delete: index out of range (i,i+size,a.len):', i, i + size, a.len)
		} else {
			panic_n2('array.delete: index out of range (i,a.len):', i, a.len)
		}
	}
	if size == 0 {
		if a.needs_unique_shrink() {
			a.clone_shallow_to_cap(a.len)
		}
		return
	}
	if a.flags.all(.noshrink | .noslices) && !a.needs_unique_shrink() {
		unsafe {
			vmemmove(&u8(a.data) + u64(i) * u64(a.element_size), &u8(a.data) + u64(i +
				size) * u64(a.element_size), u64(a.len - i - size) * u64(a.element_size))
		}
		a.len -= size
		return
	}
	if !a.needs_unique_shrink() {
		unsafe {
			vmemmove(&u8(a.data) + u64(i) * u64(a.element_size), &u8(a.data) + u64(i +
				size) * u64(a.element_size), u64(a.len - i - size) * u64(a.element_size))
		}
		a.len -= size
		a.cap = a.len
		return
	}
	// Note: if a is [12,34], a.len = 2, a.delete(0)
	// should move (2-0-1) elements = 1 element (the 34) forward
	old_data := a.data
	new_size := a.len - size
	if new_size == 0 {
		unsafe { a.flags.clear(.managed | .noscan_data | .is_slice) }
		a.data = unsafe { nil }
		a.offset = 0
		a.len = 0
		a.cap = 0
		return
	}
	new_cap := new_size
	use_noscan_data := a.uses_noscan_data()
	a.data = a.alloc_array_data_like(u64(new_cap) * u64(a.element_size))
	unsafe { vmemcpy(a.data, old_data, u64(i) * u64(a.element_size)) }
	unsafe {
		vmemcpy(&u8(a.data) + u64(i) * u64(a.element_size), &u8(old_data) + u64(i +
			size) * u64(a.element_size), u64(a.len - i - size) * u64(a.element_size))
	}
	if a.flags.has(.noslices) && !a.flags.has(.managed) {
		unsafe {
			free(old_data)
		}
	}
	a.len = new_size
	a.cap = new_cap
	a.offset = 0
	unsafe {
		if use_noscan_data {
			a.flags.set(.noscan_data)
		} else {
			a.flags.clear(.noscan_data)
		}
	}
	a.set_managed_flags(false)
}

// clear clears the array without deallocating the allocated data.
// It does it by setting the array length to `0`
// Example: mut a := [1,2]; a.clear(); assert a.len == 0
pub fn (mut a array) clear() {
	if a.needs_unique_shrink() {
		unsafe { a.flags.clear(.managed | .noscan_data | .is_slice) }
		a.data = unsafe { nil }
		a.offset = 0
		a.cap = 0
	}
	a.len = 0
}

// reset quickly sets the bytes of all elements of the array to 0.
// Useful mainly for numeric arrays. Note, that calling reset()
// is not safe, when your array contains more complex elements,
// like structs, maps, pointers etc, since setting them to 0,
// can later lead to hard to find bugs.
@[unsafe]
pub fn (mut a array) reset() {
	unsafe { vmemset(a.data, 0, a.len * a.element_size) }
}

// trim trims the array length to `index` without modifying the allocated data.
// If `index` is greater than `len` nothing will be changed.
// Example: mut a := [1,2,3,4]; a.trim(3); assert a.len == 3
pub fn (mut a array) trim(index int) {
	if index < a.len {
		if index >= 0 && a.needs_unique_shrink() {
			a.delete_many(index, a.len - index)
			return
		}
		a.len = index
	}
}

// drop advances the array past the first `num` elements whilst preserving spare capacity.
// If `num` is greater than `len` the array will be emptied.
// Example:
// ```v
// mut a := [1,2]
// a << 3
// a.drop(2)
// assert a == [3]
// assert a.cap > a.len
// ```
pub fn (mut a array) drop(num int) {
	if num <= 0 {
		return
	}
	n := if num <= a.len { num } else { a.len }
	blen := u64(n) * u64(a.element_size)
	a.data = unsafe { &u8(a.data) + blen }
	a.offset += int(blen) // TODO: offset should become 64bit as well
	a.len -= n
	a.cap -= n
}

// we manually inline this for single operations for performance without -prod
@[inline; unsafe]
fn (a array) get_unsafe(i int) voidptr {
	unsafe {
		return &u8(a.data) + u64(i) * u64(a.element_size)
	}
}

// Private function. Used to implement array[] operator.
fn (a array) get(i int) voidptr {
	$if !no_bounds_checking {
		if i < 0 || i >= a.len {
			panic_n2('array.get: index out of range (i,a.len):', i, a.len)
		}
	}
	unsafe {
		return &u8(a.data) + u64(i) * u64(a.element_size)
	}
}

@[markused]
fn (a array) get_i64(i i64) voidptr {
	$if !no_bounds_checking {
		if i < 0 || i >= i64(a.len) {
			panic_n2('array.get: index out of range (i,a.len):', i, a.len)
		}
	}
	unsafe {
		return &u8(a.data) + u64(i) * u64(a.element_size)
	}
}

@[markused]
fn (a array) get_u64(i u64) voidptr {
	$if !no_bounds_checking {
		if i >= u64(a.len) {
			panic('array.get: index out of range (i,a.len): ' + i.str() + ', ' +
				impl_i64_to_string(a.len))
		}
	}
	unsafe {
		return &u8(a.data) + i * u64(a.element_size)
	}
}

@[markused]
fn (a array) get_ni(i int) voidptr {
	return a.get(v_ni_index(i, a.len))
}

// Private function. Used to implement x = a[i] or { ... }
fn (a array) get_with_check(i int) voidptr {
	if i < 0 || i >= a.len {
		return 0
	}
	unsafe {
		return &u8(a.data) + u64(i) * u64(a.element_size)
	}
}

@[markused]
fn (a array) get_with_check_i64(i i64) voidptr {
	if i < 0 || i >= i64(a.len) {
		return 0
	}
	unsafe {
		return &u8(a.data) + u64(i) * u64(a.element_size)
	}
}

@[markused]
fn (a array) get_with_check_u64(i u64) voidptr {
	if i >= u64(a.len) {
		return 0
	}
	unsafe {
		return &u8(a.data) + i * u64(a.element_size)
	}
}

@[markused]
fn (a array) get_with_check_ni(i int) voidptr {
	return a.get_with_check(v_ni_index(i, a.len))
}

// first returns the first element of the `array`.
// If the `array` is empty, this will panic.
// However, `a[0]` returns an error object
// so it can be handled with an `or` block.
pub fn (a array) first() voidptr {
	if a.len == 0 {
		panic('array.first: array is empty')
	}
	return a.data
}

// last returns the last element of the `array`.
// If the `array` is empty, this will panic.
pub fn (a array) last() voidptr {
	if a.len == 0 {
		panic('array.last: array is empty')
	}
	unsafe {
		return &u8(a.data) + u64(a.len - 1) * u64(a.element_size)
	}
}

// pop_left returns the first element of the array and removes it by advancing the data pointer.
// If the `array` is empty, this will panic.
// NOTE: This function:
//   - Reduces both length and capacity by 1
//   - Advances the underlying data pointer by one element
//   - Leaves subsequent elements in-place (no memory copying)
// Sliced views will retain access to the original first element position,
// which is now detached from the array's active memory range.
//
// Example:
// ```v
// mut a := [1, 2, 3, 4, 5]
// b := unsafe { a[..5] } // full slice view
// first := a.pop_left()
//
// // Array now starts from second element
// dump(a) // a: [2, 3, 4, 5]
// assert a.len == 4
// assert a.cap == 4
//
// // Slice retains original memory layout
// dump(b) // b: [1, 2, 3, 4, 5]
// assert b.len == 5
//
// assert first == 1
//
// // Modifications affect both array and slice views
// a[0] = 99
// assert b[1] == 99  // changed in both
// ```
pub fn (mut a array) pop_left() voidptr {
	if a.len == 0 {
		panic('array.pop_left: array is empty')
	}
	first_elem := a.data
	unsafe {
		a.data = &u8(a.data) + u64(a.element_size)
	}
	a.offset += a.element_size
	a.len--
	a.cap--
	return first_elem
}

// pop returns the last element of the array, and removes it.
// If the `array` is empty, this will panic.
// NOTE: this function reduces the length of the given array,
// but arrays sliced from this one will not change. They still
// retain their "view" of the underlying memory.
//
// Example:
// ```v
// mut a := [1, 2, 3, 4, 5, 6, 7, 8, 9]
// b := unsafe{ a[..9] } // creates a "view" (also called a slice) into the same memory
// c := a.pop()
// assert c == 9
// a[1] = 5
// dump(a) // a: [1, 5, 3, 4, 5, 6, 7, 8]
// dump(b) // b: [1, 5, 3, 4, 5, 6, 7, 8, 9]
// assert a.len == 8
// assert b.len == 9
// ```
pub fn (mut a array) pop() voidptr {
	// in a sense, this is the opposite of `a << x`
	if a.len == 0 {
		panic('array.pop: array is empty')
	}
	new_len := a.len - 1
	last_elem := unsafe { &u8(a.data) + u64(new_len) * u64(a.element_size) }
	if a.needs_unique_shrink() {
		a.delete_many(new_len, 1)
		return last_elem
	}
	a.len = new_len
	// Note: a.cap is not changed here *on purpose*, so that
	// further << ops on that array will be more efficient.
	return last_elem
}

// delete_last efficiently deletes the last element of the array.
// It does it simply by reducing the length of the array by 1.
// If the array is empty, this will panic.
// See also: [trim](#array.trim)
pub fn (mut a array) delete_last() {
	if a.len == 0 {
		panic('array.delete_last: array is empty')
	}
	if a.needs_unique_shrink() {
		a.delete_many(a.len - 1, 1)
		return
	}
	a.len--
}

// slice returns an array using the same buffer as original array
// but starting from the `start` element and ending with the element before
// the `end` element of the original array with the length and capacity
// set to the number of the elements in the slice.
// It will remain tied to the same memory location until the length increases
// (copy on grow) or `.clone()` is called on it.
// If `start` and `end` are invalid this function will panic.
// Alternative: Slices can also be made with [start..end] notation
// Alternative: `.slice_ni()` will always return an array.
fn (a array) slice(start int, _end int) array {
	// WARNNING: The is a temp solution for bootstrap!
	end := if _end == max_i64 || _end == max_i32 { a.len } else { _end } // max_int
	$if !no_bounds_checking {
		if start > end {
			panic(
				'array.slice: invalid slice index (start>end):' + impl_i64_to_string(i64(start)) +
				', ' + impl_i64_to_string(end))
		}
		if end > a.len {
			panic('array.slice: slice bounds out of range (' + impl_i64_to_string(end) + ' >= ' +
				impl_i64_to_string(a.len) + ')')
		}
		if start < 0 {
			panic('array.slice: slice bounds out of range (start<0):' + impl_i64_to_string(start))
		}
	}
	// TODO: integrate reference counting
	unsafe { a.mark_buffer_has_slices() }
	offset := u64(start) * u64(a.element_size)
	data := unsafe { &u8(a.data) + offset }
	l := end - start
	mut flags := ArrayFlags.is_slice
	if a.uses_noscan_data() {
		unsafe { flags.set(.noscan_data) }
	}
	res := array{
		element_size: a.element_size
		data:         data
		offset:       a.offset + int(offset) // TODO: offset should become 64bit
		len:          l
		cap:          l
		flags:        flags
	}
	return res
}

// slice_ni returns an array using the same buffer as original array
// but starting from the `start` element and ending with the element before
// the `end` element of the original array.
// This function can use negative indexes `a.slice_ni(-3, a.len)`
// that get the last 3 elements of the array otherwise it return an empty array.
// This function always return a valid array.
fn (a array) slice_ni(_start int, _end int) array {
	unsafe { a.mark_buffer_has_slices() }
	mut flags := ArrayFlags.is_slice
	if a.uses_noscan_data() {
		unsafe { flags.set(.noscan_data) }
	}
	// WARNNING: The is a temp solution for bootstrap!
	mut end := if _end == max_i64 || _end == max_i32 { a.len } else { _end } // max_int
	mut start := _start

	if start < 0 {
		start = a.len + start
		if start < 0 {
			start = 0
		}
	}

	if end < 0 {
		end = a.len + end
		if end < 0 {
			end = 0
		}
	}
	if end >= a.len {
		end = a.len
	}

	if start >= a.len || start > end {
		res := array{
			element_size: a.element_size
			data:         a.data
			offset:       0
			len:          0
			cap:          0
			flags:        flags
		}
		return res
	}

	offset := u64(start) * u64(a.element_size)
	data := unsafe { &u8(a.data) + offset }
	l := end - start
	res := array{
		element_size: a.element_size
		data:         data
		offset:       a.offset + int(offset) // TODO: offset should be 64bit
		len:          l
		cap:          l
		flags:        flags
	}
	return res
}

// clone_static_to_depth() returns an independent copy of a given array.
// Unlike `clone_to_depth()` it has a value receiver and is used internally
// for slice-clone expressions like `a[2..4].clone()` and in -autofree generated code.
fn (a array) clone_static_to_depth(depth int) array {
	return unsafe { a.clone_to_depth(depth) }
}

// clone returns an independent copy of a given array.
// this will be overwritten by `cgen` with an appropriate call to `.clone_to_depth()`
// However the `checker` needs it here.
pub fn (a &array) clone() array {
	return unsafe { a.clone_to_depth(0) }
}

// recursively clone given array - `unsafe` when called directly because depth is not checked
@[unsafe]
pub fn (a &array) clone_to_depth(depth int) array {
	source_capacity_in_bytes := u64(a.cap) * u64(a.element_size)
	use_noscan_data := depth == 0 && a.uses_noscan_data()
	mut data := unsafe { nil }
	if use_noscan_data {
		data = a.alloc_array_data_like(source_capacity_in_bytes)
	} else {
		data = alloc_array_data(source_capacity_in_bytes)
	}
	mut arr := array{
		element_size: a.element_size
		data:         data
		len:          a.len
		cap:          a.cap
		flags:        if use_noscan_data { .managed | .noscan_data } else { .managed }
	}
	// Recursively clone-generated elements if array element is array type
	if depth > 0 && a.element_size == sizeof(array) && a.len >= 0 && a.cap >= a.len {
		ar := array{}
		asize := int(sizeof(array))
		for i in 0 .. a.len {
			unsafe { vmemcpy(&ar, a.get_unsafe(i), asize) }
			ar_clone := unsafe { ar.clone_to_depth(depth - 1) }
			unsafe { arr.set_unsafe(i, &ar_clone) }
		}
		return arr
	} else if depth > 0 && a.element_size == sizeof(string) && a.len >= 0 && a.cap >= a.len {
		for i in 0 .. a.len {
			str_ptr := unsafe { &string(a.get_unsafe(i)) }
			str_clone := (*str_ptr).clone()
			unsafe { arr.set_unsafe(i, &str_clone) }
		}
		return arr
	}
	if a.data != 0 && source_capacity_in_bytes > 0 {
		unsafe { vmemcpy(arr.data, a.data, source_capacity_in_bytes) }
	}
	return arr
}

// we manually inline this for single operations for performance without -prod
@[inline; unsafe]
fn (mut a array) set_unsafe(i int, val voidptr) {
	unsafe { vmemcpy(&u8(a.data) + u64(a.element_size) * u64(i), val, a.element_size) }
}

// Private function. Used to implement assignment to the array element.
fn (mut a array) set(i int, val voidptr) {
	$if !no_bounds_checking {
		if i < 0 || i >= a.len {
			panic_n2('array.set: index out of range (i,a.len):', i, a.len)
		}
	}
	unsafe { vmemcpy(&u8(a.data) + u64(a.element_size) * u64(i), val, a.element_size) }
}

@[markused]
fn (mut a array) set_i64(i i64, val voidptr) {
	$if !no_bounds_checking {
		if i < 0 || i >= i64(a.len) {
			panic_n2('array.set: index out of range (i,a.len):', i, a.len)
		}
	}
	unsafe { vmemcpy(&u8(a.data) + u64(a.element_size) * u64(i), val, a.element_size) }
}

@[markused]
fn (mut a array) set_u64(i u64, val voidptr) {
	$if !no_bounds_checking {
		if i >= u64(a.len) {
			panic('array.set: index out of range (i,a.len): ' + i.str() + ', ' +
				impl_i64_to_string(a.len))
		}
	}
	unsafe { vmemcpy(&u8(a.data) + u64(a.element_size) * i, val, a.element_size) }
}

@[markused]
fn (mut a array) set_ni(i int, val voidptr) {
	a.set(v_ni_index(i, a.len), val)
}

fn (mut a array) push(val voidptr) {
	if a.len < 0 {
		panic('array.push: negative len')
	}
	if a.len >= max_int {
		panic('array.push: len bigger than max_int')
	}
	required := a.len + 1
	if a.needs_unique_append(required) {
		a.clone_shallow_to_cap(a.cap)
	} else if required > a.cap {
		a.ensure_cap(required)
	}
	unsafe { vmemcpy(&u8(a.data) + u64(a.element_size) * u64(a.len), val, a.element_size) }
	a.len++
}

// push_many implements the functionality for pushing another array.
// `val` is array.data and user facing usage is `a << [1,2,3]`
@[unsafe]
pub fn (mut a array) push_many(val voidptr, size int) {
	if size <= 0 || val == unsafe { nil } {
		return
	}
	new_len := i64(a.len) + i64(size)
	if new_len > max_int {
		// string interpolation also uses <<; avoid it, use a fixed string for the panic
		panic('array.push_many: new len exceeds max_int')
	}
	if a.needs_unique_append(int(new_len)) {
		a.clone_shallow_to_cap(a.cap)
	}
	is_self_append := a.data == val && a.data != 0
	if int(new_len) > a.cap {
		a.ensure_cap(int(new_len))
	}
	if is_self_append {
		// handle `arr << arr`
		copy := a.clone()
		unsafe {
			vmemcpy(&u8(a.data) + u64(a.element_size) * u64(a.len), copy.data,
				u64(a.element_size) * u64(size))
		}
	} else {
		if a.data != 0 && val != 0 {
			unsafe {
				vmemcpy(&u8(a.data) + u64(a.element_size) * u64(a.len), val,
					u64(a.element_size) * u64(size))
			}
		}
	}
	a.len = int(new_len)
}

// reverse_in_place reverses existing array data, modifying original array.
pub fn (mut a array) reverse_in_place() {
	if a.len < 2 || a.element_size == 0 {
		return
	}
	unsafe {
		mut tmp_value := malloc(a.element_size)
		for i in 0 .. a.len / 2 {
			vmemcpy(tmp_value, &u8(a.data) + u64(i) * u64(a.element_size), a.element_size)
			vmemcpy(&u8(a.data) + u64(i) * u64(a.element_size), &u8(a.data) + u64(a.len - 1 -
				i) * u64(a.element_size), a.element_size)
			vmemcpy(&u8(a.data) + u64(a.len - 1 - i) * u64(a.element_size), tmp_value,
				a.element_size)
		}
		free(tmp_value)
	}
}

// reverse returns a new array with the elements of the original array in reverse order.
pub fn (a array) reverse() array {
	if a.len < 2 {
		return a
	}
	use_noscan_data := a.uses_noscan_data()
	mut arr := array{
		element_size: a.element_size
		data:         a.alloc_array_data_like(u64(a.cap) * u64(a.element_size))
		len:          a.len
		cap:          a.cap
		flags:        if use_noscan_data { .managed | .noscan_data } else { .managed }
	}
	for i in 0 .. a.len {
		unsafe { arr.set_unsafe(i, a.get_unsafe(a.len - 1 - i)) }
	}
	return arr
}

// free frees all memory occupied by the array.
@[unsafe]
pub fn (a &array) free() {
	$if prealloc {
		return
	}
	// if a.is_slice {
	// return
	// }
	if a.flags.has(.nofree) {
		return
	}
	mblock_ptr := &u8(u64(a.data) - u64(a.offset))
	if mblock_ptr != unsafe { nil } {
		unsafe {
			if a.flags.has(.managed) {
				free(mblock_ptr - array_data_header_size())
			} else {
				free(mblock_ptr)
			}
		}
	}
	unsafe {
		a.data = nil
		a.offset = 0
		a.len = 0
		a.cap = 0
	}
}

// Some of the following functions have no implementation in V and exist here
// to expose them to the array namespace. Their implementation is compiler
// specific because of their use of `it` and `a < b` expressions.
// Therefore, the implementation is left to the backend.

// filter creates a new array with all elements that pass the test.
// Ignore the function signature. `filter` does not take an actual callback. Rather, it
// takes an `it` expression.
//
// Certain array functions (`filter` `any` `all`) support a simplified
// domain-specific-language by the backend compiler to make these operations
// more idiomatic to V. These functions are described here, but their implementation
// is compiler specific.
//
// Each function takes a boolean test expression as its single argument.
// These test expressions may use `it` as a pointer to a single element at a time.
//
// Example: a := [10,20,30,3,5,99]; assert a.filter(it < 5) == [3] // create an array of elements less than 5
// Example: a := [10,20,30,3,5,99]; assert a.filter(it % 2 == 1) == [3,5,99] // create an array of only odd elements
// Example: struct Named { name string }; a := [Named{'Abc'}, Named{'Bcd'}, Named{'Az'}]; assert a.filter(it.name[0] == `A`).len == 2
pub fn (a array) filter(predicate fn (voidptr) bool) array

// any tests whether at least one element in the array passes the test.
// Ignore the function signature. `any` does not take an actual callback. Rather, it
// takes an `it` expression.
// It returns `true` if it finds an element passing the test. Otherwise,
// it returns `false`. It doesn't modify the array.
//
// Example: a := [2,3,4]; assert a.any(it % 2 == 1) // 3 is odd, so this will pass
// Example: struct Named { name string }; a := [Named{'Bob'}, Named{'Bilbo'}]; assert a.any(it.name == 'Bob') // the first element will match
pub fn (a array) any(predicate fn (voidptr) bool) bool

// count counts how many elements in array pass the test.
// Ignore the function signature. `count` does not take an actual callback. Rather, it
// takes an `it` expression.
//
// Example: a := [10,3,5,7]; assert a.count(it % 2 == 1) == 3 // will return how many elements are odd
pub fn (a array) count(predicate fn (voidptr) bool) int

// all tests whether all elements in the array pass the test.
// Ignore the function signature. `all` does not take an actual callback. Rather, it
// takes an `it` expression.
// It returns `false` if any element fails the test. Otherwise,
// it returns `true`. It doesn't modify the array.
//
// Example: a := [3,5,7,9]; assert a.all(it % 2 == 1) // will return true if every element is odd
pub fn (a array) all(predicate fn (voidptr) bool) bool

// map creates a new array populated with the results of calling a provided function
// on every element in the calling array.
// It also accepts an `it` expression.
//
// Example:
// ```v
// words := ['hello', 'world']
// r1 := words.map(it.to_upper())
// assert r1 == ['HELLO', 'WORLD']
//
// // map can also accept anonymous functions
// r2 := words.map(fn (w string) string {
// 	return w.to_upper()
// })
// assert r2 == ['HELLO', 'WORLD']
// ```
pub fn (a array) map(callback fn (voidptr) voidptr) array

// sort sorts the array in place.
// Ignore the function signature. Passing a callback to `.sort` is not supported
// for now. Consider using the `.sort_with_compare` method if you need it.
//
// sort can take a boolean test expression as its single argument.
// The expression uses 2 'magic' variables `a` and `b` as pointers to the two elements
// being compared.
// Equal elements keep their original relative order.
//
// Example: mut aa := [5,2,1,10]; aa.sort(); assert aa == [1,2,5,10] // will sort the array in ascending order
// Example: mut aa := [5,2,1,10]; aa.sort(b < a); assert aa == [10,5,2,1] // will sort the array in descending order
// Example: struct Named { name string }; mut aa := [Named{'Abc'}, Named{'Xyz'}]; aa.sort(b.name < a.name); assert aa.map(it.name) == ['Xyz','Abc'] // will sort descending by the .name field
pub fn (mut a array) sort(callback fn (voidptr, voidptr) int)

// sorted returns a sorted copy of the original array. The original array is *NOT* modified.
// See also .sort() .
// Equal elements keep their original relative order.
// Example: assert [9,1,6,3,9].sorted() == [1,3,6,9,9]
// Example: assert [9,1,6,3,9].sorted(b < a) == [9,9,6,3,1]
pub fn (a &array) sorted(callback fn (voidptr, voidptr) int) array

// sort_with_compare sorts the array in-place using the results of the
// given function to determine sort order.
//
// The function should return one of three values:
// - `-1` when `a` should come before `b` ( `a < b` )
// - `1`  when `b` should come before `a` ( `b < a` )
// - `0`  when the order cannot be determined ( `a == b` )
// Returning `0` keeps equal elements in their original relative order.
//
// Example:
// ```v
// mut a := ['hi', '1', '5', '3']
// a.sort_with_compare(fn (a &string, b &string) int {
// 		if a < b {
// 			return -1
// 		}
// 		if a > b {
// 			return 1
// 		}
// 		return 0
// })
// assert a == ['1', '3', '5', 'hi']
// ```
pub fn (mut a array) sort_with_compare(callback FnSortCB) {
	$if freestanding {
		panic('sort_with_compare does not work with -freestanding')
	} $else {
		unsafe { vqsort(a.data, usize(a.len), usize(a.element_size), callback) }
	}
}

// sorted_with_compare sorts a clone of the array. The original array is not modified.
// It uses the results of the given function to determine sort order.
// See also .sort_with_compare()
// Equal elements keep their original relative order.
pub fn (a &array) sorted_with_compare(callback FnSortCB) array {
	mut r := a.clone()
	unsafe { vqsort(r.data, usize(r.len), usize(r.element_size), callback) }
	return r
}

// contains determines whether an array includes a certain value among its elements.
// It will return `true` if the array contains an element with this value.
// It is similar to `.any` but does not take an `it` expression.
//
// Example: assert [1, 2, 3].contains(4) == false
pub fn (a array) contains(value voidptr) bool

// index returns the first index at which a given element can be found in the array or `-1` if the value is not found.
pub fn (a array) index(value voidptr) int

// last_index returns the last index at which a given element can be found in the array or `-1` if the value is not found.
pub fn (a array) last_index(value voidptr) int

@[direct_array_access; unsafe]
pub fn (mut a []string) free() {
	$if prealloc {
		return
	}
	for mut s in a {
		unsafe { s.free() }
	}
	mut arr := unsafe { &array(a) }
	unsafe { arr.free() }
}

// The following functions are type-specific functions that apply
// to arrays of different types in different ways.

// str returns a string representation of an array of strings.
// Example: assert ['a', 'b', 'c'].str() == "['a', 'b', 'c']"
@[direct_array_access; manualfree]
pub fn (a []string) str() string {
	mut sb_len := 4 // 2x" + 1x, + 1xspace
	if a.len > 0 {
		// assume that most strings will be ~large as the first
		sb_len += a[0].len
		sb_len *= a.len
	}
	sb_len += 2 // 1x[ + 1x]
	mut sb := strings.new_builder(sb_len)
	sb.write_u8(`[`)
	for i in 0 .. a.len {
		val := a[i]
		sb.write_u8(`'`)
		sb.write_string(val)
		sb.write_u8(`'`)
		if i < a.len - 1 {
			sb.write_string(', ')
		}
	}
	sb.write_u8(`]`)
	res := sb.str()
	unsafe { sb.free() }
	return res
}

// hex returns a string with the hexadecimal representation of the byte elements of the array `b`.
pub fn (b []u8) hex() string {
	if b.len == 0 {
		return ''
	}
	return unsafe { data_to_hex_string(b.data, b.len) }
}

// copy copies the `src` byte array elements to the `dst` byte array.
// The number of the elements copied is the minimum of the length of both arrays.
// Returns the number of elements copied.
// NOTE: This is not an `array` method. It is a function that takes two arrays of bytes.
// See also: `arrays.copy`.
pub fn copy(mut dst []u8, src []u8) int {
	min := if dst.len < src.len { dst.len } else { src.len }
	if min > 0 {
		unsafe { vmemmove(dst.data, src.data, min) }
	}
	return min
}

// grow_cap grows the array's capacity by `amount` elements.
// Internally, it does this by copying the entire array to
// a new memory location (creating a clone).
pub fn (mut a array) grow_cap(amount int) {
	new_cap := i64(amount) + i64(a.cap)
	if new_cap > max_int {
		panic_n('array.grow_cap: max_int will be exceeded by new cap:', new_cap)
	}
	a.ensure_cap(int(new_cap))
}

// grow_len ensures that an array has a.len + amount of length
// Internally, it does this by copying the entire array to
// a new memory location (creating a clone) unless the array.cap
// is already large enough.
@[unsafe]
pub fn (mut a array) grow_len(amount int) {
	new_len := i64(amount) + i64(a.len)
	if new_len > max_int {
		panic_n('array.grow_len: max_int will be exceeded by new len:', new_len)
	}
	a.ensure_cap(int(new_len))
	a.len = int(new_len)
}

// pointers returns a new array, where each element
// is the address of the corresponding element in the array.
@[unsafe]
pub fn (a array) pointers() []voidptr {
	mut res := []voidptr{}
	for i in 0 .. a.len {
		unsafe { res << a.get_unsafe(i) }
	}
	return res
}

// vbytes on`voidptr` makes a V []u8 structure from a C style memory buffer.
// NOTE: the data is reused, NOT copied!
@[unsafe]
pub fn (data voidptr) vbytes(len int) []u8 {
	res := array{
		element_size: 1
		data:         data
		len:          len
		cap:          len
	}
	return res
}

// vbytes on `&u8` makes a V []u8 structure from a C style memory buffer.
// NOTE: the data is reused, NOT copied!
@[unsafe]
pub fn (data &u8) vbytes(len int) []u8 {
	return unsafe { voidptr(data).vbytes(len) }
}

// free frees the memory allocated
@[unsafe]
pub fn (data &u8) free() {
	unsafe { free(data) }
}

@[if !no_bounds_checking ?; inline]
fn panic_on_negative_len(len int) {
	if len < 0 {
		panic_n('negative .len:', len)
	}
}

@[if !no_bounds_checking ?; inline]
fn panic_on_negative_cap(cap int) {
	if cap < 0 {
		panic_n('negative .cap:', cap)
	}
}
