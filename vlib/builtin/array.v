// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

import strings

// array is a struct, used for denoting all array types in V.
// `.data` is a void pointer to the backing heap memory block,
// which avoids using generics and thus without generating extra
// code for every type.
pub struct array {
pub:
	element_size int // size in bytes of one element in the array.
pub mut:
	data   voidptr
	offset int // in bytes (should be `usize`)
	len    int // length of the array in elements.
	cap    int // capacity of the array in elements.
	flags  ArrayFlags
}

[flag]
pub enum ArrayFlags {
	noslices // when <<, `.noslices` will free the old data block immediately (you have to be sure, that there are *no slices* to that specific array). TODO: integrate with reference counting/compiler support for the static cases.
	noshrink // when `.noslices` and `.noshrink` are *both set*, .delete(x) will NOT allocate new memory and free the old. It will just move the elements in place, and adjust .len.
}

// Internal function, used by V (`nums := []int`)
fn __new_array(mylen int, cap int, elm_size int) array {
	cap_ := if cap < mylen { mylen } else { cap }
	arr := array{
		element_size: elm_size
		data: vcalloc(u64(cap_) * u64(elm_size))
		len: mylen
		cap: cap_
	}
	return arr
}

fn __new_array_with_default(mylen int, cap int, elm_size int, val voidptr) array {
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		len: mylen
		cap: cap_
	}
	total_size := u64(cap_) * u64(elm_size)
	if cap_ > 0 && mylen == 0 {
		arr.data = unsafe { malloc(total_size) }
	} else {
		arr.data = vcalloc(total_size)
	}
	if val != 0 {
		mut eptr := &u8(arr.data)
		unsafe {
			for _ in 0 .. arr.len {
				vmemcpy(eptr, val, arr.element_size)
				eptr += arr.element_size
			}
		}
	}
	return arr
}

fn __new_array_with_array_default(mylen int, cap int, elm_size int, val array) array {
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		data: unsafe { malloc(u64(cap_) * u64(elm_size)) }
		len: mylen
		cap: cap_
	}
	mut eptr := &u8(arr.data)
	unsafe {
		for _ in 0 .. arr.len {
			val_clone := val.clone_to_depth(1)
			vmemcpy(eptr, &val_clone, arr.element_size)
			eptr += arr.element_size
		}
	}
	return arr
}

fn __new_array_with_map_default(mylen int, cap int, elm_size int, val map) array {
	cap_ := if cap < mylen { mylen } else { cap }
	mut arr := array{
		element_size: elm_size
		data: unsafe { malloc(u64(cap_) * u64(elm_size)) }
		len: mylen
		cap: cap_
	}
	mut eptr := &u8(arr.data)
	unsafe {
		for _ in 0 .. arr.len {
			val_clone := val.clone()
			vmemcpy(eptr, &val_clone, arr.element_size)
			eptr += arr.element_size
		}
	}
	return arr
}

// Private function, used by V (`nums := [1, 2, 3]`)
fn new_array_from_c_array(len int, cap int, elm_size int, c_array voidptr) array {
	cap_ := if cap < len { len } else { cap }
	arr := array{
		element_size: elm_size
		data: vcalloc(u64(cap_) * u64(elm_size))
		len: len
		cap: cap_
	}
	// TODO Write all memory functions (like memcpy) in V
	unsafe { vmemcpy(arr.data, c_array, u64(len) * u64(elm_size)) }
	return arr
}

// Private function, used by V (`nums := [1, 2, 3] !`)
fn new_array_from_c_array_no_alloc(len int, cap int, elm_size int, c_array voidptr) array {
	arr := array{
		element_size: elm_size
		data: c_array
		len: len
		cap: cap
	}
	return arr
}

// Private function. Increases the `cap` of an array to the
// required value by copying the data to a new memory location
// (creating a clone) unless `a.cap` is already large enough.
fn (mut a array) ensure_cap(required int) {
	if required <= a.cap {
		return
	}
	mut cap := if a.cap > 0 { a.cap } else { 2 }
	for required > cap {
		cap *= 2
	}
	new_size := u64(cap) * u64(a.element_size)
	new_data := unsafe { malloc(new_size) }
	if a.data != unsafe { nil } {
		unsafe { vmemcpy(new_data, a.data, u64(a.len) * u64(a.element_size)) }
		// TODO: the old data may be leaked when no GC is used (ref-counting?)
		if a.flags.has(.noslices) {
			unsafe {
				free(a.data)
			}
		}
	}
	a.data = new_data
	a.offset = 0
	a.cap = cap
}

// repeat returns a new array with the given array elements repeated given times.
// `cgen` will replace this with an apropriate call to `repeat_to_depth()`
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
[direct_array_access; unsafe]
pub fn (a array) repeat_to_depth(count int, depth int) array {
	if count < 0 {
		panic('array.repeat: count is negative: $count')
	}
	mut size := u64(count) * u64(a.len) * u64(a.element_size)
	if size == 0 {
		size = u64(a.element_size)
	}
	arr := array{
		element_size: a.element_size
		data: vcalloc(size)
		len: count * a.len
		cap: count * a.len
	}
	if a.len > 0 {
		a_total_size := u64(a.len) * u64(a.element_size)
		arr_step_size := u64(a.len) * u64(arr.element_size)
		mut eptr := &u8(arr.data)
		unsafe {
			for _ in 0 .. count {
				if depth > 0 {
					ary_clone := a.clone_to_depth(depth)
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
	$if !no_bounds_checking ? {
		if i < 0 || i > a.len {
			panic('array.insert: index out of range (i == $i, a.len == $a.len)')
		}
	}
	if a.len >= a.cap {
		a.ensure_cap(a.len + 1)
	}
	unsafe {
		vmemmove(a.get_unsafe(i + 1), a.get_unsafe(i), u64((a.len - i)) * u64(a.element_size))
		a.set_unsafe(i, val)
	}
	a.len++
}

// insert_many is used internally to implement inserting many values
// into an the array beginning at `i`.
[unsafe]
fn (mut a array) insert_many(i int, val voidptr, size int) {
	$if !no_bounds_checking ? {
		if i < 0 || i > a.len {
			panic('array.insert_many: index out of range (i == $i, a.len == $a.len)')
		}
	}
	a.ensure_cap(a.len + size)
	elem_size := a.element_size
	unsafe {
		iptr := a.get_unsafe(i)
		vmemmove(a.get_unsafe(i + size), iptr, u64(a.len - i) * u64(elem_size))
		vmemcpy(iptr, val, u64(size) * u64(elem_size))
	}
	a.len += size
}

// prepend prepends one or more elements to an array.
// It is shorthand for `.insert(0, val)`
pub fn (mut a array) prepend(val voidptr) {
	a.insert(0, val)
}

// prepend_many prepends another array to this array.
// NOTE: `.prepend` is probably all you need.
// NOTE: This code is never called in all of vlib
[unsafe]
fn (mut a array) prepend_many(val voidptr, size int) {
	unsafe { a.insert_many(0, val, size) }
}

// delete deletes array element at index `i`.
// This is exactly the same as calling `.delete_many(i, 1)`.
// NOTE: This function does NOT operate in-place. Internally, it
// creates a copy of the array, skipping over the element at `i`,
// and then points the original variable to the new memory location.
//
// Example:
// ```v
// mut a := ['0', '1', '2', '3', '4', '5']
// a.delete(1) // a is now ['0', '2', '3', '4', '5']
// ```
pub fn (mut a array) delete(i int) {
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
// b := a[..9] // creates a `slice` of `a`, not a clone
// a.delete_many(4, 3) // replaces `a` with a modified clone
// dump(a) // a: [1, 2, 3, 4, 8, 9] // `a` is now different
// dump(b) // b: [1, 2, 3, 4, 5, 6, 7, 8, 9] // `b` is still the same
// ```
pub fn (mut a array) delete_many(i int, size int) {
	$if !no_bounds_checking ? {
		if i < 0 || i + size > a.len {
			endidx := if size > 1 { '..${i + size}' } else { '' }
			panic('array.delete: index out of range (i == $i$endidx, a.len == $a.len)')
		}
	}
	if a.flags.all(.noshrink | .noslices) {
		unsafe {
			vmemmove(&u8(a.data) + u64(i) * u64(a.element_size), &u8(a.data) + u64(i +
				size) * u64(a.element_size), u64(a.len - i - size) * u64(a.element_size))
		}
		a.len -= size
		return
	}
	// Note: if a is [12,34], a.len = 2, a.delete(0)
	// should move (2-0-1) elements = 1 element (the 34) forward
	old_data := a.data
	new_size := a.len - size
	new_cap := if new_size == 0 { 1 } else { new_size }
	a.data = vcalloc(u64(new_cap) * u64(a.element_size))
	unsafe { vmemcpy(a.data, old_data, u64(i) * u64(a.element_size)) }
	unsafe {
		vmemcpy(&u8(a.data) + u64(i) * u64(a.element_size), &u8(old_data) + u64(i +
			size) * u64(a.element_size), u64(a.len - i - size) * u64(a.element_size))
	}
	if a.flags.has(.noslices) {
		unsafe {
			free(old_data)
		}
	}
	a.len = new_size
	a.cap = new_cap
}

// clear clears the array without deallocating the allocated data.
// It does it by setting the array length to `0`
// Example: a.clear() // `a.len` is now 0
pub fn (mut a array) clear() {
	a.len = 0
}

// trim trims the array length to `index` without modifying the allocated data.
// If `index` is greater than `len` nothing will be changed.
// Example: a.trim(3) // `a.len` is now <= 3
pub fn (mut a array) trim(index int) {
	if index < a.len {
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
[inline; unsafe]
fn (a array) get_unsafe(i int) voidptr {
	unsafe {
		return &u8(a.data) + u64(i) * u64(a.element_size)
	}
}

// Private function. Used to implement array[] operator.
fn (a array) get(i int) voidptr {
	$if !no_bounds_checking ? {
		if i < 0 || i >= a.len {
			panic('array.get: index out of range (i == $i, a.len == $a.len)')
		}
	}
	unsafe {
		return &u8(a.data) + u64(i) * u64(a.element_size)
	}
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

// first returns the first element of the `array`.
// If the `array` is empty, this will panic.
// However, `a[0]` returns an error object
// so it can be handled with an `or` block.
pub fn (a array) first() voidptr {
	$if !no_bounds_checking ? {
		if a.len == 0 {
			panic('array.first: array is empty')
		}
	}
	return a.data
}

// last returns the last element of the `array`.
// If the `array` is empty, this will panic.
pub fn (a array) last() voidptr {
	$if !no_bounds_checking ? {
		if a.len == 0 {
			panic('array.last: array is empty')
		}
	}
	unsafe {
		return &u8(a.data) + u64(a.len - 1) * u64(a.element_size)
	}
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
// b := a[..9] // creates a "view" into the same memory
// c := a.pop() // c == 9
// a[1] = 5
// dump(a) // a: [1, 5, 3, 4, 5, 6, 7, 8]
// dump(b) // b: [1, 5, 3, 4, 5, 6, 7, 8, 9]
// ```
pub fn (mut a array) pop() voidptr {
	// in a sense, this is the opposite of `a << x`
	$if !no_bounds_checking ? {
		if a.len == 0 {
			panic('array.pop: array is empty')
		}
	}
	new_len := a.len - 1
	last_elem := unsafe { &u8(a.data) + u64(new_len) * u64(a.element_size) }
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
	// copy pasting code for performance
	$if !no_bounds_checking ? {
		if a.len == 0 {
			panic('array.pop: array is empty')
		}
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
	// TODO: integrate reference counting
	// a.flags.clear(.noslices)
	offset := u64(start) * u64(a.element_size)
	data := unsafe { &u8(a.data) + offset }
	l := end - start
	res := array{
		element_size: a.element_size
		data: data
		offset: a.offset + int(offset) // TODO: offset should become 64bit
		len: l
		cap: l
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
	// a.flags.clear(.noslices)
	mut end := _end
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
			data: a.data
			offset: 0
			len: 0
			cap: 0
		}
		return res
	}

	offset := u64(start) * u64(a.element_size)
	data := unsafe { &u8(a.data) + offset }
	l := end - start
	res := array{
		element_size: a.element_size
		data: data
		offset: a.offset + int(offset) // TODO: offset should be 64bit
		len: l
		cap: l
	}
	return res
}

// used internally for [2..4]
fn (a array) slice2(start int, _end int, end_max bool) array {
	end := if end_max { a.len } else { _end }
	return a.slice(start, end)
}

// clone_static_to_depth() returns an independent copy of a given array.
// Unlike `clone_to_depth()` it has a value receiver and is used internally
// for slice-clone expressions like `a[2..4].clone()` and in -autofree generated code.
fn (a array) clone_static_to_depth(depth int) array {
	return unsafe { a.clone_to_depth(depth) }
}

// clone returns an independent copy of a given array.
// this will be overwritten by `cgen` with an apropriate call to `.clone_to_depth()`
// However the `checker` needs it here.
pub fn (a &array) clone() array {
	return unsafe { a.clone_to_depth(0) }
}

// recursively clone given array - `unsafe` when called directly because depth is not checked
[unsafe]
pub fn (a &array) clone_to_depth(depth int) array {
	mut size := u64(a.cap) * u64(a.element_size)
	if size == 0 {
		size++
	}
	mut arr := array{
		element_size: a.element_size
		data: vcalloc(size)
		len: a.len
		cap: a.cap
	}
	// Recursively clone-generated elements if array element is array type
	if depth > 0 && a.element_size == sizeof(array) && a.len >= 0 && a.cap >= a.len {
		for i in 0 .. a.len {
			ar := array{}
			unsafe { vmemcpy(&ar, a.get_unsafe(i), int(sizeof(array))) }
			ar_clone := unsafe { ar.clone_to_depth(depth - 1) }
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

// we manually inline this for single operations for performance without -prod
[inline; unsafe]
fn (mut a array) set_unsafe(i int, val voidptr) {
	unsafe { vmemcpy(&u8(a.data) + u64(a.element_size) * u64(i), val, a.element_size) }
}

// Private function. Used to implement assignment to the array element.
fn (mut a array) set(i int, val voidptr) {
	$if !no_bounds_checking ? {
		if i < 0 || i >= a.len {
			panic('array.set: index out of range (i == $i, a.len == $a.len)')
		}
	}
	unsafe { vmemcpy(&u8(a.data) + u64(a.element_size) * u64(i), val, a.element_size) }
}

fn (mut a array) push(val voidptr) {
	if a.len >= a.cap {
		a.ensure_cap(a.len + 1)
	}
	unsafe { vmemcpy(&u8(a.data) + u64(a.element_size) * u64(a.len), val, a.element_size) }
	a.len++
}

// push_many implements the functionality for pushing another array.
// `val` is array.data and user facing usage is `a << [1,2,3]`
[unsafe]
pub fn (mut a3 array) push_many(val voidptr, size int) {
	if size <= 0 || isnil(val) {
		return
	}
	a3.ensure_cap(a3.len + size)
	if a3.data == val && a3.data != 0 {
		// handle `arr << arr`
		copy := a3.clone()
		unsafe {
			vmemcpy(&u8(a3.data) + u64(a3.element_size) * u64(a3.len), copy.data, u64(a3.element_size) * u64(size))
		}
	} else {
		if a3.data != 0 && val != 0 {
			unsafe { vmemcpy(&u8(a3.data) + u64(a3.element_size) * u64(a3.len), val, u64(a3.element_size) * u64(size)) }
		}
	}
	a3.len += size
}

// reverse_in_place reverses existing array data, modifying original array.
pub fn (mut a array) reverse_in_place() {
	if a.len < 2 {
		return
	}
	unsafe {
		mut tmp_value := malloc(a.element_size)
		for i in 0 .. a.len / 2 {
			vmemcpy(tmp_value, &u8(a.data) + u64(i) * u64(a.element_size), a.element_size)
			vmemcpy(&u8(a.data) + u64(i) * u64(a.element_size), &u8(a.data) +
				u64(a.len - 1 - i) * u64(a.element_size), a.element_size)
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
	mut arr := array{
		element_size: a.element_size
		data: vcalloc(u64(a.cap) * u64(a.element_size))
		len: a.len
		cap: a.cap
	}
	for i in 0 .. a.len {
		unsafe { arr.set_unsafe(i, a.get_unsafe(a.len - 1 - i)) }
	}
	return arr
}

// free frees all memory occupied by the array.
[unsafe]
pub fn (a &array) free() {
	$if prealloc {
		return
	}
	// if a.is_slice {
	// return
	// }
	mblock_ptr := &u8(u64(a.data) - u64(a.offset))
	unsafe { free(mblock_ptr) }
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
// Example: array.filter(it < 5) // create an array of elements less than 5
// Example: array.filter(it % 2 == 1) // create an array of only odd elements
// Example: array.filter(it.name[0] == `A`) // create an array of elements whose `name` field starts with 'A'
pub fn (a array) filter(predicate fn (voidptr) bool) array

// any tests whether at least one element in the array passes the test.
// Ignore the function signature. `any` does not take an actual callback. Rather, it
// takes an `it` expression.
// It returns `true` if it finds an element passing the test. Otherwise,
// it returns `false`. It doesn't modify the array.
//
// Example: array.any(it % 2 == 1) // will return true if any element is odd
// Example: array.any(it.name == 'Bob') // will yield `true` if any element has `.name == 'Bob'`
pub fn (a array) any(predicate fn (voidptr) bool) bool

// all tests whether all elements in the array pass the test.
// Ignore the function signature. `all` does not take an actual callback. Rather, it
// takes an `it` expression.
// It returns `false` if any element fails the test. Otherwise,
// it returns `true`. It doesn't modify the array.
//
// Example: array.all(it % 2 == 1) // will return true if every element is odd
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
//
// Example: array.sort() // will sort the array in ascending order
// Example: array.sort(b < a) // will sort the array in decending order
// Example: array.sort(b.name < a.name) // will sort descending by the .name field
pub fn (mut a array) sort(callback fn (voidptr, voidptr) int)

// sort_with_compare sorts the array in-place using the results of the
// given function to determine sort order.
//
// The function should return one of three values:
// - `-1` when `a` should come before `b` ( `a < b` )
// - `1`  when `b` should come before `a` ( `b < a` )
// - `0`  when the order cannot be determined ( `a == b` )
//
// Example:
// ```v
// fn main() {
// 	mut a := ['hi', '1', '5', '3']
// 	a.sort_with_compare(fn (a &string, b &string) int {
// 		if a < b {
// 			return -1
// 		}
// 		if a > b {
// 			return 1
// 		}
// 		return 0
// 	})
// 	assert a == ['1', '3', '5', 'hi']
// }
// ```
pub fn (mut a array) sort_with_compare(callback fn (voidptr, voidptr) int) {
	$if freestanding {
		panic('sort does not work with -freestanding')
	} $else {
		unsafe { vqsort(a.data, usize(a.len), usize(a.element_size), callback) }
	}
}

// contains determines whether an array includes a certain value among its elements
// It will return `true` if the array contains an element with this value.
// It is similar to `.any` but does not take an `it` expression.
//
// Example: [1, 2, 3].contains(4) == false
pub fn (a array) contains(value voidptr) bool

// index returns the first index at which a given element can be found in the array
// or `-1` if the value is not found.
pub fn (a array) index(value voidptr) int

[unsafe]
pub fn (mut a []string) free() {
	$if prealloc {
		return
	}
	for s in a {
		unsafe { s.free() }
	}
	unsafe { (&array(&a)).free() }
}

// The following functions are type-specific functions that apply
// to arrays of different types in different ways.

// str returns a string representation of an array of strings
// Example: ['a', 'b', 'c'].str() // => "['a', 'b', 'c']".
[manualfree]
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

// hex returns a string with the hexadecimal representation
// of the byte elements of the array.
pub fn (b []u8) hex() string {
	mut hex := unsafe { malloc_noscan(u64(b.len) * 2 + 1) }
	mut dst_i := 0
	for i in b {
		n0 := i >> 4
		unsafe {
			hex[dst_i] = if n0 < 10 { n0 + `0` } else { n0 + u8(87) }
			dst_i++
		}
		n1 := i & 0xF
		unsafe {
			hex[dst_i] = if n1 < 10 { n1 + `0` } else { n1 + u8(87) }
			dst_i++
		}
	}
	unsafe {
		hex[dst_i] = 0
		return tos(hex, dst_i)
	}
}

// copy copies the `src` byte array elements to the `dst` byte array.
// The number of the elements copied is the minimum of the length of both arrays.
// Returns the number of elements copied.
// NOTE: This is not an `array` method. It is a function that takes two arrays of bytes.
// See also: `arrays.copy`.
pub fn copy(mut dst []u8, src []u8) int {
	min := if dst.len < src.len { dst.len } else { src.len }
	if min > 0 {
		unsafe { vmemmove(&u8(dst.data), src.data, min) }
	}
	return min
}

// reduce executes a given reducer function on each element of the array,
// resulting in a single output value.
// NOTE: It exists as a method on `[]int` types only.
// See also `arrays.fold`.
pub fn (a []int) reduce(iter fn (int, int) int, accum_start int) int {
	mut accum_ := accum_start
	for i in a {
		accum_ = iter(accum_, i)
	}
	return accum_
}

// grow_cap grows the array's capacity by `amount` elements.
// Internally, it does this by copying the entire array to
// a new memory location (creating a clone).
pub fn (mut a array) grow_cap(amount int) {
	a.ensure_cap(a.cap + amount)
}

// grow_len ensures that an array has a.len + amount of length
// Internally, it does this by copying the entire array to
// a new memory location (creating a clone) unless the array.cap
// is already large enough.
[unsafe]
pub fn (mut a array) grow_len(amount int) {
	a.ensure_cap(a.len + amount)
	a.len += amount
}

// pointers returns a new array, where each element
// is the address of the corresponding element in the array.
[unsafe]
pub fn (a array) pointers() []voidptr {
	mut res := []voidptr{}
	for i in 0 .. a.len {
		unsafe { res << a.get_unsafe(i) }
	}
	return res
}

// vbytes on`voidptr` makes a V []u8 structure from a C style memory buffer.
// NOTE: the data is reused, NOT copied!
[unsafe]
pub fn (data voidptr) vbytes(len int) []u8 {
	res := array{
		element_size: 1
		data: data
		len: len
		cap: len
	}
	return res
}

// vbytes on `&u8` makes a V []u8 structure from a C style memory buffer.
// NOTE: the data is reused, NOT copied!
[unsafe]
pub fn (data &u8) vbytes(len int) []u8 {
	return unsafe { voidptr(data).vbytes(len) }
}
