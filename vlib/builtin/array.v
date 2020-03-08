// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

import strings

pub struct array {
pub:
// Using a void pointer allows to implement arrays without generics and without generating
// extra code for every type.
	data         voidptr
	len          int
	cap          int
	element_size int
}

/*
struct Foo {
	a []string
	b [][]string
}
*/

// Internal function, used by V (`nums := []int`)
fn new_array(mylen int, cap int, elm_size int) array {
	cap_ := if cap == 0 { 1 } else { cap }
	arr := array{
		len: mylen
		cap: cap_
		element_size: elm_size
		data: vcalloc(cap_ * elm_size)
	}
	return arr
}

// TODO
pub fn make(len int, cap int, elm_size int) array {
	return new_array(len, cap, elm_size)
}

// Private function, used by V (`nums := [1, 2, 3]`)
fn new_array_from_c_array(len, cap, elm_size int, c_array voidptr) array {
	cap_ := if cap == 0 { 1 } else { cap }
	arr := array{
		len: len
		cap: cap
		element_size: elm_size
		data: vcalloc(cap_ * elm_size)
	}
	// TODO Write all memory functions (like memcpy) in V
	C.memcpy(arr.data, c_array, len * elm_size)
	return arr
}

// Private function, used by V (`nums := [1, 2, 3] !`)
fn new_array_from_c_array_no_alloc(len, cap, elm_size int, c_array voidptr) array {
	arr := array{
		len: len
		cap: cap
		element_size: elm_size
		data: c_array
	}
	return arr
}

// Private function. Doubles array capacity if needed
fn (a mut array) ensure_cap(required int) {
	if required <= a.cap {
		return
	}
	mut cap := if a.cap == 0 { 2 } else { a.cap * 2 }
	for required > cap {
		cap *= 2
	}
	if a.cap == 0 {
		a.data = vcalloc(cap * a.element_size)
	}
	else {
		a.data = C.realloc(a.data, cap * a.element_size)
	}
	a.cap = cap
}

// repeat returns new array with the given array elements repeated given times.
pub fn (a array) repeat(count int) array {
	if count < 0 {
		panic('array.repeat: count is negative: $count')
	}
	mut size := count * a.len * a.element_size
	if size == 0 {
		size = a.element_size
	}
	arr := array{
		len: count * a.len
		cap: count * a.len
		element_size: a.element_size
		data: vcalloc(size)
	}
	for i in 0..count {
		C.memcpy(arr.data + i * a.len * a.element_size, a.data, a.len * a.element_size)
	}
	return arr
}

// array.sort sorts array in-place using given `compare` function as comparator
pub fn (a mut array) sort_with_compare(compare voidptr) {
	C.qsort(a.data, a.len, a.element_size, compare)
}

// TODO array.insert is broken
// Cannot pass literal or primitive type as it cannot be cast to voidptr.
// In the current state only that would work:
// i := 3
// a.insert(0, &i)
// ----------------------------
pub fn (a mut array) insert(i int, val voidptr) {
	$if !no_bounds_checking? {
		if i < 0 || i > a.len {
			panic('array.insert: index out of range (i == $i, a.len == $a.len)')
		}
	}
	a.ensure_cap(a.len + 1)
	size := a.element_size
	C.memmove(a.data + (i + 1) * size, a.data + i * size, (a.len - i) * size)
	C.memcpy(a.data + i * size, val, size)
	a.len++
}

// TODO array.prepend is broken
// It depends on array.insert
// -----------------------------
pub fn (a mut array) prepend(val voidptr) {
	a.insert(0, val)
}

// array.delete deletes array element at the given index
pub fn (a mut array) delete(i int) {
	$if !no_bounds_checking? {
		if i < 0 || i >= a.len {
			panic('array.delete: index out of range (i == $i, a.len == $a.len)')
		}
	}
	size := a.element_size
	C.memmove(a.data + i * size, a.data + (i + 1) * size, (a.len - i) * size)
	a.len--
}

// clears the array without deallocating the allocated data
pub fn (a mut array) clear() {
	a.len = 0
}

// trims the array length to "index" without modifying the allocated data. If "index" is greater
// than len nothing will be changed
pub fn (a mut array) trim(index int) {
	if index < a.len {
		a.len = index
	}
}

// Private function. Used to implement array[] operator
fn (a array) get(i int) voidptr {
	$if !no_bounds_checking? {
		if i < 0 || i >= a.len {
			panic('array.get: index out of range (i == $i, a.len == $a.len)')
		}
	}
	return a.data + i * a.element_size
}

// array.first returns the first element of the array
pub fn (a array) first() voidptr {
	$if !no_bounds_checking? {
		if a.len == 0 {
			panic('array.first: array is empty')
		}
	}
	return a.data + 0
}

// array.last returns the last element of the array
pub fn (a array) last() voidptr {
	$if !no_bounds_checking? {
		if a.len == 0 {
			panic('array.last: array is empty')
		}
	}
	return a.data + (a.len - 1) * a.element_size
}

/*
// array.left returns a new array using the same buffer as the given array
// with the first `n` elements of the given array.
fn (a array) left(n int) array {
//	$if !no_bounds_checking? {
//		if n < 0 {
//			panic('array.left: index is negative (n == $n)')
//		}
//	}
	if n >= a.len {
		return a.slice(0, a.len)
	}
	return a.slice(0, n)
}

// array.right returns an array using same buffer as the given array
// but starting with the element of the given array beyond the index `n`.
// If `n` is bigger or equal to the length of the given array,
// returns an empty array of the same type as the given array.
fn (a array) right(n int) array {
//	$if !no_bounds_checking? {
//		if n < 0 {
//			panic('array.right: index is negative (n == $n)')
//		}
//	}
	if n >= a.len {
		return new_array(0, 0, a.element_size)
	}
	return a.slice(n, a.len)
}
*/

// array.slice returns an array using the same buffer as original array
// but starting from the `start` element and ending with the element before
// the `end` element of the original array with the length and capacity
// set to the number of the elements in the slice.
fn (a array) slice(start, _end int) array {
	mut end := _end
	$if !no_bounds_checking? {
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
	l := end - start
	res := array{
		element_size: a.element_size
		data: a.data + start * a.element_size
		len: l
		cap: l
	}
	return res
}

// used internally for [2..4]
fn (a array) slice2(start, _end int, end_max bool) array {
	end := if end_max { a.len } else { _end }
	return a.slice(start, end)
}

// array.clone returns an independent copy of a given array
pub fn (a array) clone() array {
	mut size := a.cap * a.element_size
	if size == 0 {
		size++
	}
	arr := array{
		len: a.len
		cap: a.cap
		element_size: a.element_size
		data: vcalloc(size)
	}
	C.memcpy(arr.data, a.data, a.cap * a.element_size)
	return arr
}

fn (a array) slice_clone(start, _end int) array {
	mut end := _end
	$if !no_bounds_checking? {
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
	l := end - start
	res := array{
		element_size: a.element_size
		data: a.data + start * a.element_size
		len: l
		cap: l
	}
	return res.clone()
}

// Private function. Used to implement assigment to the array element.
fn (a mut array) set(i int, val voidptr) {
	$if !no_bounds_checking? {
		if i < 0 || i >= a.len {
			panic('array.set: index out of range (i == $i, a.len == $a.len)')
		}
	}
	C.memcpy(a.data + a.element_size * i, val, a.element_size)
}

fn (a mut array) push(val voidptr) {
	a.ensure_cap(a.len + 1)
	C.memcpy(a.data + a.element_size * a.len, val, a.element_size)
	a.len++
}

// `val` is array.data
// TODO make private, right now it's used by strings.Builder
pub fn (a mut array) push_many(val voidptr, size int) {
	// handle `arr << arr`
	if a.data == val {
		copy := a.clone()
		a.ensure_cap(a.len + size)
		//C.memcpy(a.data, copy.data, copy.element_size * copy.len)
		C.memcpy(a.data + a.element_size * a.len, copy.data, a.element_size * size)
	} else {
		a.ensure_cap(a.len + size)
		C.memcpy(a.data + a.element_size * a.len, val, a.element_size * size)
	}
	a.len += size
}

// array.reverse returns a new array with the elements of
// the original array in reverse order.
pub fn (a array) reverse() array {
	if a.len < 2 {
		return a
	}
	arr := array{
		len: a.len
		cap: a.cap
		element_size: a.element_size
		data: vcalloc(a.cap * a.element_size)
	}
	for i in 0..a.len {
		C.memcpy(arr.data + i * arr.element_size, &a[a.len - 1 - i], arr.element_size)
	}
	return arr
}

// pub fn (a []int) free() {
[unsafe_fn]
pub fn (a array) free() {
	// if a.is_slice {
	// return
	// }
	C.free(a.data)
}

// []string.str returns a string representation of the array of strings
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

// []bool.str returns a string representation of the array of bools
// "[true, true, false]"
pub fn (a []bool) str() string {
	mut sb := strings.new_builder(a.len * 3)
	sb.write('[')
	for i in 0..a.len {
		val := a[i]
		if val {
			sb.write('true')
		}
		else {
			sb.write('false')
		}
		if i < a.len - 1 {
			sb.write(', ')
		}
	}
	sb.write(']')
	return sb.str()
}

// []byte.hex returns a string with the hexadecimal representation
// of the byte elements of the array
pub fn (b []byte) hex() string {
	mut hex := malloc(b.len * 2 + 1)
	mut ptr := &hex[0]
	for i in 0..b.len {
		// QTODO
		ptr += C.sprintf(ptr, '%02x', b[i])
	}
	//return hex as string
	return string(hex)
}

// copy copies the `src` byte array elements to the `dst` byte array.
// The number of the elements copied is the minimum of the length of both arrays.
// Returns the number of elements copied.
// TODO: implement for all types
pub fn copy(dst, src []byte) int {
	if dst.len > 0 && src.len > 0 {
		mut min := 0
		min = if dst.len < src.len { dst.len } else { src.len }
		C.memcpy(dst.data, src[..min].data, dst.element_size * min)
		return min
	}
	return 0
}

// Private function. Comparator for int type.
fn compare_ints(a, b &int) int {
	if *a < *b {
		return -1
	}
	if *a > *b {
		return 1
	}
	return 0
}

// []int.sort sorts array of int in place in ascending order.
pub fn (a mut []int) sort() {
	a.sort_with_compare(compare_ints)
}

// []string.index returns the index of the first element equal to the given value,
// or -1 if the value is not found in the array.
pub fn (a []string) index(v string) int {
	for i in 0..a.len {
		if a[i] == v {
			return i
		}
	}
	return -1
}

// []int.index returns the index of the first element equal to the given value,
// or -1 if the value is not found in the array.
pub fn (a []int) index(v int) int {
	for i in 0..a.len {
		if a[i] == v {
			return i
		}
	}
	return -1
}

// []byte.index returns the index of the first element equal to the given value,
// or -1 if the value is not found in the array.
pub fn (a []byte) index(v byte) int {
	for i in 0..a.len {
		if a[i] == v {
			return i
		}
	}
	return -1
}

// []char.index returns the index of the first element equal to the given value,
// or -1 if the value is not found in the array.
// TODO is `char` type yet in the language?
pub fn (a []char) index(v char) int {
	for i in 0..a.len {
		if a[i] == v {
			return i
		}
	}
	return -1
}

// []int.reduce executes a given reducer function on each element of the array,
// resulting in a single output value.
pub fn (a []int) reduce(iter fn(accum, curr int)int, accum_start int) int {
	mut _accum := accum_start
	for i in a {
		_accum = iter(_accum, i)
	}

	return _accum
}

// array_eq<T> checks if two arrays contain all the same elements in the same order.
// []int == []int (also for: i64, f32, f64, byte, string)
/*
fn array_eq<T>(a1, a2 []T) bool {
	if a1.len != a2.len {
		return false
	}
	for i in 0..a1.len {
		if a1[i] != a2[i] {
			return false
		}
	}
	return true
}

pub fn (a []int) eq(a2 []int) bool {
	return array_eq(a, a2)
}

pub fn (a []i64) eq(a2 []i64) bool {
	return array_eq(a, a2)
}


pub fn (a []byte) eq(a2 []byte) bool {
	return array_eq(a, a2)
}

pub fn (a []f32) eq(a2 []f32) bool {
	return array_eq(a, a2)
}
*/

pub fn (a1 []string) eq(a2 []string) bool {
	//return array_eq(a, a2)
	if a1.len != a2.len {
		return false
	}
	for i in 0..a1.len {
		if a1[i] != a2[i] {
			return false
		}
	}
	return true
}

// compare_i64 for []f64 sort_with_compare()
// sort []i64 with quicksort
// usage :
// mut x := [i64(100),10,70,28,92]
// x.sort_with_compare(compare_i64)
// println(x)     // Sorted i64 Array
// output:
// [10, 28, 70, 92, 100]
pub fn compare_i64(a, b &i64) int {
	if *a < *b {
		return -1
	}
	if *a > *b {
		return 1
	}
	return 0
}

// compare_f64 for []f64 sort_with_compare()
// ref. compare_i64(...)
pub fn compare_f64(a, b &f64) int {
	if *a < *b {
		return -1
	}
	if *a > *b {
		return 1
	}
	return 0
}

// compare_f32 for []f32 sort_with_compare()
// ref. compare_i64(...)
pub fn compare_f32(a, b &f32) int {
	if *a < *b {
		return -1
	}
	if *a > *b {
		return 1
	}
	return 0
}

// a.pointers() returns a new array, where each element 
// is the address of the corresponding element in a.
pub fn (a array) pointers() []voidptr {
	mut res := []voidptr
	for i in 0..a.len {
		res << a.data + i * a.element_size
	}
	return res
}
