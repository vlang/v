// dummy placeholder for functions from `array_d_gcboehm_opt.v`
// that might be needed for compile time
// `$if gcboehm_opt ? { ... } $else { ... }`

module builtin

// this is needed in `string.v`
[inline]
fn __new_array_noscan(mylen int, cap int, elm_size int) array {
	return __new_array(mylen, cap, elm_size)
}

[inline]
fn __new_array_with_default_noscan(mylen int, cap int, elm_size int, val voidptr) array {
	return __new_array_with_default(mylen, cap, elm_size, val)
}

[inline]
fn __new_array_with_multi_default_noscan(mylen int, cap int, elm_size int, val voidptr) array {
	return __new_array_with_multi_default(mylen, cap, elm_size, val)
}

[inline]
fn __new_array_with_array_default_noscan(mylen int, cap int, elm_size int, val array, depth int) array {
	return __new_array_with_array_default(mylen, cap, elm_size, val, depth)
}

[inline]
fn new_array_from_c_array_noscan(len int, cap int, elm_size int, c_array voidptr) array {
	return new_array_from_c_array(len, cap, elm_size, c_array)
}

[inline]
fn (a array) repeat_to_depth_noscan(count int, depth int) array {
	return unsafe { a.repeat_to_depth(count, depth) }
}

[inline]
fn (mut a array) insert_noscan(i int, val voidptr) {
	a.insert(i, val)
}

[inline]
fn (mut a array) insert_many_noscan(i int, val voidptr, size int) {
	unsafe { a.insert_many(i, val, size) }
}

[inline]
fn (mut a array) prepend_noscan(val voidptr) {
	a.prepend_noscan(val)
}

[inline]
fn (mut a array) prepend_many_noscan(val voidptr, size int) {
	unsafe { a.prepend_many_noscan(val, size) }
}

[inline]
fn (mut a array) pop_noscan() voidptr {
	return a.pop()
}

[inline]
fn (a array) clone_static_to_depth_noscan(depth int) array {
	return unsafe { a.clone_static_to_depth(depth) }
}

[inline]
fn (a &array) clone_to_depth_noscan(depth int) array {
	return unsafe { a.clone_to_depth(depth) }
}

[inline]
fn (mut a array) push_noscan(val voidptr) {
	a.push(val)
}

[inline]
fn (mut a array) push_many_noscan(val voidptr, size int) {
	unsafe { a.push_many(val, size) }
}

[inline]
fn (a array) reverse_noscan() array {
	return a.reverse()
}

[inline]
fn (mut a array) grow_cap_noscan(amount int) {
	a.grow_cap(amount)
}

[inline]
fn (mut a array) grow_len_noscan(amount int) {
	unsafe { a.grow_len(amount) }
}
