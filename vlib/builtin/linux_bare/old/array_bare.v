module builtin

pub struct array {
pub:
	data         voidptr
	len          int
	cap          int
	element_size int
}

// for now off the stack
fn new_array_from_c_array(len int, cap int, elm_size int, c_array voidptr) array {
	arr := array{
		len: len
		cap: cap
		element_size: elm_size
		data: c_array
	}
	return arr
}

// Private function. Used to implement array[] operator
fn (a array) get(i int) voidptr {
	if i < 0 || i >= a.len {
		panic('array.get: index out of range') // FIXME: (i == $i, a.len == $a.len)')
	}
	return a.data + i * a.element_size
}

// Private function. Used to implement assignment to the array element.
fn (mut a array) set(i int, val voidptr) {
	if i < 0 || i >= a.len {
		panic('array.set: index out of range') // FIXME: (i == $i, a.len == $a.len)')
	}
	mem_copy(a.data + a.element_size * i, val, a.element_size)
}

// array.repeat returns new array with the given array elements
// repeated `nr_repeat` times
pub fn (a array) repeat(nr_repeats int) array {
	assert nr_repeats >= 0

	arr := array{
		len: nr_repeats * a.len
		cap: nr_repeats * a.len
		element_size: a.element_size
		data: malloc(nr_repeats * a.len * a.element_size)
	}
	for i in 0 .. nr_repeats {
		mem_copy(arr.data + i * a.len * a.element_size, a.data, a.len * a.element_size)
	}
	return arr
}
