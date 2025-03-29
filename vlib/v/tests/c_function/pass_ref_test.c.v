module main

#include "@VMODROOT/array.c"

enum Flag_bits {
	aaa = 0
	bbb = 1
	ccc = 2
}

pub type Array_t = C.array_t

@[typedef]
struct C.array_t {
pub mut:
	array_ptr  &&char
	array_len  int
	string_len int
}

pub type Struct_array = C.struct_array

@[typedef]
struct C.struct_array {
pub mut:
	arr [4]Array_t
	enu [2]Flag_bits
}

fn C.get_string_array() &&char
@[inline]
pub fn get_string_array() &&char {
	return C.get_string_array()
}

fn C.get_struct_array() Struct_array
@[inline]
pub fn get_struct_array() Struct_array {
	return C.get_struct_array()
}

fn C.array_string_free(&Array_t)

@[keep_args_alive]
fn C.set_struct_array(&Struct_array)
@[inline]
pub fn set_struct_array(param &Struct_array) {
	C.set_struct_array(param)
}

fn test_main() {
	struct_array := get_struct_array()

	set_struct_array(&Struct_array{})
	set_struct_array(&struct_array)

	C.array_string_free(&struct_array.arr[0])
}
