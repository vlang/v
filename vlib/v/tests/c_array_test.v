import arrays

#insert "@VEXEROOT/vlib/v/tests/c_array_test.c"

fn C.gen_c_array(size int) voidptr

fn C.gen_c_int_array(size int) voidptr

fn test_carray_to_varray() {
	size := 10
	mut c_array := C.gen_c_array(size)
	v_u8_array := unsafe { arrays.carray_to_varray<u8>(c_array, size) }
	unsafe { C.free(c_array) }
	assert v_u8_array.len == size
	for i, elem in v_u8_array {
		assert elem == i
	}

	c_int_array := C.gen_c_int_array(size)
	v_int_array := unsafe { arrays.carray_to_varray<int>(c_int_array, size) }
	unsafe { C.free(c_int_array) }
	assert v_int_array.len == size
	for i, elem in v_int_array {
		assert elem == i
	}
}
