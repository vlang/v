#insert "@VEXEROOT/vlib/v/tests/c_array_test.c"

fn C.gen_c_array(size int) voidptr

fn test_carray_to_varray() {
	size := 10
	mut c_array := C.gen_c_array(size)
	v_array := unsafe { carray_to_varray(c_array, size) }
	assert v_array.len == size
	for i, elem in v_array {
		assert elem == i
	}
}
