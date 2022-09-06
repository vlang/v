fn test_for_in_ref_array_pointer() {
	arr := [1, 2, 3, 4, 5]
	mut rets := []&int{}
	mut expects := unsafe { []&int{len: 5, init: &arr[it]} }

	for val in &arr {
		println(val)
		rets << val
	}

	for i, val in &arr {
		assert voidptr(val) == voidptr(rets[i])
	}
	assert rets == expects
}
