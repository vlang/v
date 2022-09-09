fn test_for_in_ref_val_ref_arr() {
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

fn test_for_in_ref_val_ref_arr_ident() {
	arr_ := [1, 2, 3, 4, 5]
	arr := &arr_
	mut rets := []&int{}
	mut expects := unsafe { []&int{len: 5, init: &arr_[it]} }

	for val in arr {
		rets << val
	}

	for i, val in arr {
		assert voidptr(val) == voidptr(rets[i])
	}
	assert rets == expects
}
