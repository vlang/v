fn test_array_cast() {
	mut keys := ['']
	unsafe {
		vp := voidptr(&keys)
		mut p := &[]string(vp)
		(*p)[0] = 'hi'
		assert *p == ['hi']
	}
	assert keys[0] == 'hi'
}

fn test_int() {
	mut arr := [2.3,3]
	unsafe {
		vp := voidptr(&arr)
		p := &[]f64(vp)
		assert *p == arr
	}
}
