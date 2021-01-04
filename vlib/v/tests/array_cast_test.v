fn test_array_cast() {
	mut keys := ['']
	unsafe {
		// untyped
		arr := *&array(&keys)
		// retype
		mut p := &[]string(&arr)
		(*p)[0] = 'hi'
		assert *p == ['hi']
	}
	assert keys[0] == 'hi'
}

fn test_int() {
	mut arr := [2.3,3]
	unsafe {
		p := &[]f64(&arr)
		assert *p == arr
	}
}
