fn test_string_array_of_ref_type() {
	a := unsafe { []&int{len: 2} }
	println(a)
	assert '${a}' == '[nil, nil]'
}
