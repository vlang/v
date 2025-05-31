fn test_print_address_of_reference_base_type() {
	a1 := 22
	println('${&a1:p}')
	a2 := 22.22
	println('${&a2:p}')
	a3 := `a`
	println('${&a3:p}')
	a4 := 'hello'
	println('${&a4:p}')
	a5 := true
	println('${&a5:p}')

	assert true
}
