fn test_bool_assign_operator() {
	mut flag := true
	flag ||= false
	assert flag == true

	flag &&= false
	assert flag == false
}
