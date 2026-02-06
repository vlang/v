fn test_bool_assign_operator() {
	mut flag := true
	flag ||= false
	assert flag == true

	flag &&= false
	assert flag == false
}

type Bool = bool

fn test_alias_bool_assign_operator() {
	mut flag := Bool(true)
	flag = flag || false
	assert flag == true

	flag ||= false
	assert flag == true

	flag &&= false
	assert flag == false
}
