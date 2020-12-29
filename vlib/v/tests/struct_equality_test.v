struct User {
	name string
	age  int
}

fn test_struct_equality() {
	mut usr1 := User{'sanath', 28}
	mut usr2 := User{'sanath', 28}
	if usr1 == usr2 {
		println('Same User')
	} else {
		println('Not same User')
	}
	assert usr1 == usr2
}
