struct Company {
	name        string
	description string
}

fn (lhs Company) == (rhs Company) bool {
	return lhs.name == rhs.name
}

struct User {
	name    string
	age     int
	company Company
}

fn test_struct_equality() {
	mut usr1 := User{'sanath', 28, Company{'awesome company', 'we are awesome'}}
	mut usr2 := User{'sanath', 28, Company{'awesome company', 'we are awesome too'}}
	if usr1 == usr2 {
		println('Same User')
	} else {
		println('Not same User')
	}
	assert usr1 == usr2
}
