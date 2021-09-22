struct User {
	name string
	num  int
}

fn (u User) == (u1 User) bool {
	return u.name == u1.name
}

fn test_eq_op() {
	u1 := User{'Joe', 23}
	u2 := User{'Joe', 24}
	assert u1 == u2
	assert (u1 != u2) == false
}
