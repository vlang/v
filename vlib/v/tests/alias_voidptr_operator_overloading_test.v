type AF_ARRAY = voidptr

fn (a AF_ARRAY) add(b AF_ARRAY) AF_ARRAY {
	mut y := AF_ARRAY(0)
	return y
}

fn (a AF_ARRAY) mul(b AF_ARRAY) AF_ARRAY {
	mut y := AF_ARRAY(0)
	return y
}

fn (a AF_ARRAY) + (b AF_ARRAY) AF_ARRAY {
	return a.add(b)
}

fn (a AF_ARRAY) * (b AF_ARRAY) AF_ARRAY {
	return a.mul(b)
}

fn test_alias_voidptr_operator_overloading() {
	a := AF_ARRAY(0)
	b := AF_ARRAY(0)

	c := a + b
	y := a * a

	assert c == a.add(b)
	assert y == a.mul(a)
}
