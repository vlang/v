type Alias = u8

fn new_alias() Alias {
	return 0
}

fn (a Alias) add(b Alias) Alias {
	return new_alias()
}

fn (a Alias) mul(b Alias) Alias {
	return new_alias()
}

fn (a Alias) + (b Alias) Alias {
	return a.add(b)
}

fn (a Alias) * (b Alias) Alias {
	return a.mul(b)
}

fn test_alias_primitive_operator_overloading() {
	a := new_alias()
	b := new_alias()

	c := a + b
	d := a.add(b)
	assert typeof(c).name == 'Alias'
	assert typeof(d).name == 'Alias'

	e := a * b
	f := a.mul(b)
	assert typeof(e).name == 'Alias'
	assert typeof(f).name == 'Alias'
}

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

	assert typeof(c).name == 'AF_ARRAY'
	assert typeof(y).name == 'AF_ARRAY'
}
