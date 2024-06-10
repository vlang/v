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
	assert typeof(c).name == 'Alias'
}
