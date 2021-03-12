struct Cat {
	x int = 123
}

interface Adoptable {
}

fn test_casting_to_interface() {
	cat := Cat{}
	a := Adoptable(cat)
	if a is Cat {
		assert typeof(a).name == '&Cat'
		assert a.x == 123
		return
	}
	assert false
}
