interface Animal {
	breed string
}

struct Cat {
	breed string
}

struct Dog {
	breed string
}

fn use_interface(a Animal) {
	assert a.breed in ['Persian', 'Labrador']
	if a is Cat {
		assert a.breed == 'Persian'
	} else {
		assert a.breed == 'Labrador'
	}
}

fn test_interface_fields() {
	c := Cat{'Persian'}
	d := Dog{'Persian'}
	use_interface(c)
	use_interface(d)
}
