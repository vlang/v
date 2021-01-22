interface Animal {
	breed string
}

struct Cat {
	padding int // ensures that the field offsets can be different
mut:
	breed   string
}

struct Dog {
	padding  Cat
	padding2 int
mut:
	breed    string
}

fn use_interface(mut a Animal) {
	assert a.breed in ['Persian', 'Labrador']
	if a is Cat {
		assert a.breed == 'Persian'
		a.breed = 'Siamese'
	} else {
		assert a.breed == 'Labrador'
		a.breed = 'Golden Retriever'
	}
	assert a.breed in ['Siamese', 'Golden Retriever']
	if a is Cat {
		assert a.breed == 'Siamese'
	} else {
		assert a.breed == 'Golden Retriever'
	}
	a.breed = 'what??'
	assert a.breed == 'what??'
}

fn test_interface_fields() {
	mut c := Cat{ breed: 'Persian'}
	mut d := Dog{ breed: 'Labrador'}
	use_interface(mut c)
	use_interface(mut d)
	assert c.breed == 'what??'
	assert d.breed == 'what??'
}
