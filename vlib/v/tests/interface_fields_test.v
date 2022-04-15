interface Animal {
mut:
	breed string
}

struct Cat {
	padding int // ensures that the field offsets can be different
mut:
	breed string
}

struct Dog {
	padding  [256]u8
	padding2 int
mut:
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

fn mutate_interface(mut a Animal) {
	if a is Cat {
		a.breed = 'Siamese'
	} else {
		a.breed = 'Golden Retriever'
	}
	if a is Cat {
		assert a.breed == 'Siamese'
	} else {
		assert a.breed == 'Golden Retriever'
	}
	a.breed = 'what??'
	assert a.breed == 'what??'
}

fn test_interface_fields() {
	mut c := Cat{
		breed: 'Persian'
	}
	mut d := Dog{
		breed: 'Labrador'
	}
	use_interface(c)
	use_interface(d)
	mutate_interface(mut c)
	mutate_interface(mut d)
	assert c.breed == 'what??'
	assert d.breed == 'what??'
}

struct Nofun {
	foo fn (int) int
}

interface NofunInterface {
	foo fn (int) int
}

fn my_fn(a int) int {
	assert a == 123
	return a * 2
}

fn test_interface_fn_pointer_fields() {
	nf := NofunInterface(Nofun{my_fn})
	assert nf.foo(123) == 246
}
