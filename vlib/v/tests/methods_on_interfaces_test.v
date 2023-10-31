struct Cat {
	breed string
}

interface Animal {
	breed string
}

fn (a Animal) info() string {
	return "I'm a ${a.breed} ${typeof(a).name}"
}

fn new_animal(breed string) Animal {
	return &Cat{breed}
}

fn test_methods_on_interfaces() {
	mut a := new_animal('persian')
	assert a.info() == "I'm a persian Animal"
}
