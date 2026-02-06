struct Human {
	name string
}

fn (h Human) str() string {
	return 'Human: ${h.name}'
}

type Person = Human

fn (h Person) str() string {
	return 'Person: ${h.name}'
}

fn test_type_print() {
	p := Human{
		name: 'Bilbo'
	}
	println(p)
	assert p.str() == 'Human: Bilbo'
}

fn test_person_str() {
	p := Person{
		name: 'Bilbo'
	}
	println(p)
	assert p.str() == 'Person: Bilbo'
}
