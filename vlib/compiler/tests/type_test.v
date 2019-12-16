struct Human { name string }

pub fn (h Human) str() string { return 'Human: $h.name' }

type Person Human

fn test_type_print() {
	p := Person{'Bilbo'}
	println(p)
	assert p.str() == 'Human: Bilbo'
}

pub fn (h Person) str() string { return 'Person: $h.name' }

fn test_person_str() {
	p := Person{'Bilbo'}
	println(p)
	assert p.str() == 'Person: Bilbo'
}
