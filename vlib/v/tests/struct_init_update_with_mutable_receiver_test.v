module main

struct Person {
	name string
	age  int
}

fn (mut p Person) change(name string) {
	p = Person{
		...p
		name: name
	}
}

fn test_struct_init_update_with_mutable_receiver() {
	mut p := Person{'bob', 22}
	p.change('tom')
	println(p)
	assert p.name == 'tom'
	assert p.age == 22
}
