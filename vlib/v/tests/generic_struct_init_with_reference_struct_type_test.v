import datatypes { Stack }

struct Person {
mut:
	name string
	age  int
}

fn test_generic_struct_init_with_reference_struct_type() {
	mut adam := &Person{'Adam', 21}
	println(adam)

	mut people := Stack[&Person]{}
	people.push(adam)
	assert people.len() == 1

	mut top_person := people.pop()!
	top_person.age++
	println(adam)

	assert people.len() == 0
}
