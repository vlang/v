interface Animal {
	say(message string) ?
}

struct Cat {
	name string
}

struct Dog {
	name string
}

pub fn (cat Cat) say(message string) ? {
	println('${message}, meow')
}

pub fn (dog Dog) say(message string) ? {
	println('${message}, wooff')
}

fn test_map_assign_array_of_interface() {
	mut owner_and_animals := map[string][]Animal{}

	owner_and_animals = {
		'John Doe': [
			Cat{
				name: 'Bobby'
			},
			Dog{
				name: 'Hulk'
			},
		]
	}
	println(owner_and_animals)
	assert owner_and_animals['John Doe'].len == 2
	println(owner_and_animals['John Doe'][0])
	assert '${owner_and_animals['John Doe'][0]}'.contains("name: 'Bobby'")
	println(owner_and_animals['John Doe'][1])
	assert '${owner_and_animals['John Doe'][1]}'.contains("name: 'Hulk'")
}
