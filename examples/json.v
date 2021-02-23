import json

struct User {
	name string
	age  int
mut:
	is_registered bool
}

fn main() {
	s := '[{ "name":"Frodo", "age":25}, {"name":"Bobby", "age":10}]'
	mut users := json.decode([]User, s) or {
		eprintln('Failed to parse json')
		return
	}
	for user in users {
		println('$user.name: $user.age')
	}
	println('')
	for i, user in users {
		println('$i) $user.name')
		if !user.can_register() {
			println('Cannot register $user.name, they are too young')
		} else {
			users[i].register()
			println('$user.name is registered')
		}
	}
	// Let's encode users again just for fun
	println('')
	println(json.encode(users))
}

fn (u User) can_register() bool {
	return u.age >= 16
}

fn (mut u User) register() {
	u.is_registered = true
}
