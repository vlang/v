struct Dog {
	name string
	age  int
}

struct Cat {
	name string
	age  int
}

interface Animal {
	say(s string)
	greet() int
}

fn (d Dog) say(s string) {
	println('Dog ${d.name}: "${s}"')
}

fn (c Cat) say(s string) {
	println('Cat ${c.name}: "${s}"')
}

fn (d Dog) greet() int {
	d.say('Hello!')
	return d.age
}

fn (c Cat) greet() int {
	c.say('Hello!')
	return c.age
}

fn use(a Animal) {
	if a is Dog {
		println('dog')
	} else if a is Cat {
		println('cat')
	} else {
		println('its a bug!')
	}
}

fn main() {
	use(Dog{'Doggo', 5})
	use(Cat{'Nyancat', 6})
}
