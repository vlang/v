struct Cat {
	name string
}

struct Dog {
	name string
	age  int
}

struct Bird {
	name    string
	can_fly bool
}

type Animal = Bird | Cat | Dog

fn make_conditional(which int) Animal {
	result := if which == 0 {
		Animal(Cat{
			name: 'whiskers'
		})
	} else {
		Animal(Bird{
			name:    'tweety'
			can_fly: true
		})
	}
	return result
}

fn get_name(a Animal) string {
	match a {
		Cat { return a.name }
		Dog { return a.name }
		Bird { return a.name }
	}
}

fn main() {
	a := make_conditional(0)
	b := make_conditional(1)
	println(get_name(a))
	println(get_name(b))
}
