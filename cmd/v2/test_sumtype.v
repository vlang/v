type Animal = Cat | Dog

struct Cat {
	name string
}

struct Dog {
	name string
	age  int
}

fn make_animal() Animal {
	return Animal(Cat{
		name: 'whiskers'
	})
}

fn main() {
	a := make_animal()
	match a {
		Cat {
			println(a.name)
		}
		Dog {
			println(a.name)
		}
	}
}
