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

fn make_cat() Animal {
	return Animal(Cat{
		name: 'whiskers'
	})
}

fn make_bird() Animal {
	return Animal(Bird{
		name:    'tweety'
		can_fly: true
	})
}

fn indirect() Animal {
	a := make_cat()
	return a
}

fn get_name(a Animal) string {
	match a {
		Cat { return a.name }
		Dog { return a.name }
		Bird { return a.name }
	}
}

fn main() {
	a := make_cat()
	b := make_bird()
	c := indirect()
	println(get_name(a))
	println(get_name(b))
	println(get_name(c))
}
