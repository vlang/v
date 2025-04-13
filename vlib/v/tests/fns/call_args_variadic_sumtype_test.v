type Animal = Dog | Cat

struct Dog {
	name string
}

struct Cat {
	name string
}

fn print_names(animals ...Animal) {
	for animal in animals {
		assert animal.name == 'Kitty'
	}
}

fn test_main() {
	cat := Cat{
		name: 'Kitty'
	}
	mut animals := []Animal{}
	animals << cat
	print_names(animals)
}
