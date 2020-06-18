struct Cat{name string}
struct Dog{name string}

type Animal = Cat | Dog

const (
	cat = Cat{name: 'cat'}
	dog = Dog{name: 'dog'}
)

fn test_shadow() {
	mut animal := Animal{}
	animal = cat
	match animal {
		Cat {
			assert animal.name == cat.name
		}
		else{
			assert false
		}
	}
}

fn test_as() {
	mut animal := Animal{}
	animal = dog
	match animal as animal_kind {
		Dog {
			assert animal_kind.name == dog.name
		}
		else{
			assert false
		}
	}
}