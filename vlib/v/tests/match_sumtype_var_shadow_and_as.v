struct Cat{name string}
struct Dog{name string}

type Animal = Cat | Dog

fn main() {
	cat := Cat{name: 'cat'}
	dog := Cat{name: 'dog'}
	mut animal := Animal{}
	
	// test shaddow
	animal = cat
	match animal {
		Cat {
			assert animal.name == cat.name
		}
		else{
			assert false
		}
	}
	// test as
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
