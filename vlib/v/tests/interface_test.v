struct Dog {
	breed string
}

struct Cat {
	breed string
}

fn (mut c Cat) name() string {
	if c.breed != '' {
		assert c.breed == 'Persian'
	}
	return 'Cat'
}

fn (c &Cat) speak(s string) {
	if c.breed != '' {
		assert c.breed == 'Persian'
	}
	assert s == 'Hi !'
	println('meow')
}

fn (c Cat) name_detailed(pet_name string) string {
	return '$pet_name the ${typeof(c)}, breed:${c.breed}'
}

// utility function to convert to string, as a sample
fn (c Cat) str() string {
	return 'Cat: $c.breed'
}

fn (d Dog) speak(s string) {
	assert s == 'Hi !'
	println('woof')
}

fn (d Dog) name() string {
	assert d.breed == 'Labrador Retriever'
	return 'Dog'
}

fn (d Dog) name_detailed(pet_name string) string {
	return '$pet_name the ${typeof(d)}, breed:${d.breed}'
}

// do not add to Dog the utility function 'str', as a sample
fn test_todo() {
	if true {
	} else {
	}
}

fn perform_speak(a Animal) {
	a.speak('Hi !')
	assert true
	name := a.name()
	assert name == 'Dog' || name == 'Cat'
	// if a is Dog {
	// assert name == 'Dog'
	// }
	println(a.name())
}

fn test_perform_speak() {
	dog := Dog{
		breed: 'Labrador Retriever'
	}
	perform_speak(dog)
	cat := Cat{
		breed: 'Persian'
	}
	perform_speak(cat)
	perform_speak(Cat{
		breed: 'Persian'
	})
	handle_animals([dog, cat])
	/*
	f := Foo {
		speaker: dog
	}
	*/
}

fn perform_name_detailed(a Animal) {
	name_full := a.name_detailed('MyPet')
	println(name_full)
	assert name_full.starts_with('MyPet the Dog') || name_full.starts_with('MyPet the Cat')
}

fn test_perform_name_detailed() {
	dog := Dog{
		breed: 'Labrador Retriever'
	}
	println('Test on Dog: $dog ...')
	perform_name_detailed(dog)
	cat := Cat{}
	println('Test on Cat: $cat ...')
	perform_speak(cat)
	println('Test on another Cat: ...')
	perform_speak(Cat{
		breed: 'Persian'
	})
	println('Test (dummy/empty) on array of animals ...')
	handle_animals([dog, cat])
}

fn handle_animals(a []Animal) {
}

interface Register {
	register()
}

struct RegTest {
	a int
}

fn (f RegTest) register() {
}

fn handle_reg(r Register) {
}

fn test_register() {
	f := RegTest{}
	f.register()
	handle_reg(f)
}

interface Speaker2 {
	name() string
	speak()
}

struct Foo {
	animal  Animal
	animals []Animal
}

interface Animal {
	name() string
	name_detailed(pet_name string) string
	speak(s string)
}

fn test_interface_array() {
	println('Test on array of animals ...')
	mut animals := []Animal{}
	animals = [Cat{}, Dog{
		breed: 'Labrador Retriever'
	}]
	animals << Cat{}
	assert true
	// TODO .str() from the real types should be called
	// println('Animals array contains: ${animals.str()}') // explicit call to 'str' function
	// println('Animals array contains: ${animals}') // implicit call to 'str' function
	assert animals.len == 3
}
