struct Dog {
	breed string
}

struct Cat {
mut:
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
	return '$pet_name the ${typeof(c).name}, breed:$c.breed'
}

fn (mut c Cat) set_breed(new string) {
	c.breed = new
}

// utility function to override default conversion to string, as a sample
fn (c Cat) str() string {
	return 'Custom string conversion for Cat: $c.breed'
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
	return '$pet_name the ${typeof(d).name}, breed:$d.breed'
}

fn (mut d Dog) set_breed(new string) {
	println('Nah')
}

// do not add to Dog the utility function 'str', so the default one will be used, as a sample
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
	if a is Dog {
		assert name == 'Dog'
		assert a.breed == 'Labrador Retriever' // test smart casting
		println(a.breed)
	}
	println(a.name())
	println('Got animal of type: ${typeof(a).name}') // TODO: get implementation type (if possible)
	assert a is Dog || a is Cat
}

fn perform_speak_on_ptr(a &Animal) {
	a.speak('Hi !')
	assert true
	name := a.name()
	assert name == 'Dog' || name == 'Cat'
	if a is Dog {
		assert name == 'Dog'
	}
	println(a.name())
	println('Got animal of type: ${typeof(a).name}') // TODO: get implementation type (if possible)
	assert a is Dog || a is Cat
}

fn test_perform_speak() {
	dog := Dog{
		breed: 'Labrador Retriever'
	}
	perform_speak(dog)
	perform_speak_on_ptr(dog)
	cat := Cat{
		breed: 'Persian'
	}
	perform_speak(cat)
	perform_speak(Cat{
		breed: 'Persian'
	})
	perform_speak_on_ptr(cat)
	perform_speak_on_ptr(Cat{
		breed: 'Persian'
	})
	handle_animals([dog, cat])
	/*
	f := Foo {
		speaker: dog
	}
	*/
}

fn change_animal_breed(a &Animal, new string) {
	a.set_breed(new)
}

fn test_interface_ptr_modification() {
	mut cat := Cat{
		breed: 'Persian'
	}
	// TODO Should fail and require `mut cat`
	change_animal_breed(cat, 'Siamese')
	assert cat.breed == 'Siamese'
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
	println('Test on Dog: $dog ...') // using default conversion to string
	perform_name_detailed(dog)
	cat := Cat{}
	println('Test on empty Cat: $cat ...')
	perform_speak(cat)
	println('Test on a Persian Cat: ...')
	perform_speak(Cat{
		breed: 'Persian'
	})
	cat_persian2 := Cat{
		breed: 'Persian'
	}
	println('Test on another Persian Cat: "$cat_persian2" ...')
	perform_speak(cat_persian2)
	cat_persian2_str := cat_persian2.str()
	println("Persian Cat 2: '$cat_persian2_str' ...")
	assert cat_persian2_str == 'Custom string conversion for Cat: Persian'
	println('Test (dummy/empty) on array of animals ...')
	handle_animals([dog, cat])
	handle_animals_mutable([dog, cat])
	assert true
}

fn handle_animals(a []Animal) {
}

fn handle_animals_mutable(a []Animal) {
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
	r.register()
}

fn test_register() {
	f := RegTest{}
	f.register()
	handle_reg(f)
}

interface Speaker2 {
	name() string
	speak()
	return_speaker() Speaker2
	return_speaker2() ?Speaker2
}

struct Boss {
mut:
	name string
}

fn (b Boss) name() string {
	return b.name
}

fn (b Boss) speak() {
	println("i'm $b.name")
}

fn (b &Boss) return_speaker() Speaker2 {
	return b
}

fn (mut b Boss) return_speaker2() ?Speaker2 {
	if b.name == 'richard' {
		return none
	}
	b.name = 'boss'
	return b
}

fn return_speaker2(sp Speaker2) Speaker2 {
	s := sp.return_speaker()
	s2 := sp.return_speaker2() or {
		return sp
	}
	s.speak()
	s2.speak()
	return s2
}

fn test_interface_returning_interface() {
	mut b := Boss{'bob'}
	assert b.name == 'bob'
	s2 := return_speaker2(b)
	if s2 is Boss {
		assert s2.name == 'boss'
	}
}

struct Foo {
	animal  Animal
	animals []Animal
}

interface Animal {
	name() string
	name_detailed(pet_name string) string
	speak(s string)
	set_breed(s string)
}

fn test_interface_array() {
	println('Test on array of animals ...')
	mut animals := []Animal{}
	animals = [Cat{}, Dog{
		breed: 'Labrador Retriever'
	}]
	assert true
	animals << Cat{}
	assert true
	// TODO .str() from the real types should be called
	// println('Animals array contains: ${animals.str()}') // explicit call to 'str' function
	// println('Animals array contains: ${animals}') // implicit call to 'str' function
	assert animals.len == 3
}

fn test_interface_ptr_array() {
	mut animals := []&Animal{}
	animals = [Cat{}, Dog{
		breed: 'Labrador Retriever'
	}]
	assert true
	animals << Cat{}
	assert true
	assert animals.len == 3
}

fn test_is() {
	dog := Dog{}
	assert foo2(dog) == 1
}

fn foo2(a Animal) int {
	if a is Dog {
		return 1
	} else {
		return 0
	}
}

fn new_animal() Animal {
	dog := Dog{}
	return dog
}

fn new_animal2() Animal {
	return new_animal()
}

/*
// TODO
fn animal_match(a Animal) {
	match a {
		Dog { println('(dog)') }
		Cat { println('(cat)') }
		else {}
	}
}
*/
