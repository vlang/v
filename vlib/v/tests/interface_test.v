struct Dog {
	breed string
}

struct Cat {
mut:
	breed string
}

fn new_cat(breed string) Cat {
	return Cat{breed}
}

fn (c &Cat) name() string {
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
	return '${pet_name} the ${typeof(c).name}, breed:${c.breed}'
}

fn (mut c Cat) set_breed(new string) {
	c.breed = new
}

// utility function to override default conversion to string, as a sample
fn (c Cat) str() string {
	return 'Custom string conversion for Cat: ${c.breed}'
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
	return '${pet_name} the ${typeof(d).name}, breed:${d.breed}'
}

fn (mut d Dog) set_breed(new string) {
	println('Nah')
}

// do not add to Dog the utility function 'str', so the default one will be used, as a sample

struct Bird {
mut:
	breed string
}

fn (b Bird) speak(s string) {
	println('tweet')
}

fn (b Bird) name() string {
	return b.breed
}

fn (b Bird) name_detailed(pet_name string) string {
	return '${pet_name} the ${typeof(b).name}, breed:${b.breed}'
}

fn (mut b Bird) set_breed(new string) {
	println('canary')
	b.breed = new
}

// do not add to Bird the utility function 'str', so the default one will be used, as a sample

fn is_dog_or_cat(a Animal) bool {
	is_dog := a is Dog
	is_cat := a is Cat
	println('Animal is Dog or Cat: is a Dog: ${is_dog}, is a Cat: ${is_cat}')
	// return is_dog || is_cat
	// shorter syntax
	is_dog_or_cat := if (a is Dog) || (a is Cat) { true } else { false }
	println('Animal is Dog or Cat: ${is_dog_or_cat}')
	return is_dog_or_cat
}

fn is_dog_or_cat_or_bird(a Animal) bool {
	ret := a is Dog || a is Cat || a is Bird
	println('Animal is Dog or Cat or Bird: ${ret}')
	return ret
}

fn perform_speak(a Animal) {
	println('---- ${@FN}, given Animal: ${a} ----')
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
	assert is_dog_or_cat(a)
}

fn perform_speak_on_ptr(a &Animal) {
	println('---- ${@FN}, given &Animal: ${a} ----')
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
	assert is_dog_or_cat(a)
}

fn test_perform_speak() {
	println('---- ${@FN} ----')
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
	perform_speak(new_cat('Persian'))
	perform_speak_on_ptr(cat)
	perform_speak_on_ptr(Cat{
		breed: 'Persian'
	})
	perform_speak_on_ptr(new_cat('Persian'))
	handle_animals([dog, cat])
}

fn change_animal_breed(mut a Animal, new string) {
	a.set_breed(new)
}

fn test_interface_ptr_modification() {
	println('---- ${@FN} ----')
	mut cat := Cat{
		breed: 'Persian'
	}
	change_animal_breed(mut cat, 'Siamese')
	assert cat.breed == 'Siamese'
}

fn perform_name_detailed(a Animal) {
	name_full := a.name_detailed('MyPet')
	println(name_full)
	assert name_full.starts_with('MyPet the Dog') || name_full.starts_with('MyPet the Cat')
}

fn test_perform_name_detailed() {
	println('---- ${@FN} ----')
	dog := Dog{
		breed: 'Labrador Retriever'
	}
	println('Test on Dog: ${dog} ...') // using default conversion to string
	perform_name_detailed(dog)
	cat := Cat{}
	println('Test on empty Cat: ${cat} ...')
	perform_speak(cat)
	println('Test on a Persian Cat: ...')
	perform_speak(Cat{
		breed: 'Persian'
	})
	cat_persian2 := Cat{
		breed: 'Persian'
	}
	println('Test on another Persian Cat: "${cat_persian2}" ...')
	perform_speak(cat_persian2)
	cat_persian2_str := cat_persian2.str()
	println("Persian Cat 2: '${cat_persian2_str}' ...")
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
	println('---- ${@FN} ----')
	f := RegTest{}
	f.register()
	handle_reg(f)
}

interface Speaker2 {
	name() string
	speak()
	return_speaker() Speaker2
mut:
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
	println("i'm ${b.name}")
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

fn return_speaker2(mut sp Speaker2) Speaker2 {
	s := sp.return_speaker()
	s2 := sp.return_speaker2() or { return *sp }
	s.speak()
	s2.speak()
	return s2
}

fn test_interface_returning_interface() {
	mut b := Boss{'bob'}
	assert b.name == 'bob'
	s2 := return_speaker2(mut b)
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
mut:
	set_breed(s string)
}

fn test_interface_array() {
	println('---- ${@FN} ----')
	println('Test on array of animals ...')
	mut animals := []Animal{}
	animals = [Cat{}, Dog{
		breed: 'Labrador Retriever'
	}]
	assert true
	animals << Cat{}
	assert true
	animals << Bird{}
	assert true
	// println('Animals array contains: ${animals.str()}') // explicit call to 'str' function
	println('Animals array contains: ${animals}') // implicit call to 'str' function
	assert animals.len == 4
}

fn test_interface_ptr_array() {
	println('---- ${@FN} ----')
	mut animals := []&Animal{}
	animals = [Cat{}, Dog{
		breed: 'Labrador Retriever'
	}]
	assert true
	animals << Cat{}
	assert true
	animals << Bird{}
	assert true
	// println('Animals array contains: ${animals.str()}') // explicit call to 'str' function
	println('Animals array contains: ${animals}') // implicit call to 'str' function
	assert animals.len == 4
}

fn test_is() {
	println('---- ${@FN} ----')
	dog := Dog{}
	assert is_dog_int(dog) == 1
}

fn is_dog_int(a Animal) int {
	if a is Dog {
		return 1
	} else {
		return 0
	}
}

fn test_is_bool() {
	println('---- ${@FN} ----')
	dog := Dog{}
	assert is_dog(dog) == true
	assert is_dog_or_cat(dog)
	cat := Cat{}
	assert is_dog(cat) == false
	assert is_dog_or_cat(cat)
	bird := Bird{}
	assert is_dog(bird) == false
	assert !is_dog_or_cat(bird)
	assert is_dog_or_cat_or_bird(bird)
}

fn is_dog(a Animal) bool {
	println("Got animal: '${a}'") // implicit call to 'str' function of implementations
	println('with type: ${typeof(a).name}') // get implementation type (if possible)

	// sample (additional checks) here
	is_dog_or_cat := if (a is Dog) || (a is Cat) { true } else { false }
	println('Animal is Dog or Cat: ${is_dog_or_cat}')

	// use/test even another syntax
	if a is Dog {
		return true
	} else {
		return false
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
		else { println('(other)' }
	}
}
*/

interface II {
mut:
	my_field int
}

struct AA {
	BB
}

struct BB {
	pad [10]u8
mut:
	my_field int
}

// the opposite example of interface_mutability_receiver.vv
// 	 related to https://github.com/vlang/v/issues/1081 and https://github.com/vlang/v/issues/7338
//   test example code by https://github.com/nedpals copied and adapted from https://github.com/vlang/v/issues/7338
// we accept immutable get_name even if the interface specified `mut` as it is consistent with function param behavior
struct Dog2 {
pub mut:
	name string
}

fn (d Dog2) get_name() string {
	return d.name
}

// mut get_name might be a bad example
// we try to show that an interface can be more liberal in mut than the implementor,
// while our error check is for the opposite case
interface Animal2 {
mut:
	get_name() string
}

fn get_animal_name(mut a Animal2) string {
	return a.get_name()
}

fn test_aa() {
	println('---- ${@FN} ----')
	mut aa := AA{}
	mut ii := II(aa)
	assert ii.my_field == 0
	aa.my_field = 123
	assert ii.my_field == 123
	ii.my_field = 1234
	assert aa.my_field == 1234
	mut dog := Dog2{'Doggo'}
	println(dog.name)
	println(get_animal_name(mut dog))
}

type Text = string

fn (t Text) display() string {
	return t
}

interface Displayable {
	display() string
}

fn print_displayable(ds ...Displayable) {
	for d in ds {
		println(d.display())
	}
}

fn test_variadic_interface() {
	print_displayable(Text('test'), Text('hehe'))
}
