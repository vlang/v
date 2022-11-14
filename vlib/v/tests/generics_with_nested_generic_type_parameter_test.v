module main

pub struct Randomizer<T> {
}

pub struct Element<T> {
}

fn test_generics_with_nested_generic_type_parameter() {
	a := new_randomizer<int>()
	println(a)
	assert '${a}' == '&Randomizer<int>{}'
}

pub fn new_randomizer<T>() &Randomizer<T> {
	return &Randomizer<T>{}
}

pub fn (mut r Randomizer<T>) add_multiple(elements []Element<T>) {
}
