module test

// Test declarations

fn main() {
	p_1 := Programmer{'Programmer', 'Inmutable'}
	mut p_2 := Programmer{'Programmer', 'Mutable'}
}

struct Programmer {
	f_name string
	l_name string
}

fn (p Programmer) drink(cups int) string {
	return 'drink coffee, return program'
}

pub struct Brogrammer {
	f_name string
	life   []Stories
}

pub fn (p Brogrammer) drink(glasses int) string {
	return 'drink beer, return script cluster'
}

struct Stories {}
