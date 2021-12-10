import math.complex

struct Cat {
	name  string
	breed string
	age   int
}

type Feline = Cat

fn test_offsetof() {
	cat := Cat{
		name: 'Cthulhu'
		breed: 'Great Old One'
		age: 2147483647
	}
	unsafe {
		assert *(&string(&byte(&cat) + __offsetof(Cat, name))) == 'Cthulhu'
		assert *(&string(&byte(&cat) + __offsetof(Cat, breed))) == 'Great Old One'
		assert *(&int(&byte(&cat) + __offsetof(Cat, age))) == 2147483647
	}
}

fn test_offsetof_struct_from_another_module() {
	num := complex.Complex{1.0, 1.0}
	unsafe {
		assert *(&f64(&byte(&num) + __offsetof(complex.Complex, re))) == 1.0
		assert *(&f64(&byte(&num) + __offsetof(complex.Complex, im))) == 1.0
	}
}

fn test_offsetof_alias() {
	fel := Feline{
		name: 'Cthulhu'
		breed: 'Great Old One'
		age: 2147483647
	}
	unsafe {
		assert *(&string(&byte(&fel) + __offsetof(Feline, name))) == 'Cthulhu'
		assert *(&string(&byte(&fel) + __offsetof(Feline, breed))) == 'Great Old One'
		assert *(&int(&byte(&fel) + __offsetof(Feline, age))) == 2147483647
	}
}
