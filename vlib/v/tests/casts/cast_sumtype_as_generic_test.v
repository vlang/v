module main

struct Chacabum {
	a int
}

type SumType = Chacabum | int | string

fn (a SumType) cast_to[T]() T {
	return a as T
}

fn test_cast_to_generics() {
	assert SumType(1).cast_to[int]() == 1
	assert SumType('la').cast_to[string]() == 'la'
	assert SumType(Chacabum{}).cast_to[Chacabum]() == Chacabum{}
}
