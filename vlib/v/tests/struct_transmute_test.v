struct Foo {
	age  int
	name string
}

// different order
struct Bar {
	name string
	age  int
}

fn test_order() {
	f := Foo{
		age: 4
		name: 'f'
	}
	b := Bar{
		...f
	}
	assert b == Bar{'f', 4}
	b2 := Bar{
		...f
		name: 'b2'
	}
	assert b2.name == 'b2'
	assert b2.age == 4
}

struct Qux {
	name  string
	age   int
	extra bool
}

fn test_extra() {
	f := Foo{4, 'f'}
	q := Qux{
		...f
		extra: true
	}
	assert q == Qux{'f', 4, true}
}
