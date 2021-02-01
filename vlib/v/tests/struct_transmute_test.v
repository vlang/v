struct Foo {
	age int
	name string
}

// different order
struct Bar {
	name string
	age int
}

fn test_update() {
	f := Foo{
		age: 4
		name: 'f'
	}
	b := Bar{
		...f
	}
	assert b == Bar{'f',4}
	b2 := Bar{
		...f
		name: 'b2'
	}
	assert b2.name == 'b2'
	assert b2.age == 4
}

