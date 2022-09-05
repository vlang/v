module main

struct Author {
	username string
	name     string
	pass     string
	height   int
	age      int
}

fn cool_author() Author {
	return Author{
		username: 'Terisback'
		name: 'Bob'
		pass: '123456'
		height: 175
		age: 18
	}
}

fn test_struct_init_with_update_expr() {
	mut o := Author{
		...cool_author()
		age: 21
	}
	println(o)
	assert o.username == 'Terisback'
	assert o.name == 'Bob'
	assert o.pass == '123456'
	assert o.height == 175
	assert o.age == 21
}

struct Foo {
	s string
	n int
}

fn test_struct_init_with_update_expr2() {
	f := &Foo{
		s: 'AA'
	}
	b := Foo{
		...*f
		n: 3
	}
	println(b)
	assert b.s == 'AA'
	assert b.n == 3
}
