module main

struct ABC {
mut:
	s string
}

fn test_shared_optional() {
	shared abc := foo() or { panic('scared') }
	rlock abc {
		println(abc)
		assert abc.s == 'hello'
	}
}

fn foo() ?ABC {
	mut a := ABC{}
	a.bar()?
	return a
}

fn (mut a ABC) bar() ? {
	a.s = 'hello'
	println(a.s)
}
