struct Bar {}

type Fnc = fn ()

struct Foo {
	Bar
	fnc_fn Fnc = voidptr(0)
}

struct App {
mut:
	foo Foo
}

fn test_struct_init_with_complex_fields() {
	mut app := App{}
	println(app)
	ret := '$app'
	assert ret.contains('Bar: Bar{}')
	assert ret.contains('fnc_fn: fn ()')
}
