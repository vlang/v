struct Foo {
mut:
	x int
}

struct Bar {
mut:
	y int
}

type Foobar = Bar | Foo

fn test_main() {
	mut bar := Foobar(Bar{
		y: 123
	})
	assert (bar as Bar).y == 123
}
