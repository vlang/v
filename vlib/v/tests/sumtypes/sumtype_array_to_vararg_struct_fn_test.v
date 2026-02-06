struct Foo {
}

type Type = string | int

fn (f Foo) b(t ...Type) int {
	return 0
}

fn (f Foo) a(f2 &Foo, t ...Type) int {
	a := f2.b(t)
	return a
}

fn test_main() {
	f := Foo{}
	t := []Type{}
	assert f.a(&f, ...t) == 0
}
