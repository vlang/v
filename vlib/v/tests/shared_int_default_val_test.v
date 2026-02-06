module main

struct Foo {
mut:
	a shared int = 200
}

fn test_main() {
	x := Foo{}

	rlock x.a {
		k := x.a
		assert k == 200
	}
}
