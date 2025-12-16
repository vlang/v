module main

struct Foo {
mut:
	a shared int
}

fn test_main() {
	mut x := Foo{}
	lock x.a {
		x.a = 100
	}
	rlock x.a {
		k := x.a
		// can't use assert x.a == 100, to be fixed
		assert k == 100
	}
}
