import v.gen.js.tests.hello

enum Test {
	foo = 2
	bar = 5
	baz
}

// TODO Remove `fn main` once vet supports scripts
fn main() {
	mut a := hello.Ccc.a
	a = .b
	a = .c
	println(a)

	mut b := Test.foo
	b = .bar
	println(b)
}
