struct Foo {
mut:
	x int
}

fn (mut f Foo) set_x(x int) {
	f.x = x
}

fn foo(mut f Foo) {
	f.x = 42
}

fn main() {
	mut f := Foo{0}
	f.set_x(4)
	println(f.x)
	foo(mut f)
	println(f.x)
}
