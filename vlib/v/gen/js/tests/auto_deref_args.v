struct Foo {
	field &int
}

fn bar(x int) {
	println(x)
}

fn main() {
	x := 4
	foo := Foo{&x}
	bar(foo.field)
}
