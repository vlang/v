struct Aa {
}

fn (aa Aa) dump() {
	println(aa)
}

struct Bb {
}

fn (bb Bb) dump() {
	println(bb)
}

struct Foo {
}

fn (foo Foo) set[T](mut obj T) {
	obj.dump()
}

fn test_main() {
	f := Foo{}
	f.set(mut Aa{})
	f.set(mut Bb{})
}
